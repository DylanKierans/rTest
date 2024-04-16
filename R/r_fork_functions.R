# filename: r_fork_functions.R
# description: Unique functions wrappers for functions such as makeForkCluster which
#   spawn new forked R procs. Extra work required to duplicate zeromq objects safely 
#   across multiple proces.

#' body_default_function
#' @description Returns instrumented body for default functions
#' @param func_ptr function - Pointer to function
#' @param evtID int - ID number for function (func_index)
#' @return type(as.call) Updated function body
body_default_function <- function(func_ptr, evtID){
    .wrapper_expression = do.call('substitute', list( 
        get_wrapper_expression()[[1]],
        list(X_evtID_X=evtID)
    ))

    .wrapper_expression = as.expression(.wrapper_expression)

    ## Copy and wrap function definition
    orig_func_body <- body(func_ptr)[1:length(body(func_ptr))]
    as.call(c(as.name("{"), .wrapper_expression, orig_func_body))
}

#' body_cluster_function
#' @description Returns instrumented body for functions of type fork/psock
#' @param func_ptr function - Pointer to function
#' @param evtID int - ID number for function (func_index)
#' @param fork_cluster Boolean - True if fork cluster, else psock
#' @return type(as.call) Updated function body
body_cluster_function <- function(func_ptr, evtID, fork_cluster=F)
{

    if (fork_cluster){ # FORK
        wrapper_expression = get_fork_wrapper_expression()
    } else { # PSOCK
        wrapper_expression = get_psock_wrapper_expression()
    }

    entry_exp = wrapper_expression$entry;
    entry_exp = do.call('substitute', list( 
        entry_exp[[1]],
        list(X_evtID_X=evtID)
    ))
    entry_exp = as.expression(entry_exp)

    exit_exp = wrapper_expression$exit;
    exit_exp = do.call('substitute', list( 
        exit_exp[[1]],
        list(X_evtID_X=evtID)
    ))
    exit_exp = as.expression(exit_exp)

    ## Copy and wrap function definition
    orig_func_body <- body(func_ptr)[1:length(body(func_ptr))-1]
    orig_func_ret <- body(func_ptr)[[length(body(func_ptr))]] # Isolate final line

    as.call(c(as.name("{"), entry_exp, orig_func_body, exit_exp, orig_func_ret))

}


#' body_end_cluster_function
#' @description Returns instrumented body for functions of type end fork
#' @param func_ptr function - Pointer to function
#' @param evtID int - ID number for function (func_index)
#' @return type(as.call) Updated function body
body_end_cluster_function <- function(func_ptr, evtID){

    wrapper_expression <- get_end_cluster_wrapper_expression()
    entry_exp = wrapper_expression$entry;
    entry_exp = do.call('substitute', list( 
        entry_exp[[1]],
        list(X_evtID_X=evtID)
    ))
    entry_exp = as.expression(entry_exp)

    exit_exp = wrapper_expression$exit;
    exit_exp = do.call('substitute', list( 
        exit_exp[[1]],
        list(X_evtID_X=evtID)
    ))
    exit_exp = as.expression(exit_exp)

    ## Copy and wrap function definition
    orig_func_body <- body(func_ptr)[1:length(body(func_ptr))]

    ## Fix for exit_exp not getting reached at end of function!
    as.call(c(as.name("{"), entry_exp, exit_exp, orig_func_body))
}

#' get_new_function_body
#' @description Returns instrumented body for function type {fork, end fork, default}
#' @param func_ptr function - Pointer to function
#' @param func_name Char[] - Name of function, only used for debugging
#' @param evtID int - ID number for function (func_index)
#' @return type(as.call) Updated function body
get_new_function_body <- function(func_ptr, func_name, evtID)
{
    # Expression and body usage taken from: https://stackoverflow.com/a/31374476

    # Create lists for non-standard parallel functions, with specialized wrappers
    end_cluster_function_list <- c()
    fork_cluster_function_list <- c()
    psock_cluster_function_list <- c()
    if (R.utils::isPackageLoaded("parallel")){
        end_cluster_function_list <- append(end_cluster_function_list, c(parallel::stopCluster))
        fork_cluster_function_list <- append(fork_cluster_function_list, c(parallel::makeForkCluster))
        psock_cluster_function_list <- append(psock_cluster_function_list, c(parallel::makePSOCKcluster))
    }

    # If get_end_fork_function
    for (end_cluster_function in end_cluster_function_list){
        if (identical(end_cluster_function, func_ptr)){
            if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: End fork function `", func_name, "`"))
            new_body <- body_end_cluster_function(func_ptr, evtID)
            return(new_body)
        }
    }

    ## If creates forked cluster
    for (fork_cluster_function in fork_cluster_function_list){
        if (identical(fork_cluster_function, func_ptr)){
            if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: Fork function `", func_name, "`"))
            new_body <- body_cluster_function(func_ptr, evtID, fork_cluster=T)
            return(new_body)
        }
    }

    ## If creates psock cluster
    for (psock_cluster_function in psock_cluster_function_list){
        if (identical(psock_cluster_function, func_ptr)){
            if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: PSOCK function `", func_name, "`"))
            new_body <- body_cluster_function(func_ptr, evtID, fork_cluster=F)
            return(new_body)
        }
    }

    ## Else default function
    if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: Default function `", func_name, "`"))
    new_body <- body_default_function(func_ptr, evtID)
    return(new_body)
}


#' get_fork_wrapper_expression
#' @description Returns wrapper expression
#'  Split between start and end because makeForkCluster contains line `on.exit(<cmd>)`, without 
#'  arg `add=TRUE`. Overwrites my commands as a result. Also have to manually add final return line
#'  but this could be generalized better by splitting original function body
get_fork_wrapper_expression <- function() {
    exit_exp <- expression( { 
        on.exit({
            ## DEBUGGING
            #print(paste0("makeForkCluster nnodes: ", nnodes))

            # Set r proc IDs - note master=0
            clusterApply(cl, 1:as.integer(nnodes), function(x){ set_locationRef(x); }) 

            # Reopen sockets on all procs
            init_zmq_client();
            clusterEvalQ(cl, {init_zmq_client()});

            # Renable instrumentation if necessary
            if (INSTRUMENTATION_ENABLED_BEFORE){
                instrumentation_enable();
                clusterEvalQ(cl, instrumentation_enable(flag_reset_depth=TRUE));

                pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH - 1
                if ( pkg.env$FUNCTION_DEPTH < pkg.env$MAX_FUNCTION_DEPTH){
                    evtWriter_Write_client(X_evtID_X,F)
                }
            }

            # Update max number of R procs if needed
            set_maxUsedLocationRef_client(nnodes+1);
        }, add=TRUE)
    })

    entry_exp <- expression( { 
        # Save instrumentation state
        INSTRUMENTATION_ENABLED_BEFORE <- is_instrumentation_enabled()

        if (pkg.env$INSTRUMENTATION_ENABLED) {
            NULL
            ## Increment depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1

            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) 
            {
                ## zmq version - OTF2 Event
                evtWriter_Write_client(X_evtID_X,T)
            }

            instrumentation_disable(flag_check_depth=F)
        }

        # Close socket on master before forking
        finalize_zmq_client()
    } )

    list(entry = entry_exp, exit = exit_exp)
}

#' get_end_cluster_wrapper_expression
#' @description Returns wrapper expression
get_end_cluster_wrapper_expression <- function() {
    entry_exp <- expression( {
        # Save instrumentation state
        INSTRUMENTATION_ENABLED_BEFORE <- is_instrumentation_enabled()

        if (pkg.env$INSTRUMENTATION_ENABLED) {
            # Increment depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1
            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) {
                evtWriter_Write_client(X_evtID_X,T) # OTF2 Enter event
            }

            ## Disable instrumentation on all procs
            clusterEvalQ(cl, { instrumentation_disable(flag_update_measurement=F) } )
            instrumentation_disable(flag_check_depth=F)
        }

        # Close sockets on all procs clientside
        if (pkg.env$INSTRUMENTATION_INIT){
            # End slave placeholder event
            clusterEvalQ(cl, { evtWriter_proc_client(F) })

            finalize_zmq_client()
            clusterEvalQ(cl, { finalize_zmq_client() })
        }
    } )

    exit_exp <- expression( {
        on.exit( {
            if (pkg.env$INSTRUMENTATION_INIT){
                # Reopen sockets on Master clientside
                init_zmq_client()
            }

            # Restore instrumentation state
            if (INSTRUMENTATION_ENABLED_BEFORE){
                instrumentation_enable()
                if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) {
                    evtWriter_Write_client(X_evtID_X,F) # OTF2 Leave event
                }
                # Decrement depth counter
                pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH -  1
            }
        }, add=TRUE)
    } )

    list(entry=entry_exp, exit=exit_exp)
}




#' get_psock_wrapper_expression
#' @description List containing wrapper expression to insert at start and end of makePSOCKcluster and similar functions
#' @return Returns wrapper expression
get_psock_wrapper_expression <- function() {
    entry_exp <- expression( { 
        # Save instrumentation state
        INSTRUMENTATION_ENABLED_BEFORE <- is_instrumentation_enabled()

        if (pkg.env$INSTRUMENTATION_ENABLED) {
            ## Increment depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1
            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH) {
                evtWriter_Write_client(X_evtID_X,T)
            }
            instrumentation_disable(flag_check_depth=F)
        }
    } )

    exit_exp <- expression( { 
        on.exit({
            nnodes <- length(names)

            ## DEBUGGING
            print(paste0("makePSOCKcluster nnodes: ", nnodes))
            #clusterEvalQ(cl, { print(paste0("FORK makeCluster - pid: ", get_pid(), ", tid: ", get_tid(), ", locationRef id: ", get_locationRef())) })

            # Import required packages on slave
            master_init_slave(cl)

            # Set r proc IDs - note master=0 
            # WARNING: must go after master_init_slave(), after importing function
            clusterApply(cl, 1:nnodes, function(x){ set_locationRef(x); }) 

            # Instrument all functions on slave
            if ( pkg.env$INSTRUMENTATION_INIT ) {
                # Reopen sockets on all procs
                clusterEvalQ(cl, {init_zmq_client()});

                ## TODO: TESTING
                #future::plan(cluster, workers = cl)
                #for (i in 1:nodes){
                #    f <- future(instrument_all_functions(flag_slave_proc=T));
                #}

                # Assign regionRef_array on all slaves
                clusterEvalQ(cl, { evtWriter_proc_client(T); }) # start placeholder events for new procs
                clusterEvalQ(cl, { instrument_all_functions(flag_slave_proc=T); })

                # Update max number of R procs if needed
                set_maxUsedLocationRef_client(nnodes+1);
            }

            # Renable instrumentation if necessary
            if (INSTRUMENTATION_ENABLED_BEFORE){
                instrumentation_enable();
                clusterEvalQ(cl, instrumentation_enable(flag_reset_depth=TRUE));

                if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH){
                    evtWriter_Write_client(X_evtID_X,F)
                }
                # Decrement depth
                pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH - 1

            }
        }, add=TRUE)
    })

    list(entry = entry_exp, exit = exit_exp)
}

#######################################################################
# SECTION - PSOCK Cluster stuff
#######################################################################

#' master_init_slave
#' @param cl Cluster object
master_init_slave <- function(cl) {
    # Creates library export command, awkward but works
    package_list <- .packages()
    pkg_cmd <- ""
    for (pak in package_list) {
        tmp <- paste0("library('", pak, "'); ")

        # Ordered such that loaded in order; base, dependencies, packages
        # Important to preserver package order for func_list
        pkg_cmd <- paste0(tmp, pkg_cmd)
    }

    # Exports libraries
    parallel::clusterExport(cl, c("pkg_cmd"), envir=environment())
    parallel::clusterEvalQ(cl, eval(parse(text = pkg_cmd)))

    # Export rTrace variables 
    vars <- c( "INSTRUMENTATION_INIT", "INSTRUMENTATION_ENABLED",
        "INSTRUMENTATION_STATUS_SAVED", "MAX_FUNCTION_DEPTH",
        "FUNCTION_DEPTH", "UNLOCK_ENVS", "PRINT_INSTRUMENTS",
        "PRINT_SKIPS", "PRINT_FUNC_INDEXES"
    )
    parallel::clusterExport(cl, c("vars"), envir=environment())
    parallel::clusterExport(cl, vars, envir=pkg.env)
    parallel::clusterEvalQ(cl, {
        FUNCTION_DEPTH <- 0 # Reset function depth
        unlock_envs("rTrace")
        for(n in vars) { assign(n, get(n, .GlobalEnv), pkg.env) }
        lock_envs("rTrace")
    })

    # Export all user functions to slave
    user_func_list <- get_user_function_list()
    parallel::clusterExport(cl, names(user_func_list), envir=.GlobalEnv)
    
}

