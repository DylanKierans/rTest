# filename: r_fork_functions.R
# description: Unique functions wrappers for functions such as makeForkCluster which
#   spawn new forked R procs. Extra work required to duplicate zeromq objects safely 
#   across multiple proces.


#' get_fork_function_list
#' @description Returns list of known functions which R uses to fork procs
get_fork_function_list <- function() {
    func_list <- c()
    if (R.utils::isPackageLoaded("parallel")){
        tmp_func_list <- c(parallel::makeForkCluster)
        func_list <- append(func_list, tmp_func_list)
    }
    func_list
}

#' get_psock_function_list
#' @description Returns list of known functions which R uses to fork procs
get_psock_function_list <- function() {
    func_list <- c()
    if (R.utils::isPackageLoaded("parallel")){
        tmp_func_list <- c(parallel::makePSOCKcluster)
        func_list <- append(func_list, tmp_func_list)
    }
    func_list
}

#' get_end_fork_function_list
#' @description Returns list of known functions which R uses to fork procs
get_end_fork_function_list <- function() {
    func_list <- c()
    if (R.utils::isPackageLoaded("parallel")){
        tmp_func_list <- c(parallel::stopCluster)
        func_list <- append(func_list, tmp_func_list)
    }
    func_list
}

#' is_cluster_function
#' @description Returns bool for if function listed as for type of {fork, psock} function
#' @param func_ptr Function to check
#' @param fork_cluster True if checking for fork function, else check for psock
#' @return Boolean - true if fork/psock function, else false
is_cluster_function <- function(func_ptr, fork_cluster=F) {
    if (fork_cluster){
        cluster_func_list <- get_fork_function_list()
    } else {
        cluster_func_list <- get_psock_function_list()
    }

    ## Test if cluster function, uses different wrapper
    res <- FALSE
    for (cluster_func in cluster_func_list){
        if (identical(cluster_func, func_ptr)){
            res <- TRUE
        }
    }

    res
}

#' is_end_fork_function
#' @description Returns bool for if function listed as for type of end fork function
#' @param func_ptr Function to check
#' @return Boolean - true if end fork function, else false
is_end_fork_function <- function(func_ptr) {
    flag_end_fork_function <- FALSE
    for (end_fork_func in get_end_fork_function_list()){
        if (identical(end_fork_func, func_ptr)){
            flag_end_fork_function <- TRUE
        }
    }
    flag_end_fork_function
}


#' body_default_function
#' @description Returns instrumented body for default functions
#' @param func_ptr Pointer to function
#' @param regionRef OTF2 regionRef for function
#' @return type(as.call) Updated function body
body_default_function <- function(func_ptr, regionRef){
    .wrapper_expression = do.call('substitute', list( 
        get_wrapper_expression()[[1]],
        list(X_regionRef_X=regionRef)
    ))

    .wrapper_expression = as.expression(.wrapper_expression)

    ## Copy and wrap function definition
    orig_func_body <- body(func_ptr)[1:length(body(func_ptr))]
    as.call(c(as.name("{"), .wrapper_expression, orig_func_body))
}

#' body_cluster_function
#' @description Returns instrumented body for functions of type fork/psock
#' @param func_ptr Pointer to function
#' @param regionRef OTF2_regionRef - regionRef for function
#' @param fork_cluster Boolean - True if fork cluster, else psock
#' @return type(as.call) Updated function body
body_cluster_function <- function(func_ptr, regionRef, fork_cluster=F)
{

    if (fork_cluster){ # FORK
        wrapper_expression = get_fork_wrapper_expression()
    } else { # PSOCK
        wrapper_expression = get_psock_wrapper_expression()
    }

    entry_exp = wrapper_expression$entry;
    entry_exp = do.call('substitute', list( 
        entry_exp[[1]],
        list(X_regionRef_X=regionRef)
    ))
    entry_exp = as.expression(entry_exp)

    exit_exp = wrapper_expression$exit;
    exit_exp = do.call('substitute', list( 
        exit_exp[[1]],
        list(X_regionRef_X=regionRef)
    ))
    exit_exp = as.expression(exit_exp)

    ## Copy and wrap function definition
    orig_func_body <- body(func_ptr)[1:length(body(func_ptr))-1]
    orig_func_ret <- body(func_ptr)[[length(body(func_ptr))]] # Isolate final line

    as.call(c(as.name("{"), entry_exp, orig_func_body, exit_exp, orig_func_ret))

}


#' body_end_fork_function
#' @description Returns instrumented body for functions of type end fork
#' @param func_ptr Pointer to function
#' @param regionRef OTF2 regionRef for function
#' @return type(as.call) Updated function body
body_end_fork_function <- function(func_ptr, regionRef){

    wrapper_expression <- get_end_fork_wrapper_expression()
    entry_exp = wrapper_expression$entry;
    entry_exp = do.call('substitute', list( 
        entry_exp[[1]],
        list(X_regionRef_X=regionRef)
    ))
    entry_exp = as.expression(entry_exp)

    exit_exp = wrapper_expression$exit;
    exit_exp = do.call('substitute', list( 
        exit_exp[[1]],
        list(X_regionRef_X=regionRef)
    ))
    exit_exp = as.expression(exit_exp)

    ## Copy and wrap function definition
    orig_func_body <- body(func_ptr)[1:length(body(func_ptr))]

    ## Fix for exit_exp not getting reached at end of function!
    as.call(c(as.name("{"), entry_exp, exit_exp, orig_func_body))
}

#' get_new_function_body
#' @description Returns instrumented body for function type {fork, end fork, default}
#' @param func_ptr Pointer to function
#' @param func_name Char[] - Name of function, only used for debugging
#' @param regionRef Int - OTF2 regionRef for function
#' @return type(as.call) Updated function body
get_new_function_body <- function(func_ptr, func_name, regionRef)
{
    # Expression and body usage taken from: https://stackoverflow.com/a/31374476
    if (is_cluster_function(func_ptr, fork_cluster=F)){
        if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: PSOCK function `", func_name, "`"))
        new_body <- body_cluster_function(func_ptr, regionRef, fork_cluster=F)
    } else if (is_cluster_function(func_ptr, fork_cluster=T)){
        if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: Fork function `", func_name, "`"))
        new_body <- body_cluster_function(func_ptr, regionRef, fork_cluster=T)
    } else if (is_end_fork_function(func_ptr)) {
        if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: End fork function `", func_name, "`"))
        new_body <- body_end_fork_function(func_ptr, regionRef)
    } else { # Default wrapper
        if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: Default function `", func_name, "`"))
        new_body <- body_default_function(func_ptr, regionRef)
    }
    new_body
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
            print(paste0("makeForkCluster nnodes: ", nnodes))

            # Set r proc IDs - note master=0
            clusterApply(cl, 1:as.integer(nnodes), function(x){ set_locationRef(x); }) 

            # Reopen sockets on all procs
            open_EvtWriterSocket_client();
            clusterEvalQ(cl, {open_EvtWriterSocket_client()});

            # Renable instrumentation if necessary
            if (INSTRUMENTATION_ENABLED_BEFORE){
                instrumentation_enable(flag_ignore_depth=TRUE);
                clusterEvalQ(cl, instrumentation_enable(flag_ignore_depth=TRUE));

                pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH - 1
                if ( pkg.env$FUNCTION_DEPTH < pkg.env$MAX_FUNCTION_DEPTH){
                    evtWriter_Write_client(X_regionRef_X,F)
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
            ## Append to depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1

            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) 
            {
                ## zmq version - OTF2 Event
                evtWriter_Write_client(X_regionRef_X,T)
            }

            instrumentation_disable(flag_ignore_depth=TRUE)
        }

        # Close socket on master before forking
        close_EvtWriterSocket_client()

    } )

    list(entry = entry_exp, exit = exit_exp)
}

#' get_end_fork_wrapper_expression
#' @description Returns wrapper expression
get_end_fork_wrapper_expression <- function() {
    entry_exp <- expression( {  ; # Sneaky ; here for debugging
        # Save instrumentation state
        INSTRUMENTATION_ENABLED_BEFORE <- is_instrumentation_enabled()

        if (pkg.env$INSTRUMENTATION_ENABLED) {
            # Append to depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1

            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) {
                evtWriter_Write_client(X_regionRef_X,T) # OTF2 Enter event
            }

            ## Disable instrumentation on all procs
            clusterEvalQ(cl, { instrumentation_disable(flag_ignore_depth=TRUE) } )
            instrumentation_disable(flag_ignore_depth=TRUE)

        }

        # Close sockets on all procs clientside
        close_EvtWriterSocket_client()
        clusterEvalQ(cl, { close_EvtWriterSocket_client() })
    } )

    exit_exp <- expression( {
        on.exit( { ; # Sneaky ; here for debugging
            # Reopen sockets on Master clientside
            open_EvtWriterSocket_client()

            # Restore instrumentation state
            if (INSTRUMENTATION_ENABLED_BEFORE){
                instrumentation_enable(flag_ignore_depth=TRUE)

                # Deduct from depth counter
                pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH -  1

                if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) {
                    evtWriter_Write_client(X_regionRef_X,F) # OTF2 Leave event
                }

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

        ## DEBUGGING
        if (pkg.env$INSTRUMENTATION_ENABLED) {
            ## Append to depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1

            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) {
                evtWriter_Write_client(X_regionRef_X,T)
            }

            instrumentation_disable(flag_ignore_depth=TRUE)
        }
    } )

    exit_exp <- expression( { 
        on.exit({
            nnodes <- length(names)

            ## DEBUGGING
            #print(paste0("makePSOCKcluster names: ", names))
            #print(paste0("makePSOCKcluster nnodes: ", nnodes))
            #clusterEvalQ(cl, { print(paste0("FORK makeCluster - pid: ", get_pid(), ", tid: ", get_tid(), ", locationRef id: ", get_locationRef())) })

            # Import required packages on slave
            master_init_slave(cl)

            # Set r proc IDs - note master=0 
            # WARNING: muster go after master_init_slave(), after importing function
            clusterApply(cl, 1:nnodes, function(x){ set_locationRef(x); }) 

            ## DEBUGGING
            clusterEvalQ(cl, print(.packages()))
            print(.packages())

            # YOU ARE HERE #1
            ## DEBUGGING - Check if func_list are equal or nej

            # Reopen sockets on all procs
            clusterEvalQ(cl, {open_EvtWriterSocket_client()});

            # Instrument all functions on slave
            #print("Starting signal new procs to server")
            get_regionRef_array_master(nnodes)
            #print("Ending signal new procs to server")
            #print("Starting insert_instrumentation on slaves")
            clusterEvalQ(cl, { instrument_all_functions(flag_slave_proc=T); })
            #print("Ending insert_instrumentation")

            # Renable instrumentation if necessary
            if (INSTRUMENTATION_ENABLED_BEFORE){
                instrumentation_enable(flag_ignore_depth=TRUE);
                clusterEvalQ(cl, instrumentation_enable(flag_ignore_depth=TRUE));

                pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH - 1
                clusterEvalQ(cl, pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH-1);

                if ( pkg.env$FUNCTION_DEPTH < pkg.env$MAX_FUNCTION_DEPTH){
                    evtWriter_Write_client(X_regionRef_X,F)
                }
            }

            # Update max number of R procs if needed
            set_maxUsedLocationRef_client(nnodes+1);
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
    # Creates import command, awkward but works
    package_list <- .packages()
    pkg_cmd <- ""
    for (pak in package_list) {
        tmp <- paste0("library('", pak, "'); ")

        # Ordered such that loaded in order; base, dependencies, packages
        # Important to preserver package order for func_list
        pkg_cmd <- paste0(tmp, pkg_cmd)
    }

    ## DEBUGGING
    print("package_list: ")
    print(pkg_cmd)

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
        unlock_envs("rTrace")
        for(n in vars) { assign(n, get(n, .GlobalEnv), pkg.env) }
        lock_envs("rTrace")
    })

    # Export all user functions to slave
    user_func_list <- get_user_function_list()
    parallel::clusterExport(cl, names(user_func_list), envir=.GlobalEnv)
    
    ## DEBUGGING
    #print(names(user_func_list))

    ## Assign regionRef_array on slave
    #parallel::clusterEvalQ(cl, assign_regionRef_array_slave(
    #        sum(get_num_functions(flag_user_functions = T))
    #))

}

