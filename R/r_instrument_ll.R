# @file r_instrument_ll.R
# @todo - R error checking
# @todo - C error checking
# @todo - Make sure PROFILE_INSTRUMENTATION_DF exists
# @todo - Isolate .wrapper_expression
# @todo - ensure parent thread with isForkedChild()
# @todo - reduce function exception list
# @todo - instrument_all_functions merge debug flags


#######################################################################
# section - Update for ZMQ
#######################################################################
## @TODO : zmq this
## create_otf2_event
## @description Creates stringRef and regionRef for func_name
## @param func_name String - Name of function
## @return regionRef Int - Index of stringRef for function
#create_otf2_event <- function(func_name) {
#    stringRef <- globalDefWriter_WriteString(func_name)
#    regionRef <- globalDefWriter_WriteRegion(stringRef)
#    regionRef
#}

# @TODONE: zmq this
#' get_wrapper_expression
#' @description Returns wrapper expression
get_wrapper_expression <- function() {
    wrapper_expression <- expression(
    { 

        if (pkg.env$INSTRUMENTATION_ENABLED) {
            NULL
            ## Append to depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1
            on.exit( pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH -  1, add=TRUE )

            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) 
            {
#                ## OTF2 Event
#                evtWriter_Write(X_regionRef_X,T)
#                on.exit(evtWriter_Write(X_regionRef_X,F), add=TRUE)

                ## zmq version - OTF2 Event
                evtWriter_Write_client(X_regionRef_X,T)
                on.exit(evtWriter_Write_client(X_regionRef_X,F), add=TRUE)
            }

        }
    }
    )

    wrapper_expression
}


########################################################################
# SECTION - LOW LEVEL INSTRUMENTATION
########################################################################

#' insert_instrumentation
#' @description Insert instrumentation prefix for package_name:::func
#' @param func Object - Pointer to function closure to update
#' @param func_name String - function name
#' @param func_index Integer - index of function in function_list in PROFILE_INSTRUMENTATION_DF
#' @param regionRef Integer - OTF2 regionRef index
#' @param package_name String - package name
#' @param flag_user_function Boolean - TRUE if instrumenting user functoin
#' @param env_is_locked Boolean - TRUE if function name-/package- space is locked
#' @export
insert_instrumentation <- function(func, func_name, func_index, regionRef, package_name, flag_user_function=FALSE, env_is_locked=TRUE) {
    
    ## DEBUGGING
    #print(paste0("Client - func_name: ", func_name, ", regionRef: ", regionRef))
    if (func_name=="test"){print(paste0("test_index: ", func_index, ", regionRef: ", regionRef));}
    

    # Taken from: https://stackoverflow.com/a/31374476
    .wrapper_expression = do.call('substitute', list( 
        get_wrapper_expression()[[1]],
        list(X_regionRef_X=regionRef)
    ))
    .wrapper_expression = as.expression(.wrapper_expression)

    ## Copy and wrap function definition
    orig_func_body <- body(func)[1:length(body(func))]
    body(func) <- as.call(c(as.name("{"), .wrapper_expression, orig_func_body))

    ## DEBUGGING: Comment out to disable compiling for testing
    #func <- compiler::cmpfun(func) 

    ## Replace function in package and namespace
    if (flag_user_function) {
        replace_user_function(func, func_name, package_name)
    } else {
        replace_function(func, func_name, package_name, env_is_locked=env_is_locked)
    }

}

#' replace_user_function
#' @description Replace user function definition
#' @param new_func Function, function object with new definition
#' @param func_name String, name of function
#' @param package_name String, name of package function
#' @param env Environment, environment function exists in (default .GlobalEnv)
replace_user_function <- function(new_func, func_name, package_name, env=.GlobalEnv) {
    assign(func_name, new_func, envir = env)
}

#' replace_function
#' @description Replace library function definition in package- and name-space
#' @param new_func Function object with new definition
#' @param func_name String, name of function
#' @param package_name String, name of package function
#' @param env_is_locked Boolean - TRUE if function name-/package- space is locked
replace_function <- function(new_func, func_name, package_name, env_is_locked=TRUE) {

    if (env_is_locked) {
        # namespace
        env <- asNamespace(package_name)
        rlang::env_unlock(env = env)
        rlang::env_binding_unlock(env = env) 
        assign(func_name, new_func, envir = env)
        rlang::env_binding_lock(env = env)
        rlang::env_lock(env)

        # packagespace
        env <- as.environment(paste0("package:",package_name) )
        rlang::env_unlock(env = env)
        rlang::env_binding_unlock(env = env) 
        assign(func_name, new_func, envir = env)
        rlang::env_binding_lock(env = env)
        rlang::env_lock(env)
    }
    else {
        # namespace
        env <- asNamespace(package_name)
        assign(func_name, new_func, envir = env) 

        # packagespace
        env <- as.environment(paste0("package:",package_name) )
        assign(func_name, new_func, envir = env)
    }
}



#######################################################################
# section
#######################################################################
  
#' get_function_list
#' @description Get all functions in package (else all if no arg given)
#' @param packages String[] - Full name of package if specific package, else all available packages
#' @param flag_full_name Boolean - TRUE if full name of package passed using input packages (eg "package:stats")
#' @param flag_debug Boolean - Enable debugging statements
#' @return func_list Function[] - List of function ptrs
#' @export
get_function_list <- function(packages=NULL, flag_full_name=FALSE, flag_debug=FALSE) {
    func_list <- list()     # list of function pointers

    if (is.null(packages)){ packages <- .packages() }
    if (!flag_full_name) { packages <- paste0("package:", packages) }

    if (flag_debug) { ## Debugging statements
        print("########## PROFILE_GET_FUNCTION_LIST() ############")
    }

    for (package in packages) {
        objs <- mget(ls(package), inherits=T)
        funcs <- Filter(is.function, objs)
        func_list <- append(func_list, funcs)

        if (flag_debug) { ## Debugging statements
            print(paste0("Total: ", length(func_list), " package: ", length(funcs), " package_name:", package))
            print(names(funcs))
        }
    }
    func_list 
}

#' get_user_function_list
#' @description Get all user-defined functions in global_env
#' @param flag_debug Boolean - Enable debug output
#' @return user_func_list Function[] - List of user-defined functions
#' @export
get_user_function_list <- function(flag_debug=FALSE) {
    objs <- mget(ls(envir=.GlobalEnv), inherits=T)
    user_func_list <- Filter(is.function, objs)

    if (flag_debug) { ## Debugging statements
        print(paste0("Number user functions: ", length(user_func_list)))
        print("List of user functions: ")
        print(names(user_func_list))
    }
    user_func_list
}

#' get_num_functions
#' @description Find total number of loaded packages and functions
#' @param flag_user_functions Boolean - TRUE if also flagging user functions
#' @param debug_flag Boolean - TRUE to print debug information
#' @return Int[] Number of functions per package
#' @export
get_num_functions <- function(flag_user_functions=FALSE, debug_flag=FALSE) {

    packages <- .packages()
    full_packages <- paste0("package:",packages)
    num_packages <- length(packages)
    num_functions <- vector(,num_packages) # Empty list of length (num_packages)

    ## Iterate through all packages
    for (i in 1:length(packages)) {
        package <- packages[i]
        function_ptrs <- get_function_list(packages=c(package))
        num_functions[i] <- length(function_ptrs)
    }

    ## Append number of user functions if enabled
    if (flag_user_functions){
        function_ptrs <- get_user_function_list()
        num_functions <- append(num_functions, length(function_ptrs))
    }

    ## DEBUGGING
    if (debug_flag) {
        print("########## PROFILE_get_num_functions() ############")
        print(paste0("Total - number of packages: ", num_packages, 
                     ", number of functions: ", sum(num_functions)))
    }

    invisible(num_functions) # return quiet
}



#######################################################################
# section - function list helper functions
#######################################################################

#' get_function_index
#' @description Get function index from func_list
#' @param func_ptr Object - Function
#' @param func_list Object[] - Array of functions
#' @param func_name String - Name of function
#' @return index Integer - for func_name in func_list, else NULL if not found
#' @export
get_function_index <- function(func_list, func_ptr, func_name){

    # Cycle through list for match
    for (i in 1:length(func_list)) {
        tmp_func <- func_list[i][[func_name]]
        if (identical(func_ptr, tmp_func)) return(i)
    }

    print("ERROR: function not found in list")
    return(NULL)
}

#' print_function_from_index
#' @description Prints names of given functions from indexes, only intended for debugging
#' @param func_indexes Int[] 
#' @export 
print_function_from_index <- function(func_indexes) {
    func_ptrs <- get_function_list()
    for (func_index in func_indexes) {
        print(names(func_ptrs)[func_index])
    }
}



#######################################################################
# section - Check valid function for instrumentation
#######################################################################

# @DONE: zmq this
#' try_insert_instrumentation
#' @description Checks function exceptions and calls insert_instrumentation() if success
#' @param func_info Dataframe (struct) containing func_index info, func_name and packagE_name
#' @param func_ptrs Function[] - List of function objects
#' @param env_is_locked Boolean - TRUE if function name-/package- space is locked
#' @param function_exception_list Object[] - List of function exceptions to skip
#' @param function_methods_exception_list Object[] - List of function method exceptions to skip
#' @param flag_user_function Boolean - Enable if user defined function (in .GlobalEnv)
#' @param flag_debug Boolean - Enable debug output
#' @export 
try_insert_instrumentation <- function(func_info, func_ptrs, env_is_locked, 
    function_exception_list, function_methods_exception_list, 
    flag_user_function=F, flag_debug=F)
{
    func_global_index <- func_info$func_global_index
    func_local_index <- func_info$func_local_index
    func_name <- func_info$func_name
    package_name <- func_info$package_name

    ## Make sure function exists
    if ( !is.null(func_ptrs[func_local_index][[func_name]]) ){ 
        func_ptr <- func_ptrs[func_local_index][[func_name]] 
    }
    else { 
        print(paste0("ERROR: Function not found: `",func_name,"`"))
        stop()
    }

    ## DEBUGGING - Display current function (before checks)
    if (flag_debug) {
        print("#######################################")
        print(paste0("package: ", package_name, ", function: ", func_name))
        print(paste0("func_global_index: ", func_global_index))
        print(paste0("func_local_index: ", func_local_index))
        print(utils::str(body(func_ptr))) 
        print(utils::head(func_ptr))
    }

    ## Test if function should be skipped
    if (flag_user_function) {
        env <- .GlobalEnv
    } else {
        env <-  as.environment( paste0("package:",package_name) )
    }
    if ( skip_function(func_ptr, func_name, env, function_exception_list, function_methods_exception_list)) {
        return(NULL) # break or return(NULL)
    }

#    ## Create otf2 region and event descriptions
#    regionRef <- create_otf2_event(func_name)

    # ZMQ version
    regionRef <- define_otf2_event_client(func_name)

    ## Label as instrumented in instrumentation dataframe
    pkg.env$PROFILE_INSTRUMENTATION_DF[["function_instrumented"]][func_global_index] <-  TRUE

    if (pkg.env$PRINT_INSTRUMENTS) {
        print(paste0("INSTRUMENTING: function `", func_name,"`",
                    ", regionRef: ", regionRef))
    }

    ## Wrap function with debug info
    insert_instrumentation(func_ptr, func_name, func_global_index, 
                           regionRef, package_name, 
                           env_is_locked=!pkg.env$UNLOCK_ENVS, 
                           flag_user_function=flag_user_function)
}



#' instrument_all_functions
#' @description Instrument all functions
#' @param package_list String[] - Array of package names to instrument, if none instrument all packages
#' @param flag_user_functions Boolean - TRUE if also flagging user functions
#' @param flag_print_progress Boolean - Enable for package by package progress statements
#' @param flag_debug Boolean - Enable debug statements
#' @export
instrument_all_functions <- function(package_list=NULL, flag_user_functions=TRUE, flag_print_progress=TRUE, flag_debug=FALSE) 
{
    ## Make sure instrumentation_init() has been called
    if (!is_instrumentation_init()){
        print("ERROR: Must first call `instrumentation_init()` before instrumenting functions.")
        stop()
    }

    if (is.null(package_list)){ ## Get all packages from env if none given
        package_list <- .packages()
    }

    ## Get exception lists
    function_exception_list <- get_function_exception_list()
    function_methods_exception_list <- get_function_methods(function_exception_list)
    package_exception_list <-  get_package_exception_list()

    ## Needed for finding index offset
    num_func_per_package <- get_num_functions(flag_user_functions=flag_user_functions)

    ## Cycle through every package
    for (package_name in package_list) {

        ## Get function pointers and names
        func_ptrs <- get_function_list(packages=package_name)
        func_names <- names(func_ptrs)
        func_num <- length(func_ptrs)

        ## Get environment object for package 
        env = as.environment(paste0("package:",package_name))

        ## Function index offset
        package_offset <- match(package_name, .packages())
        func_global_index <- sum(num_func_per_package[1:package_offset-1])

        ## DEBUGGING
        if (flag_debug){
            print(paste0("################ PACKAGE: ", package_name, "###############"))
            print(func_num)
            print(func_names)
        }

        ## Skip package if no functions, or exception
        if (func_num < 1){ next }
        if (package_name %in% package_exception_list){ next }

        ## Unlock namespace and packagespace
        ## !!! MAKE SURE TO LOCK AFTER !!!
        if (pkg.env$UNLOCK_ENVS) { unlock_envs(package_name) }


        ## Loop through every function in package
        for (func_local_index in 1:func_num) #for (i in 1:3)  ## Useful for debugging
        {
            func_global_index <- func_global_index + 1
            func_name <- func_names[func_local_index]

            ## Dataframe for holding relevent info
            func_info <- data.frame(func_global_index, func_local_index, func_name, package_name)

            ## Instrumentation writes entry and leave events in wrapper
            try_insert_instrumentation(func_info, func_ptrs, !pkg.env$UNLOCK_ENVS, function_exception_list, function_methods_exception_list, flag_debug)
        }

        if (pkg.env$UNLOCK_ENVS) { lock_envs(package_name) }
        if (flag_print_progress) { print(paste0("Instrumented package: ", package_name)) }
    }

    if (flag_user_functions) {
        instrument_user_functions(flag_debug=flag_debug) 
        if (flag_print_progress) { print("Instrumented user functions") }
    }
    if (flag_print_progress) { print("COMPLETED INSTRUMENTATION") }

    # Flag end with zmq
    finalize_GlobalDefWriter_client()

}

#' skip_function
#' @description Check if function should be skipped for instrumentation. Reasons 
#'  for skipping include :
#'      1) function in FUNCTION_EXCEPTION_LIST
#'      2) function in FUNCTION_METHODS_EXCEPTION_LIST
#'      3) primitive functions, 
#'      4) non-language function (symbol)
#'      5) functions used in instrumentation wrapper (to avoid recursion)
#'      5) isS3stdGeneric
#' @param func_ptr Object - Pointer to function object
#' @param func_name String - Name of function
#' @param env Environment - Environment function is in
#' @param function_exception_list Object[] - List of functions to skip, generated by get_function_exception_list()
#' @param function_methods_exception_list Object[] - List of function methods to skip, generated by get_function_methods(function_exception_list)
#' @return Boolean - TRUE if skip, else FALSE
#' @export
skip_function <- function(func_ptr, func_name, env, 
                                  function_exception_list, 
                                  function_methods_exception_list)
{
    ## Make sure is a function
    if (!methods::is(func_ptr, "function")){
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is not a function"))
        return(TRUE)
    }

    ## Skip if function not defined in current package
    if ( !exists(func_name, envir = env, inherits=T)) {
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` DOES NOT exist in package env: ", env))
        return(TRUE)
    }

    ## 1 - Skip if in exception list
    for (func_exception in function_exception_list) {
        if (identical(func_exception, func_ptr)){
            if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is in exception list"))
            return(TRUE)
        }
    }

    ## 2 - Skip method if from FUNCTION_METHODS_EXCEPTION_LIST
    if ( is.element(func_name, function_methods_exception_list) ){
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is in method exception list"))
        return(TRUE)
    }

    ## 3 - Skip if primitive function - DEBUGGING (some are problematic)
    if ( is.primitive(func_ptr) ) {
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is PRIMITVE function"))
        return(TRUE)
    }

    ## 4 - Skip if not language body - DEBUGGING (symbol in na.null() was causing issues)
    if ( typeof(body(func_ptr)) != "language" ) {
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` body is type: ", typeof(body(func_ptr))))
        return(TRUE)
    }

    ## 5 - Standard generic function
    if ( utils::isS3stdGeneric(func_ptr) ) {
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is of type S3 Standard Generic" ))
        return(TRUE)
    }
    if ( methods::isGeneric(func_name) ) {
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is of type Generic Function" ))
        return(TRUE)
    }

    ## Else passed all tests
    return(FALSE)
}


#' instrument_user_functions
#' @description Instrument user functions
#' @param flag_debug Boolean - Enable debug statements
#' @export
instrument_user_functions <- function(flag_debug=FALSE) 
{
    INHERITS <- TRUE
    package_name <- "user_functions" # Placeholder for consistency


    ## Get function pointers and names
    func_ptrs <- get_user_function_list()
    func_names <- names(func_ptrs)
    func_num <- length(func_ptrs)
    env = .GlobalEnv

    ## Get exception lists
    function_exception_list <- get_function_exception_list()
    function_methods_exception_list <- get_function_methods(function_exception_list)

    ## Function index offset
    num_func_per_package <- get_num_functions()
    func_global_index <- sum(num_func_per_package)

    ## DEBUGGING
    if (flag_debug){
        print(paste0("################ PACKAGE: User Functions ###############"))
        print(func_num)
        print(func_names)
    }

    ## Skip package if no functions
    if (func_num < 1){
        return(NULL)
    }

    ## Loop through every function in package
    for (func_local_index in 1:func_num)
    {
        func_global_index <- func_global_index + 1
        func_name <- func_names[func_local_index]

        ## Dataframe for holding relevent info
        func_info <- data.frame(func_global_index, func_local_index, func_name, package_name)

        ## Instrumentation writes entry and leave events in wrapper
        try_insert_instrumentation(func_info, func_ptrs, FALSE, 
                                   function_exception_list, 
                                   function_methods_exception_list,
                                   flag_user_function=T, flag_debug)
    }

}



#######################################################################
# section - dataframe
#######################################################################

#' create_dataframe
#' @description See return
#' @param flag_debug Boolean - Enabled debug state,emts
#' @param flag_user_functions Boolean - TRUE to include user functions in dataframe
#' @return Dataframe containing each function with information on function name,
#'      package, count of function calls, total time spent in function
create_dataframe <- function(flag_user_functions=FALSE, flag_debug=FALSE) {
    packages <- .packages()
    num_functions_per_package <- get_num_functions(flag_user_functions=flag_user_functions)
    num_functions_total <- sum(num_functions_per_package)

    if (flag_user_functions) { packages <- append(packages, "user_functions") }

    ## Sections in dataframe
    function_names <- names(get_function_list())
    if (flag_user_functions) { function_names <- append(function_names, names(get_user_function_list())) }
    package_list <- array(,num_functions_total) 
    #count <- integer(num_functions_total)
    #total_time <- numeric(num_functions_total)
    instrumented <- logical(num_functions_total)

    ## DEBUGGING:
    if (flag_debug) {
        print("################ DATAFRAME #################")
        print(length(function_names))
        print(num_functions_total)
        print(num_functions_per_package)
        print(package_list)
    }

    # package each entry in function_names belongs to
    index <- 1
    for (i_package in 1:length(packages)) {
        tmp <- num_functions_per_package[i_package]
        package_list[index:(index-1+tmp)] <- packages[i_package]
        index <- index+tmp
    }

    # Init count and time arrays
    #count[1:num_functions_total] <- 0L
    #total_time[1:num_functions_total] <- 0.0
    instrumented[1:num_functions_total] <- FALSE

    #data.frame(packages=package_list, functions=function_names, function_instrumented=instrumented, function_count=count, function_time=total_time)
    data.frame(packages=package_list, functions=function_names, function_instrumented=instrumented)
}



#######################################################################
# section - helper functions
#######################################################################

#' test_instrumentation
#' @description Utility function - Call function with expr and output dataframe changes if enabled
#' @param func_ptr Object - function to test
#' @param func_name String - name of function
#' @param expr Expression - R expression to call to test func
#' @param flag_debug Boolean - Enable debug statements
#' @export
test_instrumentation <- function(func_ptr, func_name, expr, flag_debug=F) {
    func_ptrs <- get_function_list()
    index_func <- get_function_index(func_ptrs, func_ptr, func_name)

    if (flag_debug){
        print(paste0("Num functions:",length(func_ptrs),", index of function:",index_func))

        print("''''''''''''''' DATAFRAME '''''''''''''''")
        print("########### BEFORE ############")
        print(paste0("function_count: ", pkg.env$PROFILE_INSTRUMENTATION_DF[["function_count"]][index_func]))
        print(paste0("function_time: ", pkg.env$PROFILE_INSTRUMENTATION_DF[["function_time"]][index_func]))
    }

    ## Call function and print output
    print(eval(expr)) # Should add +1 to count and accum on time

    if (flag_debug){
        print("########### AFTER ############")
        print(paste0("function_count: ", pkg.env$PROFILE_INSTRUMENTATION_DF[["function_count"]][index_func]))
        print(paste0("function_time: ", pkg.env$PROFILE_INSTRUMENTATION_DF[["function_time"]][index_func]))
        print(func_ptr)
    }
}

