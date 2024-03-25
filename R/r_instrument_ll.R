# @file r_instrument_ll.R
# @todo - R error checking
# @todo - Reduce function exception list
# @todo - instrument_all_functions merge debug flags
# @todo - Resolve multiple args of which packages to instrument, user functions, etc


#######################################################################
# section - Update for ZMQ
#######################################################################


#######################################################################
# section - function wrappers
#######################################################################

#' get_wrapper_expression
#' @description Returns wrapper expression
get_wrapper_expression <- function() {
    wrapper_expression <- expression( { 

        if (pkg.env$INSTRUMENTATION_ENABLED) {
            NULL
            ## Append to depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1
            on.exit( pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH -  1, add=TRUE )

            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) 
            {
                ## zmq version - OTF2 Event
                evtWriter_Write_client(X_regionRef_X,T)
                on.exit(evtWriter_Write_client(X_regionRef_X,F), add=TRUE)
            }
        }

    } )

    wrapper_expression
}

########################################################################
# SECTION - LOW LEVEL INSTRUMENTATION
########################################################################



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
#' @param flag_user_functions Boolean - Enable to include user functions. Default FALSE for backwards compatability
#' @return func_list Function[] - List of function ptrs
#' @export
get_function_list <- function(packages=NULL, flag_full_name=FALSE, flag_user_functions=FALSE, flag_debug=FALSE) {
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

    ## Append user functions if enabled
    if (flag_user_functions){
        function_ptrs <- get_user_function_list()
        func_list <- append(func_list, function_ptrs)
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
#' @param flag_slave_proc Boolean - Enable if running on slave proc
#' @export 
try_insert_instrumentation <- function(func_info, func_ptrs, env_is_locked, 
    function_exception_list, function_methods_exception_list, 
    flag_user_function=F, flag_debug=F, flag_slave_proc=F)
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

    ## Test if function should be skipped
    if (flag_user_function) {
        env <- .GlobalEnv
    } else {
        env <-  as.environment( paste0("package:",package_name) )
    }
    if ( skip_function(func_ptr, func_name, env, function_exception_list, function_methods_exception_list)) {
        return(NULL) # break or return(NULL)
    }

    ## Create otf2 region and event descriptions
    if (flag_slave_proc){
        regionRef <- get_regionRef_from_array_slave(func_global_index) 
    } else {
        regionRef <- define_otf2_regionRef_client(func_name, func_global_index)
    }

    ## DEBUGGING - print func index and regionRef for all on master AND slave
    if (flag_debug){
        print(paste0("[",func_global_index, "] package: ", package_name, "func_name: ", func_name, ", regionRef: ", regionRef))
    }

    if (pkg.env$PRINT_INSTRUMENTS) {
        print(paste0("INSTRUMENTING: function `", func_name,"`",
                    ", regionRef: ", regionRef))
    }

    # Get new body for funcs of type: {fork_function, end_fork_function, default}
    body(func_ptr) <- get_new_function_body(func_ptr, func_name, regionRef)

    ## TODO: Add check for if compiled before, recompile
    #func_ptr <- compiler::cmpfun(func_ptr) 

    ## Replace function in package and namespace
    if (flag_user_function) {
        replace_user_function(func_ptr, func_name, package_name)
    } else {
        replace_function(func_ptr, func_name, package_name, env_is_locked=!pkg.env$UNLOCK_ENVS)
    }

}



#' instrument_all_functions
#' @description Instrument all functions
#' @param package_list String[] - Array of package names to instrument, if none instrument all packages
#' @param flag_user_functions Boolean - TRUE if also flagging user functions
#' @param flag_print_progress Boolean - Enable for package by package progress statements
#' @param flag_debug Boolean - Enable debug statements
#' @param flag_slave_proc Boolean - Enable if running on slave proc
#' @export
instrument_all_functions <- function(package_list=NULL, flag_user_functions=TRUE, 
    flag_print_progress=TRUE, flag_debug=FALSE, flag_slave_proc=FALSE) 
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
    total_num_funcs <- sum(num_func_per_package)

    if (flag_slave_proc){
        ## Make sure to free at end of function!
        assign_regionRef_array_slave(total_num_funcs)
        get_regionRef_array_slave(total_num_funcs)
    }

    for (package_name in package_list) {

        ## Get function pointers and names
        func_ptrs <- get_function_list(packages=package_name)
        func_names <- names(func_ptrs)
        func_num <- length(func_ptrs)

        ## Get environment object for package 
        env <- as.environment(paste0("package:",package_name))

        ## Function index offset - works if not all packages are being instrumented!
        package_offset <- match(package_name, .packages())
        func_global_index <- sum(num_func_per_package[1:package_offset-1])

        ## DEBUGGING
        if (flag_debug){
            print(paste0("################ PACKAGE: ", package_name, 
                    ", NUM_FUNCS: ", func_num, " #############"))
        }

        ## Skip package if no functions, or exception
        if (func_num < 1){ next }
        if (package_name %in% package_exception_list){ next }

        ## Unlock namespace and packagespace
        ## !!! MAKE SURE TO LOCK AFTER !!!
        if (pkg.env$UNLOCK_ENVS) { unlock_envs(package_name) }


        ## Loop through every function in package
        for (func_local_index in 1:func_num) 
        {
            # Append first, fix for indexing from 1
            func_global_index <- func_global_index + 1 
            func_name <- func_names[[func_local_index]]
            func_name <- func_names[[func_local_index]]

            ## Dataframe for holding relevent info
            func_info <- data.frame(func_global_index, func_local_index, func_name, package_name)

            ## Instrumentation writes entry and leave events in wrapper
            try_insert_instrumentation(func_info, func_ptrs, 
                    !pkg.env$UNLOCK_ENVS, function_exception_list, 
                    function_methods_exception_list, flag_debug=flag_debug, 
                    flag_slave_proc=flag_slave_proc)

        }

        if (pkg.env$UNLOCK_ENVS) { lock_envs(package_name) }
        if (flag_print_progress) { print(paste0("Instrumented package: ", package_name)) }
        func_global_index <- func_global_index + 1
    }

    ## Instrument user functions
    if (flag_user_functions) {
        instrument_user_functions(flag_debug=flag_debug, flag_slave_proc=flag_slave_proc) 
        if (flag_print_progress) { print("Instrumented user functions") }
    }
    if (flag_print_progress) { print("COMPLETED INSTRUMENTATION") }

    # End definition of GlobalDef and regionRef array
    if (!flag_slave_proc){ 
        finalize_GlobalDefWriter_client() 
    } else {
        free_regionRef_array_slave() 
    }

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
skip_function <- function(func_ptr, func_name, env, function_exception_list, 
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

    ## 5 - Standard generic function, tryCatch for user functions
    tryCatch( {
            if ( utils::isS3stdGeneric(func_ptr) ) {
                if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is of type S3 Standard Generic" ))
                return(TRUE)
            }
            if ( methods::isGeneric(func_name) ) {
                if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is of type Generic Function" ))
                return(TRUE)
            }
        }, 
        error = function(e){ ; }
    )

    ## Else passed all tests
    return(FALSE)
}


#' instrument_user_functions
#' @description Instrument user functions
#' @param flag_debug Boolean - Enable debug statements
#' @param flag_slave_proc Boolean - Enable if running on slave proc
#' @export
instrument_user_functions <- function(flag_debug=FALSE, flag_slave_proc=FALSE) 
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
                                   flag_user_function=T, flag_debug,
                                   flag_slave_proc=flag_slave_proc)
    }

}



#######################################################################
# section - helper functions
#######################################################################

# N/A