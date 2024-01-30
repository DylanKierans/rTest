#' @name r_intrument.R
#' file
# @todo - Consider effects on wrapping user defined functions
# @todo - R error checking
# @todo - C error checking
# @todo - Should only compile function if previous version compiled
# @todo - Make sure PROFILE_INSTRUMENTATION_DF exists
# @todo - Isolate .profile_wrapper_expression
# @todo - ensure parent thread with isForkedChild()
# @todo - reduce function exception list
# @todo - reduce usage to rTrace_init, rTrace_finalize, rTrace_start, rTrace_stop

suppressPackageStartupMessages({
    library("R.utils", quietly=TRUE)
    library("methods", quietly=TRUE)
    library("stats", quietly=TRUE)
    library("rlang", quietly=TRUE)
    library("pryr", quietly=TRUE)
    library("compiler", quietly=TRUE)
})
#source("R/r_exception_list.R")
#source("R/r_utils.R")
#source("R/r_event_instrumentation_loggers.R")


########################################################################
# SECTION - Instrument Flags
########################################################################
#' @name MAX_FUNCTION_DEPTH
#' @description Max depth of functions to creat instrumententation events for
pkg.env$MAX_FUNCTION_DEPTH <- 10 

#' @name UNLOCK_ENVS
#' @description Keep package envs unlocked when instrumenting functions
pkg.env$UNLOCK_ENVS <- TRUE # Not sure if this is safe to set TRUE, but should be quicker!

########################################################################
# SECTION - Output Flags
########################################################################
#' @name PRINT_SKIPS
#' @description Print which functions are being skipped due to exception
pkg.env$PRINT_SKIPS <- TRUE

#' @name PRINT_INSTURMENTS
#' @description Print which functions are being instrumented
pkg.env$PRINT_INSTRUMENTS <- TRUE

#' @name PRINT_FUNC_INDEXES
#' @description Print function indexes when called (only intended for verbose debugging)
pkg.env$PRINT_FUNC_INDEXES <- FALSE

########################################################################
# SECTION - Init section for instrumentation
# ~~~ DO NOT CHANGE ~~~
########################################################################
#' @name INSTRUMENTATION_ENABLED
#' @description Current status of instrumentation
pkg.env$INSTRUMENTATION_ENABLED=FALSE

#' @name FUNCTION_DEPTH
#' @description Current instrumentation depth
pkg.env$FUNCTION_DEPTH <- 0


########################################################################
# SECTION
########################################################################

#' profile_insert_instrumentation
#'  Insert instrumentation prefix for package_name:::func
#' @param func Object - Pointer to function closure to update
#' @param func_name String - function name
#' @param func_index Integer - index of function in function_list in PROFILE_INSTRUMENTATION_DF
#' @param regionRef Integer - OTF2 regionRef index
#' @param package_name String - package name
#' @param flag_user_function Boolean - True if instrumenting user functoin
#' @param env_is_locked Boolean - True if function name-/package- space is locked
#' @export
profile_insert_instrumentation <- function(func, func_name, func_index, regionRef, package_name, flag_user_function=FALSE, env_is_locked=TRUE) {

    ## Version 7
    .profile_wrapper_expression <- eval( substitute(
    expression(
    { 
        if (pkg.env$INSTRUMENTATION_ENABLED) {
            NULL
            ## DEBUGGING
            #print("Hello World!") 
            #on.exit(print("Finish!"),add=TRUE)

            if (pkg.env$PRINT_FUNC_INDEXES){
                print("regionRef index: ") 
                print(regionRef) 
            }

            ## Depth counter error check
            if (pkg.env$FUNCTION_DEPTH < 0 )  
            {
                print("Warning: Disabling instrumentation - Function_depth < 0.")
                profile_disable_instrumentation()
            }

            ## Append to depth counter
            pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH + 1
            on.exit( pkg.env$FUNCTION_DEPTH <- pkg.env$FUNCTION_DEPTH -  1, add=TRUE )

            if (pkg.env$FUNCTION_DEPTH <= pkg.env$MAX_FUNCTION_DEPTH ) 
            {
                ## Function count
                on.exit( pkg.env$PROFILE_INSTRUMENTATION_DF[["function_count"]][X_func_index_X] <- pkg.env$PROFILE_INSTRUMENTATION_DF[["function_count"]][X_func_index_X] + 1, add=TRUE )

                ## Function timing
                t0 <- time()
                on.exit( t0 <- time() - t0, add=TRUE)
                on.exit( pkg.env$PROFILE_INSTRUMENTATION_DF[["function_time"]][X_func_index_X] <- pkg.env$PROFILE_INSTRUMENTATION_DF[["function_time"]][X_func_index_X] + t0, add=TRUE )

                ## Eventlog
                profile_event_create(as.integer(X_func_index_X), TRUE, time())
                on.exit( profile_event_create(as.integer(X_func_index_X), FALSE, time()), add=TRUE )

                ## OTF2 Event
                rTest_evtWriter_Write(X_regionRef_X,T)
                on.exit(rTest_evtWriter_Write(X_regionRef_X,F), add=TRUE)
            }

        }
    }
    )
            , list(X_func_index_X=func_index, X_regionRef_X=regionRef)
    ) )


    ## Copy and wrap function definition
    orig_func_body <- body(func)[1:length(body(func))]
    body(func) <- as.call(c(as.name("{"), .profile_wrapper_expression, orig_func_body))

    ## TODO: Not byte-compiling now in order to speed up
    #func <- compiler::cmpfun(func) ## Should only compile if original is compiled

    ## Replace function in package and namespace
    if (flag_user_function) {
        profile_replace_user_function(func, func_name, package_name)
    } else {
        profile_replace_function(func, func_name, package_name, env_is_locked=env_is_locked)
    }

}



#' profile_replace_user_function
#'  Replace user function definition
#' @param new_func Function, function object with new definition
#' @param func_name String, name of function
#' @param package_name String, name of package function
#' @param env Environment, environment function exists in (default .GlobalEnv)
profile_replace_user_function <- function(new_func, func_name, package_name, env=.GlobalEnv) {
    assign(func_name, new_func, envir = env)
}

#' profile_replace_function
#'  Replace library function definition in package- and name-space
#' @param new_func Function object with new definition
#' @param func_name String, name of function
#' @param package_name String, name of package function
#' @param env_is_locked Boolean - True if function name-/package- space is locked
profile_replace_function <- function(new_func, func_name, package_name, env_is_locked=TRUE) {

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
  
#' profile_get_function_list
#'  Get all functions in package (else all if no arg given)
#' @param packages String[] - Full name of package if specific package, else all available packages
#' @param flag_full_name Boolean - TRUE if full name of package passed using input packages (eg "package:stats")
#' @param flag_debug Boolean - Enable debugging statements
#' @return func_list Function[] - List of function ptrs
#' @export
profile_get_function_list <- function(packages=NULL, flag_full_name=FALSE, flag_debug=FALSE) {
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

#' profile_get_user_function_list
#'  Get all user-defined functions in global_env
#' @param flag_debug Boolean - Enable debug output
#' @return user_func_list Function[] - List of user-defined functions
#' @export
profile_get_user_function_list <- function(flag_debug=FALSE) {
    objs <- mget(ls(envir=.GlobalEnv), inherits=T)
    user_func_list <- Filter(is.function, objs)

    if (flag_debug) { ## Debugging statements
        print(paste0("Number user functions: ", length(user_func_list)))
        print("List of user functions: ")
        print(names(user_func_list))
    }
    invisible(user_func_list)
}


#' profile_total_num_functions
#'  Find total number of loaded packages and functions
#' @param debug_flag Boolean - TRUE to print debug information
#' @param flag_user_functions Boolean - True if also flagging user functions
#' @return Int[] Number of functions per package
#' @export
profile_total_num_functions <- function(debug_flag=FALSE, flag_user_functions=FALSE) {

    packages <- .packages()
    full_packages <- paste0("package:",packages)
    num_packages <- length(packages)
    num_functions <- vector(,num_packages) # Empty list of length (num_packages)

    ## Iterate through all packages
    for (i in 1:length(packages)) {
        package <- packages[i]
        function_ptrs <- profile_get_function_list(packages=c(package))
        num_functions[i] <- length(function_ptrs)
    }

    ## Append number of user functions if enabled
    if (flag_user_functions){
        function_ptrs <- profile_get_user_function_list()
        append(num_functions, length(function_ptrs))
    }

    ## DEBUGGING
    if (debug_flag) {
        print("########## PROFILE_TOTAL_NUM_FUNCTIONS() ############")
        print(paste0("Total - number of packages: ", num_packages, 
                     ", number of functions: ", sum(num_functions)))
    }

    invisible(num_functions) # return quiet
}



#######################################################################
# section - function list helper functions
#######################################################################

#' profile_get_function_index
#'  Get function index from func_list
#' @param func_ptr Object - Function
#' @param func_list Object[] - Array of functions
#' @param func_name String - Name of function
#' @return index Integer - for func_name in func_list, else NULL if not found
#' @export
profile_get_function_index <- function(func_list, func_ptr, func_name){

    # Cycle through list for match
    for (i in 1:length(func_list)) {
        tmp_func <- func_list[i][[func_name]]
        if (identical(func_ptr, tmp_func)) return(i)
    }

    print("ERROR: function not found in list")
    return(NULL)
}

#' print_function_from_index
#'  Prints names of given functions from indexes, only intended for debugging
#' @param func_indexes Int[] 
#' @export 
print_function_from_index <- function(func_indexes) {
    func_ptrs <- profile_get_function_list()
    for (func_index in func_indexes) {
        print(names(func_ptrs)[func_index])
    }
}



#######################################################################
# section
#######################################################################

#' profile_try_insert_instrumentation
#'  Checks function exceptions and calls profile_insert_instrumentation() if success
#' @param func_info Dataframe (struct) containing func_index info, func_name and packagE_name
#' @param func_ptrs Function[] - List of function objects
#' @param env_is_locked Boolean - True if function name-/package- space is locked
#' @param function_exception_list Object[] - List of function exceptions to skip
#' @param function_methods_exception_list Object[] - List of function method exceptions to skip
#' @param flag_debug Boolean - Enable debug output
#' @export 
profile_try_insert_instrumentation <- function(func_info, func_ptrs, env_is_locked, 
    function_exception_list, function_methods_exception_list, flag_debug=F)
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
    env <-  as.environment( paste0("package:",package_name) )
    if ( profile_skip_function(func_ptr, func_name, env, function_exception_list, function_methods_exception_list)) {
        return(NULL) # break or return(NULL)
    }

    ## Create otf2 region and event descriptions
    regionRef <- profile_create_otf2_event(func_name)

    ## Label as instrumented in instrumentation dataframe
    pkg.env$PROFILE_INSTRUMENTATION_DF[["function_instrumented"]][func_global_index] <-  TRUE

    if (pkg.env$PRINT_INSTRUMENTS) {
        print(paste0("INSTRUMENTING: function `", func_name,"`"))
        print(paste0("func_name:", func_name,", regionRef: ", regionRef))
    }

    ## Wrap function with debug info
    profile_insert_instrumentation(func_ptr, func_name, func_global_index, regionRef, package_name, env_is_locked=!pkg.env$UNLOCK_ENVS)
}

#' profile_create_otf2_event
#'  Creates stringRef and regionRef for func_name
#' @param func_name String - Name of function
#' @return regionRef Int - Index of stringRef for function
#' @export
profile_create_otf2_event <- function(func_name) {
        stringRef <- rTest_globalDefWriter_WriteString(func_name)
        regionRef <- rTest_globalDefWriter_WriteRegion(stringRef)
        regionRef
}

#' profile_instrument_all_functions
#'  Instrument all functions
#' @param package_list String[] - Array of package names to instrument, if none instrument all packages
#' @param flag_user_functions Boolean - True if also flagging user functions
#' @param flag_debug Boolean - Enable debug statements
#' @export
profile_instrument_all_functions <- function(package_list=NULL, flag_user_functions=TRUE, flag_debug=FALSE) 
{
    ## Initiate OTF2 GlobalDefWriter
    rTest_init_GlobalDefWriter()

    if (is.null(package_list)){ ## Get all packages from env if none given
        package_list <- .packages()
    }

    ## Get exception lists
    function_exception_list <- get_function_exception_list()
    function_methods_exception_list <- get_function_methods(function_exception_list)
    package_exception_list <-  get_package_exception_list()

    ## Needed for finding index offset
    num_func_per_package <- profile_total_num_functions(flag_user_functions=flag_user_functions)

    ## Cycle through every package
    for (package_name in package_list) {

        ## Get function pointers and names
        func_ptrs <- profile_get_function_list(packages=package_name)
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
            profile_try_insert_instrumentation(func_info, func_ptrs, !pkg.env$UNLOCK_ENVS, function_exception_list, function_methods_exception_list, flag_debug)
        }

        if (pkg.env$UNLOCK_ENVS) { lock_envs(package_name) }
        print(paste0("Completed package: ", package_name))
    }

    if (flag_user_functions) {
        profile_instrument_user_functions(flag_debug=flag_debug) 
        print("Completed package: User functions")
    }
    print("COMPLETED FUNCTION WRAPPING")

}

#' profile_skip_function
#'  Check if function should be skipped for instrumentation. Reasons 
#'  for skipping include function in FUNCTION_EXCEPTION_LIST, primitive
#'  functions, functions used in instrumentation wrapper (to avoid recursion)
#'  etc
#' @param func_ptr Object - Pointer to function object
#' @param func_name String - Name of function
#' @param env Environment - Environment function is in
#' @param function_exception_list Object[] - List of functions to skip, generated by get_function_exception_list()
#' @param function_methods_exception_list Object[] - List of function methods to skip, generated by get_function_methods(function_exception_list)
#' @return Boolean - TRUE if skip, else FALSE
#' @export
profile_skip_function <- function(func_ptr, func_name, env, 
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

    ## DEBUGGING: Skip if primitive function (some are problematic)
    if ( is.primitive(func_ptr) ) {
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is PRIMITVE function"))
        return(TRUE)
    }

    ## Skip if in exception list - ugly fix
    for (func_exception in function_exception_list) {
        if (identical(func_exception, func_ptr)){
            if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is in exception list"))
            return(TRUE)
        }
    }

    ## Skip method if from FUNCTION_METHODS_EXCEPTION_LIST
    if ( is.element(func_name, function_methods_exception_list) ){
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` is in method exception list"))
        return(TRUE)
    }

    ## Skip if not language body (symbol in na.null() was causing issues)
    if ( typeof(body(func_ptr)) != "language" ) {
        if (pkg.env$PRINT_SKIPS) print(paste0("SKIPPING: function `", func_name, "` body is type: ", typeof(body(func_ptr))))
        return(TRUE)
    }

    ## Else passed all tests
    return(FALSE)
}


#' profile_instrument_user_functions
#'  Instrument user functions
#' @param flag_debug Boolean - Enable debug statements
#' @export
profile_instrument_user_functions <- function(flag_debug=FALSE) 
{
    INHERITS <- TRUE
    package_name <- "User functions" # Placeholder for consistency in debug statments


    ## Get function pointers and names
    func_ptrs <- profile_get_user_function_list()
    func_names <- names(func_ptrs)
    func_num <- length(func_ptrs)
    env = .GlobalEnv

    ## Get exception lists
    function_exception_list <- get_function_exception_list()
    function_methods_exception_list <- get_function_methods(function_exception_list)

    ## Function index offset
    num_func_per_package <- profile_total_num_functions()
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
        profile_try_insert_instrumentation(func_info, func_ptrs, FALSE, flag_debug)

        #func_index <- func_index + 1
        #func_name <- func_names[i]

        ### Make sure function exists
        #if ( !is.null(func_ptrs[i][[func_name]]) ){ 
        #    func_ptr <- func_ptrs[i][[func_name]] 
        #}
        #else { 
        #    print(paste0("ERROR: Function not found - `",func_name,"`"))
        #    stop()
        #}

        ### DEBUGGING - Display current function (before checks)
        #if (flag_debug) {
        #    print("#######################################")
        #    print(paste0("i=(",i," of ", func_num,")"))
        #    print(paste0("package: ", package_name, ", function: ", func_name))
        #    print(utils::str(body(func_ptr))) 
        #    print(utils::head(func_ptr))
        #}

        ### Test if function should be skipped
        #if ( profile_skip_function(func_ptr, func_name, env, function_exception_list, function_methods_exception_list)) 
        #{
        #    next 
        #}

        ### Label as instrumented in instrumentation dataframe
        #pkg.env$PROFILE_INSTRUMENTATION_DF[["func_instrumented"]][func_index] <- TRUE

        ### Wrap function with debug info
        #if (pkg.env$PRINT_INSTRUMENTS) print(paste0("INSTRUMENTING: function `", func_name,"`"))
        #profile_instrument_user_function(func_ptr, func_name, func_index, NULL)

    }
    print(paste0("Completed package: ", package_name))

}


#######################################################################
# section - helper functions
#######################################################################

#' test_instrumentation - utility function
#'  Call function with expr and output dataframe changes if enabled
#' @param func_ptr Object - function to test
#' @param func_name String - name of function
#' @param expr Expression - R expression to call to test func
#' @param flag_debug Boolean - Enable debug statements
#' @export
test_instrumentation <- function(func_ptr, func_name, expr, flag_debug=F) {
    func_ptrs <- profile_get_function_list()
    index_func <- profile_get_function_index(func_ptrs, func_ptr, func_name)

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


#######################################################################
# ENABLE/DISABLE INSTRUMENTATION
#######################################################################
#' profile_enable_instrumentation
#'  Enable instrumentation and reset function depth
#' @export
profile_enable_instrumentation <- function(){
    if (profile_is_instrumentation_enabled()){
        print("Warning: Instrumentation already enabled!")
    }
    else {
        pkg.env$FUNCTION_DEPTH <- 0
        pkg.env$INSTRUMENTATION_ENABLED <- TRUE
        rTest_init_EvtWriter()
    }
}

#' profile_disable_instrumentation
#'  Disable instrumentation
#' @export
profile_disable_instrumentation <- function(){
    if (!profile_is_instrumentation_enabled()){
        print("Warning: Instrumentation already disabled!")
    }
    else {
        if (pkg.env$FUNCTION_DEPTH != 0){ print("Warning: Function depth non-zero relative to start region.") }
        pkg.env$INSTRUMENTATION_ENABLED <<- FALSE
        rTest_finalize_EvtWriter()
    }
}

#' profile_is_instrumentation_enabled
#'  Return current instrumentation status
#' @return BOOLEAN - Instrumentation status
#' @export
profile_is_instrumentation_enabled <- function() {
    pkg.env$INSTRUMENTATION_ENABLED
}


