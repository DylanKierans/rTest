# Functions for makePSOCKcluster, and potential makeSOCKcluster

insert_instrumentation_on_new_proc <- function()
{
    flag_debug <- FALSE

    func_ptrs <- get_function_list()
    num_func_ptrs <- sum(get_num_functions())

    function_exception_list <- get_function_exception_list()
    function_methods_exception_list <- get_function_methods(function_exception_list)

    assign_regionRef_array_client(num_function_ptrs)
    open_otf2_regionRef_sockets()

    ## Starting new here
    for (func_index in 1:num_func_ptrs){

        func_ptr <- func_ptrs[[func_index]]
        func_name <- names(func_ptrs)[[func_index]]
        #package_name <- names(func_ptrs)[[func_index]]
        env <- environment(func_ptrs[[func_index]])
        package_name <- environmentName(env)
        
        ## DEBUGGING - Display current function (before checks)
        if (flag_debug) {
        #    print("#######################################")
        #    print(paste0("func_index: ", func_index))
            print(func_ptr)
            print(func_name)
            print(env)
            print(paste0("package: ", package_name, ", function: ", func_name))
        }

        flag_user_function=FALSE
        if (env==.GlobalEnv){flag_user_function=TRUE}

        #if (env == NULL){ print(paste0("NULL env, func_name: ", func_name)) }
        print(paste0("func_name: ", func_name))

        ## Test if function should be skipped
        if ( skip_function(func_ptr, func_name, env, function_exception_list, function_methods_exception_list)) {
            print(paste0("Skipping: ", func_name))
            next; # skip to next loop
        }

        ## Get otf2 regionRef
        regionRef <- get_regionReg_from_array_client(func_index)
    if (pkg.env$PRINT_INSTRUMENTS) {
        print(paste0("INSTRUMENTING: function `", func_name,"`",
                    ", regionRef: ", regionRef))
    }

    ## Wrap function with debug info
    insert_instrumentation(func_ptr, func_name, func_index, 
                           regionRef, package_name, 
                           env_is_locked=!pkg.env$UNLOCK_ENVS, 
                           flag_user_function=flag_user_function)

    }
    close_otf2_regionRef_sockets()
}


#if (FALSE){


#    ## Label as instrumented in instrumentation dataframe
#    pkg.env$PROFILE_INSTRUMENTATION_DF[["function_instrumented"]][func_global_index] <-  TRUE
#
