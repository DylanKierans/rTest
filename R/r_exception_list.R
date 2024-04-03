# @name exception_list.R
# @description List of functions to not profile, due to recursion problems amongst others
#  (will hopefully reduce with otf2 C/C++ implimentation)
library("R.utils")
library("methods")

#' get_function_exception_list()
#' @description Get function list to not insturment (mostly due to recursion problems in wrapper)
#'  Note: This is a hacky fix for many smaller problems
#' @return function_exception_list Object[] List of all functions to not instrument
#' @export
get_function_exception_list <- function() {
    # Anything in wrapper to avoid recursion when calling instrumented function
    function_exception_list <- c( eval, 
            print, search, paste0, sys.call,
            body, as.name, search, load, 
            as.name, as.list, as.function, as.matrix, as.pairlist,      # as.(type)
            as.vector, pairlist,
            asS4, 
            is.vector, is.factor, is.element, is.factor,                # is.(type)
            bindingIsLocked, assign, delayedAssign,                     # Assignment
            eval, args, formals, do.call, get, getOption,               # Function related
            match.arg, match.fun, match.call, 
            exists, typeof, inherits, identical,                        # Type checks and comparisons
            cat, format, character,                     # Output
            asNamespace, getNamespace, isNamespace, isNamespaceLoaded,  # Namespace
            environment, environmentIsLocked, new.env, lockEnvironment, # Environment
            gettext, get0, getHook, detach, isTRUE,                     # Misc
            NextMethod, 
            substitute, parent.frame, is.list, is.pairlist,       # Not sure about these
            baseenv,
            warning, message, which, NROW, match, # Try avoid recursion when instrumentation enabled
            duplicated, vapply, anyDuplicated,
            get('body<-'),
            tryCatch, # Clobbering trace results
            append) 

    if (R.utils::isPackageLoaded("R.utils")){
        package_function_exception_list <- c(R.utils::isPackageLoaded)
        function_exception_list <- append(function_exception_list, package_function_exception_list)
    }
    if (R.utils::isPackageLoaded("compiler")){
        #package_function_exception_list <- c(compiler::cmpfun)
        package_function_exception_list <- c()
        function_exception_list <- append(function_exception_list, package_function_exception_list)
    }

    if (R.utils::isPackageLoaded("methods")){
        package_function_exception_list <- c(methods::as)
        function_exception_list <- append(function_exception_list, package_function_exception_list)
    }

    if (R.utils::isPackageLoaded("utils")){
        #package_function_exception_list <- c(utils::dump.frames, utils::getAnywhere)
        package_function_exception_list <- c()
        function_exception_list <- append(function_exception_list, package_function_exception_list)
    }

    if (R.utils::isPackageLoaded("parallel")){
        package_function_exception_list <- c(parallel::clusterApply, parallel::clusterEvalQ)
        function_exception_list <- append(function_exception_list, package_function_exception_list)
    }

    # ISSUE 1: These functions contain on.exit() and blocks instrumentation insert
    on.exit_functions <- c(library, utils::read.table)

    function_exception_list <- append(function_exception_list, on.exit_functions)

    function_exception_list
}

#' get_function_methods
#' @description Get all methods of given function list
#' @param function_list Object[] List of function pointers
#' @return function_method_list Object[] List of all method functions for function_list
#' @export
get_function_methods <- function(function_list) {
    function_method_list <- c()
    for (function_exception in function_list) {

        suppressWarnings( function_methods <- utils::methods(function_exception) )

        # Methods exist
        if (length(function_methods) > 0 ){
            function_method_list = append(function_method_list, function_methods)
        }
    }
    function_method_list
}

#' get_package_exception_list
#' @description Get list of packages to not instrument
#' @return package_exception_list String[] List of packages to not instrument
#' @export
get_package_exception_list <- function() {
    package_exception_list <- c("rTrace")
    package_exception_list 
}

## Usage: Get all globals
#FUNCTION_EXCEPTION_LIST <- get_function_exception_list()
#FUNCTION_METHODS_EXCEPTION_LIST <- get_function_methods(FUNCTION_EXCEPTION_LIST)
#PACKAGE_EXCEPTION_LIST <- get_package_exception_list()
