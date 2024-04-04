# @file r_instrument_hl.R

#######################################################################
# SECTION - HIGH LEVEL, ENABLE/DISABLE INSTRUMENTATION
#######################################################################

#' instrumentation_enable
#' @param flag_reset_depth Boolean - Intended for developers, suppress depth warning
#' @description Enable instrumentation and reset function depth
#' @export
instrumentation_enable <- function(flag_reset_depth=FALSE){
    if (is_instrumentation_enabled()){
        warning("Warning: Instrumentation already enabled!")
    } else {
        evtWriter_MeasurementOnOff_client(TRUE)
    }

    # Reset depth counter if specified
    if (flag_reset_depth){ pkg.env$FUNCTION_DEPTH <- 0 } 

    pkg.env$INSTRUMENTATION_ENABLED <- TRUE
    invisible(NULL)
}


#' instrumentation_disable
#' @description Disable instrumentation
#' @param flag_update_measurement Boolean - Intended for developers, update measurement mode
#' @param flag_check_depth Boolean - Intended for developers, suppress depth warning if false
#' @export
instrumentation_disable <- function(flag_check_depth=T, flag_update_measurement=TRUE){
    if (!is_instrumentation_enabled()){
        warning("Warning: Instrumentation already disabled!")
    }
    else {
        if ( (pkg.env$FUNCTION_DEPTH != 0) && flag_check_depth) {
            warning(paste0("Warning: Function depth non-zero relative to start region. Depth: ", pkg.env$FUNCTION_DEPTH) )
        }
        pkg.env$INSTRUMENTATION_ENABLED <- FALSE
        if (flag_update_measurement){
            evtWriter_MeasurementOnOff_client(FALSE)
        }
    }
    invisible(NULL)
}


#' is_instrumentation_enabled
#' @description Return current instrumentation status
#' @return BOOLEAN - Instrumentation status
#' @export
is_instrumentation_enabled <- function() {
    pkg.env$INSTRUMENTATION_ENABLED
}


#' instrumentation_init
#' @description Create otf2 objs for instrumentation, and initiate global vars
#' @param flag_user_functions Boolean - TRUE to include user functions in dataframe
#' @param verbose_wrapping Boolean - Print info about skipping or instrumenting each function. Produces large amount of info to stdout
#' @export
instrumentation_init <- function(flag_user_functions=T, verbose_wrapping=F)
{
    if (is_instrumentation_init()){
        print("ERROR: `instrumentation_init` has already been called")
        stop()
    }
    ## Update package vars
    pkg.env$PRINT_INSTRUMENTS <- verbose_wrapping
    pkg.env$PRINT_SKIPS <- verbose_wrapping
    pkg.env$INSTRUMENTATION_INIT <- TRUE

    ## Initiate new proc - close R if not Master
    ret <- init_otf2_logger(parallelly::availableCores()) # Master R proc returns 0
    if (ret != 0){ quit(save="no"); }  # Unintended fork R proc for otf2 logger

    ## Assign array on logger proc for regionRef of each func
    total_num_funcs <- sum(get_num_functions(flag_user_functions = T))
    assign_regionRef_array_master(total_num_funcs)

    ## TODO: Run this instead as a test during pkg install
    if(test__struct_size() != 0){
        stop("ERROR: Invalid internal struct size")
    }

    return(invisible(NULL))
}

#' is_instrumentation_init
#' @description Error catching function to ensure instrumentation_init() has been called
#' @return TRUE if init, else FALSE
is_instrumentation_init <- function() {
    if ( exists("INSTRUMENTATION_INIT", where=pkg.env) ){
        return(pkg.env$INSTRUMENTATION_INIT)
    }
    return(FALSE)
}


# @TODO: zmq this
#' instrumentation_finalize
#' @description Close otf2 objs for instrumentation
#' @export
instrumentation_finalize <- function()
{
    ## Revert value for INSTRUMENTATION_INIT
    if (!is_instrumentation_init()){
        print("ERROR: Cannot call `instrumentation_finalize` before `instrumentation_init`.")
        stop()
    }
    pkg.env$INSTRUMENTATION_INIT <- FALSE

    ## Ensure instrumententation disabled
    if (is_instrumentation_enabled()){
        warning("WARNING: Instrumentation currently enabled, will force disable before finalizing.")
        instrumentation_disable()
    }

    finalize_EvtWriter_client()
    finalize_sync_client()
    finalize_zmq_client()
    return(invisible(NULL))
}



#' instrumentation_debug
#' @description Enable certain debug features
#' @param print_func_indexes info
#' @param max_function_depth info
#' @param unlock_env info
#' @export
instrumentation_debug <- function(print_func_indexes = pkg.env$PRINT_FUNC_INDEXES,
                                  max_function_depth = pkg.env$MAX_FUNCTION_DEPTH,
                                  unlock_env = pkg.env$UNLOCK_ENVS )
{
    pkg.env$PRINT_FUNC_INDEXES <- print_func_indexes 
    pkg.env$MAX_FUNCTION_DEPTH <- max_function_depth 
    pkg.env$UNLOCK_ENVS <- unlock_env
    invisible()
}

#' instrumentation_wrapper
#' @description Simple function to provider wrapper for instrumenting a single function call.
#'  Ideal use if program contained in main(). Not intended to be used multiple times in one script
#' @param func Object - Function to call
#' @param ... Args - Function args
#' @export
instrumentation_wrapper <- function(func, ...)
{
    instrumentation_init()
    instrument_all_functions()

    instrumentation_enable()
    ret <- func(...) # Call actual function
    instrumentation_disable()

    instrumentation_finalize()
    ret
}