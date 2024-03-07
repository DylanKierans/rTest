# @file r_instrument_hl.R

#######################################################################
# SECTION - HIGH LEVEL, ENABLE/DISABLE INSTRUMENTATION
#######################################################################

# @TODO: zmq this
#' instrumentation_enable
#' @description Enable instrumentation and reset function depth
#' @export
instrumentation_enable <- function(){
    if (is_instrumentation_enabled()){
        print("Warning: Instrumentation already enabled!")
    }
    else if (!pkg.env$EVTWRITER_INIT) {
        pkg.env$FUNCTION_DEPTH <- 0
        init_EvtWriter()
        pkg.env$EVTWRITER_INIT <- TRUE
    } else {
        pkg.env$FUNCTION_DEPTH <- 0
        #evtWriter_MeasurementOnOff(TRUE)
    }
    pkg.env$INSTRUMENTATION_ENABLED <- TRUE
    invisible(NULL)
}

## @TODO: zmq this
##' instrumentation_disable
##' @description Disable instrumentation
##' @export
#instrumentation_disable <- function(){
#    if (!is_instrumentation_enabled()){
#        print("Warning: Instrumentation already disabled!")
#    }
#    else {
#        if (pkg.env$FUNCTION_DEPTH != 0){ 
#            print(paste0("Warning: Function depth non-zero relative to start region. Depth: ", pkg.env$FUNCTION_DEPTH) )
#        }
#        pkg.env$INSTRUMENTATION_ENABLED <- FALSE
#        #finalize_EvtWriter()
#        #evtWriter_MeasurementOnOff(FALSE)
#    }
#    invisible(NULL)
#}


# @TODO: Implement this properly
#' instrumentation_disable
#' @description Disable instrumentation
#' @export
instrumentation_disable <- function(){
    pkg.env$INSTRUMENTATION_ENABLED <- FALSE
    invisible(NULL)
}

#' is_instrumentation_enabled
#' @description Return current instrumentation status
#' @return BOOLEAN - Instrumentation status
#' @export
is_instrumentation_enabled <- function() {
    pkg.env$INSTRUMENTATION_ENABLED
}



# @TODONE: zmq this
#' instrumentation_init
#' @description Create otf2 objs for instrumentation, and initiate global vars
#' @param flag_user_functions Boolean - TRUE to include user functions in dataframe
#' @param verbose_wrapping Boolean - Print info about skipping or instrumenting each function. Produces large amount of info to stdout
#' @export
instrumentation_init <- function(flag_user_functions=T, verbose_wrapping=F)
{
    # @name PROFILE_INSTRUMENTATION_DF
    # @description Contains function name, package, and instrumentation flag
    pkg.env$PROFILE_INSTRUMENTATION_DF <- create_dataframe(flag_user_functions=flag_user_functions)

    # @name INSTRUMENTATION_INIT
    # @description Checked when instrumenting functions to ensure init() has been called
    pkg.env$INSTRUMENTATION_INIT <- TRUE

    # @name EVTWRITER_INIT
    # @description Checked when init_EvtWriter called for first time
    pkg.env$EVTWRITER_INIT <- FALSE

    ### SECTION - Instrument Flags ###
    # @name MAX_FUNCTION_DEPTH
    # @description Max depth of functions to creat instrumententation events for
    pkg.env$MAX_FUNCTION_DEPTH <- 10 

    # @name UNLOCK_ENVS
    # @description Keep package envs unlocked when instrumenting functions
    pkg.env$UNLOCK_ENVS <- TRUE # Not sure if this is safe to set TRUE, but should be quicker!

    ### SECTION - Output Flags ###
    # @name PRINT_SKIPS
    # @description Print which functions are being skipped due to exception
    pkg.env$PRINT_SKIPS <- verbose_wrapping

    # @name PRINT_INSTURMENTS
    # @description Print which functions are being instrumented
    pkg.env$PRINT_INSTRUMENTS <- verbose_wrapping

    # @name PRINT_FUNC_INDEXES
    # @description Print function indexes when called (only intended for verbose debugging)
    pkg.env$PRINT_FUNC_INDEXES <- FALSE

    ### SECTION - Init section for instrumentation ###
    # @name INSTRUMENTATION_ENABLED
    # @description Current status of instrumentation
    pkg.env$INSTRUMENTATION_ENABLED=FALSE

    # @name FUNCTION_DEPTH
    # @description Current instrumentation depth
    pkg.env$FUNCTION_DEPTH <- 0

#    ## Initiate OTF2 Archive
#    init_Archive()
#
#    ## Initiate OTF2 GlobalDefWriter
#    init_GlobalDefWriter()
#
#    ## Initiate OTF2 EvtWriter
#    init_EvtWriter()

    ## Initiate new proc
    init_otf2_logger()

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


## @TODO: zmq this
##' instrumentation_finalize
##' @description Close otf2 objs for instrumentation
##' @export
#instrumentation_finalize <- function()
#{
#    ## Revert value for INSTRUMENTATION_INIT
#    if (!is_instrumentation_init()){
#        print("ERROR: Cannot call `instrumentation_finalize` before `instrumentation_init`.")
#        stop()
#    }
#    pkg.env$INSTRUMENTATION_INIT <- FALSE
#
#    ## Ensure instrumententation disabled
#    if (is_instrumentation_enabled()){
#        warning("WARNING: Instrumentation currently enabled, will force disable before finalizing.")
#        instrumentation_disable()
#    }
#
#    # Close EvtWriter
#    finalize_EvtWriter()
#
#    ## Close GlobalDefWriter and Archive
#    globalDefWriter_WriteSystemTreeNode(0,0)
#    globalDefWriter_WriteLocation(0) # WriteLocation must be called at end of program due to NUM_EVENTS
#    finalize_GlobalDefWriter()
#    finalize_Archive()
#    return(invisible(NULL))
#

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

    # DEBUGGING
    print("Instrumentation_disabled")

    print("finalize_EvtWriter_client")
    finalize_EvtWriter_client()
    print("End of finalize_EvtWriter_client")
    print("finalize_otf2_client")
    finalize_otf2_client()
    print("End of finalize_otf2_client")
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

#' makeCluster_test
#' @description Testing makeCluster() 
#' @param x input
#' @export
makeCluster_test <- function(x){
    set_id(x)
    print("x=: ", get_id())
}

