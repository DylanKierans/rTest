# @file otf2_evtWriter.R
# @brief Wrapper for OTF2_EvtWriter functions
# @version 0.01
# @author D.Kierans (dylanki@kth.se)
# @date 2024-01-16

#' rTest_init_ - doxygen documentation
#'  Simple wrapper for C++ otf2 init function
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTest_init_EvtWriter <- function(flag_debug=F) {
    if (flag_debug) cat("Entering wrapper\n")
    init_EvtWriter()
    if (flag_debug) cat("Finished function.\n")
}

#' rTest_finalize_Archive- doxygen documentation
#'  Simple wrapper for C++ otf2 init function
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTest_finalize_EvtWriter <- function(flag_debug=F) {
    if (flag_debug) cat("Entering wrapper\n")
    finalize_EvtWriter()
    if (flag_debug) cat("Finished function.\n")
}


#' rTest_evtWriter_Write - doxygen documentation
#'  More info
#' @param regionRef Int - regionRef
#' @param event_type Bool - True if enter, False if exit
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTest_evtWriter_Write <- function(regionRef, event_type, flag_debug=F) {
    if (flag_debug) cat("Entering wrapper:\n")
    evtWriter_Write(regionRef, event_type)
    if (flag_debug) cat("Finished wrapper.\n")
}
