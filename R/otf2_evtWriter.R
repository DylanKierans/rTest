# @file otf2_evtWriter.R
# @brief Wrapper for OTF2_EvtWriter functions
# @version 0.01
# @author D.Kierans (dylanki@kth.se)
# @date 2024-01-16

#' rTest_init_ - doxygen documentation
#'  Simple wrapper for C++ otf2 init function
#' @export
rTest_init_EvtWriter <- function() {
    cat("Entering wrapper\n")
    init_EvtWriter()
    cat("Finished function.\n")
}

#' rTest_finalize_Archive- doxygen documentation
#'  Simple wrapper for C++ otf2 init function
#' @export
rTest_finalize_EvtWriter <- function() {
    cat("Entering wrapper\n")
    finalize_EvtWriter()
    cat("Finished function.\n")
}


#' rTest_evtWriter_Write - doxygen documentation
#'  More info
#' @param regionRef Int - regionRef
#' @param event_type Bool - True if enter, False if exit
#' @export
rTest_evtWriter_Write <- function(regionRef, event_type) {
    cat("Entering wrapper:\n")
    evtWriter_Write(regionRef, event_type)
    cat("Finished wrapper.\n")
}
