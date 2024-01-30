# @file OTF2_GlobalDefWriter.R
# @brief Wrapper for OTF2_GlobalDefWriter functions
# @version 0.01
# @author D.Kierans (dylanki@kth.se)
# @date 2024-01-16

#' init_GlobalDefWriter - doxygen documentation
#'  More info
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTrace_init_GlobalDefWriter <- function(flag_debug=F) {
    if (flag_debug) cat("Entering wrapper rTrace_init_GlobalDefWriter\n")
    init_GlobalDefWriter()
    if (flag_debug) cat("Finished wrapper rTrace_init_GlobalDefWriter.\n")
}

#' finalize_GlobalDefWriter - doxygen documentation
#'  More info
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTrace_finalize_GlobalDefWriter <- function(flag_debug=F) {
    if (flag_debug) cat("Entering wrapper rTrace_finalize_GlobalDefWriter\n")
    finalize_GlobalDefWriter()
    if (flag_debug) cat("Finished wrapper rTrace_finalize_GlobalDefWriter.\n")
}

#
#' globalDefWriter_WriteString - doxygen documentation
#'  More info
#' @param stringRefValue String - String for globalDefWriter
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTrace_globalDefWriter_WriteString <- function(stringRefValue, flag_debug=F) {
    if (flag_debug) cat("Entering wrapper rTrace_globalDefWriter_WriteString\n")
    stringRef <- globalDefWriter_WriteString(stringRefValue)
    if (flag_debug) cat("Finished wrapper rTrace_globalDefWriter_WriteString.\n")
    stringRef
}

#' globalDefWriter_WriteRegion - doxygen documentation
#'  More info
#' @param stringRef_RegionName Int - Index for names, taken from stringRef's
#' @return regionRef Int - Index for regionRef
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTrace_globalDefWriter_WriteRegion <- function(stringRef_RegionName, flag_debug=F){
    if (flag_debug) cat("Entering wrapper rTrace_globalDefWriter_WriteRegion\n")
    regionRef <- globalDefWriter_WriteRegion(stringRef_RegionName)
    if (flag_debug) cat("Finished wrapper rTrace_globalDefWriter_WriteRegion\n")
    regionRef
}

#' globalDefWriter_WriteSystemTreeNode - doxygen documentation
#'  More info 
#' @param stringRef_name Int - Index for name of system tree
#' @param stringRef_class Int - Index for class of system tree
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTrace_globalDefWriter_WriteSystemTreeNode <- function(stringRef_name, stringRef_class, flag_debug=F) {
    if (flag_debug) cat("Entering globalDefWriter_WriteSystemTreeNode\n")
    globalDefWriter_WriteSystemTreeNode(stringRef_name, stringRef_class)
    if (flag_debug) cat("Finished globalDefWriter_WriteSystemTreeNode.\n")
}


#' globalDefWriter_WriteLocation - doxygen documentation
#' @param stringRef_name Int - Index for name of location
#'  More info
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTrace_globalDefWriter_WriteLocation <- function(stringRef_name, flag_debug=F) {
    if (flag_debug) cat("Entering wrapper rTrace_globalDefWriter_WriteLocation\n")
    globalDefWriter_WriteLocation(stringRef_name)
    if (flag_debug) cat("Finished wrapper rTrace_globalDefWriter_WriteLocation.\n")
}


