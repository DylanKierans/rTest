#' @file OTF2_GlobalDefWriter.R
#' @brief Wrapper for OTF2_GlobalDefWriter functions
#' @version 0.01
#' @author D.Kierans (dylanki@kth.se)
#' @date 2024-01-16

#' init_GlobalDefWriter - doxygen documentation
#'  More info
#' @export
rTest_init_GlobalDefWriter <- function() {
    cat("Entering wrapper\n")
    init_GlobalDefWriter()
    cat("Finished wrapper.\n")
}

#' globalDefWriter_WriteStrings - doxygen documentation
#'  More info
#' @param stringRef_array Int[] - Index list for stringRef's 
#' @param stringRefValue_array String[] - String list for globalDefWriter
#' @export
rTest_globalDefWriter_WriteStrings <- function(stringRef_array, stringRefValue_array) {
    cat("Entering wrapper\n")
    for ( i in 1:length(stringRef_array) ){
        stringRef <- stringRef_array[i]
        stringRefValue <- stringRefValue_array[i]
        globalDefWriter_WriteString(stringRef, stringRefValue)
    }
    cat("Finished wrapper.\n")
}


#' globalDefWriter_WriteRegions - doxygen documentation
#'  More info
#' @param regionRef_array Int[] - Index list for regionReg's
#' @param regionRefName_array Int[] - Index list for names, taken from stringRef's
#' @export
rTest_globalDefWriter_WriteRegions <- function(regionRef_array, regionRefName_array) {
    cat("Entering wrapper\n")
    for ( i in 1:length(regionRef_array) ){
        regionRef <- regionRef_array[i]
        regionRefName <- regionRefName_array[i]
        globalDefWriter_WriteRegion(regionRef, regionRefName)
    }
    cat("Finished wrapper.\n")
}

#' globalDefWriter_WriteSystemTreeNode - doxygen documentation
#'  More info 
#' @param stringRef_name Int - Index for name of system tree
#' @param stringRef_class Int - Index for class of system tree
#' @export
rTest_globalDefWriter_WriteSystemTreeNode <- function(stringRef_name, stringRef_class) {
    cat("Entering globalDefWriter_WriteSystemTreeNode\n")
    globalDefWriter_WriteSystemTreeNode(stringRef_name, stringRef_class)
    cat("Finished globalDefWriter_WriteSystemTreeNode.\n")
}



#' globalDefWriter_WriteLocation - doxygen documentation
#' @param stringRef_name Int - Index for name of location
#'  More info
#' @export
rTest_globalDefWriter_WriteLocation <- function(stringRef_name) {
    cat("Entering wrapper\n")
    globalDefWriter_WriteLocation(stringRef_name)
    cat("Finished wrapper.\n")
}


