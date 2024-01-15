#' @useDynLib rTest
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"

if (!require("Rcpp")) install.packages("Rcpp")
library("Rcpp")

# Load cpp source files
#Rcpp::sourceCpp("src/test.cpp")

###
# SECTION - Functions
###
#' rTest_helloWorld - doxygen documentation
#'  Simple wrapper for C++ hello world function
#' @export
rTest_helloWorld <- function() {
    cat("Entering wrapper\n")
    helloWorld()
    cat("Finished function.\n")
}

#' rTest_init - doxygen documentation
#'  Simple wrapper for C++ otf2 init function
#' @export
rTest_init <- function() {
    cat("Entering wrapper\n")
    init()
    cat("Finished function.\n")
}

#' rTest_finalize - doxygen documentation
#'  Simple wrapper for C++ otf2 finalize function
#' @export
rTest_finalize <- function() {
    cat("Entering wrapper\n")
    simple_finalize()
    cat("Finished function.\n")
}


#' rTest_init_GlobalDefWriter - doxygen documentation
#'  More info
#' @export
rTest_init_GlobalDefWriter <- function() {
    cat("Entering wrapper\n")
    init_GlobalDefWriter()
    cat("Finished wrapper.\n")
}

#' rTest_globalDefWriter_WriteStrings - doxygen documentation
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

#' rTest_globalDefWriter_WriteRegions - doxygen documentation
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
