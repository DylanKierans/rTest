#' @useDynLib rTest
#' @importFrom Rcpp evalCpp 
#' @importFrom methods is
#' @importFrom utils head str
#' @importFrom R.utils isPackageLoaded
#' @importFrom rlang env_unlock env_lock env_binding_unlock env_binding_lock 
#' @exportPattern "^[[:alpha:]]+"

if (!require("Rcpp")) install.packages("Rcpp")
library("Rcpp")

# source files
#source("R/helloWorld.R")
#source("R/otf2_archive.R")
#source("R/otf2_evtWriter.R")
#source("R/otf2_globalDefWriter.R")

#' rTest_helloWorld_wrapper - doxygen documentation
#' @export
rTest_helloWorld_wrapper <- function() {
    cat("Wrapper for R/helloWorld.R::rTest_helloWorld\n")
    rTest_helloWorld()
}

#' Package level environment
#'  Used for storing package level variables (flags eg INSTRUMENTATION_ENABLED, and counters eg PROFILE_EVENTLOG)
#'  - INSTRUMENTATION_ENABLED
#'  - FLAG_INSTRUMENT_ALL
#'  - FLAG_INSTRUMENT_USER_FUNCTIONS
#'  - MAX_FUNCTION_DEPTH
#'  - FUNCTION_DEPTH
#'  - UNLOCK_ENVS
#'  - PROFILE_EVENTLOG
#'  - PROFILE_EVENTLOG_NROWS
#'  - PROFILE_INSTRUMENTATION_DF
#'  - PRINT_INSTRUMENTS
#'  - PRINT_SKIPS
#'  - PRINT_FUNC_INDEXES
pkg.env <- new.env()
