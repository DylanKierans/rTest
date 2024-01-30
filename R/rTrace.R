#' @useDynLib rTrace
#' @importFrom Rcpp evalCpp 
#' @importFrom methods is
#' @importFrom utils head str
#' @importFrom R.utils isPackageLoaded
#' @importFrom rlang env_unlock env_lock env_binding_unlock env_binding_lock 
#' @exportPattern "^[[:alpha:]]+"

if (!require("Rcpp")) install.packages("Rcpp")
library("Rcpp")

#' rTrace_helloWorld_wrapper 
#' @export
rTrace_helloWorld_wrapper <- function() {
    cat("Wrapper for R/helloWorld.R::rTrace_helloWorld\n")
    rTrace_helloWorld()
}

#' pkg.env
#' @description Package-level environment used for storing package variables
#' @format
#' \describe{
#' 	\item{INSTRUMENTATION_ENABLED}{}
#' 	\item{FLAG_INSTRUMENT_ALL}{}
#'  \item{FLAG_INSTRUMENT_USER_FUNCTIONS}{}
#'  \item{MAX_FUNCTION_DEPTH}{}
#'  \item{FUNCTION_DEPTH}{}
#'  \item{UNLOCK_ENVS}{}
#'  \item{PROFILE_EVENTLOG}{}
#'  \item{PROFILE_EVENTLOG_NROWS}{}
#'  \item{PROFILE_INSTRUMENTATION_DF}{}
#'  \item{PRINT_INSTRUMENTS}{}
#'  \item{PRINT_SKIPS}{}
#'  \item{PRINT_FUNC_INDEXES}{}
#' }
#' 
pkg.env <- new.env(parent = emptyenv())
