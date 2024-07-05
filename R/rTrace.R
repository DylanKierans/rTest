# @name rTrace
# @author D.Kierans, \email{dylanki@@kth.se}
# @references \url{https://github.com/DylanKierans/rTrace}
# @usage Usage:
# \describe{
#  <import packages>
#  <define user functions>
#  instrumentation_init() - otf2 updated
#  instrument_all_functions() - otf2 updated
#  <...>
#  <enter relevant area>
#  instrumentation_enable()
#  <do work>
#  instrumentation_disable()
#  <exit relevant area>
#  instrumentation_finalize()
# }

#' @useDynLib rTrace
#' @importFrom Rcpp evalCpp 
#' @importFrom methods is
#' @importFrom utils head str
#' @importFrom R.utils isPackageLoaded
#' @importFrom rlang env_unlock env_lock env_binding_unlock env_binding_lock 
#' @importFrom parallelly availableCores
#' @exportPattern "^[[:alpha:]]+"



if (!require("Rcpp")) install.packages("Rcpp")
library("Rcpp")

#' pkg.env
#' @description Package-level environment used for storing package variables
#' @format
#' \describe{
#' 	\item{INSTRUMENTATION_INIT}{}
#' 	\item{INSTRUMENTATION_ENABLED}{}
#' 	\item{INSTRUMENTATION_STATUS_SAVED}{}
##' 	\item{FLAG_INSTRUMENT_ALL}{}
##'  \item{FLAG_INSTRUMENT_USER_FUNCTIONS}{}
#'  \item{MAX_FUNCTION_DEPTH}{}
#'  \item{FUNCTION_DEPTH}{}
#'  \item{UNLOCK_ENVS}{}
#'  \item{PRINT_INSTRUMENTS}{}
#'  \item{PRINT_SKIPS}{}
#'  \item{PRINT_FUNC_INDEXES}{}
#'  \item{COLLECT_METRICS}{}
#'  \item{RTRACE_VARS}{}
#' }
#' 
pkg.env <- new.env(parent = emptyenv())

### SECTION - Init section for instrumentation ###
# @name INSTRUMENTATION_INIT
# @description Boolean - Checked when instrumenting functions to ensure init() has been called
pkg.env$INSTRUMENTATION_INIT <- FALSE

# @name INSTRUMENTATION_ENABLED
# @description Boolean - Current status of instrumentation
pkg.env$INSTRUMENTATION_ENABLED <- FALSE

# @name INSTRUMENTATION_STATUS_SAVED
# @description Boolean - Saved status of instrumentation
pkg.env$INSTRUMENTATION_STATUS_SAVED <- FALSE

### SECTION - Instrument Flags ###
# @name MAX_FUNCTION_DEPTH
# @description int - Max depth of functions to creat instrumententation events for
pkg.env$MAX_FUNCTION_DEPTH <- 10 

# @name FUNCTION_DEPTH
# @description int - Current instrumentation depth
pkg.env$FUNCTION_DEPTH <- 0

# @name UNLOCK_ENVS
# @description Boolean - Keep package envs unlocked when instrumenting functions
pkg.env$UNLOCK_ENVS <- TRUE # Not sure if this is safe to set TRUE, but should be quicker!

### SECTION - Output Flags ###
# @name PRINT_INSTRUMENTS
# @description Boolean - Print which functions are being instrumented
pkg.env$PRINT_INSTRUMENTS <- FALSE

# @name PRINT_SKIPS
# @description Boolean - Print which functions are being skipped due to exception
pkg.env$PRINT_SKIPS <- FALSE

# @name PRINT_FUNC_INDEXES
# @description Boolean - Print function indexes when called (only intended for verbose debugging)
pkg.env$PRINT_FUNC_INDEXES <- FALSE


# @name COLLECT_METRICS
# @description Boolean - Enable collection of Hardware Performance Metrics (HWPC)
pkg.env$COLLECT_METRICS <- FALSE


# @name RTRACE_VARS
# @description List of all global rTrace variables
pkg.env$RTRACE_VARS <- c( "INSTRUMENTATION_INIT", "INSTRUMENTATION_ENABLED",
        "INSTRUMENTATION_STATUS_SAVED", "MAX_FUNCTION_DEPTH",
        "FUNCTION_DEPTH", "UNLOCK_ENVS", "PRINT_INSTRUMENTS",
        "PRINT_SKIPS", "PRINT_FUNC_INDEXES", 
        "COLLECT_METRICS"
    )

