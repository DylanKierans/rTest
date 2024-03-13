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
#' 	\item{INSTRUMENTATION_ENABLED}{}
#' 	\item{FLAG_INSTRUMENT_ALL}{}
#'  \item{FLAG_INSTRUMENT_USER_FUNCTIONS}{}
#'  \item{MAX_FUNCTION_DEPTH}{}
#'  \item{FUNCTION_DEPTH}{}
#'  \item{UNLOCK_ENVS}{}
#'  \item{PROFILE_INSTRUMENTATION_DF}{}
#'  \item{PRINT_INSTRUMENTS}{}
#'  \item{PRINT_SKIPS}{}
#'  \item{PRINT_FUNC_INDEXES}{}
#' }
#' 
pkg.env <- new.env(parent = emptyenv())
