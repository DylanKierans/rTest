#' @useDynLib rTest
#' @importFrom Rcpp evalCpp 
#' @exportPattern "^[[:alpha:]]+"

if (!require("Rcpp")) install.packages("Rcpp")
library("Rcpp")

# source files
source("R/helloWorld.R")

#' rTest_helloWorld_wrapper - doxygen documentation
#' @export
rTest_helloWorld_wrapper <- function() {
    cat("Wrapper for R/helloWorld.R::rTest_helloWorld\n")
    rTest_helloWorld()
}
