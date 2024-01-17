#' @useDynLib rTest
#' @importFrom Rcpp evalCpp 
#' @exportPattern "^[[:alpha:]]+"

if (!require("Rcpp")) install.packages("Rcpp")
library("Rcpp")

# Load cpp source files
#Rcpp::sourceCpp("src/test.cpp")
