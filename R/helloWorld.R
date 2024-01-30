# @file helloWorld.R
# @brief Wrapper for simple helloWorld function
# @version 0.01
# @author D.Kierans (dylanki@kth.se)
# @date 2024-01-16

#' rTrace_helloWorld - doxygen documentation
#'  Simple wrapper for C++ hello world function
#' @export
rTrace_helloWorld <- function() {
    cat("Entering wrapper\n")
    helloWorld()
    cat("Finished function.\n")
}
