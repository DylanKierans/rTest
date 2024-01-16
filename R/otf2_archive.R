#' @file otf2_archive
#' @brief Wrapper for OTF2_Archive functions
#' @version 0.01
#' @author D.Kierans (dylanki@kth.se)
#' @date 2024-01-16

#' rTest_init_Archive- doxygen documentation
#'  Simple wrapper for C++ otf2 init function
#' @export
rTest_init_Archive <- function() {
    cat("Entering wrapper\n")
    init_Archive()
    cat("Finished function.\n")
}

#' rTest_finalize_Archive- doxygen documentation
#'  Simple wrapper for C++ otf2 init function
#' @export
rTest_finalize_Archive <- function() {
    cat("Entering wrapper\n")
    finalize_Archive()
    cat("Finished function.\n")
}


