# @file otf2_archive
# @brief Wrapper for OTF2_Archive functions
# @version 0.01
# @author D.Kierans (dylanki@kth.se)
# @date 2024-01-16

#' rTest_init_Archive
#'  Simple wrapper for C++ otf2 init function
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTest_init_Archive <- function(flag_debug=F) {
    if (flag_debug) cat("Entering wrapper init_Archive\n")
    init_Archive()
    if (flag_debug) cat("Finished function init_Archive.\n")
}

#' rTest_finalize_Archive
#'  Simple wrapper for C++ otf2 finalize function
#' @param flag_debug Boolean - True to enable verbose stdout
#' @export
rTest_finalize_Archive <- function(flag_debug=F) {
    if (flag_debug) cat("Entering wrapper finalize_Archive\n")
    finalize_Archive()
    if (flag_debug) cat("Finished function finalize_Archive.\n")
}


