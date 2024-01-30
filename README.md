  <!-- badges: start -->
[![R-CMD-check](https://github.com/DylanKierans/rTest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DylanKierans/rTest/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# rTest - LibOTF2 Wrapper for R

This is a test package for creating a high-level otf2 wrapper for R. Wrapper built using [`Rcpp`](https://cran.r-project.org/web/packages/Rcpp/index.html).

## Dependencies

* LibOTF2

## TODO

* Simplify init/finalize process

* Check documentation generated as expected

* Clean up naming convention

* Add get/set functions for `pkg.env` global variables
    * `MAX_FUNCTION_DEPTH`
    * `PRINT_SKIPS`, `PRINT_FUNC_INDEXES`, `PRINT_INSTRUMENTS`
    * `FLAG_INSTRUMENT_ALL`, `FLAG_INSTRUMENT_USER_FUNCTIONS`

* Compile functions with `cmpfun` (disabled for speed during testing)

* Allow users to specify functions to instrument or add exceptions

