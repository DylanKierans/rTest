  <!-- badges: start -->
[![R-CMD-check](https://github.com/DylanKierans/rTest/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DylanKierans/rTest/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# rTrace - LibOTF2 Wrapper for R

This is a development package for creating a high-level otf2 wrapper for tracing R scripts.

Wrapper built using [`Rcpp`](https://cran.r-project.org/web/packages/Rcpp/index.html).


## Installation

Installation steps:

1. Install dependencies: [`libotf2`](https://www.vi-hps.org/projects/score-p/)

2. Clone git repository

3. Install from source with R command: `install.packages("/path/to/rTest/repo",repos=NULL,type="source")`

4. Verify installation by loading package with R command: `library("rTrace")`


## TODO

* Simplify init/finalize process

* Clean up naming convention for:
    * Functions
    * Package variables in `pkg.env`

* Add get/set functions for `pkg.env` global variables
    * `MAX_FUNCTION_DEPTH`
    * `PRINT_SKIPS`, `PRINT_FUNC_INDEXES`, `PRINT_INSTRUMENTS`
    * `FLAG_INSTRUMENT_ALL`, `FLAG_INSTRUMENT_USER_FUNCTIONS`

* Compile functions with `cmpfun` (disabled to speed up testing workflow)

* Allow users to specify functions to instrument or add exceptions

