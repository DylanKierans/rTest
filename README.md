  <!-- badges: start -->
[![R-CMD-check](https://github.com/DylanKierans/rTrace/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DylanKierans/rTrace/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# rTrace - LibOTF2 Wrapper for R

This is a development package for creating a high-level otf2 wrapper for tracing R scripts.

Wrapper built using [`Rcpp`](https://cran.r-project.org/web/packages/Rcpp/index.html).

Using `ZeroMQ` socket to communicate with process responsible for otf2 logging.

Using `PMPMEAS (Poor Man's Performance Measurement tool)` to interface with papi/perf for performance metrics.


## Installation

Install [`libotf2`](https://www.vi-hps.org/projects/score-p/), and [`zeromq`](https://github.com/zeromq) required dependency. 

Install [`papi`](https://hpc.llnl.gov/software/development-environment-software/papi-performance-application-programming-interface) optional (but recommended) dependency.

Then install `rTrace` from github with:

```R
devtools::install_github("DylanKierans/rTrace")
```

Refer to #debugging if you are having errors.

### DEBUGGING

Dependencies not found during installation. Either dependencies are not installed or are in non-standard directories.
```
configure: error: Unable to find FOO.h
ERROR: configuration failed for package ‘rTrace’
```

1. Error - `configure: error: Unable to find zmq.h`. Solution:
    
    ```R
    devtools::install_github("DylanKierans/rTrace", configure.args="--with-zmq=/path/to/zeromq/directory")
    ```

2. Error - `configure: error: Unable to find papi.h`. Solution: 
    
    ```R
    devtools::install_github("DylanKierans/rTrace", configure.args="--with-papi=/path/to/papi/directory")
    ```

3. Error - `configure: error: Unable to find linux/perf_event.h`. Solution:
    
    ```R
    devtools::install_github("DylanKierans/rTrace", configure.args="--with-perf=/path/to/perf/directory")
    ```

If you are receiving error about `devtools` not installed, first install with:

```R
install.packages("devtools") # if not yet installed
```


## Usage

```R
# <import packages>
# <define user functions>

instrumentation_init()
instrument_all_functions()

# <...enter relevant area...>
instrumentation_enable()
# <...do work...>
instrumentation_disable()
# <...exit relevant area...>

instrumentation_finalize()
```

## Authors 

Dylan Kierans

Dirk Pleiter (PMPMEAS)

## License 

Licensed under GPL-3.0

## TODO

* Clean up naming convention for:
    * Functions
    * Package variables in `pkg.env`

* Add get/set function for `pkg.env` global variables
    * `MAX_FUNCTION_DEPTH`
    * `PRINT_SKIPS`, `PRINT_FUNC_INDEXES`, `PRINT_INSTRUMENTS`
    * `FLAG_INSTRUMENT_ALL`, `FLAG_INSTRUMENT_USER_FUNCTIONS`

* Compile functions with `cmpfun` (disabled to speed up testing workflow)

* Allow users to specify functions to instrument or add exceptions

* Integrate socket and port management from tests branch

