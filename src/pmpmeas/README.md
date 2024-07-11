# PMPMEAS: Poor Man's Performance Measurement tool

## Building

To build the software, run ```cmake <path_to_source>``` in the directory where you want to compile the software. Next run ```make```.

### Integrating PMPMEAS in your code

The library provides only four functions to be used in your code:

- ```pmpmeas_init```: Initialise the library
- ```pmpmeas_start(tag)```: Start a measurement
- ```pmpmeas_stop(weight)```: Stop a measurement
- ```pmpmeas_finish```: Final clean-up and dump of measurements

See examples for how to use PMPMEAS from C, C++ or Fortran in the directory ```src/tests```.

```pmpmeas_start``` has to be provided with a unique tag that identifies a measurement.

```pmpmeas_stop``` always stops a previously started measurement, i.e. no tag has to be specified. The weight factor allows to give a given measurement a specific weight. For instance, if the measurements includes a loop then the weight may be chosen as the inverse of the number of loop iterations.

### Using PMPMEAS

The measurements are steered through the environment variable ```PMPMEAS_MEAS_TYPES```. This means that different measurements can be performed using the same binary.

The content of the environment variable has to take the following form: ```<type>[;<type>]...```. Where ```type``` can take the following values:

- ```TIME_BOOT```: Measure time by calling the function ```clock_gettime``` with the clock ```CLOCK_BOOTTIME```
- ```TIME_CPU```: Measure time by calling the function ```clock_gettime``` with the clock ```CLOCK_PROCESS_CPUTIME_ID```
- ```TIME_THRD```: Measure time by calling the function ```clock_gettime``` with the clock ```CLOCK_THREAD_CPUTIME_ID```
- ```PAPI=<event>[,<event>]```: Use the PAPI low-level interface with ```event``` being the name of an event as shown by ```papi_avail``` or ```papi_native_avail```
- ```PERF=<event>[,event>]```: Use the perf interface with ```event``` being either of the following values (check ```src/include/perfinftypesxx.h``` for possible changes):
  - ```cycles```
  - ```ref_cycles```
  - ```L1_dcache_loads```
  - ```L1_dcache_load_misses```
  - ```L1_dcache_stores```
  - ```LLC_loads```
  - ```LLC_load_misses```
  - ```LLC_stores```
  - ```dTLB_loads```
  - ```dTLB_load_misses```

Some examples:

- ```export PMPMEAS_MEAS_TYPES="TIME_CPU;PAPI=PAPI_L1_DCM,PAPI_L2_DCM"```
- ```export PMPMEAS_MEAS_TYPES="TIME_CPU;PERF=cycles,LLC_loads"```

WARNING: Both PAPI and Perf may access the performance monitor unit (PMU) and thus interfere. It is therefore not recommended to use PAPI and Perf counters in the same run.

At the end of each run, the results will be dumped in a file in the local directory with a unique name.
