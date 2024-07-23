#ifndef INCLUDE_RTRACE_H_HJSCJAAJSJJSA
#define INCLUDE_RTRACE_H_HJSCJAAJSJJSA

// Required for rTrace.h
#include <list>
#include <otf2/otf2.h>
#include "Rcpp.h"
#include "config.h"

// PMPMEAS
#include "pmpmeas-api.h"
#include "pmpmeas.hpp"
#include "meastypes.hpp"
#include "meas.hpp"

//#define DEBUG /* Uncomment to enable verbose debug info */
#define MAX_FUNCTION_NAME_LEN 40 // Max length of R function
#define SIGRTRACE SIGUSR1

// Different events during entry collection phase
typedef enum {
    ZMQ_OTF2_EVENT_ENTER, 
    ZMQ_OTF2_EVENT_LEAVE, 
    ZMQ_OTF2_MEASUREMENT_ON, 
    ZMQ_OTF2_MEASUREMENT_OFF, 
    ZMQ_OTF2_USED_LOCATIONREFS, 
    ZMQ_OTF2_SOCK_CLUSTER_ON,
    ZMQ_OTF2_SOCK_CLUSTER_OFF
} zmq_otf2_datatypes;

// Struct used for majority of data transfer during event collection phase
typedef struct Zmq_otf2_data {
    OTF2_TimeStamp time;  
    OTF2_RegionRef regionRef; ///< Could probably generalize this datatype better, used for diverse int-like datatypes, see set_maxUsedLocationRef()
    pid_t pid;
    zmq_otf2_datatypes datatype;
} Zmq_otf2_data;

// Struct used for defining globalDefWriter key-values
typedef struct Zmq_otf2_defWriter {
    char func_name[MAX_FUNCTION_NAME_LEN];
    int func_index; 
} Zmq_otf2_defWriter;


///////////////////////////////
// Global vars
///////////////////////////////

extern std::list<PMPMEAS::MeasType*> pmpmeas_type_lst;
extern std::list<PMPMEAS::Meas*> pmpmeas_meas_lst;
extern std::list<PMPMEAS::Meas*> pmpmeas_match_lst;
extern OTF2_EvtWriter** evt_writers;

// DEBUGGING
static FILE *fp; ///* Log file on server proc
char log_filename[]="log.log"; ///* Name of log file on server proc


///////////////////////////////
// Function declarations
///////////////////////////////
// R client functions (master/slaves)
RcppExport int init_otf2_logger(int max_nprocs, Rcpp::String archivePath = "rTrace", 
        Rcpp::String archiveName = "rTrace", bool overwrite_archivePath=false, bool collect_metrics=false,
        bool flag_print_pids=false);
RcppExport SEXP finalize_GlobalDefWriter_client();
RcppExport int define_otf2_regionRef_client(Rcpp::String, int);
RcppExport SEXP open_EvtWriterSocket_client();
RcppExport SEXP close_EvtWriterSocket_client();
RcppExport SEXP evtWriter_Write_client(int, bool);
RcppExport SEXP evtWriter_MeasurementOnOff_client(bool);
RcppExport int set_maxUsedLocationRef_client(int);
RcppExport SEXP stopCluster_master();
RcppExport SEXP finalize_EvtWriter_client();
RcppExport SEXP finalize_otf2_client();
RcppExport SEXP assign_regionRef_array_master(int);
RcppExport SEXP get_regionRef_array_master(const int);
RcppExport SEXP assign_regionRef_array_slave(int);
RcppExport int get_regionRef_from_array_slave(int);
RcppExport SEXP free_regionRef_array_slave();
RcppExport SEXP get_regionRef_array_slave(const int);

// OTF2 Server/logger functions
void init_Archive_server(const char* archivePath="./rTrace", const char* archiveName="./rTrace");
void finalize_Archive_server();
void init_EvtWriters_server();
void finalize_EvtWriters_server();
void init_GlobalDefWriter_server();
void finalize_GlobalDefWriter_server();
void run_EvtWriters_server(bool);
void globalDefWriter_server();
void finalize_otf2_server();
OTF2_StringRef globalDefWriter_WriteString_server(const char* stringRefValue);
OTF2_RegionRef globalDefWriter_WriteRegion_server(OTF2_StringRef stringRef_RegionName);
void globalDefWriter_WriteSystemTreeNode_server(OTF2_StringRef, OTF2_StringRef);
void globalDefWriter_WriteLocations_server(bool flag_server_logfile=true);
void globalDefWriter_WriteLocationGroups_server();
void evtWriter_MeasurementOnOff_server(OTF2_EvtWriter*, OTF2_TimeStamp, bool);
void assign_regionRef_array_server();
int get_regionRef_array_server(OTF2_RegionRef, void*);
void free_regionRef_array_server();
void globalDefWriter_metrics_server();
void evtWriter_metric_server();

// Wrappers for pmpmeas
RcppExport SEXP r_pmpmeas_init();
RcppExport SEXP r_pmpmeas_finish();
RcppExport SEXP r_pmpmeas_start();
RcppExport SEXP r_pmpmeas_stop(float);

// Helper functions for debugging
void fupdate_server(FILE*, const char*);
RcppExport SEXP print_errnos();
void set_maxLocationRef(OTF2_LocationRef x);
void set_collectMetrics(bool);

// Universal functions
OTF2_TimeStamp get_time();
void sighup_handler(int signal);
void sigrtrace_handler(int signal);
void sigint_handler(int signal);

#endif /* Include guards rTrace.h */