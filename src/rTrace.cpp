// @file rTrace.cpp
// @brief Rcpp functions for underlying otf2 library
// @date 2024-07-02
// @version 0.03
// @author D.Kierans (dylanki@kth.se)
// @note Location ~= thread, LocationGroup ~= Process, SystemTree ~= Node
// @note errno 98 indicates socket alreayd in use 
// @note errno 88 (ENOTSOCK) indicates not socket
// @note errno 4 (EINTR) Interrupted system call
// @note errno 11 (EAGAIN/EWOULDBLOCK) Resource (socket) not available
// @note Clients refer to active R processes {master,slaves}=={clients}. 
//      Server refers to otf2 logger proc {server}n{clients}=0
// @note WARNING - Was receiving consistent noise on port 5558
// @todo Look at timing offsets per proc
// @todo Signal handling for both procs
// @todo get epochs from Master proc
// @todo Multipart message implimentation for repeated buffer usage
// @todo Test removal of regionRef on client end. 
//      Would reduce synchronization time when spawning new procs, but may increase 
//      evtWrite time on server due to random memory access
//
// @note Use pmpmeas_init to parse environment variables
// @note New object Meas for each meas_type, contains multiple metrics via _cnt
//   Names and values stored in papi/perf interface (include/papiinf.hh)
// @note Move pmpmeas to sub-directory and update buildchain

#include "Rcpp.h"
#include <otf2/otf2.h>
#include <sys/time.h>
#include <stdio.h>
#include "utils.h"

// getpid
#include <sys/types.h>
#include <unistd.h>

// ZeroMQ
#include <zmq.h>
#include <sys/wait.h>

// PMPMEAS
#include <stdbool.h>
#include "pmpmeas.h"
#include "meas.h"
#include "meastypes.h"
#include "pmpmeas-api.h"
using namespace PMPMEAS;

//#define DEBUG /* Uncomment to enable verbose debug info */
//#define DUMMY_TIMESTEPS /* Uncomment for 1s timestep for each subsequent event call */
#define MAX_FUNCTION_NAME_LEN 40 // Max length of R function

using namespace Rcpp;

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

// OTF2 objects for logger
static OTF2_Archive* archive;
static OTF2_GlobalDefWriter* global_def_writer;
OTF2_TimeStamp epoch_start, epoch_end;  // OTF2_GlobalDefWriter_WriteClockProperties
static OTF2_EvtWriter** evt_writers;

// ZeroMQ sockets
static bool IS_LOGGER=false; ///* Used by report_and_exit() to abtain exit behaviour
static void *context;      ///* zmq context - clients and server
static void *requester;    ///* zmq socket - master(5555) and slaves(5559) (comm with responder for globalDefWriter)
static void *pusher;    ///* zmq socket - clients (comm with puller for EvtWriter)

// Counters
static const OTF2_StringRef OFFSET_NUM_STRINGREF=10; ///* Offset for NUM_STRINGREF to avoid overwriting
static OTF2_StringRef NUM_STRINGREF=OFFSET_NUM_STRINGREF; ///* Number of events recorded with WriteString, offset to avoid overwriting
static OTF2_RegionRef NUM_REGIONREF=0; ///* Number of regions recorded with WriteRegion
static OTF2_RegionRef *regionRef_array; ///* regionRef for each func_index on server
static int NUM_FUNCS;  ///* total num R functions to instrument - length(reigonRef_array)

// IDs
static OTF2_LocationRef maxLocationRef=0; ///* Cap for max number of R procs
static OTF2_LocationRef maxUsedLocationRef=1; ///* Maximum number of used R procs <maxLocationRef
static int locationRef=0; ///* LocationRef of current client proc

// PMPMEAS - Metric collection
static bool COLLECT_METRICS = false;
static int NUM_METRICS=0;  ///* Number of metrics, only applicable if COLLECT_METRICS is defined/enabled
static long long *pmpmeas_vals;
static int pmpmeas_n;
static OTF2_Type *typeIDs;
std::list<MeasType*> pmpmeas_type_lst;
std::list<Meas*> pmpmeas_meas_lst;
std::list<Meas*> pmpmeas_match_lst;

// DEBUGGING
static FILE *fp; ///* Log file on server proc
char log_filename[]="log.log"; ///* Name of log file on server proc


///////////////////////////////
// Function declarations
///////////////////////////////
// R client functions (master/slaves)
//RcppExport int init_otf2_logger(int, Rcpp::String, Rcpp::String, bool);
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
void init_Archive_server(Rcpp::String, Rcpp::String);
void finalize_Archive_server();
void init_EvtWriters_server();
void finalize_EvtWriters_server();
void init_GlobalDefWriter_server();
void finalize_GlobalDefWriter_server();
void run_EvtWriters_server(bool);
void globalDefWriter_server();
void finalize_otf2_server();
OTF2_StringRef globalDefWriter_WriteString_server(Rcpp::String stringRefValue);
OTF2_RegionRef globalDefWriter_WriteRegion_server(OTF2_StringRef stringRef_RegionName);
void globalDefWriter_WriteSystemTreeNode_server(OTF2_StringRef, OTF2_StringRef);
void globalDefWriter_WriteLocations_server();
void globalDefWriter_WriteLocationGroups_server();
void evtWriter_MeasurementOnOff_server(OTF2_EvtWriter*, OTF2_TimeStamp, bool);
void assign_regionRef_array_server();
int get_regionRef_array_server(OTF2_RegionRef, void*);
void free_regionRef_array_server();
void globalDefWriter_metrics_server();

// Wrappers for pmpmeas
RcppExport SEXP r_pmpmeas_init();
RcppExport SEXP r_pmpmeas_finish();
RcppExport SEXP r_pmpmeas_start();
RcppExport SEXP r_pmpmeas_stop(float);

// Helper functions for debugging
void fupdate_server(FILE*, const char*);
RcppExport SEXP print_errnos();

///////////////////////////////
// Function definitions
///////////////////////////////

// TODO: Ensure this doesn't cause overflow of wtime
// @name get_time
// @description Returns wall-clock time in units of milliseconds (1E-6s)
// @return OTF2_Timestamp - Wallclock time (or ncounts if DUMMY_TIMESTEPS #defined)
static OTF2_TimeStamp get_time() {
    static OTF2_TimeStamp wtime;

    // Dummy timestamps O(1)
#ifdef DUMMY_TIMESTEPS
#ifdef DEBUG
    Rcout << "time: " << wtime << "\n";
#endif 
    return wtime++;
#endif 

    // Wallclock time O(1E-6)
	struct timeval t;
	gettimeofday(&t, NULL);
    wtime = t.tv_sec*1E6 + t.tv_usec;
#ifdef DEBUG
    Rcout << "time: " << wtime << "\n";
#endif /* ifdef DEBUG */
	return wtime;
}


static OTF2_FlushType pre_flush( void* userData, OTF2_FileType fileType,
           OTF2_LocationRef location, void* callerData, bool final ) {
    return OTF2_FLUSH;
}


static OTF2_TimeStamp post_flush( void* userData, OTF2_FileType fileType,
            OTF2_LocationRef location ) {
    return get_time();
}


static OTF2_FlushCallbacks flush_callbacks =
{
    .otf2_pre_flush  = pre_flush,
    .otf2_post_flush = post_flush
};

//////////////////////////////////////
// Signal hanlders
//////////////////////////////////////

// TODO: Review usage of SIGHUP during R makeCluster()
// @name signal_hup_handler
// @description This was introduced due to R procs being sent SIGHUP during forking
void signal_hup_handler(int signal) {
    // Make sure only catching intended signal, else rethrow
    if (signal == SIGHUP) { /*ignore*/; }
    else { raise(signal); }
}


//////////////////////////////////////
// Spawn otf2 process, and give task list
//  Awaits messages from main process at suitable steps
//////////////////////////////////////

//' Fork and initialize zeromq sockets for writing globalDef definitions
//' @param max_nprocs Maximum number of R processes (ie evtWriters required)
//' @param archivePath Path to otf2 archive
//' @param archiveName Name of otf2 archive
//' @param collect_metrics Collect HWPC metrics via pmpmeas
//' @param flag_print_pids True to print pids of parent and child procs
//' @return <0 if error, 0 if R master, else >0 if child
// [[Rcpp::export]]
RcppExport int init_otf2_logger(int max_nprocs, Rcpp::String archivePath = "./rTrace", 
        Rcpp::String archiveName = "rTrace", bool collect_metrics=false,
        bool flag_print_pids=false)
{
    // TODO: Verify this acts as intended to save child proc
    signal(SIGHUP, signal_hup_handler);

    // YOU ARE HERE
    // Set COLLECT_METRICS global on server and client before fork
    if (collect_metrics){
        #ifndef _COLLECT_METRICS
            Rcpp::stop("rTrace not built for metric collection. Rebuild or run with `collect_metrics=false`");
        #endif

        // Called via R r_pmpmeas_init() before init_otf2_logger()
        //pmpmeas_init();
        //pmpmeas_read_init(&pmpmeas_vals, &pmpmeas_n);
    }
    COLLECT_METRICS = collect_metrics;

    pid_t child_pid = fork();
    if (child_pid == (pid_t) -1 ){ // ERROR
        report_and_exit("Forking logger process", NULL);
        return(1);
    }

    if (child_pid == 0) { // Child process

        // DEBUGGING
        if (flag_print_pids){
            Rcout << "LOGGER PROC - pid: " << getpid() << ", child_pid:" << child_pid << "\n";
        }

        IS_LOGGER = true;
        maxLocationRef = max_nprocs;

        // Open log file
        fp = fopen(log_filename, "w");
        if (fp==NULL){ report_and_exit("Opening log file", NULL); }
        fupdate_server(fp, "File opened\n");

        // OTF2 Objs
        init_Archive_server(archivePath, archiveName);
        fupdate_server(fp, "Init archive complete\n");
        init_EvtWriters_server( );
        fupdate_server(fp, "Init evt_writers complete\n");
        init_GlobalDefWriter_server();
        fupdate_server(fp, "Init of otf2 objs complete\n");

        // Init zmq context
        context = zmq_ctx_new();

        // Server creates metrics from pmpmeas objects
        if (COLLECT_METRICS){
            globalDefWriter_metrics_server();
            fupdate_server(fp, "globalDefWriter_metrics_server complete\n");
        }

        // Assign array for regionRefs of each func
        assign_regionRef_array_server();
        fupdate_server(fp, "assign_regionRef_array_server complete\n");

        // Server listens for GlobalDefWriter strings&regions
        globalDefWriter_server();
        fupdate_server(fp, "globalDefWriter_server complete\n");

        // Server listens for events
        fupdate_server(fp, "evtWriter\n");
        run_EvtWriters_server(false);
        fupdate_server(fp, "evtWriter complete\n");

        // Clean up pmpmeas objects
        if (COLLECT_METRICS){
            r_pmpmeas_finish();
        }

        // Write definitions for proc structures
        globalDefWriter_WriteSystemTreeNode_server(0,0); // 1 system tree node
        globalDefWriter_WriteLocationGroups_server(); // n location groups (n procs)
        globalDefWriter_WriteLocations_server(); // n locations (1 per proc)

        // Finalization
        finalize_EvtWriters_server(); // Moving this after globalDef because num_events used in WriteLocation_server
        finalize_GlobalDefWriter_server(); // @TODO: Rename to globalDefWriter_Clock
        finalize_Archive_server();
        finalize_otf2_server();
        free_regionRef_array_server();
        fupdate_server(fp, "COMPLETE!\n");
        if (fp!=NULL){fclose(fp);}

        // Clean up pmpmeas metric_array memory
        if (COLLECT_METRICS){
            pmpmeas_read_finalize();
        }

        return(1);
        // exit(0); 
    } else {

        // DEBUGGING
        if (flag_print_pids){
            Rcout << "MASTER PROC - pid: " << getpid() << ", child_pid:" << child_pid << "\n";
        }

        IS_LOGGER = false;
        context = zmq_ctx_new();
        requester = zmq_socket(context, ZMQ_REQ);
        pusher = zmq_socket(context, ZMQ_PUSH);

        // @TODO Error check connect
        zmq_connect(requester, "tcp://localhost:5555");
        zmq_connect(pusher, "tcp://localhost:5556");
    }
    return(0);
}

//' r_pmpmeas_init
//' @description Wrapper for pmpmeas_init
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP r_pmpmeas_init(){
    pmpmeas_init();
    pmpmeas_read_init(&pmpmeas_vals, &pmpmeas_n);
    return (R_NilValue);
}

//' r_pmpmeas_finish
//' @description Wrapper for pmpmeas_finish
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP r_pmpmeas_finish(){
    if (pmpmeas_vals != NULL) { pmpmeas_read_finalize(); }
    pmpmeas_finish();
    return (R_NilValue);
}

//' r_pmpmeas_start
//' @description Wrapper for pmpmeas_start
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP r_pmpmeas_start(){
    pmpmeas_start("tag");
    return (R_NilValue);
}

//' r_pmpmeas_stop
//' @description Wrapper for pmpmeas_stop
//' @param weight Weight to scale metric values (eg average over N runs)
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP r_pmpmeas_stop(float weight){
    pmpmeas_stop(weight);
    return (R_NilValue);
}

//' assign_regionRef_array_master
//' @description Array is not assigned on master, 
//'     rather is signalled to assign on server
//' @param num_funcs Required length of array to store regionRef for each func
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP assign_regionRef_array_master(int num_funcs){
    int zmq_ret;
    void *regionRef_socket_client;

    NUM_FUNCS = num_funcs;

    regionRef_socket_client = zmq_socket(context, ZMQ_PUSH);
    zmq_ret = zmq_connect(regionRef_socket_client, "tcp://localhost:5554");
    if (zmq_ret<0){ report_and_exit("assign_regionRef_array_client zmq_connect", regionRef_socket_client); }
    zmq_ret = zmq_send(regionRef_socket_client, &NUM_FUNCS, sizeof(NUM_FUNCS), 0); // ZMQ_ID: 0
    if (zmq_ret<0){ report_and_exit("assign_regionRef_array_client zmq_send", regionRef_socket_client); }
    zmq_ret = zmq_close(regionRef_socket_client);
    if (zmq_ret<0){ report_and_exit("assign_regionRef_array_client zmq_close", regionRef_socket_client); }
    return (R_NilValue);
}

//' assign_regionRef_array_slave
//' @param num_funcs Required length of array to store regionRef for each func
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP assign_regionRef_array_slave(int num_funcs) {
    NUM_FUNCS = num_funcs;
    regionRef_array = (OTF2_RegionRef*) malloc(num_funcs*sizeof(*regionRef_array));
    if (regionRef_array==NULL){ report_and_exit("assign_regionRef_array_slave malloc", NULL);}

    // Set all entries to -1 for debugging
    for (int i=0; i<num_funcs; ++i){
        regionRef_array[i] = -1;
    }

    return (R_NilValue);
}

//' get_regionRef_from_array_slave
//' @param func_index Index of function to get regionRef for
//' @return regionRef
// [[Rcpp::export]]
RcppExport int get_regionRef_from_array_slave(int func_index) {
    return(regionRef_array[func_index-1]); // Fix offset in C
}

// @name assign_regionRef_array_server
// @description Listen for num_funcs then alloc OTF2_RegionRef[] as required
void assign_regionRef_array_server(){
    void *regionRef_socket_server;
    int rc, zmq_ret;

    // Bind socket to recv num_funcs
    regionRef_socket_server = zmq_socket(context, ZMQ_PULL); // ZMQ_PULL
    rc = zmq_bind(regionRef_socket_server, "tcp://*:5554");
    if (rc!=0){ report_and_exit("assign_regionRef_array_server regionRef_socket_server", regionRef_socket_server); }

    // Assign regionRef array of length num_funcs
    zmq_ret = zmq_recv(regionRef_socket_server, &NUM_FUNCS, sizeof(NUM_FUNCS), 0); // ZMQ ID: 0
    if ( zmq_ret <= 0 ) { report_and_exit("assign_regionRef_array_server pull recv", regionRef_socket_server); }

    // Assign and reset all values to -1 for debugging
    regionRef_array = (OTF2_RegionRef*) malloc(NUM_FUNCS*sizeof(*regionRef_array));
    if (regionRef_array==NULL){ report_and_exit("assign_regionRef_array_server regionRef_array", regionRef_socket_server); }
    for (int i=0; i<NUM_FUNCS; ++i){ regionRef_array[i] = -1; }

    zmq_close(regionRef_socket_server); // Close socket immediately (reopened later for events)

}

// @name free_regionRef_array_server
// @description Free memory assigned for regionRef_array
void free_regionRef_array_server(){
    free(regionRef_array);
}

//' free_regionRef_array_slave
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP free_regionRef_array_slave(){
    free(regionRef_array);
    return(R_NilValue);
}

// @name globalDefWriter_server
// @description Receive globalDef strings, and return with send regionRef
//  Ends when recvs message of length 0 from client
void globalDefWriter_server() { // Server
    int zmq_ret, rc; // Error check send/recvs, and sockets
    void *responder; // Recv global_defs, respond with regionRef

    //  Socket to talk to clients
    responder = zmq_socket(context, ZMQ_REP);
    rc = zmq_bind(responder, "tcp://*:5555");
    if (rc!=0){ report_and_exit("Binding responder", responder); }

    // DEBUGGING
    char fp_buffer[50];
    snprintf(fp_buffer, 50, "(pid: %d) Listening for globalDefWriter\n", getpid());
    fupdate_server(fp, fp_buffer);

    // Receive globalDef strings, and return with send regionRef
    int iter=0; ///< Number of messages received
    Zmq_otf2_defWriter buffer;
    while (1) {
		//char buffer[MAX_FUNCTION_NAME_LEN];

        zmq_ret = zmq_recv(responder, &buffer, sizeof(buffer), 0); // ZMQ ID: 1
        if ( zmq_ret < 0 ) { 
            report_and_exit("globalDefWriter_server zmq_recv", responder); 
        } else if (zmq_ret == 0) { // ZMQ ID: 4
            break; // Signal end of globalDef from client
        } else {
            // Define as stringRef, regionRef
            OTF2_StringRef stringRef = globalDefWriter_WriteString_server(buffer.func_name);
            OTF2_RegionRef regionRef = globalDefWriter_WriteRegion_server(stringRef);
            regionRef_array[buffer.func_index-1] = regionRef; // populate regionRef array, C indexing from 0

            // DEBUGGING
            #ifdef DEBUG
            Rcout << "Server - func_name: "<< buffer << ", regionRef:" << regionRef << std::endl;
            #endif

            // Return regionRef ID
            zmq_ret = zmq_send(responder, &regionRef, sizeof(regionRef), 0); // ZMQ ID: 2
            if (zmq_ret<0) { report_and_exit("globalDefWriter_server zmq_send regionRef", responder); }
            iter++;
        }
    }

    // DEBUGGING
    snprintf(fp_buffer, 50, "(pid: %d) Finished listening for globalDefWriter\n", getpid());
    fupdate_server(fp, fp_buffer);

    // Cleanup socket
    zmq_close(responder);
}

//' finalize_GlobalDefWriter_client
//' @return RNilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_GlobalDefWriter_client() { // client
    int zmq_ret;
    // Send 0 length signal to end this portion on server
    zmq_ret = zmq_send(requester, NULL, 0, 0); // ZMQ ID: 4
    if (zmq_ret<0) { report_and_exit("finalize_GlobalDefWriter_client zmq_send", NULL); }

    // Close requester socket
    zmq_ret = zmq_close(requester);
    if (zmq_ret<0) { report_and_exit("finalize_GlobalDefWriter_client zmq_close", NULL); }
    return(R_NilValue);
}


//' define_otf2_regionRef_client
//' @param func_name Name of function to create event for
//' @param func_index Global index of function in R namespace
//' @return regionRef regionRef for use when logging events
// [[Rcpp::export]]
RcppExport int define_otf2_regionRef_client(Rcpp::String func_name, int func_index) {
    Zmq_otf2_defWriter buffer;
    OTF2_RegionRef regionRef;
    int zmq_ret;

    // Send function index and name in Zmq_otf2_defWriter struct
    buffer.func_index = func_index;
    strncpy(buffer.func_name, func_name.get_cstring(), sizeof(buffer.func_name));
    zmq_ret = zmq_send(requester, &buffer, sizeof(buffer), 0); // ZMQ ID: 1
    if (zmq_ret < 0 ) { report_and_exit("define_otf2_regionRef_client zmq_send"); }

    // Recv regionRef
    zmq_ret = zmq_recv(requester, &regionRef, sizeof(regionRef), 0); // ZMQ ID: 2
    if (zmq_ret < 0 ) { report_and_exit("define_otf2_regionRef_client zmq_recv"); }

    // DEBUGGING
    #ifdef DEBUG
    Rcout << "Client defining - func_name: " << func_name.get_cstring() << ", regionRef: " << regionRef << std::endl;
    #endif

    // Cast in case OTF2_RegionRef != int
    int ret = regionRef;
    return ret;
}

//' finalize_EvtWriter_client
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_EvtWriter_client() {
    int zmq_ret;

    // Make sure socket defined
    if (pusher == NULL ) { report_and_exit("finalize_EvtWriter_client pusher"); }

    // Send 0 message to Master to sync
    zmq_ret = zmq_send(pusher, NULL, 0, 0); // ZMQ ID: 5x
    if (zmq_ret < 0 ) { report_and_exit("finalize_EvtWriter_client zmq_send"); }

    // Cleanup socket
    zmq_ret = zmq_close(pusher);
    if (zmq_ret < 0 ) { report_and_exit("finalize_EvtWriter_client zmq_close"); }

    return(R_NilValue);
}

//' finalize_otf2_client
//' @description Send signal to server to stop collecting event information
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_otf2_client() {
    void *syncer_client;
    int zmq_ret, rc; // Debugging recv/sends and socket
    char buffer; // Expecting length 0 message

    syncer_client = zmq_socket(context, ZMQ_PULL); 
    if (syncer_client == NULL){ report_and_exit("finalize_otf2_client syncer_client"); }
    rc = zmq_bind(syncer_client, "tcp://*:5557");
    assert (rc == 0); 

    // sent from logger/server at end of events
    zmq_ret = zmq_recv(syncer_client, &buffer, sizeof(buffer), 0);  // ZMQ ID: 7
    if (zmq_ret < 0 ) { 

        if (errno==4){ // Retry
            // sent from logger/server at end of events
            zmq_ret = zmq_recv(syncer_client, &buffer, sizeof(buffer), 0); // ZMQ ID: 7
            if (zmq_ret < 0 ) { 
                report_and_exit("finalize_otf2_client zmq_ret retry"); 
            }
        }
        else {
            report_and_exit("finalize_otf2_client zmq_ret"); 
        }

    }

    // Clean up zmq socket and context
    zmq_ret = zmq_close(syncer_client);
    if (zmq_ret < 0 ) { report_and_exit("finalize_otf2_client zmq_close"); }
    zmq_ret = zmq_ctx_destroy(context);
    if (zmq_ret < 0 ) { report_and_exit("finalize_otf2_client zmq_ctx_destroy"); }

    return(R_NilValue);
}


// @name finalize_otf2_server
// @description Signal to client end of server work (synchronization)
// @return R_NilValue
void finalize_otf2_server() {
    int zmq_ret;
    void *syncer_server;

    syncer_server = zmq_socket(context, ZMQ_PUSH); 
    zmq_ret = zmq_connect(syncer_server, "tcp://localhost:5557");
    if (zmq_ret < 0 ) { report_and_exit("finalize_otf2_server zmq_connect", NULL); }

    // sent from logger/server at end of events
    zmq_ret = zmq_send(syncer_server, NULL, 0, 0);  // ZMQ ID: 7
    if (zmq_ret < 0 ) { report_and_exit("finalize_otf2_server zmq_send", syncer_server); }

    // Clean up zmq socket and context
    zmq_ret = zmq_close(syncer_server);
    if (zmq_ret < 0 ) { report_and_exit("finalize_otf2_server zmq_close", NULL); }
    zmq_ret = zmq_ctx_destroy(context);
    if (zmq_ret < 0 ) { report_and_exit("finalize_otf2_server zmq_ctx_destroy", NULL); }
}


// @name init_Archive_server
// @description Initialize static otf2 {archive} objs
// @param archivePath Path to the archive i.e. the directory where the anchor file is located.
// @param archiveName Name of the archive. It is used to generate sub paths e.g. "archiveName.otf2"
void init_Archive_server(Rcpp::String archivePath="./rTrace", Rcpp::String archiveName="rTrace") 
{
    archive = OTF2_Archive_Open( archivePath.get_cstring(),
                                               archiveName.get_cstring(),
                                               OTF2_FILEMODE_WRITE,
                                               1024 * 1024 /* event chunk size */,
                                               4 * 1024 * 1024 /* def chunk size*/,
                                               OTF2_SUBSTRATE_POSIX,
                                               OTF2_COMPRESSION_NONE );
    if (archive == NULL) { report_and_exit("OTF2_Archive_Open", NULL); }

    // Set the previously defined flush callbacks.
    OTF2_Archive_SetFlushCallbacks( archive, &flush_callbacks, NULL );

    // We will operate in a serial context.
    OTF2_Archive_SetSerialCollectiveCallbacks( archive );

    // Now we can create the event files. Though physical files aren't created yet.
    OTF2_Archive_OpenEvtFiles( archive );
}


// @name finalize_Archive_server
// @description Close static otf2 {archive} objs
void finalize_Archive_server() {
    // DEBUGGING
    if (archive == NULL) { report_and_exit("finalize_Archive archive", NULL); }

    // At the end, close the archive and exit.
    OTF2_Archive_Close( archive );

    // Reset counters
    NUM_STRINGREF = OFFSET_NUM_STRINGREF;
    NUM_REGIONREF = 0;
}


// @name init_EvtWriters_server
// @description Initialize static otf2 {evt_writers} objs
void init_EvtWriters_server() {
    // DEBUGGING: OTF2_Archive_GetEvtWriter throwing error 
    if (archive == NULL) { report_and_exit("init_EvtWriters_server archive", NULL); }

    // Make sure maxLocationRef set
    if (maxLocationRef < 1){ report_and_exit("init_EvtWriters_server maxLocationRef", NULL); }

    // Get a event writer for each location
    evt_writers = (OTF2_EvtWriter**)malloc(maxLocationRef*sizeof(*evt_writers));
    if (evt_writers == NULL) { report_and_exit("init_EvtWriters_server evt_writers", NULL); }
    for (OTF2_LocationRef i=0; i<maxLocationRef; ++i){
        evt_writers[i] = OTF2_Archive_GetEvtWriter( archive, i );
    }
}


// @name finalize_EvtWriters_server
// @description Close static otf2 {evt_writers} objs
void finalize_EvtWriters_server() {
    // DEBUGGING
    if (evt_writers == NULL) { report_and_exit("finalize_EvtWriters_server evt_writers", NULL); }
    if (archive == NULL) { report_and_exit("finalize_EvtWriters_server archive", NULL); }

    // Now close the event writer, before closing the event files collectively.
    for (OTF2_LocationRef i=0; i<maxLocationRef; ++i){
        if (evt_writers[i] == NULL) { report_and_exit("finalize_EvtWriters_server evt_writers[i]", NULL); }
        OTF2_Archive_CloseEvtWriter( archive, evt_writers[i]);
    }
    free(evt_writers);

    // After we wrote all of the events we close the event files again.
    OTF2_Archive_CloseEvtFiles( archive );
}


// Enable or disable event measurement
// @name evtWriter_MeasurementOnOff_server
// @param evt_writer Event writer linked to proc
// @param time Timestamp
// @param measurementMode True to enable, else disable
void evtWriter_MeasurementOnOff_server(OTF2_EvtWriter *evt_writer, OTF2_TimeStamp time, bool measurementMode) {
    if (measurementMode){
        OTF2_EvtWriter_MeasurementOnOff(evt_writer,
                NULL /* attributeList */, 
                time, 
                OTF2_MEASUREMENT_ON);
    } else {
        OTF2_EvtWriter_MeasurementOnOff(evt_writer,
                NULL /* attributeList */, 
                time, 
                OTF2_MEASUREMENT_OFF);
    }
}


//' Send message to enable or disable event measurement from client side
//' @param measurementMode True to enable, else disable
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP evtWriter_MeasurementOnOff_client(bool measurementMode) {
    Zmq_otf2_data buffer;
    int zmq_ret;

    // Embed locationRef, timestamp and on/off eventtype
    buffer.pid = locationRef; 
    buffer.time = get_time();
    if (measurementMode){ buffer.datatype = ZMQ_OTF2_MEASUREMENT_ON; }
    else { buffer.datatype = ZMQ_OTF2_MEASUREMENT_OFF; }

    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5a // ZMQ ID: 5b
    if (zmq_ret<0){ report_and_exit("zmq_send Zmq_otf2_data measurement"); }
    return(R_NilValue);
}


// @name init_GlobalDefWriter_server
// @description Initialize static otf2 {globaldefwriter} obj
void init_GlobalDefWriter_server() {
    // DEBUGGING
    if (archive == NULL) { report_and_exit("init_GlobalDefWriter archive", NULL); }

    epoch_start = get_time(); // Get init time for finalize

    // Now write the global definitions by getting a writer object for it.
    global_def_writer = OTF2_Archive_GetGlobalDefWriter( archive );
    if (global_def_writer == NULL) { report_and_exit("OTF2_Archive_GetGlobalDefWriter", NULL); }

    // Define empty string in first stringRef value
    Rcpp::String stringRefValue="";
    globalDefWriter_WriteString_server(stringRefValue);
}

// @name finalize_GlobalDefWriter_server
// @description Finalize static otf2 {globaldefwriter} obj
//     Write clock information before ending tracing
void finalize_GlobalDefWriter_server() {
#ifdef DUMMY_TIMESTEPS
    // We need to define the clock used for this trace and the overall timestamp range.
    OTF2_GlobalDefWriter_WriteClockProperties( 
            global_def_writer /* writerHandle */,
            1 /* resolution - 1 tick per second */,
            0 /* epoch - 0 for dummy */,
            NUM_EVENTS /* traceLength */,
            OTF2_UNDEFINED_TIMESTAMP );
#else
    epoch_end =  get_time();

    // We need to define the clock used for this trace and the overall timestamp range.
    OTF2_GlobalDefWriter_WriteClockProperties( 
            global_def_writer /* writerHandle */,
            1E6 /* resolution - 1 tick per second */,
            epoch_start /* epoch - globalOffset */,
            epoch_end - epoch_start /* traceLength */,
            OTF2_UNDEFINED_TIMESTAMP );
#endif
}


// @name globalDefWriter_WriteString_server
// @description Define new id-value pair in globaldefwriter
// @param stringRefValue String assigned to given id
// @return NUM_STRINGREF 
OTF2_StringRef globalDefWriter_WriteString_server(Rcpp::String stringRefValue)
{
    OTF2_GlobalDefWriter_WriteString(global_def_writer, NUM_STRINGREF, stringRefValue.get_cstring() );
    return(NUM_STRINGREF++); // ++ applied after return value!
}


// @name globalDefWriter_WriteRegion
// @description Define new region description in global writer
// @param stringRef_RegionName Name to be associated with region
// @return regionRef id/index for string
OTF2_RegionRef globalDefWriter_WriteRegion_server(OTF2_StringRef stringRef_RegionName) {
    OTF2_GlobalDefWriter_WriteRegion( global_def_writer,
            NUM_REGIONREF /* RegionRef */,
            stringRef_RegionName /* region name - stringRef */,
            0 /* alternative name */,
            0 /* description */,
            OTF2_REGION_ROLE_FUNCTION,
            OTF2_PARADIGM_USER,
            OTF2_REGION_FLAG_NONE,
            1 /* source file */,
            0 /* begin lno */, 
            0 /* end lno */ );

    return((int)NUM_REGIONREF++);
}


// TODO: Get names from sys calls
// @name globalDefWriter_WriteSystemTreeNode_server
// @description Write the system tree including a definition for the location group to the global definition writer.
// @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
// @param stringRef_class Class to be associated with SystemTreeNode (eg node)
void globalDefWriter_WriteSystemTreeNode_server( OTF2_StringRef stringRef_name, OTF2_StringRef stringRef_class) {

    // Write the system tree incl definition for location group to global definition writer.
    OTF2_StringRef stringRef_SystemTreeNodeName = globalDefWriter_WriteString_server("MyHost");
    OTF2_StringRef stringRef_SystemTreeNodeClass = globalDefWriter_WriteString_server("node");
    OTF2_GlobalDefWriter_WriteSystemTreeNode( global_def_writer,
            0 /* SystemTreeNodeRef id */,
            stringRef_SystemTreeNodeName /* StringRef name */,
            stringRef_SystemTreeNodeClass /* StringRef class */,
            OTF2_UNDEFINED_SYSTEM_TREE_NODE /* parent */ );
}

// @name globalDefWriter_WriteLocationGroups_server
// @description Write LocationGroup (ie proc) information
void globalDefWriter_WriteLocationGroups_server() {

    // Do master process first
    OTF2_StringRef locationGroupRef_Name_master = globalDefWriter_WriteString_server("Master Process");
    OTF2_GlobalDefWriter_WriteLocationGroup( global_def_writer,
            0 /* OTF2_LocationGroupRef  */,
            locationGroupRef_Name_master /* name */,
            OTF2_LOCATION_GROUP_TYPE_PROCESS /* New LocationGroup for each process */, 
            0 /* system tree */,
            OTF2_UNDEFINED_LOCATION_GROUP /* creating process */ );
 
    // Then do all slaves
    OTF2_StringRef locationGroupRef_Name_slave = globalDefWriter_WriteString_server("Slave Processes");
    for (OTF2_LocationRef i=1; i<maxUsedLocationRef; ++i){
        OTF2_GlobalDefWriter_WriteLocationGroup( global_def_writer,
                i /* OTF2_LocationGroupRef  */,
                locationGroupRef_Name_slave /* name */,
                OTF2_LOCATION_GROUP_TYPE_PROCESS /* New LocationGroup for each process */, 
                0 /* system tree */,
                OTF2_UNDEFINED_LOCATION_GROUP /* creating process */ );
    }
}

// @name globalDefWriter_WriteLocations_server
// @description Write a definition for the location to the global definition writer.
void globalDefWriter_WriteLocations_server() {
    OTF2_StringRef stringRef_LocationName = globalDefWriter_WriteString_server("Main thread");
    
    char fp_buffer[100];
    snprintf(fp_buffer, 100, "maxUsedLocationRef: %lu\n", maxUsedLocationRef);
    fupdate_server(fp, fp_buffer);

    for (OTF2_LocationRef i=0; i<maxUsedLocationRef; ++i){
        // Write a definition for the location to the global definition writer.
        uint64_t num_events;
        OTF2_EvtWriter_GetNumberOfEvents(evt_writers[i], &num_events);

        // DEBUGGING
        char fp_buffer[100];
        snprintf(fp_buffer, 100, "globalDefWriter_WriteLocation - LocationRef: %lu, Num events: %ld\n",
            i, num_events);
        fupdate_server(fp, fp_buffer);

        OTF2_GlobalDefWriter_WriteLocation( global_def_writer,
                i /* locationRef */,
                stringRef_LocationName /* stringRef_name */,
                OTF2_LOCATION_TYPE_CPU_THREAD,
                num_events /* #events */,
                i /* locationGroupRef */ );
    }
}


//' close_EvtWriterSocket_client
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP close_EvtWriterSocket_client(){
    // Closer pusher to avoid sharing socket
    if (pusher==NULL){ report_and_exit("close_EvtWriterSocket_client() pusher"); }
    if ( zmq_close(pusher) < 0 ){ report_and_exit("close_EvtWriterSocket_client socket"); }

    // Also close context! Will respawn after fork
    if ( zmq_ctx_destroy(context) < 0 ){ report_and_exit("close_EvtWriterSocket_client context"); }
    
    return(R_NilValue);
}


//' open_EvtWriterSocket_client
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP open_EvtWriterSocket_client(){
    // Reopen context
    context = zmq_ctx_new ();
    if ( context == NULL ){ report_and_exit("open_EvtWriterSocket_client context ctx_new"); }

    pusher = zmq_socket(context, ZMQ_PUSH);
    if ( pusher == NULL ){ report_and_exit("open_EvtWriterSocket_client pusher socket"); }
    if ( zmq_connect(pusher, "tcp://localhost:5556") != 0 ){ report_and_exit("open_EvtWriterSocket_client pusher connect"); }
    return(R_NilValue);
}



//' Write event to evt_writer
//' @param regionRef Region id
//' @param event_type True for enter, False for leave region
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP evtWriter_Write_client(int regionRef, bool event_type)
{
#ifdef DEBUG
    Rcout << "region: " << regionRef << ", event_type: " << event_type << "\n";
#endif /* ifdef DEBUG */

    int zmq_ret; 
    Zmq_otf2_data buffer;

    // Pack info into struct
    buffer.pid = locationRef; // TODO - update name of buffer.pid
    buffer.regionRef = regionRef;
    buffer.time = get_time();
    if (event_type) { buffer.datatype = ZMQ_OTF2_EVENT_ENTER; }
    else { buffer.datatype = ZMQ_OTF2_EVENT_LEAVE; }

    // DEBUGGING - Write event info to unique file
    if (buffer.pid == 0){ // Master
        FILE *fp = fopen("write_client_0.log", "a");
        fprintf(fp, "pid: %d, time: %lu, regionRef: %u\n", buffer.pid, buffer.time, buffer.regionRef);
        fclose(fp);
    }
    if (buffer.pid == 1){ // Slave #1
        FILE *fp = fopen("write_client_1.log", "a");
        fprintf(fp, "pid: %d, time: %lu, regionRef: %u\n", buffer.pid, buffer.time, buffer.regionRef);
        fclose(fp);
    }

    if (pusher==NULL){ report_and_exit("evtWriter_Write_client pusher"); }

    // YOU ARE HERE
    if (COLLECT_METRICS){
        pmpmeas_read(pmpmeas_vals);

        // Send multi-part message. First event, then metrics
        zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), ZMQ_SNDMORE); // ZMQ ID: 5c // ZMQ ID: 5d
        zmq_ret = zmq_send(pusher, pmpmeas_vals, sizeof(pmpmeas_n*sizeof(*pmpmeas_vals)), 0); // ZMQ ID: 5c_ii // ZMQ ID: 5d_ii
    } else {
        zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5c // ZMQ ID: 5d
    }



    if (zmq_ret<0){ 
        // DEBUGGING - Common error after opening and closing socket, 
        print_errnos(); //  print static errno values to help debug
        report_and_exit("evtWriter_Write_client event"); 
    }

    return(R_NilValue);
}

/*
// YOU ARE HERE
OTF2_ErrorCode OTF2_EvtWriter_Metric(OTF2_EvtWriter *	writer,
		OTF2_AttributeList *  	attributeList,
		OTF2_TimeStamp  	time,
		OTF2_MetricRef  	metric,
		uint8_t  	numberOfMetrics,
		const OTF2_Type *  	typeIDs,
		const OTF2_MetricValue *  	metricValues 
	) 	

OTF2_ErrorCode OTF2_GlobalDefWriter_WriteMetricMember 	( 	OTF2_GlobalDefWriter *  	writerHandle,
		OTF2_MetricMemberRef  	self,
		OTF2_StringRef  	name,
		OTF2_StringRef  	description,
		OTF2_MetricType  	metricType,
		OTF2_MetricMode  	metricMode,
		OTF2_Type  	valueType,
		OTF2_Base  	base,
		int64_t  	exponent,
		OTF2_StringRef  	unit 
	) 	

OTF2_ErrorCode OTF2_GlobalDefWriter_WriteMetricClass 	( 	OTF2_GlobalDefWriter *  	writerHandle,
		OTF2_MetricRef  	self,
		uint8_t  	numberOfMetrics,
		const OTF2_MetricMemberRef *  	metricMembers,
		OTF2_MetricOccurrence  	metricOccurrence,
		OTF2_RecorderKind  	recorderKind 
	) 	
*/

// YOU ARE HERE
// @name globalDefWriter_metrics_server
// @description Create metricClass encapsulating all metrics in globalDefWriter
void globalDefWriter_metrics_server()
{
    OTF2_StringRef stringRef_name; 
    OTF2_StringRef stringRef_description;
    OTF2_StringRef stringRef_unit;
    OTF2_MetricType metricType;
    OTF2_ErrorCode ret;

    // Meas contains all metrics of specific type (eg time, papi, perf)
    for (std::list<Meas*>::iterator m = pmpmeas_match_lst.begin(); m != pmpmeas_match_lst.end(); m++){
        switch( (*m)->type() )
        {
            case (MeasType::PAPI):
                metricType = OTF2_METRIC_TYPE_PAPI;
                break;
            case (MeasType::PERF):
                metricType = OTF2_METRIC_TYPE_OTHER; // perf
                break;
            default: // TIME or unrecognized
                continue;
                break;
        }

        // Cycle through each metric of given type, and create metric
        for ( int i=0; i<(*m)->cnt(); i++ ){
            const char *ename = (*m)->ename(i);    
            Rcpp::String stringEname(ename);

            stringRef_name = globalDefWriter_WriteString_server(stringEname);

            ret = OTF2_GlobalDefWriter_WriteMetricMember(global_def_writer,
                NUM_METRICS++ /* MetricMemberRef */,
                stringRef_name, 
                0 /*stringRef_description*/,
                metricType, 
                OTF2_METRIC_ACCUMULATED_START /* placeholder */,
                OTF2_TYPE_INT64, 
                OTF2_BASE_DECIMAL,
                0 /* exponent */, 
                0 /*stringRef_unit*/);

            if (ret != OTF2_SUCCESS){
                report_and_exit("globalDefWriter_metrics_server WriteMetricMember");
            }
        }
    }

    OTF2_MetricMemberRef *metricMembers;
    
    metricMembers = (OTF2_MetricMemberRef*) malloc(NUM_METRICS*sizeof(*metricMembers));
    typeIDs = (OTF2_Type*) malloc(NUM_METRICS*sizeof(*typeIDs));

    for (int i=0; i<NUM_METRICS; ++i){ 
        metricMembers[i] = i; 
        typeIDs[i] = OTF2_TYPE_INT64;
    }

    ret = OTF2_GlobalDefWriter_WriteMetricClass(global_def_writer,
		0 /* MetricRef */,
		NUM_METRICS,
		metricMembers,
		OTF2_METRIC_SYNCHRONOUS_STRICT /* OTF2_MetricOccurrence */,
		OTF2_RECORDER_KIND_CPU /* OTF2_RecorderKind */);

    if (ret != OTF2_SUCCESS){
        report_and_exit("globalDefWriter_metrics_server WriteMetricClass");
    }
    free(metricMembers);
}

// TODO: function for evtWriters err check
// @name run_EvtWriters_server
// @description Main function during which all otf2 event information is processed. 
//      Terminated by call to `finalize_EvtWriter_client` on client (ZMQ ID: 5x)
// @param flag_lgo Log all events in log file
//  and logged
void run_EvtWriters_server(bool flag_log){
    void *puller;               ///< Recv otf2 eventlog
    void *new_proc_rep;
    int zmq_ret, rc; // Debugging recv/sends and socket
    Zmq_otf2_data buffer;
    int nprocs=1;
    OTF2_StringRef slaveActive_stringRef;
    OTF2_RegionRef slaveActive_regionRef;
    int rcvmore, rcvmore_ret;
    size_t rcvmore_len = sizeof(rcvmore);

    // Placeholder for region of ZMQ_OTF2_SOCK_CLUSTER
    slaveActive_stringRef = globalDefWriter_WriteString_server("SLAVE_ACTIVE");
    slaveActive_regionRef = globalDefWriter_WriteRegion_server(slaveActive_stringRef);

    // Ensure evt_writers defined
    if (evt_writers == NULL) { report_and_exit("run_EvtWriters_server evt_writers", NULL); }

    //  Socket to talk to clients for EvtWriter
    puller = zmq_socket(context, ZMQ_PULL);
    rc = zmq_bind(puller, "tcp://*:5556");
    if (rc!=0){ report_and_exit("run_EvtWriters_server zmq_bind puller", puller); }

    // Socket to publish func list to new R procs
    new_proc_rep = zmq_socket(context, ZMQ_REP);
    zmq_ret = zmq_bind(new_proc_rep, "tcp://*:5559");
    if ( zmq_ret != 0 ){ 
        zmq_close(new_proc_rep);
        zmq_close(puller);
        report_and_exit("globalDefWriter_update_new_proc new_proc_publisher zmq_bind", NULL); 
    }

    // DEBUGGING
    char fp_buffer[50];
    if (flag_log){
        snprintf(fp_buffer, 50, "(pid: %d) Listening for evtWriters\n", getpid());
        fupdate_server(fp, fp_buffer);
    }

    // Receive globalDef strings, and return with send regionRef
    while (1) {
        // YOU ARE HERE
        if (COLLECT_METRICS){
            zmq_ret = zmq_recv(puller, &buffer, sizeof(buffer), 0); // ZMQ ID: 5

            // DEBUGGING
            //fupdate_server(fp, "COLLECT_METRICS=T: Initial recv\n");

            /* int zmq_getsockopt (void *socket, int option_name, void *option_value, size_t *option_len); */
            zmq_getsockopt(puller, ZMQ_RCVMORE, &rcvmore, &rcvmore_len);
            if (rcvmore){
                rcvmore_ret = zmq_recv(puller, pmpmeas_vals, sizeof(pmpmeas_n*sizeof(*pmpmeas_vals)), 0); // ZMQ ID: 5c_ii
                OTF2_EvtWriter_Metric(evt_writers[buffer.pid],
                        NULL /* attribute list */,
                        buffer.time,
                        0 /* MetricRef */,
                        pmpmeas_n,
                        typeIDs,
                        (OTF2_MetricValue*)pmpmeas_vals);

                // DEBUGGING
                //fupdate_server(fp, "COLLECT_METRICS=T: EvtWriter_Metric complete\n");
            } else { ;
                // DEBUGGING
                //fupdate_server(fp, "COLLECT_METRICS=T: no 2nd recv\n");
            }

        } else {
            zmq_ret = zmq_recv(puller, &buffer, sizeof(buffer), 0); // ZMQ ID: 5
            fupdate_server(fp, "COLLECT_METRICS=F\n");
        }

        if (zmq_ret < 0){
            report_and_exit("run_EvtWriters_server puller zmq_recv", NULL); 

        } else if (zmq_ret == 0) { // Signal to stop listening
            break; // ZMQ ID: 5x

        } else if (zmq_ret == sizeof(Zmq_otf2_data)) { 

            // DEBUGGING - Write all events to logfile
            char fp_buffer[100];
            snprintf(fp_buffer, 100, "Server recv datatype: %d, pid: %d, time: %lu, regionRef(if applicable): %u\n", 
                buffer.datatype, buffer.pid, buffer.time, buffer.regionRef);
            fupdate_server(fp, fp_buffer);

            // Usual part for non-metric collection
            if (buffer.datatype == ZMQ_OTF2_MEASUREMENT_ON ){ // ZMQ ID: 5a
                evtWriter_MeasurementOnOff_server(evt_writers[buffer.pid], buffer.time, true);

            } else if (buffer.datatype == ZMQ_OTF2_MEASUREMENT_OFF ){ // ZMQ ID: 5b
                evtWriter_MeasurementOnOff_server(evt_writers[buffer.pid], buffer.time, false);

            } else if (buffer.datatype == ZMQ_OTF2_EVENT_ENTER ){ // ZMQ ID: 5c
                OTF2_EvtWriter_Enter( evt_writers[buffer.pid], NULL /* attributeList */,
                        buffer.time, buffer.regionRef /* region */ );

            } else if (buffer.datatype == ZMQ_OTF2_EVENT_LEAVE ){ // ZMQ ID: 5d
                OTF2_EvtWriter_Leave( evt_writers[buffer.pid], NULL /* attributeList */,
                        buffer.time, buffer.regionRef /* region */ );

            } else if (buffer.datatype == ZMQ_OTF2_USED_LOCATIONREFS ){ // ZMQ ID: 5e
                if (buffer.regionRef > maxUsedLocationRef){ maxUsedLocationRef = buffer.regionRef; }

            } else if (buffer.datatype == ZMQ_OTF2_SOCK_CLUSTER_ON){ // ZMQ ID: 5f
                nprocs = buffer.regionRef;
                if (get_regionRef_array_server(nprocs, new_proc_rep) != 0){
                    zmq_close(new_proc_rep);
                    zmq_close(puller);
                    report_and_exit("get_regionRef_array_server");
                }
                for (int i=1; i<=nprocs; ++i){
                    OTF2_EvtWriter_Enter( evt_writers[i], NULL /* attributeList */,
                            buffer.time, slaveActive_regionRef /* region */ );
                }

            } else if (buffer.datatype == ZMQ_OTF2_SOCK_CLUSTER_OFF){ // ZMQ ID: 5g
                for (int i=1; i<=nprocs; ++i){
                    OTF2_EvtWriter_Leave( evt_writers[i], NULL /* attributeList */,
                            buffer.time, slaveActive_regionRef /* region */ );
                }
                nprocs=1;
            }

        } else if (zmq_ret > 0) { // Unknown datatype
            zmq_close(new_proc_rep);
            zmq_close(puller);
            report_and_exit("run_EvtWriters_server puller unknown data", NULL); 
        }

    }

    snprintf(fp_buffer, 50, "(pid: %d) Finished listening for evtWriters\n", getpid());
    fupdate_server(fp, fp_buffer);

    // Cleanup socket
    zmq_close(puller);
    zmq_close(new_proc_rep);

}

// @name get_regionRef_array_server
// @description Triggered by receiving ZMQ_OTF2_SOCK_CLUSTER_ON
// @param num_new_procs Number of new procs spawned
// @param responder ZMQ_REP socket recv proc_id, send regionRef_array
// @return non-zero if error
int get_regionRef_array_server(OTF2_RegionRef num_new_procs, void *responder){
    int proc_id;
    int zmq_ret; 
    char fp_buffer[50];

    for (OTF2_RegionRef i = 0; i < num_new_procs; ++i){

        zmq_ret = zmq_recv(responder, &proc_id, sizeof(proc_id), 0); // ZMQ ID: 6a
        if (zmq_ret < 0 ) { return (-1); }

        // DEBUGGING
        snprintf(fp_buffer, 50, "Received newproc signal from proc %d\n", proc_id);
        fupdate_server(fp, fp_buffer);
        fupdate_server(fp, "Starting send regionRef_array");

        zmq_ret = zmq_send(responder, regionRef_array, NUM_FUNCS*sizeof(*regionRef_array), 0); // ZMQ ID: 6b
        if ( zmq_ret < 0 ){ return (-2); }
        
        // DEBUGGING
        fupdate_server(fp, "Finished send regionRef_array");
    }
    return(0);

}

///////////////////////////////
// Helper functions
///////////////////////////////

//@TODO: Replace usage of this with more R-compliant exit strategy
//@TODO: Replace usage of this with more R-friendly error message strategy
// @name report_and_exit
// @description Print error to log file, close zmq sockets 
//     and context then exit
// @param msg Error message to display
// @param socket Additional non-global zmq socket to close
void report_and_exit(const char* msg, void *socket){
    // Close any open sockets
    if (pusher != NULL ) zmq_close(pusher);
    if (requester != NULL ) zmq_close(requester);
    if (socket != NULL ) zmq_ctx_destroy(socket);
    if (context != NULL ) zmq_ctx_destroy(context);

    if (IS_LOGGER){ // Print to log file

        if (fp==NULL){
            // Open log file
            fp = fopen(log_filename, "w");
            fprintf(fp, "ERROR: Couldn't find log file pointer, made new one\n");
        }
        fprintf(fp, "[errno: %d] ERROR: %s\n", errno, msg);
        fprintf(fp, "File closing\n");
        fclose(fp);

    } else { // Print to Rcout (recommend using logfile for makeCluster)
        FILE *fp;
        char filename[20];

        snprintf(filename, 20 , "slave_error_%d.log", locationRef);

        Rcout << "[R proc id: " << locationRef << "] CLIENT ERROR: " << msg << "\n";
        Rcout << "ERROR INFO - pid:" << getpid() << ", ppid: " << getppid() << ",sid: " << getsid(getpid()) << "\n";
        Rcout << "ERROR INFO - Errno:" << errno << "\n"; 
        Rcout << "ERROR INFO - Output file: " << filename << "\n";

        fp = fopen(filename,"w");
        fprintf(fp, "[R proc id: %d] CLIENT ERROR: %s\n", locationRef, msg);
        fprintf(fp, " ERROR INFO - pid: %d , ppid: %d, sid: %d\n", getpid(), getppid(), getsid(getpid()) );
        fprintf(fp, "ERROR INFO - Errno: %d\n", errno);
        fclose(fp);

        kill(0, SIGTERM);
    }

}

// @name fupdate_server
// @description Write message to server log file `log_filename`
// @param fp File pointer to log file
// @param msg Message to write to log file
void fupdate_server(FILE *fp, const char* msg){
    if (fp!=NULL){
       fprintf(fp, "%s\n", msg);
    } else {
        char fp_buffer[100];
        snprintf(fp_buffer, 100, "Log file not found - msg: %s\n", msg);
        report_and_exit(fp_buffer, NULL);
    }

    // DEBUGGING
    fclose(fp);
    fp = fopen(log_filename, "a");
}

//' set_locationRef
//' @param id ID value belonging to this R proc
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP set_locationRef(const int id) {
    if (id < 0 ){ report_and_exit("Negative ID"); }
    locationRef = id;    
    return (R_NilValue);
}

//' get_locationRef
//' @return locationRef Proc number between 0,nprocs-1
// [[Rcpp::export]]
RcppExport int get_locationRef() {
    return (locationRef);
}

//' set_maxUsedLocationRef_client
//' @param nprocs Current number of active evtWriters/procs
//' @return maxUsedLocationRef Current maximum evtWriters which were active so far
// [[Rcpp::export]]
RcppExport int set_maxUsedLocationRef_client(int nprocs) {

    OTF2_LocationRef tmp = nprocs;
    if (tmp > maxUsedLocationRef){
        maxUsedLocationRef = tmp;

        // Send to logger
        Zmq_otf2_data buffer;
        buffer.datatype = ZMQ_OTF2_USED_LOCATIONREFS;
        buffer.regionRef = tmp; 

        if (pusher==NULL) { report_and_exit("set_maxUsedLocationRef_client pusher"); }
        int zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5d
        if (zmq_ret<0) { report_and_exit("set_maxUsedLocationRef_client zmq_send"); }
    }
    return (maxUsedLocationRef);
}

//' print_errnos
//' @description Print error numbers relating to zmq sockets
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP print_errnos() {
    Rcout<< "ZMQ_SEND ERRNOS: \n";
    Rcout<< "EAGAIN: " << EAGAIN << "\n";
    Rcout<< "ENOTSTUP: " << ENOTSUP << "\n";
    Rcout<< "EINVAL: " << EINVAL << "\n";
    Rcout<< "EFSM: " << EFSM << "\n";
    Rcout<< "ETERM: " << ETERM << "\n";
    Rcout<< "ENOTSOCK: " << ENOTSOCK << "\n";
    Rcout<< "EINTR: " << EINTR << "\n";
    Rcout<< "EHOSTUNREACH: " << EHOSTUNREACH << "\n";
    Rcout<< "#######################\n";
    return (R_NilValue);
}


//' get_regionRef_array_master
//' @description Signal to server to send regionRef array to new procs
//' @param nprocs Number of new procs to update
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP get_regionRef_array_master(const int nprocs){
    int zmq_ret;
    Zmq_otf2_data buffer;

    // Pack buffer
    buffer.regionRef = nprocs;
    buffer.pid = locationRef;
    buffer.time = get_time();
    buffer.datatype = ZMQ_OTF2_SOCK_CLUSTER_ON;

    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5f
    if (zmq_ret < 0 ) { report_and_exit("get_regionRef_array_master pusher zmq_send"); }
    return(R_NilValue);
}

//' stopCluster_master
//' @description Signal to end cluster
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP stopCluster_master(){
    int zmq_ret;
    Zmq_otf2_data buffer;

    // Pack buffer
    buffer.regionRef = 0;
    buffer.pid = locationRef;
    buffer.time = get_time();
    buffer.datatype = ZMQ_OTF2_SOCK_CLUSTER_OFF;

    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5g
    if (zmq_ret < 0 ) { report_and_exit("stopCluster_master pusher zmq_send"); }
    return(R_NilValue);
}

//' get_regionRef_array_slave
//' @description Requests regionRef array from logger proc
//' @param num_funcs Total number of functions in R namespace
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP get_regionRef_array_slave(const int num_funcs){
    int zmq_ret;

    // Make sure array assigned
    if (regionRef_array==NULL){ report_and_exit("get_regionRef_array_slave regionRef_array"); }

    // Subscriber for func_list from server
    requester = zmq_socket(context, ZMQ_REQ);
    zmq_ret = zmq_connect(requester, "tcp://localhost:5559");
    if ( zmq_ret != 0 ){ report_and_exit("get_regionRef_array_slave new_proc_subscriber connect"); }

    Rcout << "[R id: " << locationRef << "] Staring req send. size: " << 0 << "\n";

    zmq_ret = zmq_send(requester, &locationRef, sizeof(locationRef), 0); // ZMQ ID: 6a
    if (zmq_ret < 0 ) { report_and_exit("get_regionRef_array_slave new_proc_pusher zmq_send"); }
    Rcout << "[R id: " << locationRef << "] Finished req send\n";

    Rcout << "[R id: " << locationRef << "] Waiting for regionRef array\n";
    zmq_ret = zmq_recv(requester, regionRef_array, num_funcs*sizeof(*regionRef_array), 0); // ZMQ ID: 6b
    if ( zmq_ret < 0 ){ report_and_exit("get_regionRef_array_slave new_proc_subscriber zmq_recv"); }
    Rcout << "[R id: " << locationRef << "] Recived for regionRef array\n";

    // Cleanup
    zmq_ret = zmq_close(requester);
    if ( zmq_ret != 0 ){ report_and_exit("get_regionRef_array_slave new_proc_subscriber zmq_close"); }

    return(R_NilValue);
}


///////////////////////////////
// Testing
///////////////////////////////

//' get_pid
// [[Rcpp::export]]
RcppExport int get_pid() {
    return((int)getpid());
}

//' get_tid
// [[Rcpp::export]]
RcppExport int get_tid() {
    return((int)gettid());
}

//' get_ppid
// [[Rcpp::export]]
RcppExport int get_ppid() {
    return((int)getppid());
}

