// @file rTrace.cpp
// @brief Rcpp functions for underlying otf2 library
// @date 2024-03-23
// @version 0.02
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
// @todo Reduce function complexity - maybe adapt C++ style 

// Testing
#define EXIT exit

#include "Rcpp.h"
#include <otf2/otf2.h>
#include <sys/time.h>
#include <stdio.h>

// getpid
#include <sys/types.h>
#include <unistd.h>

// ZeroMQ
#include <zmq.h>
#include <sys/wait.h>

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
    ZMQ_OTF2_INIT_PROC,
    ZMQ_OTF2_FINALIZE_PROC,
    //ZMQ_OTF2_SOCK_CLUSTER_ON,
    //ZMQ_OTF2_SOCK_CLUSTER_OFF
} zmq_otf2_datatypes;

// Different stages to tracing workflow - update once stage begins!
typedef enum {
    STAGE_PRE_INIT, 
    STAGE_INIT, 
    STAGE_ASSIGN_REGIONREF_ARRAY, 
    STAGE_GLOBALDEFWRITER, 
    STAGE_EVTWRITER, 
    STAGE_PRE_FINALIZE, 
    STAGE_FINALIZE, 
    STAGE_SYNC
} rTrace_stage_type;


// Struct used for majority of data transfer during event collection phase
typedef struct Zmq_otf2_data {
    OTF2_TimeStamp time;  
    OTF2_RegionRef regionRef; ///< Could probably generalize this datatype better, used for diverse int-like datatypes, see set_maxUsedLocationRef()
    pid_t pid;
    zmq_otf2_datatypes datatype;
} Zmq_otf2_data;

typedef struct ERR_MSG {
    bool value;
} ERR_MSG;

// Struct used for defining globalDefWriter key-values
typedef struct Zmq_otf2_defWriter {
    char func_name[MAX_FUNCTION_NAME_LEN];
    int func_index; 
} Zmq_otf2_defWriter;

// Track progress via enum-ed type (useful for unexpected termination)
rTrace_stage_type current_stage;

// OTF2 objects for logger
static OTF2_Archive* archive;
static OTF2_GlobalDefWriter* global_def_writer;
OTF2_TimeStamp epoch_start, epoch_end;  // OTF2_GlobalDefWriter_WriteClockProperties
static OTF2_EvtWriter** evt_writers;

// ZeroMQ sockets
static bool IS_LOGGER=false; ///* Used by report_and_exit() to abtain exit behaviour
static void *context;      ///* zmq context - clients and server
static void *syncer;    ///* zmq socket - clients and server synchronization
static void *pusher;    ///* zmq socket - clients (comm with puller for EvtWriter)
static void *puller;           ///* zmq socket - server recv otf2 eventlog
static pid_t child_pid;           ///* child_pid for server (used by master)

#define NUM_PORTS 2
static int PORTS[NUM_PORTS]; ///* Port numbers for push-pull

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


// DEBUGGING
static FILE *fp; ///* Log file on server proc
char log_filename[]="log.log"; ///* Name of log file on server proc


///////////////////////////////
// Function declarations
///////////////////////////////
RcppExport SEXP init_zmq_client();
RcppExport SEXP finalize_zmq_client();
//RcppExport int init_otf2_logger(int, Rcpp::String, Rcpp::String, bool);
RcppExport int init_otf2_logger(int, Rcpp::String, Rcpp::String, Rcpp::NumericVector, bool);
RcppExport SEXP assign_regionRef_array_master(int);
RcppExport int define_otf2_regionRef_client(Rcpp::String, int);
RcppExport SEXP finalize_GlobalDefWriter_client();
RcppExport SEXP evtWriter_MeasurementOnOff_client(bool);
RcppExport SEXP evtWriter_Write_client(int, bool);
RcppExport SEXP set_locationRef(const int);
RcppExport int get_locationRef();
RcppExport int set_maxUsedLocationRef_client(int);
RcppExport SEXP finalize_EvtWriter_client();
RcppExport SEXP finalize_sync_client();

// OTF2 Server/logger functions
void init_zmq_server();
void finalize_zmq_server();
void init_Archive_server(Rcpp::String, Rcpp::String);
void init_EvtWriters_server();
void init_GlobalDefWriter_server();
int run_evtWriters_server(bool flag_log);
void globalDefWriter_server(bool);
void finalize_otf2_objs_server();
void finalize_sync_server();
OTF2_StringRef globalDefWriter_WriteString_server(Rcpp::String stringRefValue);
OTF2_RegionRef globalDefWriter_WriteRegion_server(OTF2_StringRef stringRef_RegionName);
void globalDefWriter_WriteSystemTreeNode_server(OTF2_StringRef, OTF2_StringRef);
void globalDefWriter_WriteLocations_server();
void globalDefWriter_WriteLocationGroups_server();
void evtWriter_MeasurementOnOff_server(OTF2_EvtWriter*, OTF2_TimeStamp, bool);
void assign_regionRef_array_server();
void free_regionRef_array_server();

// Helper functions for debugging
void report_and_exit(const char*, void *sock=NULL);
void fupdate_server(FILE*, const char*);
RcppExport SEXP print_errnos();

// Testing
RcppExport int test__struct_size();
RcppExport int test__sockets();

// Temporary debugging
RcppExport int get_pid();
RcppExport int get_tid();
RcppExport int get_ppid();


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
// @name sighup_handler
// @description This was introduced due to R procs being sent SIGHUP during forking
void sighup_handler(int signal) {
    // Make sure only catching intended signal, else rethrow
    if (signal == SIGHUP) { /*ignore*/; }
    else { raise(signal); }
}

// @name signal_term_handler
// @description TODO
//void sigterm_handler(int signal) {
//    // Make sure only catching intended signal, else rethrow
//    if (signal == SIGTERM) { 
//        if (current_stage==STAGE_EVTWRITER) {
//            ERR_MSG err;
//            err.value = true; // Not used currently
//            if ( zmq_send(pusher, &err, sizeof(err), 0) < 0 ) {
//                report_and_exit("ERROR: SIGTERM - unable to send err_msg to server");
//            }
//            report_and_exit("ERROR: SIGTERM");
//        } else { // Unknown stage
//            raise(signal); 
//        }
//    }
//    else { raise(signal); }
//}

//////////////////////////////////////
// Spawn otf2 process, and give task list
//  Awaits messages from main process at suitable steps
//////////////////////////////////////

//' set_ports
//' @param ports Ports to use for zmq sockets
//' @return 0 , else Rcpp::stop on error
RcppExport int set_ports(Rcpp::NumericVector ports){
    int num_ports = ports.length();
    if (num_ports>NUM_PORTS){
        Rcpp::stop("ERROR: Number of ports given greater than NUM_PORTS");
    }
    for (int i=0; i<num_ports; ++i){
        if ( (ports[i]<1024) || (ports[i]>49151) ){
            Rcpp::stop("ERROR: Invalid port range");
        }
        PORTS[i] = ports[i];
    }
    return(0);
}

//' Fork and initialize zeromq sockets for writing globalDef definitions
//' @param max_nprocs Maximum number of R processes (ie evtWriters required)
//' @param archivePath Path to otf2 archive
//' @param archiveName Name of otf2 archive
//' @param ports Port numbers for zmq sockets (must be length <NUM_PORTS)
//' @param flag_print_pids True to print pids of parent and child procs
//' @return <0 if error, 0 if R master, else >0 if child
// [[Rcpp::export]]
RcppExport int init_otf2_logger(int max_nprocs, Rcpp::String archivePath = "./rTrace", 
        Rcpp::String archiveName = "rTrace", 
        Rcpp::NumericVector ports = {5556,5557},
        bool flag_print_pids=false)
{
    // TODO: Verify this acts as intended to save child proc
    signal(SIGHUP, sighup_handler);

    // Progress tracker
    current_stage = STAGE_PRE_INIT;

    // Check port ranges
    set_ports(ports);

    child_pid = fork();
    if (child_pid == (pid_t) -1 ){ // ERROR
        report_and_exit("Forking logger process", NULL);
        return(-1);
    }

    if (child_pid == 0) { // Child process

        // Set logger variables
        IS_LOGGER = true;
        maxLocationRef = max_nprocs;

        // Open log file
        fp = fopen(log_filename, "w");
        if (fp==NULL){ report_and_exit("Opening log file", NULL); }
        fupdate_server(fp, "File opened\n");

        // OTF2 Objs
        current_stage = STAGE_INIT;
        init_Archive_server(archivePath, archiveName);
        fupdate_server(fp, "Init archive complete\n");
        init_EvtWriters_server( );
        fupdate_server(fp, "Init evt_writers complete\n");
        init_GlobalDefWriter_server();
        fupdate_server(fp, "Init of otf2 objs complete\n");

        // Generate context, and sockets
        init_zmq_server();

        // Assign array for regionRefs of each func
        current_stage = STAGE_ASSIGN_REGIONREF_ARRAY;
        assign_regionRef_array_server();
        fupdate_server(fp, "assign_regionRef_array_server complete\n");


        // Server for logging GlobalDefWriter strings&regions
        current_stage = STAGE_GLOBALDEFWRITER;
        globalDefWriter_server(false /* flag_log */);
        fupdate_server(fp, "globalDefWriter_server complete\n");

        // Server listens for events
        current_stage = STAGE_EVTWRITER;
        fupdate_server(fp, "evtWriter\n");
        int evtWriters_flag = run_evtWriters_server(true /* flag_log */);
        fupdate_server(fp, "evtWriter complete\n");

        // Cleanup regionRef_array
        current_stage = STAGE_PRE_FINALIZE;
        free_regionRef_array_server();

        // Write definitions for proc structures
        globalDefWriter_WriteSystemTreeNode_server(0,0); // 1 system tree node
        globalDefWriter_WriteLocationGroups_server(); // n location groups (n procs)
        globalDefWriter_WriteLocations_server(); // n locations (1 per proc)

        // Finalization
        current_stage = STAGE_FINALIZE;
        finalize_otf2_objs_server();
        if (evtWriters_flag==0){ // Only sync on success
            current_stage = STAGE_SYNC;
            finalize_sync_server();
            fupdate_server(fp, "COMPLETE!\n");
        } else {
            fupdate_server(fp, "ERROR - cleanup complete!\n");
        }
        
        if (fp!=NULL){fclose(fp);}

        return(1);
        // Must be called by master at some later point
        //kill_child_after_sync(context, child_pid); 

        // exit(0); 
    } else {
        // TODO: impliment this signal handler!
        // signal(SIGTERM, sigterm_handler);

        // DEBUGGING
        if (flag_print_pids){
            Rcout << "MASTER PROC - pid: " << getpid() << ", child_pid:" << child_pid << "\n";
        }

        current_stage = STAGE_INIT; // Update stage
        IS_LOGGER = false;

        // Generate context, and sockets
        init_zmq_client();
    }
    return(0);
}

//' assign_regionRef_array_master
//' @description Array is not assigned on master, 
//'     rather is signalled to assign on server
//' @param num_funcs Required length of array to store regionRef for each func
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP assign_regionRef_array_master(int num_funcs){
    int zmq_ret;

    current_stage = STAGE_ASSIGN_REGIONREF_ARRAY; // Update stage
    NUM_FUNCS = num_funcs;

    zmq_ret = zmq_send(pusher, &NUM_FUNCS, sizeof(NUM_FUNCS), 0); // ZMQ_ID: 0
    if (zmq_ret<0){ report_and_exit("assign_regionRef_array_client zmq_send"); }
    return (R_NilValue);
}


// @name assign_regionRef_array_server
// @description Listen for num_funcs then alloc OTF2_RegionRef[] as required
void assign_regionRef_array_server(){
    int zmq_ret;

    // Assign regionRef array of length num_funcs
    zmq_ret = zmq_recv(puller, &NUM_FUNCS, sizeof(NUM_FUNCS), 0); // ZMQ ID: 0
    if ( zmq_ret <= 0 ) { report_and_exit("assign_regionRef_array_server pull recv"); }

    // DEBUGGING
    char fp_buffer[50];
    snprintf(fp_buffer, 50, "regionRef_array length: %d", NUM_FUNCS);
    fupdate_server(fp, fp_buffer);

    // Assign and reset all values to -1 for debugging
    regionRef_array = (OTF2_RegionRef*) malloc(NUM_FUNCS*sizeof(*regionRef_array));
    if (regionRef_array==NULL){ report_and_exit("assign_regionRef_array_server regionRef_array"); }
    for (int i=0; i<NUM_FUNCS; ++i){ regionRef_array[i] = -1; }
}

// @name free_regionRef_array_server
// @description Free memory assigned for regionRef_array
void free_regionRef_array_server(){
    free(regionRef_array);
}

// @name globalDefWriter_server
// @description Receive globalDef strings, and return with send regionRef
//  Ends when recvs message of length 0 from client
void globalDefWriter_server(bool flag_log) { // Server
    int zmq_ret; // Error check send/recvs, and sockets

    // DEBUGGING
    char fp_buffer[50];
    snprintf(fp_buffer, 50, "(pid: %d) Listening for globalDefWriter\n", getpid());
    fupdate_server(fp, fp_buffer);

    // Receive globalDef strings, and return with send regionRef
    Zmq_otf2_defWriter buffer;
    while (1) {
        zmq_ret = zmq_recv(puller, &buffer, sizeof(buffer), 0); // ZMQ ID: 1a
        if ( zmq_ret < 0 ) { 
            report_and_exit("globalDefWriter_server zmq_recv", puller); 
        } else if (zmq_ret == 0) { // ZMQ ID: 1b
            break; // Signal end of globalDef from client
        } else {
            // Define as stringRef, regionRef
            OTF2_StringRef stringRef = globalDefWriter_WriteString_server(buffer.func_name);
            OTF2_RegionRef regionRef = globalDefWriter_WriteRegion_server(stringRef);
            regionRef_array[buffer.func_index-1] = regionRef; // populate regionRef array, C indexing from 0
        }

        // DEBUGGING
        if (flag_log){
            snprintf(fp_buffer, 50, "func_index: %d, regionRef: %u\n", 
                    buffer.func_index, regionRef_array[buffer.func_index-1]);
            fupdate_server(fp, fp_buffer);
        }
    }

    // DEBUGGING
    snprintf(fp_buffer, 50, "(pid: %d) Finished listening for globalDefWriter\n", getpid());
    fupdate_server(fp, fp_buffer);
}

//' finalize_GlobalDefWriter_client
//' @return RNilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_GlobalDefWriter_client() { // client
    int zmq_ret;
    // Send 0 length signal to end this portion on server
    zmq_ret = zmq_send(pusher, NULL, 0, 0); // ZMQ ID: 1b
    if (zmq_ret<0) { report_and_exit("finalize_GlobalDefWriter_client zmq_send", NULL); }

    return(R_NilValue);
}


void finalize_otf2_objs_server(){
    current_stage = STAGE_FINALIZE;

    ////
    //finalize_EvtWriters_server(); // Moving this after globalDef because num_events used in WriteLocation_server
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

    ////
    //finalize_GlobalDefWriter_server(); 
    epoch_end =  get_time();

    // We need to define the clock used for this trace and the overall timestamp range.
    OTF2_GlobalDefWriter_WriteClockProperties( 
            global_def_writer /* writerHandle */,
            1E6 /* resolution - 1 tick per second */,
            epoch_start /* epoch - globalOffset */,
            epoch_end - epoch_start /* traceLength */,
            OTF2_UNDEFINED_TIMESTAMP );

    ////
    //finalize_Archive_server();
    // At the end, close the archive and exit.
    OTF2_Archive_Close( archive );

    // Reset counters
    NUM_STRINGREF = OFFSET_NUM_STRINGREF;
    NUM_REGIONREF = 0;
}


//' define_otf2_regionRef_client
//' @param func_name Name of function to create event for
//' @param func_index Global index of function in R namespace
//' @return regionRef regionRef for use when logging events
// [[Rcpp::export]]
RcppExport int define_otf2_regionRef_client(Rcpp::String func_name, int func_index) {
    Zmq_otf2_defWriter buffer;
    int zmq_ret;

    // Send function index and name in Zmq_otf2_defWriter struct
    buffer.func_index = func_index;
    strncpy(buffer.func_name, func_name.get_cstring(), sizeof(buffer.func_name));
    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 1a
    if (zmq_ret < 0 ) { report_and_exit("define_otf2_regionRef_client zmq_send"); }

    // DEBUGGING
    #ifdef DEBUG
    Rcout << "Client defining - func_name: " << func_name.get_cstring() << ", func_index: " << func_index << std::endl;
    #endif

    return 0;
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
    //zmq_ret = zmq_close(pusher);
    //if (zmq_ret < 0 ) { report_and_exit("finalize_EvtWriter_client zmq_close"); }

    return(R_NilValue);
}

//' finalize_sync_client
//' @description Send signal to server to stop collecting event information
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_sync_client() {
    int zmq_ret; // Debugging recv/sends and socket
    char buffer; // Expecting length 0 message

    current_stage = STAGE_SYNC;

    // sent from logger/server at end of events
    zmq_ret = zmq_recv(syncer, &buffer, sizeof(buffer), 0);  // ZMQ ID: 7
    if (zmq_ret > 0 ) { // Recv-ed noise on port
        report_and_exit("finalize_sync_client zmq_recv non-empty"); 
    }
    else if (zmq_ret < 0 ) { 

        if (errno==4){ // Retry
            // sent from logger/server at end of events
            zmq_ret = zmq_recv(syncer, &buffer, sizeof(buffer), 0); // ZMQ ID: 7
            if (zmq_ret < 0 ) { 
                report_and_exit("finalize_sync_client zmq_ret retry"); 
            }
        }
        else {
            report_and_exit("finalize_sync_client zmq_ret"); 
        }
    }

    // Clean up zmq socket and context
    //zmq_ret = zmq_close(syncer);
    //if (zmq_ret < 0 ) { report_and_exit("finalize_sync_client zmq_close"); }
    //zmq_ret = zmq_ctx_destroy(context);
    //if (zmq_ret < 0 ) { report_and_exit("finalize_sync_client zmq_ctx_destroy"); }

    return(R_NilValue);
}


// @name finalize_sync_server
// @description Signal to client end of server work (synchronization)
// @return R_NilValue
void finalize_sync_server() {
    int zmq_ret;

    // sent from logger/server at end of events
    zmq_ret = zmq_send(syncer, NULL, 0, 0);  // ZMQ ID: 7
    if (zmq_ret < 0 ) { report_and_exit("finalize_sync_server zmq_send", syncer); }
}

void finalize_zmq_server(){
    zmq_close(puller);
    zmq_close(syncer);
    //zmq_close(responder);
    zmq_ctx_destroy(context);
}

void init_zmq_server(){
    int zmq_ret; 
    char buffer[30];

    // Init zmq context
    context = zmq_ctx_new();
    if (context==NULL){ report_and_exit("init_otf2_logger server context"); }

    // Puller
    puller = zmq_socket(context, ZMQ_PULL);
    if (puller==NULL){ report_and_exit("init_otf2_logger server puller"); }
    //zmq_ret = zmq_bind(puller, "tcp://*:5556");
    snprintf(buffer, 30, "tcp://*:%d", PORTS[0]);
    zmq_ret = zmq_bind(puller, buffer);
    if (zmq_ret!=0){ report_and_exit("server zmq_bind puller", puller); }

    // Syncer
    syncer = zmq_socket(context, ZMQ_PUSH); 
    if (syncer==NULL){ report_and_exit("init_otf2_logger server syncer"); }
    //zmq_ret = zmq_bind(syncer, "tcp://*:5557");
    snprintf(buffer, 30, "tcp://*:%d", PORTS[1]);
    zmq_ret = zmq_bind(puller, buffer);
    if (zmq_ret < 0 ) { report_and_exit("server zmq_bind syncer", NULL); }
}

//' init_zmq_client
//' @description Open zmq context and sockets
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_zmq_client(){
    int zmq_ret; 
    char buffer[30];

    // Init otf2 objs
    context = zmq_ctx_new();
    if (context==NULL){report_and_exit("init_otf2_logger client context");}

    // Connect pusher used for globalDebWriter and evtWriter
    pusher = zmq_socket(context, ZMQ_PUSH);
    if (pusher==NULL){report_and_exit("init_otf2_logger client pusher");}
    zmq_ret = zmq_connect(pusher, "tcp://localhost:5556");
    snprintf(buffer, 30, "tcp://localhost:%d", PORTS[0]);
    zmq_ret = zmq_connect(pusher, buffer);
    if (zmq_ret<0){report_and_exit("init_otf2_logger client connect pusher");}

    // Syncer
    syncer = zmq_socket(context, ZMQ_PULL); 
    if (syncer == NULL){ report_and_exit("init_otf2_logger client syncer"); }
    //zmq_ret = zmq_connect(syncer, "tcp://localhost:5557");
    snprintf(buffer, 30, "tcp://localhost:%d", PORTS[1]);
    zmq_ret = zmq_connect(syncer, buffer);
    if (zmq_ret != 0){ report_and_exit("init_otf2_logger client connect syncer"); }
    return(R_NilValue);
}

//' finalize_zmq_client
//' @description Close zmq context and sockets
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_zmq_client(){
    zmq_close(pusher);
    zmq_close(syncer);
    //zmq_close(requester);
    zmq_ctx_destroy(context);
    return(R_NilValue);
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

    if (pusher==NULL){ report_and_exit("evtWriter_MeasurementOnOff_client pusher"); }
    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5a // ZMQ ID: 5b
    if (zmq_ret<0){ report_and_exit("evtWriter_MeasurementOnOff zmq_send"); }
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

/// @param regionRef Region id
//' Write event to evt_writer
//' @param func_index Function index in global list
//' @param event_type True for enter, False for leave region
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP evtWriter_Write_client(int func_index, bool event_type)
{
#ifdef DEBUG
    Rcout << "region: " << regionRef << ", event_type: " << event_type << "\n";
#endif /* ifdef DEBUG */

    int zmq_ret; 
    Zmq_otf2_data buffer;

    // @TODO: use func_index here
    // Pack info into struct
    buffer.pid = locationRef; // TODO - update name of buffer.pid
    buffer.regionRef = func_index;
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
    /*
    */

    if (pusher==NULL){ report_and_exit("evtWriter_Write_client pusher"); }
    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5c // ZMQ ID: 5d
    if (zmq_ret<0){ 
        // DEBUGGING - Common error after opening and closing socket, 
        print_errnos(); //  print static errno values to help debug
        report_and_exit("evtWriter_Write_client event"); 
    }

    return(R_NilValue);
}


// @name run_evtWriters_server
// @description Main function during which all otf2 event information is processed
//  and logged
// @param flag_log Log all events in log file
// @return 0 if success, else non-0
int run_evtWriters_server(bool flag_log){
    int zmq_ret; // Debugging recv/sends and socket
    int retflag = 0;
    Zmq_otf2_data buffer;
    OTF2_StringRef slaveActive_stringRef;
    OTF2_RegionRef slaveActive_regionRef;

    // Placeholder for region of ZMQ_OTF2_SOCK_CLUSTER
    slaveActive_stringRef = globalDefWriter_WriteString_server("SLAVE_ACTIVE");
    slaveActive_regionRef = globalDefWriter_WriteRegion_server(slaveActive_stringRef);

    // Ensure evt_writers defined
    if (evt_writers == NULL) { report_and_exit("run_evtWriters_server evt_writers"); }
    if (puller == NULL) { report_and_exit("run_evtWriters_server puller"); }
    if (regionRef_array == NULL) { report_and_exit("run_evtWriters_server regionRef_array"); }

    // DEBUGGING
    char fp_buffer[50];
    if (flag_log){
        snprintf(fp_buffer, 50, "(pid: %d) Listening for evtWriters\n", getpid());
        fupdate_server(fp, fp_buffer);
        snprintf(fp_buffer, 50, "NUM_FUNCS: %u\n", NUM_FUNCS);
        fupdate_server(fp, fp_buffer);
    }

    // Receive globalDef strings, and return with send regionRef
    while (1) {
        zmq_ret = zmq_recv(puller, &buffer, sizeof(buffer), 0); // ZMQ ID: 5

        if (zmq_ret < 0){
            report_and_exit("run_evtWriters_server puller zmq_recv", NULL); 
        } else if (zmq_ret == 0) { // Signal to stop listening
            break; // ZMQ ID: 5x
        } else if (zmq_ret == sizeof(Zmq_otf2_data)) { 

            // DEBUGGING - Write all events to logfile
            int func_index = buffer.regionRef;
            char fp_buffer[120];
            if ((buffer.datatype==ZMQ_OTF2_EVENT_ENTER)||(buffer.datatype==ZMQ_OTF2_EVENT_LEAVE)){
                snprintf(fp_buffer, 120, "Server recv datatype: %d, pid: %d, time: %lu, func_index(if applicable): %u, regionRef: %u\n", 
                    buffer.datatype, buffer.pid, buffer.time, buffer.regionRef, regionRef_array[func_index-1]);
            } else {
                snprintf(fp_buffer, 120, "Server recv datatype: %d, pid: %d, time: %lu, func_index(if applicable): %u\n", 
                        buffer.datatype, buffer.pid, buffer.time, buffer.regionRef);
            }
            fupdate_server(fp, fp_buffer);

            if (buffer.datatype == ZMQ_OTF2_MEASUREMENT_ON ){ // ZMQ ID: 5a
                evtWriter_MeasurementOnOff_server(evt_writers[buffer.pid], buffer.time, true);
            } else if (buffer.datatype == ZMQ_OTF2_MEASUREMENT_OFF ){ // ZMQ ID: 5b
                evtWriter_MeasurementOnOff_server(evt_writers[buffer.pid], buffer.time, false);
            } else if (buffer.datatype == ZMQ_OTF2_EVENT_ENTER ){ // ZMQ ID: 5c
                OTF2_EvtWriter_Enter( evt_writers[buffer.pid], NULL /* attributeList */,
                        buffer.time, regionRef_array[buffer.regionRef-1] /* region */ );
            } else if (buffer.datatype == ZMQ_OTF2_EVENT_LEAVE ){ // ZMQ ID: 5d
                OTF2_EvtWriter_Leave( evt_writers[buffer.pid], NULL /* attributeList */,
                        buffer.time, regionRef_array[buffer.regionRef-1] /* region */ );
            } else if (buffer.datatype == ZMQ_OTF2_USED_LOCATIONREFS ){ // ZMQ ID: 5e
                if (buffer.regionRef > maxUsedLocationRef){ maxUsedLocationRef = buffer.regionRef; }
            } 
            else if (buffer.datatype == ZMQ_OTF2_INIT_PROC){ // ZMQ ID: 5h
                OTF2_EvtWriter_Enter( evt_writers[buffer.pid], NULL /* attributeList */,
                        buffer.time, slaveActive_regionRef /* region */ );
            } else if (buffer.datatype == ZMQ_OTF2_FINALIZE_PROC){ // ZMQ ID: 5i
                OTF2_EvtWriter_Leave( evt_writers[buffer.pid], NULL /* attributeList */,
                        buffer.time, slaveActive_regionRef /* region */ );
            } else {
                report_and_exit("run_evtWriters_server buffer.datatype unknown event", NULL); 
            }
        } else if (zmq_ret == sizeof(ERR_MSG)) { // ZMQ ID: xx
            retflag = -1;
            break; // exit from while loop
        } else if (zmq_ret > 0) { // Unknown datatype
            report_and_exit("run_evtWriters_server puller unknown data", NULL); 
        }
    }

    snprintf(fp_buffer, 50, "(pid: %d) Finished listening for evtWriters\n", getpid());
    fupdate_server(fp, fp_buffer);

    // Cleanup socket
    //zmq_close(puller);
    return(retflag);
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

    if (IS_LOGGER){ // Print to log file
        // Close context and sockets
        finalize_zmq_server();

        if (fp==NULL){ // Open log file if needed
            fp = fopen(log_filename, "w");
            fprintf(fp, "WARNING: Couldn't find log file pointer, made new one\n");
        }
        fprintf(fp, "[errno: %d] ERROR: %s\n", errno, msg);
        fprintf(fp, "File closing\n");
        fclose(fp);
        //kill(0, SIGTERM);
        kill(getpid(), SIGKILL);
    } else { // Print to Rcout (recommend using logfile for makeCluster)
        // Close context and sockets
        finalize_zmq_client();
        Rcout << "[R proc id: " << locationRef << "] CLIENT ERROR: " << msg << "\n";
        Rcout << "ERROR INFO - pid:" << getpid() << ", ppid: " << getppid() << ",sid: " << getsid(getpid()) << "\n";
        Rcout << "ERROR INFO - Errno:" << errno << "\n"; 

        Rcpp::stop("[R proc id: %d] CLIENT ERROR: %s\n" , locationRef, msg);
        //kill(0, SIGTERM);
    }
}

// @name fupdate_server
// @description Write message to server log file
// @param fp File pointer to log file
// @param msg Message to write to log file
void fupdate_server(FILE *fp, const char* msg){
    if (fp==NULL){
        fp = fopen(log_filename, "w");
    }
    fprintf(fp, "%s\n", msg);
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
        int zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5e
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


//' otf2_handle_proc
//' @description Signal to server to send regionRef array to new procs
//' @param is_init True if init proc, else false if finalizing proc
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP otf2_handle_proc(bool is_init){
    int zmq_ret;
    Zmq_otf2_data buffer;

    // Pack buffer
    buffer.regionRef = 0;
    buffer.pid = locationRef;
    buffer.time = get_time();
    if (is_init){
        buffer.datatype = ZMQ_OTF2_INIT_PROC;
    } else {
        buffer.datatype = ZMQ_OTF2_FINALIZE_PROC;
    }

    if (pusher == NULL ) { report_and_exit("otf2_handle_proc pusher"); }
    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // ZMQ ID: 5h, ID: 5i
    if (zmq_ret < 0 ) { report_and_exit("otf2_handle_proc pusher zmq_send"); }
    return(R_NilValue);
}


///////////////////////////////
// Testing
///////////////////////////////

//' Verify structs have unique sizes in order to distinguish between events or errors.
//'     Intended for developers
//' @return 0 if success, else non-zero
// [[Rcpp::export]]
RcppExport int test__struct_size(){
    if ( sizeof(Zmq_otf2_data)==sizeof(ERR_MSG) ){
        return -1;
    }
    return 0;
}

// @description Test Request-Respond sockets
int _test__ports_req_rep(pid_t child_pid, void *context,
        char* bind_name, char* connect_name, int port_num, int i_port,
        int *send_buf, int *recv_buf, int len_buf, int num_msg)
{
    void *socket;
    int size_buf = len_buf*sizeof(*send_buf);

    if ( child_pid != 0){ // Parent
        IS_LOGGER=false;
        if ( (socket = zmq_socket(context, ZMQ_REQ)) == NULL ){
            return(-200-i_port);
        }
        if ( zmq_bind(socket, bind_name) != 0 ){
            zmq_close(socket);
            return(-300-i_port);
        }

        for (int i_msg; i_msg<num_msg; ++i_msg){
            for (int i=0; i<len_buf; ++i){ send_buf[i] = i_msg; }
            if ( zmq_send(socket, send_buf, size_buf, 0) != size_buf ){
                zmq_close(socket);
                return(-400-i_port);
            }
            if ( zmq_recv(socket, recv_buf, size_buf, 0) != size_buf ){
                zmq_close(socket);
                return(-500-i_port);
            }
            for (int i=0; i<len_buf; ++i){ 
                if (recv_buf[i] != i_msg+1){ 
                    zmq_close(socket);
                    return(-600-i_port); 
                }
            }
        }
    } else { // Child
        IS_LOGGER=true;
        socket = zmq_socket(context, ZMQ_REP);
        zmq_connect(socket, connect_name);
        for (int i_msg; i_msg<num_msg; ++i_msg){
            zmq_recv(socket, recv_buf, size_buf, 0);
            for (int i=0; i<len_buf; ++i){ send_buf[i] = recv_buf[i]+1; }
            zmq_send(socket, send_buf, size_buf, 0);
        }
    }
    zmq_close(socket);
    return(0);
}

// TODO: Impliment sigterm handler for clients and server
void sigterm_handler_master(int signal){
    report_and_exit("RECEIVED SIGTERM");
}
void sigterm_handler_slave(int signal){
    report_and_exit("RECEIVED SIGTERM");
}
void sigterm_handler_server(int signal){
    report_and_exit("RECEIVED SIGTERM");
}
void tmp_sigterm_handler(int signal){
    report_and_exit("RECEIVED SIGTERM");
}

void kill_child_after_sync(void *context, pid_t child_pid){
    void *socket;
    if (child_pid != 0){ // Parent
        bool buffer=false;
        if ( ( socket = zmq_socket(context, ZMQ_PULL) ) == NULL ){
            report_and_exit("kill_child_after_sync zmq_socket");
        }

        if ( zmq_bind(socket, "tcp://*:5550") != 0 ){
            report_and_exit("kill_child_after_sync zmq_bind");
        }

        int zmq_ret = zmq_recv(socket, &buffer, sizeof(buffer), 0);
        if (zmq_ret==0){
            kill(child_pid, SIGTERM);
        } else if ( zmq_ret>0 ){
            report_and_exit("kill_child_after_sync zmq_recv >0");
        } else {
            report_and_exit("kill_child_after_sync zmq_recv <1");
        }

        kill(child_pid, SIGTERM);
        zmq_close(socket);
    } else { // Child
        signal(SIGTERM, tmp_sigterm_handler);
        if ( ( socket = zmq_socket(context, ZMQ_PUSH) ) == NULL ){
            report_and_exit("kill_child_after_sync zmq_socket");
        }
        if ( zmq_connect(socket, "tcp://localhost:5550") != 0 ){
            report_and_exit("kill_child_after_sync zmq_connect");
        }
        if ( zmq_send(socket, NULL, 0, 0) < 0 ){
            report_and_exit("kill_child_after_sync zmq_send");
        }
        zmq_send(socket, NULL, 0, 0); // Hangs until recv sigterm
        zmq_close(socket);
        sleep(10);
    }
}




//' Test ports used by rTrace
//'     Intended for developers
//' @param ports List of ports to test on valid range \[1024,49151\], default {5554,5556,5557,5559}
//' @param port_type 1:=req-rep, 2:=push-pull, 3:=pub-sub, else error
//' @param num_messages Number of messages to send
//' @param message_size Size of each msg in KB
//' @return 0 if success, else non-zero
// [[Rcpp::export]]
RcppExport int test__ports( int port_type, 
        Rcpp::NumericVector ports = Rcpp::NumericVector::create(5554,5556,5557,5559),
        int num_messages=1000, size_t message_size=8 ) 
{
    pid_t child_pid;
    int num_ports, port, len_buf;
    int *send_buf, *recv_buf;
    char bind_name[21], connect_name[21];

    // Check valid port number not in valid range
    for (int i=0; i<ports.length(); ++i){
        if ( (ports[i]<1024) || (ports[i]>49151) ){
            return(-100-i);
        }
    }
    // Check valid port type
    if ( (port_type < 0) || (port_type > 3) ){
        return(-99);
    }

    num_ports = ports.length();
    send_buf = (int*)malloc(message_size*1024);
    recv_buf = (int*)malloc(message_size*1024);
    len_buf = message_size/sizeof(*send_buf) * 1024;


    // Fork "server" proc as child
    child_pid = fork();
    if ( child_pid == (pid_t) -1 ){ // ERROR
        return(-1);
    }

    // Spawn context on new proc
    if ( (context = zmq_ctx_new()) == NULL ){
        return(-2);
    }

    for (int i=0; i<num_ports; ++i){
        port = ports[i];

        snprintf(bind_name, 21, "tcp://*:%4d", port);
        snprintf(connect_name, 21, "tcp://localhost:%4d", port);

        if (child_pid != 0){
            Rcout << "Testing socket on port: " << ports[i] << 
                ", bind_name: " << bind_name <<
                ", connect_name: " << connect_name << std::endl;
        }

        switch (port_type){

            case 2:
            {
                int ret_test = _test__ports_req_rep(child_pid, context,
                        bind_name, connect_name, port, i, 
                        send_buf, recv_buf, len_buf, num_messages);
                if (ret_test != 0){ 
                    Rcout << "ERROR: _test__ports_req_rep fail, return code: " << ret_test << "\n";
                    return(ret_test); }
                if (child_pid != 0) { Rcout << "SUCCESS\n"; }
                break;
            }
            default:
            {
                // Unrecognized port_type
                break;
            }
        }
    }
    free(send_buf);
    free(recv_buf);

    kill_child_after_sync(context, child_pid);
    zmq_ctx_destroy(context);
    //if (child_pid!=0) {EXIT(1);}
    //if (child_pid!=0) {return 1;}
    return 0;
}

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

