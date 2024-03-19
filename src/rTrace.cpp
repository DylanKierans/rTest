// @file rTrace.cpp
// @brief Rcpp functions for underlying otf2 library
// @date 2024-01-16
// @version 0.02
// @author D.Kierans (dylanki@kth.se)
// @note Location ~= thread, LocationGroup ~= Process, SystemTree ~= Node
// @note errno 98 indicates socket alreayd in use 
// @note errno 88 indicates not socket (ENOTSOCK)
// @note errno 4 (EINTR) Interrupted system call
// @todo Check documentation
// @todo Look at timing offsets per proc
// @todo Error checking (otf2 objects existence, sockets, send/recv messages)
// @todo Signal handling for both procs
// @todo get epochs from Master proc
// @todo Impliment _enable and _disable correctly

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
#define LEN 40 // Max length of R function

using namespace Rcpp;

// TODO - Send maxUsedLocationRef to server
// TODO - impliment this enum-ing!
typedef enum {
    ZMQ_OTF2_EVENT_ENTER, 
    ZMQ_OTF2_EVENT_LEAVE, 
    ZMQ_OTF2_MEASUREMENT_ON, 
    ZMQ_OTF2_MEASUREMENT_OFF, 
    ZMQ_OTF2_USED_LOCATIONREFS, 
} zmq_otf2_datatypes;

typedef struct Zmq_otf2_data {
    OTF2_TimeStamp time;
    OTF2_RegionRef regionRef; ///< Could probably generalize this datatype better, see set_maxUsedLocationRef()
    pid_t pid;
    zmq_otf2_datatypes datatype;
} Zmq_otf2_data;

static OTF2_Archive* archive;
static OTF2_GlobalDefWriter* global_def_writer;
OTF2_TimeStamp epoch_start, epoch_end;  // OTF2_GlobalDefWriter_WriteClockProperties
static OTF2_EvtWriter** evt_writers;

// ZeroMQ 
static bool IS_LOGGER=false;
static void *context;      // zmq context
static void *requester;    // zmq socket
static void *pusher;    // zmq socket


// Largest required buffer size of zmq_otf2 events/measurements
const size_t ZMQ_OTF2_BUFFERSIZE = sizeof(Zmq_otf2_data);

// Counters
static OTF2_StringRef NUM_STRINGREF=0; ///* Number of events recorded with WriteString
static OTF2_RegionRef NUM_REGIONREF=0; ///* Number of regions recorded with WriteRegion

// IDs
static OTF2_LocationRef maxLocationRef=0; ///< Cap for max number of R procs
static OTF2_LocationRef maxUsedLocationRef=1; ///< Maximum number of used R procs <maxLocationRef
static int locationRef=0;


// DEBUGGING
static int id;
char filename[]="log.log";
static FILE *fp;


///////////////////////////////
// Function declarations
///////////////////////////////
RcppExport SEXP finalize_EvtWriter_client();
RcppExport SEXP finalize_otf2_client();

// OTF2 Server/logger functions
void init_Archive_server(Rcpp::String, Rcpp::String);
void finalize_Archive_server();
void init_EvtWriters_server();
void finalize_EvtWriters_server();
void init_GlobalDefWriter_server();
void finalize_GlobalDefWriter_server();
void evtWriters_server();
void globalDefWriter_server();
void finalize_otf2_server();
OTF2_StringRef globalDefWriter_WriteString_server(Rcpp::String stringRefValue);
OTF2_RegionRef globalDefWriter_WriteRegion_server(OTF2_StringRef stringRef_RegionName);
void globalDefWriter_WriteSystemTreeNode_server(OTF2_StringRef, OTF2_StringRef);
//void globalDefWriter_WriteLocation_server(OTF2_LocationRef, OTF2_LocationGroupRef);
//void globalDefWriter_WriteLocationGroup_server(OTF2_LocationGroupRef);
void globalDefWriter_WriteLocations_server();
void globalDefWriter_WriteLocationGroups_server();
void evtWriter_MeasurementOnOff_server(OTF2_EvtWriter*, OTF2_TimeStamp, bool);



// Helper functions for debugging
void report_and_exit(const char* msg);
void logger_error(const char*, void*);
void fupdate(FILE*, const char*);
RcppExport SEXP print_errnos();


///////////////////////////////
// Function definitions
///////////////////////////////

/////
// OTF2 Functions
/////

// This example uses a function delivering dummy timestamps
//     Walltime O(E-6)
// @return time - Current time
static OTF2_TimeStamp get_time() {
    static OTF2_TimeStamp wtime;
#ifdef DUMMY_TIMESTEPS
#ifdef DEBUG
    Rcout << "time: " << wtime << "\n";
#endif 
    return wtime++;
#else // Wall clock time O(E-6)

	struct timeval t;
	gettimeofday(&t, NULL);
    wtime = t.tv_sec*1E6 + t.tv_usec;
#ifdef DEBUG
    Rcout << "time: " << wtime << "\n";
#endif /* ifdef DEBUG */
	return wtime;
#endif
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
// Spawn otf2 process, and give task list
//  Awaits messages from main process at suitable steps
//////////////////////////////////////

void signal_hup_handler(int signal) {
    // Make sure only catching intended signal, else rethrow
    if (signal == SIGHUP) { /*ignore*/; }
    else { raise(signal); }
}

//' Fork and initialize zeromq sockets for writing globalDef definitions
//' @param max_nprocs Maximum number of R processes (ie evtWriters required)
//' @param archivePath Path to otf2 archive
//' @param archiveName Name of otf2 archive
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport int init_otf2_logger(int max_nprocs, Rcpp::String archivePath = "./rTrace", 
        Rcpp::String archiveName = "rTrace" )
{
    // TODO: Verify this acts as intended to save child proc
    signal(SIGHUP, signal_hup_handler);

    pid_t child_pid = fork();
    if (child_pid == (pid_t) -1 ){ // ERROR
        report_and_exit("Forking logger process");
        return(1);
    }

    if (child_pid == 0) { // Child process

        // DEBUGGING
        Rcout << "Child - pid: " << getpid() << ", ppid: " << getppid() << "\n";

        // Create daemonized process using double fork
        //  Avoids duplication when R forks 
        if (fork() != 0){
            sleep(1);
            return(1);
        } 
        setsid(); // Else create own server id ...
    /*
    */
        IS_LOGGER = true;
        maxLocationRef = max_nprocs;

        // Open log file
        fp = fopen(filename, "w");
        if (fp==NULL){ logger_error("Opening file", NULL); }
        fupdate(fp, "File opened\n");

        // OTF2 Objs
        //Rcpp::String archivePath = "./rTrace";
        //Rcpp::String archiveName = "rTrace";
        init_Archive_server(archivePath, archiveName);
        fupdate(fp, "Init archive complete\n");
        init_EvtWriters_server( );
        fupdate(fp, "Init evt_writers complete\n");
        init_GlobalDefWriter_server();
        fupdate(fp, "Init of otf2 objs complete\n");

        // Server for logging GlobalDefWriter strings&regions
        globalDefWriter_server();
        fupdate(fp, "globalDefWriter_server complete\n");

        // Server listens for events
        fupdate(fp, "evtWriter\n");
        evtWriters_server();
        fupdate(fp, "evtWriter complete\n");

        // Write definitions for proc structures
        globalDefWriter_WriteSystemTreeNode_server(0,0); // 1 system tree node
        globalDefWriter_WriteLocationGroups_server(); // n location groups (n procs)
        globalDefWriter_WriteLocations_server(); // n locations (1 per proc)

        // Finalization
        finalize_EvtWriters_server(); // Moving this after globalDef because num_events used in WriteLocation_server
        finalize_GlobalDefWriter_server(); // @TODO: Rename to globalDefWriter_Clock
        finalize_Archive_server();
        finalize_otf2_server();
        fupdate(fp, "COMPLETE!\n");
        if (fp!=NULL){fclose(fp);}

        return(1);
        // exit(0); 
    } else {

        // DEBUGGING
        Rcout << "Parent - pid: " << getpid() << ", child_pid:" << child_pid << "\n";

        IS_LOGGER = false;
        context = zmq_ctx_new ();
        requester = zmq_socket (context, ZMQ_REQ); // ZMQ_PUSH
        pusher = zmq_socket (context, ZMQ_PUSH);

        // @TODO Error check connect
        zmq_connect(requester, "tcp://localhost:5555");
        zmq_connect(pusher, "tcp://localhost:5556");
    }
    return(0);
}


// Send 0 length signal to end this portion
void globalDefWriter_server() { // Server

    //  Socket to talk to clients
    context = zmq_ctx_new ();
    void *responder = zmq_socket (context, ZMQ_REP); // ZMQ_PULL
    int rc = zmq_bind (responder, "tcp://*:5555");
    //assert (rc == 0); // errno 98 indicates socket alreayd in use
    if (rc!=0){ logger_error("Binding responder", responder); }

    // DEBUGGING
    char buffer[50];
    snprintf(buffer, 50, "(pid: %d) Listening for globalDefWriter\n", getpid());
    fupdate(fp, buffer);

    int iter=0; ///< Number of messages received

    // Receive globalDef strings, and return with send regionRef
    while (1) {
		char buffer[LEN];
        int zmq_ret = zmq_recv(responder, buffer, LEN*sizeof(*buffer), 0);
        if ( zmq_ret < 0 ) { 
            logger_error("zmq_recv", responder); 
        } else if (zmq_ret == 0) {
            break; // Signal end of globalDef from client
        } else {
            buffer [zmq_ret] = '\0';
            //printf ("[%d] Received: %s\n", iter, buffer);
            // Define as stringRef, regionRef
            OTF2_StringRef stringRef = globalDefWriter_WriteString_server(buffer);
            OTF2_RegionRef regionRef = globalDefWriter_WriteRegion_server(stringRef);

            // DEBUGGING
            #ifdef DEBUG
            Rcout << "Server - func_name: "<< buffer << ", regionRef:" << regionRef << std::endl;
            #endif

            // Return regionRef ID
            zmq_ret = zmq_send (responder, &regionRef, sizeof(regionRef), 0);
            if (zmq_ret<0) { report_and_exit("server zmq_send"); }
            iter++;
        }
    }

    // DEBUGGING
    snprintf(buffer, 50, "(pid: %d) Finished listening for globalDefWriter\n", getpid());
    fupdate(fp, buffer);

    // Cleanup socket
    zmq_close(responder);
}

//' finalize_GlobalDefWriter_client
//' @return RNilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_GlobalDefWriter_client() { // client
    // Send 0 length signal to end this portion on server
    zmq_send(requester, NULL, 0, 0);

    // Close requester socket
    zmq_close(requester);
    return(R_NilValue);
}


//' define_otf2_event_client
//' @param func_name Name of function to create event for
//' @return regionRef regionRef for use when logging events
// [[Rcpp::export]]
RcppExport int define_otf2_event_client(Rcpp::String func_name) {
    OTF2_RegionRef regionRef;
    int send_ret = zmq_send(requester, func_name.get_cstring(), LEN*sizeof(char), 0);
    if (send_ret < 0 ) { report_and_exit("define_otf2_event_client zmq_send"); }
    int recv_ret = zmq_recv(requester, &regionRef, sizeof(regionRef), 0);
    if (recv_ret < 0 ) { report_and_exit("define_otf2_event_client zmq_recv"); }

    // DEBUGGING
    #ifdef DEBUG
    Rcout << "Client defining - func_name: " << func_name.get_cstring() << std::endl;
    Rcout << "regionRef: " << regionRef << std::endl;
    #endif
    return (int)regionRef;
}

//' finalize_EvtWriter_client
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_EvtWriter_client() {
    if (pusher == NULL ) { report_and_exit("finalize_EvtWriter_client pusher"); }

    int zmq_ret;
    zmq_ret = zmq_send(pusher, NULL, 0, 0); // Send 0 message to Master to sync
    if (zmq_ret < 0 ) { report_and_exit("client finalize_EvtWriter_client"); }

    // Cleanup socket
    zmq_close(pusher);

    return(R_NilValue);
}

//' finalize_otf2_client
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_otf2_client() {
    void *syncer_client = zmq_socket(context, ZMQ_PULL); 
    if (syncer_client == NULL){ report_and_exit("finalize_otf2_client syncer_client"); }
    int rc = zmq_bind (syncer_client, "tcp://*:5557");
    assert (rc == 0); 

    // DEBUGGING - Add delay to see if server registers events from other procs
    sleep(5);

    char buffer;
    int zmq_ret = zmq_recv(syncer_client, &buffer, sizeof(buffer), 0); // sent from logger/server at end of events
    if (zmq_ret < 0 ) { 

        if (errno==4){
            int zmq_ret = zmq_recv(syncer_client, &buffer, sizeof(buffer), 0); // sent from logger/server at end of events
            if (zmq_ret < 0 ) { 
                report_and_exit("finalize_otf2_client zmq_ret retry"); 
            }
        }
        else {
            report_and_exit("finalize_otf2_client zmq_ret"); 
        }

        //report_and_exit("finalize_otf2_client zmq_ret"); 

    }

    // Clean up zmq socket and context
    zmq_close(syncer_client);
    zmq_ctx_destroy(context);

    return(R_NilValue);
}


//' finalize_otf2_server
//' @return R_NilValue
void finalize_otf2_server() {
    void *syncer_server = zmq_socket(context, ZMQ_PUSH); 
    zmq_connect (syncer_server, "tcp://localhost:5557");

    int zmq_ret = zmq_send(syncer_server, NULL, 0, 0); // sent from logger/server at end of events
    if (zmq_ret < 0 ) { report_and_exit("finalize_otf2_server"); }

    // Clean up zmq socket and context
    zmq_close(syncer_server);
    zmq_ctx_destroy(context);
}



//////////////////////////////////////
// END OF TODO
//////////////////////////////////////


//' Initialize static otf2 {archive} objs
//' @param archivePath Path to the archive i.e. the directory where the anchor file is located.
//' @param archiveName Name of the archive. It is used to generate sub paths e.g. "archiveName.otf2"
//' @return R_NilValue
void init_Archive_server(Rcpp::String archivePath="./rTrace", Rcpp::String archiveName="rTrace") 
{

    archive = OTF2_Archive_Open( archivePath.get_cstring(),
                                               archiveName.get_cstring(),
                                               OTF2_FILEMODE_WRITE,
                                               1024 * 1024 /* event chunk size */,
                                               4 * 1024 * 1024 /* def chunk size*/,
                                               OTF2_SUBSTRATE_POSIX,
                                               OTF2_COMPRESSION_NONE );
    if (archive == NULL) { logger_error("OTF2_Archive_Open", NULL); }

    // Set the previously defined flush callbacks.
    OTF2_Archive_SetFlushCallbacks( archive, &flush_callbacks, NULL );

    // We will operate in a serial context.
    OTF2_Archive_SetSerialCollectiveCallbacks( archive );

    // Now we can create the event files. Though physical files aren't created yet.
    OTF2_Archive_OpenEvtFiles( archive );
}


//' Close static otf2 {archive} objs
void finalize_Archive_server() {
    // DEBUGGING
    if (archive == NULL) { logger_error("finalize_Archive archive", NULL); }

    // At the end, close the archive and exit.
    OTF2_Archive_Close( archive );

    // Reset counters
    NUM_STRINGREF = 0;
    NUM_REGIONREF = 0;
}


// TODO: Multiproc this!
//' Initialize static otf2 {evt_writers} objs
void init_EvtWriters_server() {
    if (IS_LOGGER){ // Ensure isn't called from main proc!
        // DEBUGGING: OTF2_Archive_GetEvtWriter throwing error 
        if (archive == NULL) { logger_error("init_EvtWriters_server", NULL); }

        // Make sure maxLocationRef set
        if (maxLocationRef < 1){ logger_error("init_EvtWriters_server maxLocationRef", NULL); }

        // DEBUGGING
        char buffer[50];
        snprintf(buffer, 50, "maxLocationRef: %lu\n", maxLocationRef);
        fupdate(fp, buffer);

        // Get a event writer for each location
        evt_writers = (OTF2_EvtWriter**)malloc(maxLocationRef*sizeof(*evt_writers));
        if (evt_writers == NULL) { logger_error("init_EvtWriters_server evt_writers", NULL); }
        for (OTF2_LocationRef i=0; i<maxLocationRef; ++i){
            evt_writers[i] = OTF2_Archive_GetEvtWriter( archive, i );
        }

    }
}


//' Close static otf2 {evt_writers} objs
void finalize_EvtWriters_server() {
    // DEBUGGING
    if (evt_writers == NULL) { logger_error("finalize_EvtWriters_server evt_writers", NULL); }
    if (archive == NULL) { logger_error("finalize_EvtWriters_server archive", NULL); }

    // Now close the event writer, before closing the event files collectively.
    for (OTF2_LocationRef i=0; i<maxLocationRef; ++i){
        if (evt_writers[i] == NULL) { logger_error("finalize_EvtWriters_server evt_writers[i]", NULL); }
        OTF2_Archive_CloseEvtWriter( archive, evt_writers[i]);
    }
    free(evt_writers);

    // After we wrote all of the events we close the event files again.
    OTF2_Archive_CloseEvtFiles( archive );
}


// @TODO: zmq this
//' Enable or disable event measurement
//' @param measurementMode True to enable, else disable
//' @return R_NilValue
void evtWriter_MeasurementOnOff_server(OTF2_EvtWriter *evt_writer, OTF2_TimeStamp time, bool measurementMode) {
    // Enable/disable measurement
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


// @TODO: zmq this
//' Enable or disable event measurement
//' @param measurementMode True to enable, else disable
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP evtWriter_MeasurementOnOff_client(bool measurementMode) {
    Zmq_otf2_data buffer;
    buffer.pid = locationRef; 
    buffer.time = get_time();
    if (measurementMode){ buffer.datatype = ZMQ_OTF2_MEASUREMENT_ON; }
    else { buffer.datatype = ZMQ_OTF2_MEASUREMENT_OFF; }

    int zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // Zmq_otf2_data
    if (zmq_ret<0){ report_and_exit("zmq_send Zmq_otf2_data measurement"); }
    return(R_NilValue);
}


//' Init static otf2 {globaldefwriter} obj
void init_GlobalDefWriter_server() {
    // DEBUGGING
    if (archive == NULL) { logger_error("init_GlobalDefWriter archive", NULL); }

    epoch_start = get_time(); // Get init time for finalize

    // Now write the global definitions by getting a writer object for it.
    global_def_writer = OTF2_Archive_GetGlobalDefWriter( archive );
    if (global_def_writer == NULL) { logger_error("OTF2_Archive_GetGlobalDefWriter", NULL); }

    // Define empty string in first stringRef value
    Rcpp::String stringRefValue="";
    globalDefWriter_WriteString_server(stringRefValue);
}

//' Finalize static otf2 {globaldefwriter} obj
//'     Write clock information before ending tracing
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


//' Define new id-value pair in globaldefwriter
//' @param stringRefValue String assigned to given id
//' @return NUM_STRINGREF 
OTF2_StringRef globalDefWriter_WriteString_server(Rcpp::String stringRefValue)
{
    OTF2_GlobalDefWriter_WriteString(global_def_writer, NUM_STRINGREF, stringRefValue.get_cstring() );
    return(NUM_STRINGREF++); // ++ applied after return value!
}


//' globalDefWriter_WriteRegion
//'     Define new region description in global writer
//' @param stringRef_RegionName Name to be associated with region
//' @return regionRef id/index for string
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
//' Write the system tree including a definition for the location group to the global definition writer.
//' @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
//' @param stringRef_class Class to be associated with SystemTreeNode (eg node)
void globalDefWriter_WriteSystemTreeNode_server( OTF2_StringRef stringRef_name, OTF2_StringRef stringRef_class) {

    // TODO: Get NodeName from syscall
    // Write the system tree incl definition for location group to global definition writer.
    OTF2_StringRef stringRef_SystemTreeNodeName = globalDefWriter_WriteString_server("MyHost");
    OTF2_StringRef stringRef_SystemTreeNodeClass = globalDefWriter_WriteString_server("node");
    OTF2_GlobalDefWriter_WriteSystemTreeNode( global_def_writer,
            0 /* SystemTreeNodeRef id */,
            stringRef_SystemTreeNodeName /* StringRef name */,
            stringRef_SystemTreeNodeClass /* StringRef class */,
            OTF2_UNDEFINED_SYSTEM_TREE_NODE /* parent */ );
}

// TODO: multiproc this! locationGroupID_Master
// Write LocationGroup (ie proc) information
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

// TODO: multiproc this! locationGroupID_Master
//void globalDefWriter_WriteLocationGroup_server(OTF2_LocationGroupRef locationGroupID) {
//
//    // Write LocationGroup (ie proc) information
//    OTF2_StringRef locationGroupRef_Name;
//    //if (locationGroupID == locationGroupID_Master) {
//    if (locationGroupID == 0) {
//        locationGroupRef_Name = globalDefWriter_WriteString_server("Master Process");
//    } else {
//        locationGroupRef_Name = globalDefWriter_WriteString_server("Slave Processes");
//    }
//
//    OTF2_GlobalDefWriter_WriteLocationGroup( global_def_writer,
//            locationGroupID /* OTF2_LocationGroupRef  */,
//            locationGroupRef_Name /* name */,
//            OTF2_LOCATION_GROUP_TYPE_PROCESS /* New LocationGroup for each process */, 
//            0 /* system tree */,
//            OTF2_UNDEFINED_LOCATION_GROUP /* creating process */ );
//}

//' Write a definition for the location to the global definition writer.
void globalDefWriter_WriteLocations_server() {
    OTF2_StringRef stringRef_LocationName = globalDefWriter_WriteString_server("Main thread");
    
    char fp_buffer[100];
    snprintf(fp_buffer, 100, "maxUsedLocationRef: %lu\n", maxUsedLocationRef);
    fupdate(fp, fp_buffer);

    for (OTF2_LocationRef i=0; i<maxUsedLocationRef; ++i){
        // Write a definition for the location to the global definition writer.
        uint64_t num_events;
        OTF2_EvtWriter_GetNumberOfEvents(evt_writers[i], &num_events);
        //OTF2_EvtWriter_GetNumberOfEvents(evt_writer, &num_events);

        // DEBUGGING
        char fp_buffer[100];
        snprintf(fp_buffer, 100, "globalDefWriter_WriteLocation - LocationRef: %lu, Num events: %ld\n",
            i, num_events);
        fupdate(fp, fp_buffer);

        OTF2_GlobalDefWriter_WriteLocation( global_def_writer,
                i /* locationRef */,
                stringRef_LocationName /* stringRef_name */,
                OTF2_LOCATION_TYPE_CPU_THREAD,
                num_events /* #events */,
                i /* locationGroupRef */ );
    }
}

////' Write a definition for the location to the global definition writer.
////' @param locationID Identifier for location (eg PID)
//void globalDefWriter_WriteLocation_server(OTF2_LocationRef locationID, OTF2_LocationGroupRef locationGroupID ) {
    //OTF2_StringRef stringRef_LocationName = globalDefWriter_WriteString_server("Main thread");
    
    //// Write a definition for the location to the global definition writer.
    //uint64_t num_events;
    //OTF2_EvtWriter_GetNumberOfEvents(evt_writers[locationID], &num_events);
    ////OTF2_EvtWriter_GetNumberOfEvents(evt_writer, &num_events);

    //Rcout << "globalDefWriter_WriteLocation - Num events: " << num_events << "\n";
    //OTF2_GlobalDefWriter_WriteLocation( global_def_writer,
            //locationID /* id */,
            //stringRef_LocationName /* stringRef_name */,
            //OTF2_LOCATION_TYPE_CPU_THREAD,
            //num_events /* #events */,
            //locationGroupID /* location group */ );
//}


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
    if ( context == NULL ){ report_and_exit("open_EvtWriterSocket_client context"); }

    pusher = zmq_socket (context, ZMQ_PUSH);
    if ( pusher == NULL ){ report_and_exit("open_EvtWriterSocket_client pusher"); }
    if ( zmq_connect(pusher, "tcp://localhost:5556") != 0 ){ report_and_exit("open_EvtWriterSocket_client"); }
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
    buffer.pid = locationRef; // TODO - update pid 
    buffer.regionRef = regionRef;
    buffer.time = get_time();
    if (event_type) { buffer.datatype = ZMQ_OTF2_EVENT_ENTER; }
    else { buffer.datatype = ZMQ_OTF2_EVENT_LEAVE; }

    // DEBUGGING 
    if (buffer.pid == 0){
        FILE *fp = fopen("write_client_0.log", "a");
        fprintf(fp, "pid: %d, time: %lu, regionRef: %u\n", buffer.pid, buffer.time, buffer.regionRef);
        fclose(fp);
    }
    if (buffer.pid == 1){
        FILE *fp = fopen("write_client_1.log", "a");
        fprintf(fp, "pid: %d, time: %lu, regionRef: %u\n", buffer.pid, buffer.time, buffer.regionRef);
        fclose(fp);
    }
        /*
    if (buffer.pid != 0){
    Rcout << "evtWriter_Write_client start of zmq_send";
    Rcout << ", pid: " << buffer.pid;
    Rcout << ", regionRef: " << buffer.regionRef;
    Rcout << ", time: " << buffer.time;
    Rcout << ", datatype: " << buffer.datatype;
    Rcout << "\n";
    }
        */
    #ifdef DEBUG
    #endif

    if (pusher==NULL){ report_and_exit("evtWriter_Write_client pusher"); }
    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // Zmq_otf2_data
    if (zmq_ret<0){ 
        Rcout << "ENOTSOCK: " << ENOTSOCK << "\n";
        Rcout << "EINTR: " << EINTR << "\n";
        Rcout << "ERROR DETAILS - regionRef: " << regionRef << "\n";
        Rcout << "ERROR DETAILS - zmq_ret: " << zmq_ret << "\n";
        Rcout << "ERROR DETAILS - errno: " << errno << "\n";
        report_and_exit("evtWriter_Write_client event"); 
    }

    return(R_NilValue);
}


// TODO: multiproc this! evtWriter dependent on LocationGroup/PID
// TODO: function for evtWriters err check
void evtWriters_server(){

    if (evt_writers == NULL) { logger_error("evtWriters_server evt_writers", NULL); }

    //  Socket to talk to clients
    void *puller = zmq_socket (context, ZMQ_PULL); // ZMQ_PULL
    int rc = zmq_bind (puller, "tcp://*:5556");
    //assert (rc == 0); // errno 98 indicates socket alreayd in use
    if (rc!=0){ logger_error("Binding puller", puller); }

    // DEBUGGING
    char buffer[50];
    snprintf(buffer, 50, "(pid: %d) Listening for evtWriters\n", getpid());
    fupdate(fp, buffer);

    // Receive globalDef strings, and return with send regionRef
    while (1) {

        Zmq_otf2_data buffer;
        int zmq_ret;

        zmq_ret = zmq_recv(puller, &buffer, ZMQ_OTF2_BUFFERSIZE, 0); // Zmq_otf2_data

        if (zmq_ret<0){
            report_and_exit("zmq_recv Zmq_otf2_data"); 
        } else if (zmq_ret == 0) { // Signal to stop listening
            break;
        } else if (zmq_ret == sizeof(Zmq_otf2_data)) { 

            // DEBUGGING
            char evt_buffer[100];
            snprintf(evt_buffer, 100, "Server recv datatype: %d, pid: %d, time: %lu, regionRef(if applicable): %u\n", 
                buffer.datatype, buffer.pid, buffer.time, buffer.regionRef);
            fupdate(fp, evt_buffer);

            if (buffer.datatype == ZMQ_OTF2_MEASUREMENT_ON ){
                evtWriter_MeasurementOnOff_server(evt_writers[buffer.pid], buffer.time, true);
            } else if (buffer.datatype == ZMQ_OTF2_MEASUREMENT_OFF ){
                evtWriter_MeasurementOnOff_server(evt_writers[buffer.pid], buffer.time, false);
            } else if (buffer.datatype == ZMQ_OTF2_EVENT_ENTER ){
                OTF2_EvtWriter_Enter( evt_writers[buffer.pid], NULL /* attributeList */,
                        buffer.time, buffer.regionRef /* region */ );
            } else if (buffer.datatype == ZMQ_OTF2_EVENT_LEAVE ){
                OTF2_EvtWriter_Leave( evt_writers[buffer.pid], NULL /* attributeList */,
                        buffer.time, buffer.regionRef /* region */ );
            } else if (buffer.datatype == ZMQ_OTF2_USED_LOCATIONREFS ){
                //if (buffer.regionRef > maxUsedLocationRef){ maxUsedLocationRef = buffer.regionRef; }
                maxUsedLocationRef = buffer.regionRef;
            } else { // Unknown datatype
                report_and_exit("evtWriters_server unknown_datatype"); 
            }
        } else { // Unknown datasize
            report_and_exit("evtWriters_server unknown_data_size"); 
        }
    }
    snprintf(buffer, 50, "(pid: %d) Finished listening for evtWriters\n", getpid());
    fupdate(fp, buffer);

    // Cleanup socket
    zmq_close(puller);
}


///////////////////////////////
// Helper functions
///////////////////////////////

// @TODO: Replace usage of this with more R-friendly version
void report_and_exit(const char* msg){
    Rcout << "[R proc id: " << locationRef << "] CLIENT ERROR: " << msg << "\n";
    Rcout << "ERROR INFO - pid:" << getpid() << ", ppid: " << getppid() << ",sid: " << getsid(getpid()) << "\n";
    Rcout << "ERROR INFO - Errno:" << errno << "\n"; 
    if (locationRef==0){ kill(0, SIGTERM); }
    //exit(-1);
    ;
}

// Attempt to throw error to parent process
void logger_error(const char* msg, void *socket){
    if (fp==NULL){
        // Open log file
        fp = fopen("log_failure.log", "w");
        fprintf(fp, "ERROR: Couldn't find log file pointer, made new one\n");
        fprintf(fp, "pid: %u, ppid: %u\n", getpid(), getppid());
        fclose(fp);
    } else {
        fprintf(fp, "ERROR: %s\n", msg);
        fprintf(fp, "File closing\n");
        fclose(fp);
    }

    // Cleanup sockets and zmq
    if (socket!=NULL){ zmq_close(socket); }
    if (context!=NULL){ zmq_ctx_destroy(context); }

    //pid_t ppid = getppid();
    kill(0, SIGTERM);
}

void fupdate(FILE *fp, const char* msg){
    if (fp!=NULL){
        fprintf(fp, "%s\n", msg);
        fclose(fp);
        fp = fopen(filename, "a");
    }
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
        int zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0);
        if (zmq_ret<0) { report_and_exit("set_maxUsedLocationRef_client zmq_send"); }
    }
    return (maxUsedLocationRef);
}

//' print_errnos
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


///////////////////////////////
// Testing
///////////////////////////////

//' set_id
//' @param newid new id
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP set_id(const int newid) {
    id = newid;    
    return (R_NilValue);
}

//' get_id
//' @return id int
// [[Rcpp::export]]
RcppExport int get_id() {
    return (id);
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
