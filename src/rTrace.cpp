// @file rTrace.cpp
// @brief Rcpp functions for underlying otf2 library
// @date 2024-01-16
// @version 0.02
// @author D.Kierans (dylanki@kth.se)
// @todo Error checking
// @todo Fix timing offset problems - caused by taking epoch at different time to evtWriter start
// @todo Fix timing coming from other proc
// @note Location := thread, LocationGroup := Process, SystemTree := Node

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

static OTF2_Archive* archive;
static OTF2_EvtWriter* evt_writer;
static OTF2_GlobalDefWriter* global_def_writer;
OTF2_TimeStamp epoch_start, epoch_end;  // OTF2_GlobalDefWriter_WriteClockProperties

// ZeroMQ 
static bool IS_LOGGER=false;
static void *context;      // zmq context
static void *requester;    // zmq socket
static void *pusher;    // zmq socket

struct zmq_otf2_event {
    OTF2_TimeStamp time;
    OTF2_RegionRef regionRef;
    pid_t pid;
    bool event_type;
} zmq_otf2_event;

// Counters
static uint64_t NUM_EVENTS=0; ///* Number of events recorded for WriteLocation
static OTF2_StringRef NUM_STRINGREF=0; ///* Number of events recorded with WriteString
static OTF2_RegionRef NUM_REGIONREF=0; ///* Number of regions recorded with WriteRegion

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
void init_EvtWriter_server(OTF2_LocationRef);
void finalize_EvtWriter_server();
void init_GlobalDefWriter_server();
void finalize_GlobalDefWriter_server();
void evtWriter_server();
void globalDefWriter_server();
void finalize_otf2_server();
OTF2_StringRef globalDefWriter_WriteString_server(Rcpp::String stringRefValue);
OTF2_RegionRef globalDefWriter_WriteRegion_server(OTF2_StringRef stringRef_RegionName);
void globalDefWriter_WriteSystemTreeNode_server(int, int);
void globalDefWriter_WriteLocation_server(int);

// Helper functions for debugging
void report_and_exit(const char* msg);
void logger_error(const char*, void*);
void fupdate(FILE*, const char*);


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
// TODO
//////////////////////////////////////

//' Fork and initialize zeromq sockets for writing globalDef definitions
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_otf2_logger()
{
    pid_t child_pid = fork();
    if (child_pid != 0){
        Rcout << "Parent - pid: " << getpid() << ", child_pid:" << child_pid << "\n";
    }
    if (child_pid == 0){
        IS_LOGGER = true;

        // Open log file
        fp = fopen(filename, "w");
        if (fp==NULL){ logger_error("Opening file", NULL); }
        fupdate(fp, "File opened\n");

        // OTF2 Objs
        Rcpp::String archivePath = "./rTrace";
        Rcpp::String archiveName = "rTrace";
        init_Archive_server(archivePath, archiveName);
        init_EvtWriter_server( (OTF2_LocationRef)0 );
        init_GlobalDefWriter_server();
        fupdate(fp, "Init of otf2 objs complete\n");

        // Server for logging GlobalDefWriter strings&regions
        globalDefWriter_server();
        fupdate(fp, "globalDefWriter_server complete\n");

        // Server listens for events
        fupdate(fp, "evtWriter\n");
        evtWriter_server();
        fupdate(fp, "evtWriter complete\n");

        // Finalization
        finalize_EvtWriter_server();
        globalDefWriter_WriteSystemTreeNode_server(0,0);
        globalDefWriter_WriteLocation_server(0); // WriteLocation must be called at end of program due to NUM_EVENTS
        finalize_GlobalDefWriter_server(); // @TODO: Rename to globalDefWriter_Clock
        finalize_Archive_server();
        finalize_otf2_server();
        fupdate(fp, "COMPLETE!\n");
        if (fp!=NULL){fclose(fp);}
    } else {
        IS_LOGGER = false;
        context = zmq_ctx_new ();
        requester = zmq_socket (context, ZMQ_REQ); // ZMQ_PUSH
        pusher = zmq_socket (context, ZMQ_PUSH);

        // @TODO Error check connect
        zmq_connect (requester, "tcp://localhost:5555");
        zmq_connect (pusher, "tcp://localhost:5556");
    }
    return(R_NilValue);
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
        //fupdate(fp, "globalDefWriter_server start of zmq_recv"); // DEBUGGING
        int zmq_ret = zmq_recv(responder, buffer, LEN*sizeof(*buffer), 0);
        //fupdate(fp, "globalDefWriter_server end of zmq_recv"); // DEBUGGING
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
    //printf("(pid: %d) Finished listening\n", getpid());
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
    char buffer[LEN];
    //buffer = func_name.get_cstring();
    int send_ret = zmq_send(requester, func_name.get_cstring(), LEN*sizeof(char), 0);
    if (send_ret < 0 ) { report_and_exit("client zmq_send"); }
    int recv_ret = zmq_recv(requester, &regionRef, sizeof(regionRef), 0);
    if (recv_ret < 0 ) { report_and_exit("client zmq_recv"); }

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
    zmq_ret = zmq_send(pusher, NULL, 0, 0); // zmq_otf2_event
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
    int rc = zmq_bind (syncer_client, "tcp://*:5557");
    assert (rc == 0); // errno 98 indicates socket alreayd in use

    int zmq_ret = zmq_recv(syncer_client, NULL, 0, 0); // sent from logger/server at end of events
    if (zmq_ret < 0 ) { report_and_exit("finalize_otf2_client"); }

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


//' Close static otf2 {archive, evt_writer} objs
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
//' Initialize static otf2 {evt_writer} objs
void init_EvtWriter_server(OTF2_LocationRef locationRef) {
    if (IS_LOGGER){ // Ensure isn't called from main proc!
        // DEBUGGING: OTF2_Archive_GetEvtWriter throwing error 
        if (archive == NULL) { logger_error("init_EvtWriter_server", NULL); }

        // Get a local event writer for location 0. [ PROC 0? ]
        evt_writer = OTF2_Archive_GetEvtWriter( archive, locationRef );
    }
}


//' Close static otf2 {evt_writer} objs
void finalize_EvtWriter_server() {
    // DEBUGGING
    if (evt_writer == NULL) { logger_error("finalize_EvtWriter_server evt_writer", NULL); }
    if (archive == NULL) { logger_error("finalize_EvtWriter_server archive", NULL); }

    // Now close the event writer, before closing the event files collectively.
    OTF2_EvtWriter_GetNumberOfEvents(evt_writer, &NUM_EVENTS);
    OTF2_Archive_CloseEvtWriter( archive, evt_writer );

    // After we wrote all of the events we close the event files again.
    OTF2_Archive_CloseEvtFiles( archive );
}


// @TODO: zmq this
//' Enable or disable event measurement
//' @param measurementMode True to enable, else disable
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP evtWriter_MeasurementOnOff(bool measurementMode) {
    // Enable/disable measurement
    if (measurementMode){
        OTF2_EvtWriter_MeasurementOnOff(evt_writer, 
                NULL /* attributeList */, 
                get_time(), 
                OTF2_MEASUREMENT_ON);
    } else {
        OTF2_EvtWriter_MeasurementOnOff(evt_writer, 
                NULL /* attributeList */, 
                get_time(), 
                OTF2_MEASUREMENT_OFF);
    }
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


// TODO: Multiproc this! Partial MakeCluster
//' Write the system tree including a definition for the location group to the global definition writer.
//' @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
//' @param stringRef_class Class to be associated with SystemTreeNode (eg node)
void globalDefWriter_WriteSystemTreeNode_server( int stringRef_name, int stringRef_class) {

    // TODO: Get NodeName from syscall
    // Write the system tree incl definition for location group to global definition writer.
    OTF2_StringRef stringRef_SystemTreeNodeName = globalDefWriter_WriteString_server("MyHost");
    OTF2_StringRef stringRef_SystemTreeNodeClass = globalDefWriter_WriteString_server("node");
    OTF2_GlobalDefWriter_WriteSystemTreeNode( global_def_writer,
            0 /* SystemTreeNodeRef id */,
            stringRef_SystemTreeNodeName /* StringRef name */,
            stringRef_SystemTreeNodeClass /* StringRef class */,
            OTF2_UNDEFINED_SYSTEM_TREE_NODE /* parent */ );

    // TODO: Multiproc this! MakeCluster
    // Write LocationGroup (ie proc) information
    OTF2_StringRef stringRef_GroupName = globalDefWriter_WriteString_server("Initial Process");
    OTF2_GlobalDefWriter_WriteLocationGroup( global_def_writer,
            0 /* OTF2_LocationGroupRef  */,
            stringRef_GroupName /* name */,
            OTF2_LOCATION_GROUP_TYPE_PROCESS /* New LocationGroup for each process */, 
            0 /* system tree */,
            OTF2_UNDEFINED_LOCATION_GROUP /* creating process */ );

}

// TODO: Multiproc this (NUM_EVENTS only)
//' Write a definition for the location to the global definition writer.
//' @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
void globalDefWriter_WriteLocation_server( int stringRef_name ) {
    OTF2_StringRef stringRef_LocationName = globalDefWriter_WriteString_server("Main thread");
    
    // Get location id
    //OTF2_LocationRef locationRef;
    //OTF2_GlobalDefWriter_GetLocationID(global_def_writer, &locationRef);

    // Write a definition for the location to the global definition writer.
    Rcout << "globalDefWriter_WriteLocation - Num events: " << NUM_EVENTS << "\n";
    OTF2_GlobalDefWriter_WriteLocation( global_def_writer,
            0 /* id */,
            stringRef_LocationName /* stringRef_name */,
            OTF2_LOCATION_TYPE_CPU_THREAD,
            NUM_EVENTS /* #events */,
            0 /* location group */ );
}


// @TODONE: zmq this
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
    struct zmq_otf2_event buffer;

    // Pack info into struct
    buffer.pid = 0;
    buffer.regionRef = regionRef;
    buffer.time = get_time();
    buffer.event_type = event_type;

    // DEBUGGING 
    #ifdef DEBUG
    Rcout << "evtWriter_Write_client start of zmq_send";
    Rcout << ", regionRef: " << buffer.regionRef;
    Rcout << ", time: " << buffer.time;
    Rcout << ", event_type: " << buffer.event_type;
    Rcout << "\n";
    #endif

    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // zmq_otf2_event
    if (zmq_ret<0){ report_and_exit("zmq_send zmq_otf2_event"); }

    return(R_NilValue);
}


// TODO: multiproc this! evtWriter dependent on LocationGroup/PID
void evtWriter_server(){

    if (evt_writer == NULL) { logger_error("evtWriter_server", NULL); }

    //  Socket to talk to clients
    void *puller = zmq_socket (context, ZMQ_PULL); // ZMQ_PULL
    int rc = zmq_bind (puller, "tcp://*:5556");
    //assert (rc == 0); // errno 98 indicates socket alreayd in use
    if (rc!=0){ logger_error("Binding puller", puller); }

    // DEBUGGING
    //fprintf(fp, "(pid: %d) Listening for tracing events\n", getpid());
    char buffer[50];
    snprintf(buffer, 50, "(pid: %d) Listening for evtWriter\n", getpid());
    fupdate(fp, buffer);

    // Receive globalDef strings, and return with send regionRef
    while (1) {
        struct zmq_otf2_event buffer;
        int zmq_ret;

        zmq_ret = zmq_recv(puller, &buffer, sizeof(buffer), 0); // zmq_otf2_event

        // @TODO: evt_writer for each pid
        if (zmq_ret<0){
            report_and_exit("zmq_recv zmq_otf2_event"); 
        } else if (zmq_ret == 0) { // Signal to stop listening
            break;
        } else {

            // DEBUGGING
            char evt_buffer[100];
            snprintf(evt_buffer, 100, "Server recv event: %d, pid: %d, time: %lu\n", 
                buffer.regionRef, buffer.pid, buffer.time);
            fupdate(fp, evt_buffer);

            // Check type of event for enter or leave
            if (buffer.event_type){
                OTF2_EvtWriter_Enter( evt_writer, NULL /* attributeList */,
                        buffer.time, buffer.regionRef /* region */ );
            }
            else {
                OTF2_EvtWriter_Leave( evt_writer, NULL /* attributeList */,
                        buffer.time, buffer.regionRef /* region */ );
            }
        }
    }
    //fprintf(fp, "(pid: %d) Ending listening for tracing events\n", getpid());
    snprintf(buffer, 50, "(pid: %d) Finished listening for evtWriter\n", getpid());
    fupdate(fp, buffer);

    // Cleanup socket
    zmq_close(puller);
}

//' Write event to evt_writer
//' @param regionRef Region id
//' @param event_type True for enter, False for leave region
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP evtWriter_Write(int regionRef, bool event_type)
{
#ifdef DEBUG
    Rcout << "region: " << regionRef << ", event_type: " << event_type << "\n";
#endif /* ifdef DEBUG */


    // Check type of event for enter or leave
    if (event_type){
        OTF2_EvtWriter_Enter( evt_writer, NULL /* attributeList */,
                get_time(), regionRef /* region */ );
    }
    else {
        OTF2_EvtWriter_Leave( evt_writer, NULL /* attributeList */,
                get_time(), regionRef /* region */ );
    }
    return(R_NilValue);
}

///////////////////////////////
// Helper functions
///////////////////////////////

// @TODO: Replace usage of this with more R-friendly version
void report_and_exit(const char* msg){
    //perror(msg);
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

    pid_t ppid = getppid();
    kill(0, SIGTERM);
}

void fupdate(FILE *fp, const char* msg){
    if (fp!=NULL){
        fprintf(fp, "%s\n", msg);
        fclose(fp);
        fp = fopen(filename, "a");
    }
}

///////////////////////////////
// Testing
///////////////////////////////

//' set_id
//' @param idnew new id
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP set_id(const int idnew) {
    id = idnew;    
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
