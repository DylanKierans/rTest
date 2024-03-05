// @file rTrace.cpp
// @brief Rcpp functions for underlying otf2 library
// @date 2024-01-16
// @version 0.01
// @author D.Kierans (dylanki@kth.se)
// @todo Error checking
// @todo Fix timing offset problems - caused by taking epoch at different time to evtWriter start
// @todo Fix timing coming from other proc

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


///////////////////////////////
// Function declrations
///////////////////////////////
OTF2_StringRef globalDefWriter_WriteString(Rcpp::String stringRefValue);
OTF2_RegionRef globalDefWriter_WriteRegion(OTF2_StringRef stringRef_RegionName);
RcppExport SEXP init_Archive(Rcpp::String, Rcpp::String);
RcppExport SEXP init_EvtWriter();
RcppExport SEXP init_GlobalDefWriter();
RcppExport SEXP finalize_GlobalDefWriter();
RcppExport SEXP finalize_EvtWriter();
void evtWriter_server();
RcppExport SEXP globalDefWriter_WriteSystemTreeNode(int, int);
RcppExport SEXP globalDefWriter_WriteLocation(int);
RcppExport SEXP finalize_Archive();
RcppExport SEXP finalize_EvtWriter_client();
void globalDefWriter_server();

void report_and_exit(const char* msg);


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
    if (child_pid == 0){
        IS_LOGGER = true;

        // OTF2 Objs
        Rcpp::String archivePath = "./rTrace";
        Rcpp::String archiveName = "rTrace";
        init_Archive(archivePath, archiveName);
        init_EvtWriter();
        init_GlobalDefWriter();

        // Server for logging GlobalDefWriter strings&regions
        globalDefWriter_server();

        // Server listens for events
        evtWriter_server();

        // Finalization
        finalize_EvtWriter();
        globalDefWriter_WriteSystemTreeNode(0,0);
        globalDefWriter_WriteLocation(0); // WriteLocation must be called at end of program due to NUM_EVENTS
        finalize_GlobalDefWriter(); // @TODO: Rename to globalDefWriter_Clock
        finalize_Archive();
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
    assert (rc == 0); // errno 98 indicates socket alreayd in use

    //printf("(pid: %d) Listening\n", getpid());

    int iter=0; ///< Number of messages received

    // Receive globalDef strings, and return with send regionRef
    while (1) {
		char buffer[LEN];
        int zmq_ret = zmq_recv(responder, buffer, LEN*sizeof(*buffer), 0);
        if ( zmq_ret < 0 ) { 
            report_and_exit("zmq_recv"); 
        } else if (zmq_ret == 0) {
            break; // Signal end of globalDef from client
        } else {
            buffer [zmq_ret] = '\0';
            //printf ("[%d] Received: %s\n", iter, buffer);
            // Define as stringRef, regionRef
            OTF2_StringRef stringRef = globalDefWriter_WriteString(buffer);
            OTF2_RegionRef regionRef = globalDefWriter_WriteRegion(stringRef);

            // DEBUGGING
            //Rcout << "Server - func_name: "<< buffer << ", regionRef:" << regionRef << std::endl;

            // Return regionRef ID
            zmq_ret = zmq_send (responder, &regionRef, sizeof(regionRef), 0);
            if (zmq_ret<0) { report_and_exit("server zmq_send"); }
            iter++;
        }
    }

    //printf("(pid: %d) Finished listening\n", getpid());
    zmq_close(responder);
}


//' define_otf2_event_client
//' @param func_name Name of function to create event for
//' @return regionRef regionRef for use when logging events
// [[Rcpp::export]]
RcppExport int define_otf2_event_client(Rcpp::String func_name) {
    OTF2_RegionRef regionRef;
    char buffer[LEN];
    //buffer = func_name.get_cstring();
    int send_ret = zmq_send(requester, func_name.get_cstring(), LEN*sizeof(*buffer), 0);
    if (send_ret < 0 ) { report_and_exit("client zmq_send"); }
    int recv_ret = zmq_recv(requester, &regionRef, sizeof(regionRef), 0);
    if (recv_ret < 0 ) { report_and_exit("client zmq_recv"); }

    // DEBUGGING
    //Rcout << "func_name: " << func_name.get_cstring() << std::endl;
    //Rcout << "regionRef: " << regionRef << std::endl;
    return (int)regionRef;
}

//' finalize_EvtWriter_client
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_EvtWriter_client() {
    int zmq_ret;
    zmq_ret = zmq_send(pusher, NULL, 0, 0); // zmq_otf2_event
    if (zmq_ret < 0 ) { report_and_exit("client finalize_EvtWriter_client"); }

    // Wait for fork to end?
    int w;
    wait(&w); 

    return(R_NilValue);
}


//////////////////////////////////////
// END OF TODO
//////////////////////////////////////


//' Initialize static otf2 {archive} objs
//' @param archivePath Path to the archive i.e. the directory where the anchor file is located.
//' @param archiveName Name of the archive. It is used to generate sub paths e.g. "archiveName.otf2"
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_Archive(Rcpp::String archivePath="./rTrace", Rcpp::String archiveName="rTrace") 
{

    archive = OTF2_Archive_Open( archivePath.get_cstring(),
                                               archiveName.get_cstring(),
                                               OTF2_FILEMODE_WRITE,
                                               1024 * 1024 /* event chunk size */,
                                               4 * 1024 * 1024 /* def chunk size*/,
                                               OTF2_SUBSTRATE_POSIX,
                                               OTF2_COMPRESSION_NONE );

    // Set the previously defined flush callbacks.
    OTF2_Archive_SetFlushCallbacks( archive, &flush_callbacks, NULL );

    // We will operate in a serial context.
    OTF2_Archive_SetSerialCollectiveCallbacks( archive );

    // Now we can create the event files. Though physical files aren't created yet.
    OTF2_Archive_OpenEvtFiles( archive );
    return(R_NilValue);
}


//' Close static otf2 {archive, evt_writer} objs
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_Archive() {
    // At the end, close the archive and exit.
    OTF2_Archive_Close( archive );

    // Reset counters
    NUM_STRINGREF = 0;
    NUM_REGIONREF = 0;

    return(R_NilValue);
}




//' Initialize static otf2 {evt_writer} objs
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_EvtWriter() {
    // Get a local event writer for location 0. [ PROC 0? ]
    evt_writer = OTF2_Archive_GetEvtWriter( archive, 0 /* LocationRef */ );

    return(R_NilValue);
}


//' Close static otf2 {evt_writer} objs
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_EvtWriter() {
    // Now close the event writer, before closing the event files collectively.
    OTF2_EvtWriter_GetNumberOfEvents(evt_writer, &NUM_EVENTS);
    OTF2_Archive_CloseEvtWriter( archive, evt_writer );

    // After we wrote all of the events we close the event files again.
    OTF2_Archive_CloseEvtFiles( archive );
    return(R_NilValue);
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
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_GlobalDefWriter() {
    epoch_start = get_time(); // Get init time for finalize

    // Now write the global definitions by getting a writer object for it.
    global_def_writer = OTF2_Archive_GetGlobalDefWriter( archive );

    // Define empty string in first stringRef value
    Rcpp::String stringRefValue="";
    globalDefWriter_WriteString(stringRefValue);

    return(R_NilValue);
}

//' Finalize static otf2 {globaldefwriter} obj
//'     Write clock information before ending tracing
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_GlobalDefWriter() {
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
    return(R_NilValue);
}


//' Define new id-value pair in globaldefwriter
//' @param stringRefValue String assigned to given id
//' @return NUM_STRINGREF 
OTF2_StringRef globalDefWriter_WriteString(Rcpp::String stringRefValue)
{
    OTF2_GlobalDefWriter_WriteString(global_def_writer, NUM_STRINGREF, stringRefValue.get_cstring() );
    return(NUM_STRINGREF++); // ++ applied after return value!
}


//' globalDefWriter_WriteRegion
//'     Define new region description in global writer
//' @param stringRef_RegionName Name to be associated with region
//' @return regionRef id/index for string
OTF2_RegionRef globalDefWriter_WriteRegion(OTF2_StringRef stringRef_RegionName) {
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


//' Write the system tree including a definition for the location group to the global definition writer.
//' @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
//' @param stringRef_class Class to be associated with SystemTreeNode (eg node)
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP globalDefWriter_WriteSystemTreeNode( int stringRef_name, int stringRef_class) {
    OTF2_StringRef stringRef_SystemTreeNodeName = globalDefWriter_WriteString("MyHost");
    OTF2_StringRef stringRef_SystemTreeNodeClass = globalDefWriter_WriteString("node");
    OTF2_StringRef stringRef_GroupName = globalDefWriter_WriteString("Initial Process");

    // Write the system tree incl definition for location group to global definition writer.
    OTF2_GlobalDefWriter_WriteSystemTreeNode( global_def_writer,
            0 /* SystemTreeNodeRef id */,
            stringRef_SystemTreeNodeName /* StringRef name */,
            stringRef_SystemTreeNodeClass /* StringRef class */,
            OTF2_UNDEFINED_SYSTEM_TREE_NODE /* parent */ );
    OTF2_GlobalDefWriter_WriteLocationGroup( global_def_writer,
            0 /* id */,
            stringRef_GroupName /* name */,
            OTF2_LOCATION_GROUP_TYPE_PROCESS,
            0 /* system tree */,
            OTF2_UNDEFINED_LOCATION_GROUP /* creating process */ );

    return(R_NilValue);
}

//' Write a definition for the location to the global definition writer.
//' @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP globalDefWriter_WriteLocation( int stringRef_name ) {
    OTF2_StringRef stringRef_LocationName = globalDefWriter_WriteString("Main thread");
    
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

    return(R_NilValue);
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

    zmq_ret = zmq_send(pusher, &buffer, sizeof(buffer), 0); // zmq_otf2_event
    if (zmq_ret<0){ report_and_exit("zmq_send zmq_otf2_event"); }

    return(R_NilValue);
}


void evtWriter_server(){
    //  Socket to talk to clients
    void *puller = zmq_socket (context, ZMQ_REP); // ZMQ_PULL
    int rc = zmq_bind (puller, "tcp://*:5556");
    assert (rc == 0); // errno 98 indicates socket alreayd in use

    //printf("(pid: %d) Listening for tracing events\n", getpid());

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
    //printf("(pid: %d) Ending listening for tracing events\n", getpid());
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
