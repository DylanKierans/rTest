// @file rTrace.cpp
// @brief Rcpp functions for underlying otf2 library
// @date 2024-01-16
// @version 0.01
// @author D.Kierans (dylanki@kth.se)
// @todo Error checking
// @todo Fix timing offset problems - caused by taking epoch at different time to evtWriter start

#include "Rcpp.h"
#include <otf2/otf2.h>
#include <sys/time.h>

// getpid
#include <sys/types.h>
#include <unistd.h>

// if (MPI)
#include <mpi.h>


//#define DEBUG /* Uncomment to enable verbose debug info */
//#define DUMMY_TIMESTEPS /* Uncomment for 1s timestep for each subsequent event call */

using namespace Rcpp;

static OTF2_Archive* archive;
static OTF2_EvtWriter* evt_writer;
static OTF2_GlobalDefWriter* global_def_writer;
OTF2_TimeStamp epoch_start, epoch_end;  // OTF2_GlobalDefWriter_WriteClockProperties

// Multithreading 
static int MASTER_PID=-1;

// Counters
static uint64_t NUM_EVENTS=0; ///* Number of events recorded for WriteLocation
static uint64_t NUM_STRINGREF=0; ///* Number of events recorded with WriteString
static uint64_t NUM_REGIONREF=0; ///* Number of regions recorded with WriteRegion

// TODO - implement this offset. can still use num_stringref for non-function defns
//  Will make easier for non-Fork process spawning. func_stringRef = func_index + offset_stringref
static uint64_t OFFSET_STRINGREF=100; ///* Number of stringRef offset before starting function names
                                 
// DEBUGGING
static int id;


///////////////////////////////
// Function declrations
///////////////////////////////
RcppExport uint64_t globalDefWriter_WriteString(Rcpp::String stringRefValue);


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


//' Initialize static otf2 {archive} objs
//' @param archivePath Path to the archive i.e. the directory where the anchor file is located.
//' @param archiveName Name of the archive. It is used to generate sub paths e.g. "archiveName.otf2"
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_Archive(Rcpp::String archivePath="./rTrace", Rcpp::String archiveName="rTrace") {

//init_Archive(Rcpp::String archivePath="./rTrace", Rcpp::String archiveName="rTrace", isSerial=true)

    // Should only be done on master
    MASTER_PID=getpid(); 

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
// [[Rcpp::export]]
RcppExport uint64_t globalDefWriter_WriteString(Rcpp::String stringRefValue)
{
    OTF2_GlobalDefWriter_WriteString(global_def_writer, NUM_STRINGREF, stringRefValue.get_cstring() );
    return(NUM_STRINGREF++); // ++ applied after return value!
}


//' globalDefWriter_WriteRegion
//'     Define new region description in global writer
//' @param stringRef_RegionName Name to be associated with region
//' @return regionRef id/index for string
// [[Rcpp::export]]
RcppExport uint64_t globalDefWriter_WriteRegion( int stringRef_RegionName) {
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

    return(NUM_REGIONREF++);
}


//' Write the system tree including a definition for the location group to the global definition writer.
//' @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
//' @param stringRef_class Class to be associated with SystemTreeNode (eg node)
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP globalDefWriter_WriteSystemTreeNode( int stringRef_name, int stringRef_class) {
    uint64_t stringRef_SystemTreeNodeName = globalDefWriter_WriteString("MyHost");
    uint64_t stringRef_SystemTreeNodeClass = globalDefWriter_WriteString("node");
    uint64_t stringRef_GroupName = globalDefWriter_WriteString("Initial Process");

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
    uint64_t stringRef_LocationName = globalDefWriter_WriteString("Main thread");
    
    // Get location id
    //OTF2_LocationRef locationRef;
    //OTF2_GlobalDefWriter_GetLocationID(global_def_writer, &locationRef);

    // Write a definition for the location to the global definition writer.
    Rcout << "globalDefWriter_WriteLocation - Num events: " << NUM_EVENTS << "\n";
    OTF2_GlobalDefWriter_WriteLocation( global_def_writer,
            0 /* id */,
            stringRef_LocationName /* stringRef_name */,
            OTF2_LOCATION_TYPE_CPU_THREAD,
            NUM_EVENTS /* # events */,
            0 /* location group */ );

    return(R_NilValue);
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

//' mpi_init
// [[Rcpp::export]]
RcppExport int mpi_init() {
    int flag;
    int fake_argc = 0;
    char **fake_argv = NULL;
//char **fake_argv = malloc(1*sizeof(*fake_argv));
    MPI_Init(&fake_argc, &fake_argv);
    free(fake_argv);

    MPI_Initialized(&flag);
    if (flag) {
        Rcout << "MPI is initialized.\n";
        return(0);
    }
    Rcout << "MPI UNABLE to initialize.\n";
    return (-1);
}

//' mpi_finalize
// [[Rcpp::export]]
RcppExport SEXP mpi_finalize() {
    MPI_Finalize();
    return(R_NilValue);
}

//' mpi_is_init
// [[Rcpp::export]]
RcppExport int mpi_is_init() {
    int init_flag;
    MPI_Initialized(&init_flag);
    if (init_flag) {
        Rcout << "MPI is initialized.\n";
        return(0);
    }
    Rcout << "MPI is NOT initialized\n";
    return (-1);
}

//' get_mpi_rank
// [[Rcpp::export]]
RcppExport int get_mpi_rank() {
    int rank, init_flag;
    MPI_Initialized(&init_flag);
    if (init_flag) {
        Rcout << "MPI is initialized.\n";
        MPI_Comm_rank(MPI_COMM_WORLD, &rank);
        return(rank);
    }
    Rcout << "MPI is NOT initialized\n";
    return (-1);
}

//' get_mpi_size
// [[Rcpp::export]]
RcppExport int get_mpi_size() {
    int size, init_flag;
    MPI_Initialized(&init_flag);
    if (init_flag) {
        Rcout << "MPI is initialized.\n";
        MPI_Comm_size(MPI_COMM_WORLD, &size);
        return(size);
    }
    Rcout << "MPI is NOT initialized\n";
    return (-1);
}


