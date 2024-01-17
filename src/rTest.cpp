//' @file rTest.cpp
//' @brief Rcpp functions for underlying otf2 library
//' @date 2024-01-16
//' @version 0.01
//' @author D.Kierans (dylanki@kth.se)
//' @todo Error checking

#include "Rcpp.h"
#include <otf2/otf2.h>
// #include <otf2.h>

#define DEBUG

using namespace Rcpp;

static OTF2_Archive* archive;
static OTF2_EvtWriter* evt_writer;
static OTF2_GlobalDefWriter* global_def_writer;

// Counters
static uint64_t NUM_EVENTS=0; ///* Number of events recorded for WriteLocation
static uint64_t NUM_STRINGREF=0; ///* Number of events recorded for WriteLocation


///////////////////////////////
// Function declrations
///////////////////////////////
RcppExport uint64_t globalDefWriter_WriteString(int stringRef, Rcpp::String stringRefValue);


///////////////////////////////
// Function definitions
///////////////////////////////

//' @brief Simple hello world function streaming to Rcout
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP helloWorld(){
    Rcout << "Hello World!\n";
	return (R_NilValue);
}

/////
// OTF2 Functions
/////


//' @brief This example uses a function delivering dummy timestamps
//' @return sequence - Next integer in sequence
static OTF2_TimeStamp get_time() {
    static uint64_t sequence;
#ifdef DEBUG
    Rcout << "time: " << sequence+1 << "\n";
#endif /* ifdef DEBUG */
    return sequence++;
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


//' @brief Initialize static otf2 {archive} objs
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_Archive() {
    //static OTF2_Archive* archive = OTF2_Archive_Open( "ArchivePath",
    archive = OTF2_Archive_Open( "ArchivePath",
                                               "ArchiveName",
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


//' @brief Close static otf2 {archive, evt_writer} objs
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP finalize_Archive() {
    // At the end, close the archive and exit.
    OTF2_Archive_Close( archive );
    return(R_NilValue);
}




//' @brief Initialize static otf2 {evt_writer} objs
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_EvtWriter() {
    // Get a local event writer for location 0. [ PROC 0? ]
    //static OTF2_EvtWriter* evt_writer = OTF2_Archive_GetEvtWriter( archive, 0 );
    evt_writer = OTF2_Archive_GetEvtWriter( archive, 0 );
    return(R_NilValue);
}


//' @brief Close static otf2 {evt_writer} objs
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

//' @brief Init static otf2 {globaldefwriter} obj
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP init_GlobalDefWriter() {
    // Now write the global definitions by getting a writer object for it.
    global_def_writer = OTF2_Archive_GetGlobalDefWriter( archive );

    // We need to define the clock used for this trace and the overall timestamp range.
    OTF2_GlobalDefWriter_WriteClockProperties( global_def_writer,
            1 /* 1 tick per second */,
            0 /* epoch */,
            1 /* length - globalOffset A timestamp smaller than all event timestamps. */,
            OTF2_UNDEFINED_TIMESTAMP );

    //OTF2_GlobalDefWriter_WriteString( global_def_writer, NUM_STRINGREF, "");
    Rcpp::String stringRefValue="";
    globalDefWriter_WriteString(NUM_STRINGREF, stringRefValue);

    // DEBUGGING
    //uint64_t tmp = globalDefWriter_WriteString(NUM_STRINGREF, stringRefValue);
    //Rcout << "tmp: " << tmp << "\n";


    return(R_NilValue);
}

//' @brief Define new id-value pair in globaldefwriter
//' @param stringRef id/index for stringRef
//' @param stringRefValue String assigned to given id
//' @return NUM_STRINGREF 
// [[Rcpp::export]]
RcppExport uint64_t globalDefWriter_WriteString(int stringRef, Rcpp::String stringRefValue)
{
    OTF2_GlobalDefWriter_WriteString( global_def_writer, NUM_STRINGREF, stringRefValue.get_cstring() );
    return(NUM_STRINGREF++); // ++ applied after return value!
}


//' @brief Define new region description in global writer
//' @param regionRef id/index for string
//' @param stringRef_name Name to be associated with region
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP globalDefWriter_WriteRegion( int regionRef, int stringRef_name) {
    OTF2_GlobalDefWriter_WriteRegion( global_def_writer,
            regionRef /* RegionRef */,
            stringRef_name /* region name - stringRef */,
            0 /* alternative name */,
            0 /* description */,
            OTF2_REGION_ROLE_FUNCTION,
            OTF2_PARADIGM_USER,
            OTF2_REGION_FLAG_NONE,
            0 /* source file */,
            0 /* begin lno */, 
            0 /* end lno */ );

    return(R_NilValue);
}


//' @brief Write the system tree including a definition for the location group to the global definition writer.
//' @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
//' @param stringRef_class Class to be associated with SystemTreeNode (eg node)
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP globalDefWriter_WriteSystemTreeNode( int stringRef_name, int stringRef_class) {
    OTF2_GlobalDefWriter_WriteString( global_def_writer, 6, "MyHost" );
    OTF2_GlobalDefWriter_WriteString( global_def_writer, 7, "node" );
    OTF2_GlobalDefWriter_WriteString( global_def_writer, 8, "Initial Process" );

    // Write the system tree incl definition for location group to global definition writer.
    OTF2_GlobalDefWriter_WriteSystemTreeNode( global_def_writer,
            0 /* SystemTreeNodeRef id */,
            6 /* StringRef name */,
            7 /* StringRef class */,
            OTF2_UNDEFINED_SYSTEM_TREE_NODE /* parent */ );
    OTF2_GlobalDefWriter_WriteLocationGroup( global_def_writer,
            0 /* id */,
            8 /* name */,
            OTF2_LOCATION_GROUP_TYPE_PROCESS,
            0 /* system tree */,
            OTF2_UNDEFINED_LOCATION_GROUP /* creating process */ );
    return(R_NilValue);
}

//' @brief Write a definition for the location to the global definition writer.
//' @param stringRef_name Name to be associated with SystemTreeNode (eg MyHost)
//' @return R_NilValue
// [[Rcpp::export]]
RcppExport SEXP globalDefWriter_WriteLocation( int stringRef_name ) {
    OTF2_GlobalDefWriter_WriteString( global_def_writer, 9, "Main thread" );
    
    // Get location id
    //OTF2_LocationRef locationRef;
    //OTF2_GlobalDefWriter_GetLocationID(global_def_writer, &locationRef);

    // Write a definition for the location to the global definition writer.
    OTF2_GlobalDefWriter_WriteLocation( global_def_writer,
            0 /* id */,
            9 /* stringRef_name */,
            OTF2_LOCATION_TYPE_CPU_THREAD,
            NUM_EVENTS /* # events */,
            0 /* location group */ );

    return(R_NilValue);
}



//' @brief Write event to evt_writer
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


