// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// init_otf2_logger
RcppExport int init_otf2_logger(int max_nprocs, Rcpp::String archivePath, Rcpp::String archiveName, Rcpp::NumericVector ports, bool flag_print_pids);
RcppExport SEXP _rTrace_init_otf2_logger(SEXP max_nprocsSEXP, SEXP archivePathSEXP, SEXP archiveNameSEXP, SEXP portsSEXP, SEXP flag_print_pidsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type max_nprocs(max_nprocsSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type archivePath(archivePathSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type archiveName(archiveNameSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ports(portsSEXP);
    Rcpp::traits::input_parameter< bool >::type flag_print_pids(flag_print_pidsSEXP);
    rcpp_result_gen = Rcpp::wrap(init_otf2_logger(max_nprocs, archivePath, archiveName, ports, flag_print_pids));
    return rcpp_result_gen;
END_RCPP
}
// assign_regionRef_array_master
RcppExport SEXP assign_regionRef_array_master(int num_funcs);
RcppExport SEXP _rTrace_assign_regionRef_array_master(SEXP num_funcsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type num_funcs(num_funcsSEXP);
    rcpp_result_gen = Rcpp::wrap(assign_regionRef_array_master(num_funcs));
    return rcpp_result_gen;
END_RCPP
}
// finalize_GlobalDefWriter_client
RcppExport SEXP finalize_GlobalDefWriter_client();
RcppExport SEXP _rTrace_finalize_GlobalDefWriter_client() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(finalize_GlobalDefWriter_client());
    return rcpp_result_gen;
END_RCPP
}
// define_otf2_regionRef_client
RcppExport int define_otf2_regionRef_client(Rcpp::String func_name, int func_index);
RcppExport SEXP _rTrace_define_otf2_regionRef_client(SEXP func_nameSEXP, SEXP func_indexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::String >::type func_name(func_nameSEXP);
    Rcpp::traits::input_parameter< int >::type func_index(func_indexSEXP);
    rcpp_result_gen = Rcpp::wrap(define_otf2_regionRef_client(func_name, func_index));
    return rcpp_result_gen;
END_RCPP
}
// finalize_EvtWriter_client
RcppExport SEXP finalize_EvtWriter_client();
RcppExport SEXP _rTrace_finalize_EvtWriter_client() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(finalize_EvtWriter_client());
    return rcpp_result_gen;
END_RCPP
}
// finalize_sync_client
RcppExport SEXP finalize_sync_client();
RcppExport SEXP _rTrace_finalize_sync_client() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(finalize_sync_client());
    return rcpp_result_gen;
END_RCPP
}
// init_zmq_client
RcppExport SEXP init_zmq_client();
RcppExport SEXP _rTrace_init_zmq_client() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(init_zmq_client());
    return rcpp_result_gen;
END_RCPP
}
// finalize_zmq_client
RcppExport SEXP finalize_zmq_client();
RcppExport SEXP _rTrace_finalize_zmq_client() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(finalize_zmq_client());
    return rcpp_result_gen;
END_RCPP
}
// evtWriter_MeasurementOnOff_client
RcppExport SEXP evtWriter_MeasurementOnOff_client(bool measurementMode);
RcppExport SEXP _rTrace_evtWriter_MeasurementOnOff_client(SEXP measurementModeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type measurementMode(measurementModeSEXP);
    rcpp_result_gen = Rcpp::wrap(evtWriter_MeasurementOnOff_client(measurementMode));
    return rcpp_result_gen;
END_RCPP
}
// epoch_time_client
RcppExport SEXP epoch_time_client();
RcppExport SEXP _rTrace_epoch_time_client() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(epoch_time_client());
    return rcpp_result_gen;
END_RCPP
}
// evtWriter_Write_client
RcppExport SEXP evtWriter_Write_client(int func_index, bool event_type);
RcppExport SEXP _rTrace_evtWriter_Write_client(SEXP func_indexSEXP, SEXP event_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type func_index(func_indexSEXP);
    Rcpp::traits::input_parameter< bool >::type event_type(event_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(evtWriter_Write_client(func_index, event_type));
    return rcpp_result_gen;
END_RCPP
}
// set_locationRef
RcppExport SEXP set_locationRef(const int id);
RcppExport SEXP _rTrace_set_locationRef(SEXP idSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const int >::type id(idSEXP);
    rcpp_result_gen = Rcpp::wrap(set_locationRef(id));
    return rcpp_result_gen;
END_RCPP
}
// get_locationRef
RcppExport int get_locationRef();
RcppExport SEXP _rTrace_get_locationRef() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_locationRef());
    return rcpp_result_gen;
END_RCPP
}
// set_maxUsedLocationRef_client
RcppExport int set_maxUsedLocationRef_client(int nprocs);
RcppExport SEXP _rTrace_set_maxUsedLocationRef_client(SEXP nprocsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type nprocs(nprocsSEXP);
    rcpp_result_gen = Rcpp::wrap(set_maxUsedLocationRef_client(nprocs));
    return rcpp_result_gen;
END_RCPP
}
// print_errnos
RcppExport SEXP print_errnos();
RcppExport SEXP _rTrace_print_errnos() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(print_errnos());
    return rcpp_result_gen;
END_RCPP
}
// otf2_handle_proc
RcppExport SEXP otf2_handle_proc(bool is_init);
RcppExport SEXP _rTrace_otf2_handle_proc(SEXP is_initSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< bool >::type is_init(is_initSEXP);
    rcpp_result_gen = Rcpp::wrap(otf2_handle_proc(is_init));
    return rcpp_result_gen;
END_RCPP
}
// test__struct_size
RcppExport int test__struct_size();
RcppExport SEXP _rTrace_test__struct_size() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(test__struct_size());
    return rcpp_result_gen;
END_RCPP
}
// test__ports
RcppExport int test__ports(int port_type, Rcpp::NumericVector ports, int num_messages, size_t message_size);
RcppExport SEXP _rTrace_test__ports(SEXP port_typeSEXP, SEXP portsSEXP, SEXP num_messagesSEXP, SEXP message_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type port_type(port_typeSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ports(portsSEXP);
    Rcpp::traits::input_parameter< int >::type num_messages(num_messagesSEXP);
    Rcpp::traits::input_parameter< size_t >::type message_size(message_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(test__ports(port_type, ports, num_messages, message_size));
    return rcpp_result_gen;
END_RCPP
}
// get_pid
RcppExport int get_pid();
RcppExport SEXP _rTrace_get_pid() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_pid());
    return rcpp_result_gen;
END_RCPP
}
// get_tid
RcppExport int get_tid();
RcppExport SEXP _rTrace_get_tid() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_tid());
    return rcpp_result_gen;
END_RCPP
}
// get_ppid
RcppExport int get_ppid();
RcppExport SEXP _rTrace_get_ppid() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_ppid());
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rTrace_init_otf2_logger", (DL_FUNC) &_rTrace_init_otf2_logger, 5},
    {"_rTrace_assign_regionRef_array_master", (DL_FUNC) &_rTrace_assign_regionRef_array_master, 1},
    {"_rTrace_finalize_GlobalDefWriter_client", (DL_FUNC) &_rTrace_finalize_GlobalDefWriter_client, 0},
    {"_rTrace_define_otf2_regionRef_client", (DL_FUNC) &_rTrace_define_otf2_regionRef_client, 2},
    {"_rTrace_finalize_EvtWriter_client", (DL_FUNC) &_rTrace_finalize_EvtWriter_client, 0},
    {"_rTrace_finalize_sync_client", (DL_FUNC) &_rTrace_finalize_sync_client, 0},
    {"_rTrace_init_zmq_client", (DL_FUNC) &_rTrace_init_zmq_client, 0},
    {"_rTrace_finalize_zmq_client", (DL_FUNC) &_rTrace_finalize_zmq_client, 0},
    {"_rTrace_evtWriter_MeasurementOnOff_client", (DL_FUNC) &_rTrace_evtWriter_MeasurementOnOff_client, 1},
    {"_rTrace_epoch_time_client", (DL_FUNC) &_rTrace_epoch_time_client, 0},
    {"_rTrace_evtWriter_Write_client", (DL_FUNC) &_rTrace_evtWriter_Write_client, 2},
    {"_rTrace_set_locationRef", (DL_FUNC) &_rTrace_set_locationRef, 1},
    {"_rTrace_get_locationRef", (DL_FUNC) &_rTrace_get_locationRef, 0},
    {"_rTrace_set_maxUsedLocationRef_client", (DL_FUNC) &_rTrace_set_maxUsedLocationRef_client, 1},
    {"_rTrace_print_errnos", (DL_FUNC) &_rTrace_print_errnos, 0},
    {"_rTrace_otf2_handle_proc", (DL_FUNC) &_rTrace_otf2_handle_proc, 1},
    {"_rTrace_test__struct_size", (DL_FUNC) &_rTrace_test__struct_size, 0},
    {"_rTrace_test__ports", (DL_FUNC) &_rTrace_test__ports, 4},
    {"_rTrace_get_pid", (DL_FUNC) &_rTrace_get_pid, 0},
    {"_rTrace_get_tid", (DL_FUNC) &_rTrace_get_tid, 0},
    {"_rTrace_get_ppid", (DL_FUNC) &_rTrace_get_ppid, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_rTrace(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
