void *context;
void *regionRef_socket_client;
void *regionRef_socket_server;
int *regionReg_array;

//int *regionReg_array = malloc(num_func * sizeof(*region_ref_vs_func_index));
//free(regionReg_array);

// Confirm num_functions, then send all regionRef in order
void open_otf2_regionRef_sockets_server(){
    context = zmq_ctx_new ();
    regionRef_socket_server = zmq_socket (context, ZMQ_PUSH);   
    int rc = zmq_bind (regionRef_socket_server, "tcp://*:5558");

    int zmq_ret = zmq_send(regionRef_socket_server, regionReg_array, num_functions*sizeof(*region_ref_vs_func_index));
}

RcppExport SEXP open_otf2_regionRef_sockets_clients(){
    context = zmq_ctx_new ();
    regionRef_socket_client = zmq_socket (context, ZMQ_PULL);
    zmq_bind (regionRef_socket_client, "tcp://localhost:5558");

    int zmq_ret = zmq_recv();
}

RcppExport SEXP assign_regionRef_array_client(int num_function_ptrs){
    regionReg_array = malloc(num_function_ptrs*sizeof(*regionRef_array));
    return(R_NilValue);
}

RcppExport SEXP get_regionRef_from_array_client(int func_index){
    return(regionRef_array[func_index]);
}

RcppExport SEXP free_regionRef_array_client(){
    free(regionReg_array);
    return(R_NilValue);
}