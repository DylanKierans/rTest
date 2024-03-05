/**  
 * @brief Design:
 *  1. Main proc (a client) performs initialization 
 *      a. Gets reproduceable function indexes
 *      b. Wrapper contains zmq_send's to logger
 *          - process identifier (pid?)
 *          - func identifier
 *          - func state (enter/exit)
 *          - time of record
 *      c. Spawns otf2 logger process (the server) and socket to bind
 *      d. Connects to socket
 *  2. Should run like so in serial
 *  3. Main proc calls finalize
 *      a. Sends message to server
 *      b. Server cleans up and shuts down (zeromq + otf2)
 *      c. Client cleans up zeromq
 */
#include <zmq.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <otf2.h>

#define LEN 10
#define FtokBackingFile "."
#define AccessPerms 0644

void server(); // Server
pid_t init();
void client();
void finalize(pid_t pid);
void report_and_exit(const char* msg);
void rTrace_send_otf2_event(void *requester, pid_t pid, int func_index, int event_type, OTF2_TimeStamp time);

// Struct packing all info required to otf2 event
struct rTraceOtf2Event {
   int event_type; // 1:=MAIN_PROC, 2:=NEW_PROC, 3:=CLOSE_PROC, 4:=OTF2_ENTER, 5:=OTF2_LEAVE
   pid_t pid; 
   int func_index;
   OTF2_TimeStamp time;
} rTraceOtf2Event;

struct rTraceOtf2WriteString { // CallResponse socket
   char func_name[32];
   // respond with stringRef
} rTraceOtf2WriteString;


int main (void)
{
    pid_t child_pid = init();

    if (child_pid != 0) {
        client();
    } 
    finalize(child_pid);
    return 0;
}


// Init of rTrace library - should fork logger thread
pid_t init(){
    pid_t child_pid = fork();
	printf("Hello world (%d)\n", getpid());

    if ( child_pid == 0 ){ // child
        server();
    }
    return child_pid;
}

void server() { // Server

    //  Socket to talk to clients
    void *context = zmq_ctx_new ();
    void *responder = zmq_socket (context, ZMQ_PULL); // ZMQ_PULL
    int rc = zmq_bind (responder, "tcp://*:5555");
    assert (rc == 0); // errno 98 indicates socket alreayd in use

    printf("(pid: %d) Listening\n", getpid());

    int iter=0; ///< Number of messages received

    // Method 3: Check for zero-length signal
    //  ISSUE: Must be adapted for multiproc, use counter
    while (1) {
		int buffer[LEN];
        int size = zmq_recv (responder, buffer, 1*sizeof(int), 0);
        if ( size < 0 ) { 
            report_and_exit("zmq_recv"); 
        } else if (size == 0) {
            break;
        } else if (size == sizeof(char)) { // TODO - work this out
            //printf("[%d] Received one char %c\n", *((char*)buffer));
            printf("[%d] Received one char `%c`\n", (char) (*buffer));
        //} else if (size == sizeof(rTraceOtf2Event)) {
        } else {
            printf ("[%d] Received: %d\n", iter, buffer[0]);
            //sleep (1);          //  Do some 'work'
            iter++;
        }
    }

    printf("(pid: %d) Finished listening\n", getpid());

    // Clean up and zmq socket
    zmq_close(responder);
    zmq_ctx_destroy(context);
}

void client() { // Client
    void *context = zmq_ctx_new ();
    void *requester = zmq_socket (context, ZMQ_PUSH); // ZMQ_PUSH
    zmq_connect (requester, "tcp://localhost:5555");

    for (int request_nbr = 0; request_nbr != 10; request_nbr++) {
		int buffer [LEN];
        buffer[request_nbr] = request_nbr+LEN;
        printf ("Sending %d…%d\n", request_nbr, buffer[request_nbr]);

        int send_ret = zmq_send (requester, &buffer[request_nbr], 1*sizeof(int), 0);
        if (send_ret < 0 ) { report_and_exit("zmq_send"); }
        //sleep (1);          //  Do some 'work'
    }

    int send_ret;

    // Test sending just one char (smaller than int)
    char foo = 'a';
    send_ret = zmq_send (requester, &foo, sizeof(foo), 0);
    if (send_ret < 0 ) { report_and_exit("zmq_send char"); }

    // Signal end with length 0 message
    send_ret = zmq_send (requester, NULL, 0, 0); 
    if (send_ret < 0 ) { report_and_exit("zmq_send 0 length"); }

    zmq_close (requester);
    zmq_ctx_destroy (context);

	printf("Closing client/parent\n");
}

void finalize(pid_t child_pid){ // Rejoin fork
    if (child_pid != 0) { // parent
		printf("Parent finalizing\n");
    } else { 
		printf("Child finalizing\n"); 
	}
    //kill(child_pid, SIGTERM);
	int wstatus;
    waitpid(-1, &wstatus, 0);
}

void report_and_exit(const char* msg){
    perror(msg);
    exit(-1);
}

void rTrace_send_otf2_event(void *requester, pid_t pid, int func_index, int event_type, OTF2_TimeStamp time){
    struct rTraceOtf2Event payload;
    payload.pid = pid;
    payload.func_index = func_index;
    payload.event_type = event_type;
    payload.time = time;

    int send_ret = zmq_send(requester, &payload, sizeof(payload), 0);
    if (send_ret < 0 ) { report_and_exit("zmq_send payload"); }
}
