
#' test_ports
#' @param port_type 1:=req-rep, 2:=push-pull, 3:=pub-sub, else error
#' @param ports List of ports to test on valid range \[1024,49151\], default {5554,5555,5556,5557,5559}
#' @param num_messages Number of messages to send
#' @param message_size Size of each msg in KB
#' @export
test_ports <- function(port_type, 
        ports=c(5554, 5555, 5556, 5557, 5559),
        num_messages=1000, message_size=8) 
{
    ret <- test__ports(port_type, ports=ports, 
            num_messages=num_messages, 
            message_size=message_size)

    print(paste0("Ret: ", ret))
    if (ret>0){ # Forked child 
        quit("no") 
    }
    ret
}