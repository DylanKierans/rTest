# r_event_instrumentation_loggers.R
# .GlobalEnv vars: PROFILE_EVENTLOG, PROFILE_EVENTLOG_NROWS, PROFILE_INSTRUMENTATION_DF

#######################################################################
# section - event log
#######################################################################

#' profile_create_eventlog
#'  See return
#' @return Dataframe for event logging
#' @export
profile_create_eventlog <- function() {
    ## Sections in eventlog - region, event_type, time
    region <- integer()
    event_type <- logical() #' 1 for enter, 0 for exit
    timestamp <- numeric()

    df <- data.frame( region=region, event_type=event_type, timestamp=timestamp )

    ## DEBUGGING - Check datatypes
    #print(class(region))
    #print(class(event_type))
    #print(sapply(df, class))

    pkg.env$PROFILE_EVENTLOG_NROWS <- 0
    pkg.env$PROFILE_EVENTLOG <- df
    df # Kept for compatability after change
}

#' profile_event_create
#'  Create new start or end event in global eventlog
#' @param region Integer - Index of event/function in INSTRUMENTATION_DF
#' @param event_type Boolean - TRUE for start, FALSE for end event
#' @param timestamp numeric - Time of event occurence
profile_event_create <- function(region, event_type, timestamp) {
    new_row <- list(as.integer(region), event_type, timestamp)

    ## DEBUGGING - Check datatypes
    #print(sapply(new_row, class))
 
    pkg.env$PROFILE_EVENTLOG[pkg.env$PROFILE_EVENTLOG_NROWS+1,] <- new_row
    pkg.env$PROFILE_EVENTLOG_NROWS <- pkg.env$PROFILE_EVENTLOG_NROWS + 1
}

#' profile_reset_event_log
#'  Assign new eventlog dataframe 
#' @return Dataframe for eventlog
profile_reset_eventlog <- function() {
    #PROFILE_EVENTLOG <<- profile_create_eventlog()
    pkg.env$PROFILE_EVENTLOG <- profile_create_eventlog()
}


#' profile_print_eventlog
#'  Print global event log
#' @param df - Dataframe object for eventlog
profile_print_eventlog <- function(df=pkg.env$PROFILE_EVENTLOG) {
    print(df)
}


#' profile_save_eventlog
#'  Save event log to file
#' @param filename String - Name of file to save eventlog to, .csv
#' @param df - Dataframe object for eventlog
#' @param flag_debug Boolean - True to enable debug statements
profile_save_eventlog <- function(filename, df=pkg.env$PROFILE_EVENTLOG, flag_debug=FALSE) {
    print(paste0("############## SAVING TO FILE: ", filename, "###############"))
    utils::write.csv(df, filename, row.names=FALSE)
    print("############## SAVED TO FILE ###############")
}


#######################################################################
# section - instrumentation log
#######################################################################

# BASE VALS: package:function, ncalls, total_time
# EXTRA VALS: min time, max time, call stack

#' profile_create_dataframe
#'  See return
#' @param flag_debug Boolean - Enabled debug state,emts
#' @return Dataframe containing each function with information on function name,
#'      package, count of function calls, total time spent in function
#' @export
profile_create_dataframe <- function(flag_debug=FALSE) {
    packages <- .packages()
    num_functions_per_package <- profile_total_num_functions()
    num_functions_total <- sum(num_functions_per_package)

    ## Sections in dataframe
    function_names <- names(profile_get_function_list())
    package_list <- array(,num_functions_total) 
    count <- integer(num_functions_total)
    total_time <- numeric(num_functions_total)
    instrumented <- logical(num_functions_total)

    ## DEBUGGING:
    if (flag_debug) {
        print("################ DATAFRAME #################")
        print(length(function_names))
        print(num_functions_total)
        print(num_functions_per_package)
        print(package_list)
    }

    # package each entry in function_names belongs to
    index <- 0
    for (i in 1:length(packages)) {
        if (num_functions_per_package[i] > 0 ) {
            index_end <- index+num_functions_per_package[i]
            package_list[index:index_end] <- packages[i]
            index <- index_end
        }
    }

    # Init count and time arrays
    for (i in 1:num_functions_total) {
        count[i] <- 0L
        total_time[i] <- 0.0
        instrumented[i] <- FALSE
    }

    data.frame( packages=package_list, functions=function_names, function_instrumented=instrumented,
               function_count=count, function_time=total_time )
}

#' profile_reset_dataframe
#'  Reset function_count and function_time info to zero
#' @param df Dataframe - Created by profile_create_dataframe() for collecting tracing info
#' @export
profile_reset_dataframe <- function(df) {
    num_functions <- length(df[["functions"]])
    for ( i in 1:num_functions ){
        df[["function_count"]][i] <- 0
        df[["function_time"]][i] <- 0
    }
}

#' profile_reduce_dataframe
#'  Reset function_count and function_time info to zero
#' @param df Dataframe - Created by profile_create_dataframe() for collecting tracing info
#' @export
profile_reduce_dataframe <- function(df) {
    num_functions <- length(df[["functions"]])
    non_zero_count_indices <- ( df[["function_count"]] != 0 )
    reduced_df <- df[non_zero_count_indices,]
    reduced_df
}

#' profile_save_dataframe
#'  Write dataframe to file (preferably reduced)
#' @param df Dataframe - Created by profile_create_dataframe() for collecting tracing info
#' @param filename String - Filename to save output to
#' @export
profile_save_dataframe <- function(df, filename) {
    utils::write.table(df,filename,sep=",",row.names=FALSE)
}

#' profile_print_instrumentation
#'  Print table of reduce dataframe
#' @param flag_debug Boolean - Enable debug header
#' @export
profile_print_instrumentation <- function(flag_debug=TRUE) {
    reduced_df <- profile_reduce_dataframe(pkg.env$PROFILE_INSTRUMENTATION_DF)
    if (flag_debug) {
        print("########### INSTRUMENTATION_DATAFRAME ############")
    }
    print(reduced_df)
}

