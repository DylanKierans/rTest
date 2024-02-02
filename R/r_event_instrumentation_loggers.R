# r_event_instrumentation_loggers.R
# .GlobalEnv vars: PROFILE_EVENTLOG, PROFILE_EVENTLOG_NROWS, PROFILE_INSTRUMENTATION_DF

#######################################################################
# section - event log
#######################################################################

#' create_eventlog
#' @description See return
#' @return Dataframe for event logging
#' @export
create_eventlog <- function() {
    ## Sections in eventlog - region, event_type, time
    region <- integer()
    event_type <- logical() #' 1 for enter, 0 for exit
    timestamp <- numeric()

    df <- data.frame( region=region, event_type=event_type, timestamp=timestamp )

    pkg.env$PROFILE_EVENTLOG_NROWS <- 0
    pkg.env$PROFILE_EVENTLOG <- df
    df # Kept for compatability after change
}

#' event_create
#' @description Create new start or end event in global eventlog
#' @param region Integer - Index of event/function in INSTRUMENTATION_DF
#' @param event_type Boolean - TRUE for start, FALSE for end event
#' @param timestamp numeric - Time of event occurence
event_create <- function(region, event_type, timestamp) {
    new_row <- list(as.integer(region), event_type, timestamp)

    ## DEBUGGING - Check datatypes
    #print(sapply(new_row, class))
 
    pkg.env$PROFILE_EVENTLOG[pkg.env$PROFILE_EVENTLOG_NROWS+1,] <- new_row
    pkg.env$PROFILE_EVENTLOG_NROWS <- pkg.env$PROFILE_EVENTLOG_NROWS + 1
}

#' reset_event_log
#' @description Assign new eventlog dataframe 
#' @return Dataframe for eventlog
reset_eventlog <- function() {
    pkg.env$PROFILE_EVENTLOG <- create_eventlog()
}


#' print_eventlog
#' @description Print global event log
#' @param df - Dataframe object for eventlog
#' @export
print_eventlog <- function(df=pkg.env$PROFILE_EVENTLOG) {
    print(df)
}


#' save_eventlog
#' @description Save event log to file
#' @param filename String - Name of file to save eventlog to, .csv
#' @param df - Dataframe object for eventlog
#' @param flag_debug Boolean - True to enable debug statements
#' @export
save_eventlog <- function(filename, df=pkg.env$PROFILE_EVENTLOG, flag_debug=FALSE) {
    if (flag_debug) print(paste0("############## SAVING TO FILE: ", filename, "###############"))
    utils::write.csv(df, filename, row.names=FALSE)
    if (flag_debug) print("############## SAVED TO FILE ###############")
}


#######################################################################
# section - instrumentation log
#######################################################################

# BASE VALS: package:function, ncalls, total_time
# EXTRA VALS: min time, max time, call stack


#' create_dataframe
#' @description See return
#' @param flag_debug Boolean - Enabled debug state,emts
#' @param flag_user_functions Boolean - TRUE to include user functions in dataframe
#' @return Dataframe containing each function with information on function name,
#'      package, count of function calls, total time spent in function
create_dataframe <- function(flag_user_functions=FALSE, flag_debug=FALSE) {
    packages <- .packages()
    num_functions_per_package <- total_num_functions(flag_user_functions=flag_user_functions)
    num_functions_total <- sum(num_functions_per_package)

    if (flag_user_functions) { packages <- append(packages, "user_functions") }

    ## Sections in dataframe
    function_names <- names(get_function_list())
    if (flag_user_functions) { function_names <- append(function_names, names(get_user_function_list())) }
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
    index <- 1
    for (i_package in 1:length(packages)) {
        tmp <- num_functions_per_package[i_package]
        package_list[index:(index-1+tmp)] <- packages[i_package]
        index <- index+tmp
    }

    # Init count and time arrays
    count[1:num_functions_total] <- 0L
    total_time[1:num_functions_total] <- 0.0
    instrumented[1:num_functions_total] <- FALSE

    data.frame( packages=package_list, functions=function_names, function_instrumented=instrumented,
               function_count=count, function_time=total_time )
}

#' reset_dataframe
#' @description Reset function_count and function_time info to zero
#' @param df Dataframe - Created by create_dataframe() for collecting tracing info
reset_dataframe <- function(df=pkg.env$PROFILE_INSTRUMENTATION_DF) {
    num_functions <- length(df[["functions"]])
    for ( i in 1:num_functions ){
        df[["function_count"]][i] <- 0
        df[["function_time"]][i] <- 0
    }
}

#' reduce_dataframe
#' @description Reduce dataframe to non-zero function_count entries
#' @param df Dataframe - Created by create_dataframe() for collecting tracing info
#' @return reduced_df Dataframe - All entries of functions which were called
reduce_dataframe <- function(df=pkg.env$PROFILE_INSTRUMENTATION_DF) {
    num_functions <- length(df[["functions"]])
    non_zero_count_indices <- ( df[["function_count"]] != 0 )
    reduced_df <- df[non_zero_count_indices,]
    reduced_df
}

#' save_dataframe
#' @description Write dataframe to file (preferably reduced)
#' @param df Dataframe - Created by create_dataframe() for collecting tracing info
#' @param filename String - Filename to save output to
#' @param flag_reduced Boolean - True to display only non-zero count functions, else display all
#' @export
save_dataframe <- function(filename, df=pkg.env$PROFILE_INSTRUMENTATION_DF, flag_reduced=TRUE) {
    if(flag_reduced) {
        utils::write.table(reduce_dataframe(df),filename,sep=",",row.names=FALSE)
    } else {
        utils::write.table(df,filename,sep=",",row.names=FALSE)
    }
}

#' print_instrumentation
#' @description Print table of reduce dataframe
#' @param df Dataframe - Created by create_dataframe() for collecting tracing info
#' @param flag_debug Boolean - Enable debug header
#' @param flag_reduced Boolean - True to display only non-zero count functions, else display all
#' @export
print_dataframe <- function(df=pkg.env$PROFILE_INSTRUMENTATION_DF, flag_reduced=TRUE, flag_debug=TRUE) {
    if (flag_debug) {
        print("########### INSTRUMENTATION_DATAFRAME ############")
    }
    if (flag_reduced) {
        print(reduce_dataframe(df))
    } else {
        print(df)
    }
}

