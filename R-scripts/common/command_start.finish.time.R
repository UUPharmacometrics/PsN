#' Get command line text, start and finish time of the run
#' 
#' @param yaml_file A yaml file name
#' @return A list of three elements: command, run_start, run_finish
command_start.finish.time <- function(yaml_file) {
  command <- c()
  run_start <- c()
  run_finish <- c() 
  if(file.exists(yaml_file)) {
    yaml_file <- yaml.load_file(yaml_file)
    
    if(exists("command_line",yaml_file) && yaml_file$command_line!="") {
      full_command <- strsplit(yaml_file$command_line," ")
      for (i in 1:length(full_command[[1]])) {
        if (i==1) {
          tool_name <- sub('.*\\/','',full_command[[1]][i])
          command <- tool_name
        } else {
          command <- paste(command,full_command[[1]][i])
        }
      }
    }
   
    if(exists("start_time",yaml_file) && yaml_file$start_time!="") {
      run_start <- paste("Run started:",yaml_file$start_time)
    }
    if(exists("finish_time",yaml_file) && yaml_file$finish_time!="") {
      run_finish <- yaml_file$finish_time
    }
    if(length(run_finish)==0) {
      run_finish <- as.vector(strsplit(as.character(Sys.time())," "))
      run_finish <- paste(run_finish[[1]][1],run_finish[[1]][2])
    }
    run_finish <- paste("Run finished:",run_finish)
    
  }
  out <- list(command=command,
              run_start=run_start,
              run_finish=run_finish)
  return(out)
}
