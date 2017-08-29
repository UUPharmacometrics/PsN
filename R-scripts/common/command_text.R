command_text <- function(directory) {
  command <- c()
  if(file.exists(file.path(working.directory,"command.txt")) &&
     file.size(file.path(working.directory,"command.txt"))!=0) {
    full_command <- read.table(file.path(working.directory,"command.txt"))
    for (i in 1:ncol(full_command)) {
      if (i==1) {
        tool_name <- sub('.*\\/','',as.character(full_command[1,i]))
        command <- tool_name
      } else {
        command <- paste(command,as.character(full_command[1,i]))
      }
    }
  }
  
  run_start <- c()
  run_finish <- c()
  if(file.exists(file.path(working.directory,"version_and_option_info.txt")) &&
     file.size(file.path(working.directory,"version_and_option_info.txt"))!=0) {
    vers_opt_file <- read.delim(file.path(working.directory,"version_and_option_info.txt"))
    for (i in 1:nrow(vers_opt_file)) {
      if (grepl("^Run started:",as.character(vers_opt_file[i,1]))) {
        run_start <- as.character(vers_opt_file[i,1])
      }
      if (grepl("^Run finished:",as.character(vers_opt_file[i,1]))) {
        run_finish <- as.character(vers_opt_file[i,1])
      }
    }
    if (!exists("run_finish")) {
      run_finish <- as.vector(strsplit(as.character(Sys.time())," "))
      run_finish <- paste("Run finished:",run_finish[[1]][1],"at",run_finish[[1]][2])
    }
  }

  
  out <- list(command=command,
              run_start=run_start,
              run_finish=run_finish)
  return(out)
}
