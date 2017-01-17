# check if tool names are different
check_tool_names <- function(tool_folder_1,tool_folder_2,quit_opt=TRUE) {
  if (tool_folder_1 == tool_folder_2) {
    message(paste("Error:Input folders can't be equal! Use '-h' for help!"))
    if(quit_opt) { # need for tests
      quit()
    }
  }
}

#check if folders exists
folder_existence <- function(folders.directory,folder,quit_opt=TRUE) {
  all_files <- list.files(folders.directory)
  if(!(any(all_files == folder))) {
    message(paste0("Error:Folder with the name ",folder," doesn't exist in the running directory ",folders.directory,"!"))
    if(quit_opt) { # need for tests
      quit()
    }
  }
}

# function to find model name
get_model_name <- function(folders.directory,folder,quit_opt=TRUE) {
  file <- read.table(paste0(folders.directory,folder,"/","command.txt"))
  for (i in 2:ncol(file)) {
    if (!(grepl("^-",file[,i]))) {
      model_name <- as.character(file[,i])
    }
  }
  if(exists("model_name")) {
    return(model_name)
  } else {
    command_file_direct <- paste0(folders.directory,folder,"/","command.txt")
    message(paste0("Error:Can't find model name in the file ",command_file_direct))
    if(quit_opt) { # need for tests
      quit()
    }
  }
  
}

#get toolnames form command.txt
get_toolname_foldername <- function(folders.directory,folder,quit_opt=TRUE) {
  file <- read.table(paste0(folders.directory,folder,"/","command.txt"))
  toolname <- sub(".*\\/", "",file[1,1])
  if(toolname == "") {
    command_file_direct <- paste0(folders.directory,folder,"/","command.txt")
    message(paste0("Error:Can't find tool name in the file ",command_file_direct))
    if(quit_opt) { # need for tests
      quit()
    }
  } else {
    return(c(toolname,folder))
  }
}

#create a new folder
create_folder_name <- function(folders.directory,toolname_foldername_1,toolname_foldername_2) {
  number <- 0
  #check if folder already exists
  toolname_1 <- toolname_foldername_1[1]
  toolname_2 <- toolname_foldername_2[1]
  all_files <- list.files(folders.directory)
  for (n in 1:length(all_files)) {
    if(grepl(paste0(toolname_1,".",toolname_2,"_dir"),all_files[n])) {
      existing_folder <- all_files[n] 
    }
  }
  if (exists("existing_folder")) {
    delete_part <- paste0(toolname_1,".",toolname_2,"_dir")
    left <- gsub(delete_part, "",existing_folder)
    if(grepl('^[0-9]+$',left)) { 
      number <- as.integer(left)
    }
  }
  number <- number +1
  new_folder_name <- paste0(toolname_1,".",toolname_2,"_dir",number)
  return(new_folder_name)
}

# create folder directory
create_folder_directory <- function(folders.directory,new_folder_name,create_dir=TRUE) {
  if(create_dir) {
    dir.create(file.path(folders.directory,new_folder_name))
  }
  new_folder_directory <- paste0(folders.directory,new_folder_name)
  return(new_folder_directory)
}

#function to find raw results only if both model names are equal
get_raw_results_file_name <- function(folders.directory,tool_folder_1,tool_folder_2,quit_opt=TRUE) {
  # find model names of given folders
  model_name_tool_1 <- get_model_name(folders.directory,tool_folder_1)
  model_name_tool_2 <- get_model_name(folders.directory,tool_folder_2)
  #check if model names are equal (if not stop working)
  if (model_name_tool_1 != model_name_tool_2) {
    message(paste0("Error:model names ",model_name_tool_1," and ",model_name_tool_2," are not equal!\nTo compare two tools you have to run them with the same model!"))
    if(quit_opt) { # need for tests
      quit()
    }
  } else {
    # find raw_results_'modelname'.csv files
    model_name <- gsub("\\.mod", "",model_name_tool_1) # delete .mod from model names
    raw_results <- paste0("raw_results_",model_name,".csv")
    all_files_tool_folder_1 <- list.files(paste0(folders.directory,"/",tool_folder_1))
    all_files_tool_folder_2 <- list.files(paste0(folders.directory,"/",tool_folder_2))
    if(!(any(all_files_tool_folder_1 == raw_results)) 
       || !(any(all_files_tool_folder_2 == raw_results))) {
      message(paste0("Raw results file with the model name ",model_name_tool_1," doesn't exist in the given folders ",tool_folder_1," or ",tool_folder_2,"!"))
      if(quit_opt) { # need for tests
        quit()
      }
    }
  return(raw_results) # just a name (both have same names)
  }
}

#get more needed csv file names (ADD SCRIPT FOR COMPARABLE TOOLS!)
get_more_csv_file_names <- function(folders.directory,toolname_foldername_1,toolname_foldername_2,quit_opt=TRUE) {
  toolname_1 <- toolname_foldername_1[1]
  toolname_2 <- toolname_foldername_2[1]
  foldername_1 <- toolname_foldername_1[2]
  foldername_2 <- toolname_foldername_2[2]
  if ((grepl("^simeval$",toolname_1) && grepl("^cdd$",toolname_2)) ||
      (grepl("^cdd$",toolname_1) && grepl("^simeval$",toolname_2))) {
    if (grepl("^simeval$",toolname_1)) {
      simeval_dir <- foldername_1
      cdd_dir <- foldername_2
    } else {
      simeval_dir <- foldername_2
      cdd_dir <- foldername_1
    }
    all_cdd_files <- list.files(paste0(folders.directory,"/",cdd_dir))
    all_simeval_files <- list.files(paste0(folders.directory,"/",simeval_dir))
    for (i in 1:length(all_cdd_files)) {
      if(grepl("^skipped_individuals",all_cdd_files[i])) {
        skipped_individuals <- all_cdd_files[i]
      }
    }
    if(!(exists("skipped_individuals"))) {
      message(paste0("Error:Skipped individuals csv file doesn't exist in the folder ",cdd_dir,"!"))
      if(quit_opt) { # need for tests
        quit()
      }
    }
    for (i in 1:length(all_simeval_files)) {
      if(grepl("^raw_all_iofv.csv$",all_simeval_files[i])) {
        all.iofv.file <- all_simeval_files[i]
      }
      if(grepl("^residual_outliers.csv$",all_simeval_files[i])) {
        residual.outliers.file <- all_simeval_files[i]
      }
      if(grepl("^ebe_npde.csv$",all_simeval_files[i])) {
        ebe.npde.file <- all_simeval_files[i]
      }
    }
    if(!(exists("all.iofv.file"))) {
      message(paste0("Error:File raw_all_iofv.csv doesn't exist in the folder ",simeval_dir,"!"))
      if(quit_opt) { # need for tests
        quit()
      }
    }
    if(!(exists("residual.outliers.file"))) {
      message(paste0("Error:File residual_outliers.csv doesn't exist in the folder ",simeval_dir,"!"))
      if(quit_opt) { # need for tests
        quit()
      }
    }
    if(!(exists("ebe.npde.file"))) {
      message(paste0("Error:File ebe_npde.csv doesn't exist in the folder ",simeval_dir,"!"))
      if(quit_opt) { # need for tests
        quit()
      }
    }
    if((exists("skipped_individuals")) && (exists("all.iofv.file")) && (exists("residual.outliers.file")) && (exists("ebe.npde.file"))) {
      return(list(skipped_individuals=skipped_individuals,all.iofv.file=all.iofv.file,
                  residual.outliers.file=residual.outliers.file,ebe.npde.file=ebe.npde.file))
    }
  } else { # not comparable tools
    message(paste0("Error:Can't compare tools ",toolname_1," and ",toolname_2,"!"))
    if(quit_opt) { # need for tests
      quit()
    }
  }
}

#get list of files (ADD SCRIPT FOR COMPARABLE TOOLS!)
get_list_of_files <- function(folders.directory,toolname_foldername_1,toolname_foldername_2,
                              raw_result_file,other_files=NULL) {
  toolname_1 <- toolname_foldername_1[1]
  toolname_2 <- toolname_foldername_2[1]
  foldername_1 <- toolname_foldername_1[2]
  foldername_2 <- toolname_foldername_2[2]
  list_of_files <- c()
  if ((grepl("^simeval$",toolname_1) && grepl("^cdd$",toolname_2)) ||
      (grepl("^cdd$",toolname_1) && grepl("^simeval$",toolname_2))) {
    skipped_individuals <- other_files$skipped_individuals
    all.iofv.file <- other_files$all.iofv.file
    residual.outliers.file <- other_files$residual.outliers.file
    ebe.npde.file <- other_files$ebe.npde.file
    if (grepl("^simeval$",toolname_1)) {
      simeval_dir <- foldername_1
      cdd_dir <- foldername_2
    } else {
      simeval_dir <- foldername_2
      cdd_dir <- foldername_1
    }
    #skipped_individuals <- other_files$skipped_individuals
    list_of_files[1] <- paste0(folders.directory,simeval_dir,"/",all.iofv.file)
    list_of_files[2] <- paste0(folders.directory,cdd_dir,"/",other_files$skipped_individuals)
    list_of_files[3] <- paste0(folders.directory,cdd_dir,"/",raw_result_file)
    list_of_files[4] <- paste0(folders.directory,simeval_dir,"/",ebe.npde.file)
    list_of_files[5] <- paste0(folders.directory,simeval_dir,"/",residual.outliers.file)
  }
  return(list_of_files)
}

#calculate needed input values #(ADD SCRIPT FOR COMPARABLE TOOLS!)
get_input_values <- function(list_of_files,toolname_foldername_1,toolname_foldername_2) {
  toolname_1 <- toolname_foldername_1[1]
  toolname_2 <- toolname_foldername_2[1]
  foldername_1 <- toolname_foldername_1[2]
  foldername_2 <- toolname_foldername_2[2]
  if ((grepl("^simeval$",toolname_1) && grepl("^cdd$",toolname_2)) ||
      (grepl("^cdd$",toolname_1) && grepl("^simeval$",toolname_2))) {
    all.iofv.csv <- list_of_files[1]
    ebe.npde.csv <- list_of_files[4]
    # get n.subjects, successful.samples
    all.iofv.file_table <- read.csv(all.iofv.csv)
    n.subjects <- nrow(all.iofv.file_table) # get n.subjects
    successful.samples <- length(grep("^sample.",colnames(all.iofv.file_table))) # get number of successful samples
    # get all ETA names (in calculations use all ETA names, don't care about iiv.etas and iov.etas)
    ebe.npde.file_table <- read.csv(ebe.npde.csv)
    eta.names <- grep("^ETA.",colnames(ebe.npde.file_table),value=TRUE)
  return(list(successful.samples=successful.samples,n.subjects=n.subjects,eta.names=eta.names))
  }
}

#copy needed files to the created folder
copy_files <- function(list_of_files,new_folder_directory) {
  for (i in 1:length(list_of_files)) {
    file.copy(from=list_of_files[i],to=new_folder_directory)
  }
}

create_pdf.filename <- function(toolname_foldername_1,toolname_foldername_2) {
  toolname_1 <- toolname_foldername_1[1]
  toolname_2 <- toolname_foldername_2[1]
  pdf.filename <- paste0(toolname_1,".",toolname_2,".pdf")
  return(pdf.filename)
}

# write a text file with Rscript input (ADD SCRIPT FOR COMPARABLE TOOLS!)
create_R_script <- function(rscripts.directory,new_folder_directory,
                            toolname_foldername_1,toolname_foldername_2,
                            raw_result_file,other_files,values,pdf.filename) {
  toolname_1 <- toolname_foldername_1[1]
  toolname_2 <- toolname_foldername_2[1]
  foldername_1 <- toolname_foldername_1[2]
  foldername_2 <- toolname_foldername_2[2]
  R_input <- c()
  R_input[1] <- paste0("setwd('",new_folder_directory,"')")
  R_input[2] <- paste0("rscripts.directory <- '",rscripts.directory,"'")
  R_input[3] <- paste0("input_folder_1 <- '",foldername_1,"'")
  R_input[4] <- paste0("input_folder_2 <- '",foldername_2,"'")
  R_input[5] <- paste0("tool_1 <- '",toolname_1,"'")
  R_input[6] <- paste0("tool_2 <- '",toolname_2,"'")
  R_input[7] <- paste0("pdf.filename <- '",pdf.filename,"'")
  next_nr <- length(R_input) + 1

  if ((grepl("^simeval$",toolname_1) && grepl("^cdd$",toolname_2)) ||
      (grepl("^cdd$",toolname_1) && grepl("^simeval$",toolname_2))) {
    R_input[next_nr] <- paste0("successful.samples <- ",values$successful.samples)
    R_input[next_nr+1] <- paste0("n.subjects <- ",values$n.subjects)
    R_input[next_nr+2] <- paste0("all.iofv.file <- '",other_files$all.iofv.file,"'")
    R_input[next_nr+3] <- paste0("skipped.id.file <- '",other_files$skipped_individuals,"'")
    R_input[next_nr+4] <- paste0("raw.results.file <- '",raw_result_file,"'")
    R_input[next_nr+5] <- paste0("residual.outliers.file <- '",other_files$residual.outliers.file,"'")
    R_input[next_nr+6] <- paste0("ebe.npde.file <- '",other_files$ebe.npde.file,"'")
    R_input[next_nr+7] <- paste0("eta.names <- c('",paste(values$eta.names,collapse="','"),"')")
    R_input[next_nr+8] <- ""
    R_input[next_nr+9] <- paste0("source(paste0(rscripts.directory,'cdd.simeval_default.R'))")
    R_input[next_nr+10] <- paste0("cdd.simeval(rscripts.directory,all.iofv.file,n.subjects,samples=successful.samples,
              raw.results.file,skipped.id.file,residual.outliers.file,ebe.npde.file,eta.names,
              pdf.filename,cutoff_cook=0.8,cutoff_delta.ofv=3.84)")
  }
return(R_input)
}