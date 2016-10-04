#check if folders exists
folder_existence <- function(folders.directory,tool_folder) {
  all_files <- list.files(folders.directory)
  if(!(any(all_files == tool_folder))) {
    stop(paste0("Folder with the name ",tool_folder," doesn't exist in the running directory ",folders.directory,"!"))
  }
}

# function to find model name
get_model_name <- function(folders.directory,folder) {
  file <- read.table(paste0(folders.directory,"/",folder,"/","command.txt"))
  for (i in 2:ncol(file)) {
    if (!(grepl("^-",file[,i]))) {
      model_name <- as.character(file[,i])
    }
  }
  return(model_name)
}

#function for creating toolname based on input folder name
get_tool_name <- function(tool_folder_name) {
  toolname <- gsub("\\d+", "",tool_folder_name)
  toolname <- gsub("\\_dir", "",toolname)
}

# check if tool names ar different
check_tool_names <- function(tool_folder_1,tool_folder_2) {
  if (tool_folder_1 == tool_folder_2) {
    stop(paste("Input folders can't be equal!"))
  }
}

#create a new folder
create_folder_name <- function(folders.directory,tool_folder_1,tool_folder_2) {
  number <- 0
  #check if folder already exists
  toolname_1 <- get_tool_name(tool_folder_1)
  toolname_2 <- get_tool_name(tool_folder_2)
  all_files <- list.files(folders.directory)
  for (n in 1:length(all_files)) {
    if(grepl(paste0(toolname_1,".",toolname_2,"_dir"),all_files[n])) {
      existing_folder <- all_files[n] 
    }
  }
  if (exists("existing_folder")) {
    delete_part <- paste0(toolname_1,".",toolname_2,"_dir")
    number <- as.integer(gsub(delete_part, "",existing_folder))
  }
  number <- number +1
  new_folder_name <- paste0(toolname_1,".",toolname_2,"_dir",number)
  return(new_folder_name)
}

# create folder directory
create_folder_directory <- function(folders.directory,new_folder_name) {
  dir.create(file.path(folders.directory,new_folder_name))
  new_folder_directory <- paste0(folders.directory,"/",new_folder_name)
  return(new_folder_directory)
}

#function to find raw results only if both model names are equal
get_raw_results_file_name <- function(folders.directory,tool_folder_1,tool_folder_2) {
  # find model names of given folders
  model_name_tool_1 <- get_model_name(folders.directory,tool_folder_1)
  model_name_tool_2 <- get_model_name(folders.directory,tool_folder_2)
  #check if model names are equal (if not stop working)
  if (model_name_tool_1 != model_name_tool_2) {
    stop(paste0("model names ",model_name_tool_1," and ",model_name_tool_2," are not equal! 
  To compare two tools you have to run them with the same model!"))
  } else {
    # find raw_results_'modelname'.csv files
    model_name <- gsub("\\.mod", "",model_name_tool_1) # delete .mod from model names
    raw_results <- paste0("raw_results_",model_name,".csv")
    all_files_tool_folder_1 <- list.files(paste0(folders.directory,"/",tool_folder_1))
    all_files_tool_folder_2 <- list.files(paste0(folders.directory,"/",tool_folder_2))
    if(!(any(all_files_tool_folder_1 == raw_results)) 
       && !(any(all_files_tool_folder_2 == raw_results))) {
      stop(paste0("Raw results file with the model name ",model_name_tool_1," doesn't exist in the given folders ",tool_folder_1," or ",tool_folder_2,"!"))
    }
  }
  return(raw_results) # just a name (both have same names)
}

#get more needed csv file names (ADD SCRIPT FOR COMPARABLE TOOLS!)
get_more_csv_file_names <- function(folders.directory,tool_folder_1,tool_folder_2) {
  if ((grepl("^simeval_dir",tool_folder_1) && grepl("^cdd_dir",tool_folder_2)) ||
      (grepl("^cdd_dir",tool_folder_1) && grepl("^simeval_dir",tool_folder_2))) {
    if (grepl("^simeval_dir",tool_folder_1)) {
      simeval_dir <- tool_folder_1
      cdd_dir <- tool_folder_2
    } else {
      simeval_dir <- tool_folder_2
      cdd_dir <- tool_folder_1
    }
    all_cdd_files <- list.files(paste0(folders.directory,"/",cdd_dir))
    all_simeval_files <- list.files(paste0(folders.directory,"/",simeval_dir))
    for (i in 1:length(all_cdd_files)) {
      if(grepl("^skipped_individuals",all_cdd_files[i])) {
        skipped_individuals <- all_cdd_files[i]
      }
    }
    if(!(exists("skipped_individuals"))) {
      stop(paste0("Skipped individuals csv file doesn't exist in the folder ",simeval_dir,"!"))
    }
    for (i in 1:length(all_simeval_files)) {
      if(grepl("^raw_all_iofv.csv$",all_simeval_files[i])) {
        all.iofv.file <- all_simeval_files[i]
      }
    }
    if(!(exists("all.iofv.file"))) {
      stop(paste0("File raw_all_iofv.csv doesn't exist in the folder ",cdd_dir,"!"))
    }
    return(list(skipped_individuals=skipped_individuals,all.iofv.file=all.iofv.file))
  }
  
}

#get list of files (ADD SCRIPT FOR COMPARABLE TOOLS!)
get_list_of_files <- function(folders.directory,rscripts.directory,
                              tool_folder_1,tool_folder_2,
                              raw_result_file,other_files) {
  toolname_1 <- get_tool_name(tool_folder_1)
  toolname_2 <- get_tool_name(tool_folder_2)
  list_of_files <- c()
  if ((grepl("^simeval_dir",tool_folder_1) && grepl("^cdd_dir",tool_folder_2)) ||
      (grepl("^cdd_dir",tool_folder_1) && grepl("^simeval_dir",tool_folder_2))) {
    skipped_individuals <- other_files$skipped_individuals
    all.iofv.file <- other_files$all.iofv.file
    if (grepl("^simeval_dir",tool_folder_1)) {
      simeval_dir <- tool_folder_1
      cdd_dir <- tool_folder_2
    } else {
      simeval_dir <- tool_folder_2
      cdd_dir <- tool_folder_1
    }
    skipped_individuals <- other_files$skipped_individuals
    list_of_files[1] <- paste0(folders.directory,"/",simeval_dir,"/",all.iofv.file)
    list_of_files[2] <- paste0(folders.directory,"/",cdd_dir,"/",skipped_individuals)
    list_of_files[3] <- paste0(folders.directory,"/",cdd_dir,"/",raw_result_file)
  }
  return(list_of_files)
}

#copy needed files to the created folder
copy_files <- function(list_of_files,new_folder_directory) {
  for (i in 1:length(list_of_files)) {
    file.copy(from=list_of_files[i],to=new_folder_directory)
  }
}

#calculate needed input values #(ADD SCRIPT FOR COMPARABLE TOOLS!)
get_input_values <- function(list_of_files) {
  if ((grepl("^simeval_dir",tool_folder_1) && grepl("^cdd_dir",tool_folder_2)) ||
      (grepl("^cdd_dir",tool_folder_1) && grepl("^simeval_dir",tool_folder_2))) {
    all.iofv.file <- list_of_files[1]
    # get n.subjects, successful.samples,outlying_criteria
    all.iofv.file_table <- read.csv(all.iofv.file)
    n.subjects <- nrow(all.iofv.file_table) # get n.subjects
    successful.samples <- length(grep("^sample.",colnames(all.iofv.file_table))) # get number of successful samples
  return(c(successful.samples,n.subjects))
  }
}

# write a text file with Rscript input (ADD SCRIPT FOR COMPARABLE TOOLS!)
create_R_script <- function(rscripts.directory,new_folder_directory,tool_folder_1,tool_folder_2,
                        raw_result_file,other_files,values,pdf.filename) {
  if(missing(values)) {
    R_input <- c()
  }
  R_input <- c()
  next_input_nr <- length(R_input) + 1
  if ((grepl("^simeval_dir",tool_folder_1) && grepl("^cdd_dir",tool_folder_2)) ||
      (grepl("^cdd_dir",tool_folder_1) && grepl("^simeval_dir",tool_folder_2))) {
    skipped_individuals <- other_files$skipped_individuals
    all.iofv.file <- other_files$all.iofv.file
    R_input[next_input_nr] <- paste0("successful.samples <- ",values[1])
    R_input[next_input_nr+1] <- paste0("n.subjects <- ",values[2])
    R_input[next_input_nr+2] <- paste0("pdf.filename <- '",pdf.filename,"'")
    R_input[next_input_nr+3] <- paste0("rscripts.directory <- '",rscripts.directory,"'")
    R_input[next_input_nr+4] <- paste0("all.iofv.file <- '",all.iofv.file,"'")
    R_input[next_input_nr+5] <- paste0("skipped.id.file <- '",skipped_individuals,"'")
    R_input[next_input_nr+6] <- paste0("raw.results.file <- '",raw_result_file,"'")
    R_input[next_input_nr+7] <- paste0("tool_folder_1 <- '",tool_folder_1,"'")
    R_input[next_input_nr+8] <- paste0("tool_folder_2 <- '",tool_folder_2,"'")
    R_input[next_input_nr+9] <- paste0("setwd('",new_folder_directory,"')")
    R_input[next_input_nr+10] <- paste0("source(paste0(rscripts.directory,'cdd.simeval_default.R'))")
    R_input[next_input_nr+11] <- paste0("cdd.simeval(rscripts.directory,all.iofv.file,n.subjects,samples=successful.samples,
              raw.results.file,skipped.id.file,pdf.filename)")
  }
return(R_input)
}
