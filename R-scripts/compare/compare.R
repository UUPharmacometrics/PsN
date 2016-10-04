# don't show warning
options(warn=-1)

# get actual this script directory
source_local <- function(){
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
  return(base_dir)
}
rscripts.directory <- source_local()
rscripts.directory <- gsub("\\compare", "",rscripts.directory)

# get directory of the folders
folders.directory <- getwd()

# input arguments from command line
args <- commandArgs(TRUE)

# check if input arguments are added
if(length(args) < 2) {
  stop("Two folder names as input are required!")
}
if(length(args) == 2) {
  # default value for pdf
  if(grepl("_dir", args[1]) && grepl("_dir", args[2])) {
    tool_folder_1 <- as.character(args[1]) # folder 1 (example: simeval_dir6)
    tool_folder_2 <- as.character(args[2]) # folder 2 (example: cdd_dir4)
    pdf.filename <- "PsN_cdd.simeval.pdf" # default for pdf filename
  } else {
    stop("Please add two folder names as input arguments!")
  }
}
if(length(args) == 3) { 
  folder <- c()
  index <- 0
  for (i in 1:3) {
    if(!(grepl("_dir", args[i]))) {
      pdf.filename <- as.character(args[i]) # pdf file name
      if (!(grepl(".pdf$",pdf.filename))) {
        pdf.filename <- paste0(pdf.filename,".pdf") # add .pdf to the filename if it is not added
      }
    } else {
      index <- index + 1
      folder[index] <- as.character(args[i])
    }
  }
tool_folder_1 <- folder[1]
tool_folder_2 <- folder[2]
}
if(length(args) > 3) {
  stop("No more than three arguments can be passed!
       Two folder names are required. If pdf filename is not passed, it will be created by the default! \n")
}

source(paste0(rscripts.directory,"/compare/two_tools_functions.R"))
# check if folder names are not equal
check_tool_names(tool_folder_1,tool_folder_2)

# check folder existence
folder_existence(folders.directory,tool_folder_1)
folder_existence(folders.directory,tool_folder_2)

# get name of the raw results file (based on model name)
raw_result_file <- get_raw_results_file_name(folders.directory,tool_folder_1,tool_folder_2)

# get other csv files
other_files <- get_more_csv_file_names(folders.directory,tool_folder_1,tool_folder_2)

# create new folder
new_folder_name <- create_folder_name(folders.directory,tool_folder_1,tool_folder_2)

# create new folders directory
new_folder_directory <- create_folder_directory(folders.directory,new_folder_name)

#find files which are going to be copied
list_of_files <- get_list_of_files(folders.directory,rscripts.directory,
                                   tool_folder_1,tool_folder_2,
                                   raw_result_file,other_files)

# get input values
values <- get_input_values(list_of_files)

#copy files to new folder
copy_files(list_of_files,new_folder_directory)

# create list of Rscript input and sourced function
R_input <- create_R_script(rscripts.directory,new_folder_directory,tool_folder_1,tool_folder_2,
                       raw_result_file,other_files,values,pdf.filename) 

#run R script
cat("Running R script... \n")
if ((grepl("^simeval_dir",tool_folder_1) && grepl("^cdd_dir",tool_folder_2)) ||
     (grepl("^cdd_dir",tool_folder_1) && grepl("^simeval_dir",tool_folder_2))) {
  # write R file
  setwd(new_folder_directory)
  write(R_input,"cdd.simeval.R")

  pdf.filename <- pdf.filename
  all.iofv.file <- other_files$all.iofv.file
  raw.results.file <- raw_result_file
  skipped.id.file <- other_files$skipped_individuals
  
  source(paste0(rscripts.directory,"cdd.simeval_default.R"))
  cdd.simeval(rscripts.directory,all.iofv.file,n.subjects=values[2],samples=values[1],
              raw.results.file,skipped.id.file,pdf.filename)
}
cat("DONE! \n")
cat("Pdf file",pdf.filename,"is saved in the folder",new_folder_directory, "\n")

