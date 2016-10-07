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
folders.directory <- paste0(getwd(),"/")

# input arguments from command line
args <- commandArgs(TRUE)

# check if input arguments are added
if(length(args) < 2) {
  if((length(args) == 1) && (grepl("^--help",as.character(args[1])))) {
    message("Input:\n    Two folder names are required.
    The following options are valid, but not required:")
    message("-pdf,\n    A name of pdf file.")
    message("-f,\n    A folder name, where created pdf file, needed csv files and R script are going to be saved.")
    message("\nExamples:\n    Rscript C:\\PsN\\R-scripts\\compare\\compare.R folder1 folder2 -pdf=my_results.pdf -f=new_folder_name")
    message("    Rscript C:\\PsN\\R-scripts\\compare\\compare.R folder1 folder2")
    quit()
  } else {
    message("Error:Two folder names as input are required! Use '--help' for help!")
    quit()
  }
}
if(length(args) > 4) {
  message("Error:Too many inputs! No more than four input arguments can be passed! Use '--help' for help!")
  quit()
}
folder_names_in_text <- c()
folder <- c()
index <- 0
for(i in 1:length(args)) {
  if(grepl("^-pdf=",as.character(args[i]))) {
    pdf.filename <- gsub("\\-pdf=", "",as.character(args[i]))
    if (!(grepl(".pdf$",pdf.filename))) {
      pdf.filename <- paste0(pdf.filename,".pdf")
    }
  } else if(grepl("^-f=",as.character(args[i]))) {
    new_folder_name <- gsub("\\-f=", "",as.character(args[i]))
  } else {
    if(!(grepl("^--help$",as.character(args[i])))) {
      index <- index + 1
      folder[index] <- as.character(args[i])
      folder_names_in_text <- paste0(folder_names_in_text,", ",folder[index])
    }
  }
}
if (length(folder_names_in_text) > 0) {
  folder_names_in_text <- substring(folder_names_in_text,2)
}

if(length(folder) > 2) {
  message(paste0("Error:Too many input folder names:",folder_names_in_text,"!\nTwo folder names as input are required! Use '--help' for help!"))
  quit()
} else if(length(folder) < 2){
  message(paste0("Error:Not enough input folder names:",folder_names_in_text,"!\nTwo folder names as input are required! Use '--help' for help!"))
  quit()
} else {
  tool_folder_1 <- folder[1]
  tool_folder_2 <- folder[2]
}

source(paste0(rscripts.directory,"/compare/two_tools_functions.R"))
# check if folder names are not equal
check_tool_names(tool_folder_1,tool_folder_2)

# check folder existence
folder_existence(folders.directory,tool_folder_1)
folder_existence(folders.directory,tool_folder_2)

# get tool names
toolname_foldername_1 <- get_toolname_foldername(folders.directory,tool_folder_1)
toolname_foldername_2 <- get_toolname_foldername(folders.directory,tool_folder_2)

# get name of the raw results file (based on model name)
raw_result_file <- get_raw_results_file_name(folders.directory,tool_folder_1,tool_folder_2)

# get other csv files
other_files <- get_more_csv_file_names(folders.directory,toolname_foldername_1,toolname_foldername_2)

# create new folder
if (!exists("new_folder_name")) {
  new_folder_name <- create_folder_name(folders.directory,toolname_foldername_1,toolname_foldername_2)
}

# create new folders directory
new_folder_directory <- create_folder_directory(folders.directory,new_folder_name)

#find files which are going to be copied
list_of_files <- get_list_of_files(folders.directory,toolname_foldername_1,toolname_foldername_2,
                                   raw_result_file,other_files)

# get input values
values <- get_input_values(list_of_files,toolname_foldername_1,toolname_foldername_2)

#copy files to new folder
copy_files(list_of_files,new_folder_directory)

# create pdf filename if it in not passed as input
if (!exists("pdf.filename")) {
  pdf.filename <- create_pdf.filename(toolname_foldername_1,toolname_foldername_2)
}

# create list of Rscript input and sourced function
R_input <- create_R_script(rscripts.directory,new_folder_directory,
                           toolname_foldername_1,toolname_foldername_2,
                           raw_result_file,other_files,values,pdf.filename) 

#run R script
message("Running R script... \n")
toolname_1 <- toolname_foldername_1[1]
toolname_2 <- toolname_foldername_2[1]
if ((grepl("^simeval$",toolname_1) && grepl("^cdd$",toolname_2)) ||
    (grepl("^cdd$",toolname_1) && grepl("^simeval$",toolname_2))) {
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
message("Pdf file ",pdf.filename," is saved in the folder ",new_folder_directory)
message("DONE!")

