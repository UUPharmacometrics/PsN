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
  if((length(args) == 1) && (grepl("^-h",as.character(args[1])))) {
    cat("Input:\n    A compare.R file with the full path is required.\n    The following options are valid:\n\n")
    cat("-f1,\n    A name of folder 1, is required.\n")
    cat("-f2,\n    A name of folder 2, is required.\n")
    cat("-pdf,\n    A name of pdf file, not required.\n\n")
    cat("Examples:\n    C:\\PsN\\R-scripts\\compare\\compare.R -f1=folder1 -f2=folder2 -pdf=my_results.pdf \n")
    cat("    C:\\PsN\\R-scripts\\compare\\compare.R -f1=folder1 -f2=folder2 \n")
    quit()
  } else {
    cat("Two folder names as input are required! Use '-h' for help!")
    quit()
  }
}
if(length(args) > 3) {
  cat("No more than three arguments can be passed!\nTwo folder names are required. Use '-h' for help!\n")
  quit()
}

for(i in 1:length(args)) {
  if(grepl("^-f1=",as.character(args[i]))) {
    tool_folder_1 <- gsub("\\-f1=", "",as.character(args[i]))
  } else if(grepl("^-f2=",as.character(args[i]))) {
    tool_folder_2 <- gsub("\\-f2=", "",as.character(args[i]))
  } else if(grepl("^-pdf=",as.character(args[i]))) {
    pdf.filename <- gsub("\\pdf=", "",as.character(args[i]))
    pdf.filename <- substring(pdf.filename,2)
    if (!(grepl(".pdf$",pdf.filename))) {
      pdf.filename <- paste0(pdf.filename,".pdf")
    }
  } else {
    cat(paste0("Invalid option ",as.character(args[i]),"! Use '-h' for help!"))
    quit()
  }
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
new_folder_name <- create_folder_name(folders.directory,toolname_foldername_1,toolname_foldername_2)

# create new folders directory
new_folder_directory <- create_folder_directory(folders.directory,new_folder_name)

#find files which are going to be copied
list_of_files <- get_list_of_files(folders.directory,rscripts.directory,
                                   toolname_foldername_1,toolname_foldername_2,
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
cat("Running R script... \n")
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
cat("DONE! \n")
cat("Pdf file",pdf.filename,"is saved in the folder",new_folder_directory, "\n")

