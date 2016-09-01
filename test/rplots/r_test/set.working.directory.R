# set working directory for the rscripts
fun.rscript.w.dir <- function() {
  rscript.w.dir <- "../../../../R-scripts"
  return(rscript.w.dir)
}

# set working directory for the testing files (csv files)
fun.files.w.dir <- function(toolname) {
  files.w.dir <- paste0("../../test_files/",toolname,"/csv_for_testthat/")
  return(files.w.dir)
}

# get all R files from the folder, add the path to each of the files and save them in the vector 
find_r_files_in_subdir <- function(toolname,topdir) {
  new.folder.directory <- paste0(topdir,"/",toolname)
  all.files <- dir(path=new.folder.directory)
  directory_and_file <- c()
  for (i in 1:length(all.files)) {
    directory_and_file[i] <- paste0(new.folder.directory,"/",all.files[i])
  }
  # add common functions
  common.folder.directory <- paste0(topdir,"/common")
  common.files <- dir(path=common.folder.directory)
  common_directory_and_file <- c()
  for (i in 1:length(common.files)) {
    common_directory_and_file[i][i] <- paste0(common.folder.directory,"/",common.files[i])
  }
  directory_and_file <- c(directory_and_file,common_directory_and_file)
  return(directory_and_file)
}