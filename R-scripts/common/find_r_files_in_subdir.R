# get all R files from the folder, add the path to each of the files and save them in the vector 
find_r_files_in_subdir <- function(toolname,topdir) {
  new.folder.directory <- paste0(topdir,"/",toolname)
  all.files <- dir(path=new.folder.directory)
  directory_and_file <- c()
  for (i in 1:length(all.files)) {
    directory_and_file[i] <- paste0(new.folder.directory,"/",all.files[i])
  }
  return(directory_and_file)
}