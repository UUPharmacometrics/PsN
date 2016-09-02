file_existence_in_directory <- function(directory,file_name) {
  all.files <- dir(path=directory)
  if (any(all.files == file_name)) {
    file_exists <- TRUE
  } else {
    file_exists <- FALSE
  }
  return(file_exists)
}