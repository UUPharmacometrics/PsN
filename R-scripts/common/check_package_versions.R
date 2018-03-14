#' Check package versions. Will print an error message if some package version is too low.
#' 
#' @param pkg A character vector with package names
#' @param versions A character vector with package versions
#' @param toolname A string of the tool name
#' @param do_quit A logical indicating whether function should quit if some package version is too low. By default do_quit=TRUE
check_package_versions <- function(pkg,versions,toolname,do_quit=T) {
  if(length(pkg)==length(versions)) {
    for(i in 1:length(pkg)) {
      if(packageVersion(pkg[i])<versions[i]) {
        message(paste0("ERROR: ",pkg[i]," package version must be ",versions[i]," or later for ",toolname," plots. PDF file not created!"))
        if(do_quit) {
          quit()
        }
      }
    }
  } else {
    message("ERROR: Length of package names are not equal with length of versions!")
    if(do_quit) {
      quit()
    }
  }
}