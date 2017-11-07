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