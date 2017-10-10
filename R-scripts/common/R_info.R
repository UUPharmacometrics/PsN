R_info <- function(directory,only_libPaths=F) {
  if(only_libPaths) {
    if(file.exists(file.path(directory,"meta.yaml"))) {
      yaml_file <- yaml::yaml.load_file(file.path(directory,"meta.yaml"))
      if(!exists("R_LIB_PATHS",yaml_file)) {
        cat(yaml::as.yaml(list(R_LIB_PATHS=.libPaths()),
                          column.major = F,
                          indent.mapping.sequence = TRUE),
            file=file.path(directory,"meta.yaml"),
            append = TRUE)
      } else {
        yaml_file$R_LIB_PATHS <- NULL
        yaml_file$R_LIB_PATHS <- .libPaths()
        cat(yaml::as.yaml(yaml_file,
                          column.major = F,
                          indent.mapping.sequence = TRUE),
            file=file.path(directory,"meta.yaml"))
      }
      
    } else {
      cat(yaml::as.yaml(list(R_LIB_PATHS=.libPaths()),
                        column.major = F,
                        indent.mapping.sequence = TRUE),
          file=file.path(directory,"meta.yaml"))
    }
  } else {
    #get loaded R packages
    R_packages <- as.data.frame(devtools::session_info()[[2]])[,c("package","version","source")]
    R_packages_vec <- paste0(R_packages$package[1],"-",R_packages$version[1]," from ",R_packages$source[1])
    for(i in 2:nrow(R_packages)) {
      R_packages_vec <- c(R_packages_vec,paste0(R_packages$package[i],"-",R_packages$version[i]," from ",R_packages$source[i]))
    }
    R_packages <- R_packages_vec
    
    #write in to the yaml file
    if(file.exists(file.path(directory,"meta.yaml"))) {
      yaml_file <- yaml::yaml.load_file(file.path(directory,"meta.yaml"))
      if(!exists("R_version",yaml_file)) {
        cat(yaml::as.yaml(list(R_version=strsplit(devtools::session_info()[[1]]$version," ")[[1]][3],
                               R_system=devtools::session_info()[[1]]$system,
                               R_packages=R_packages),
                          column.major = F,
                          indent.mapping.sequence = TRUE),
            file=file.path(directory,"meta.yaml"),
            append = TRUE)
      } else {
        yaml_file$R_packages <- NULL
        yaml_file$R_system <- NULL
        yaml_file$R_version <- NULL
        yaml_file$R_version <- strsplit(devtools::session_info()[[1]]$version," ")[[1]][3]
        yaml_file$R_system <- devtools::session_info()[[1]]$system
        yaml_file$R_packages <- R_packages
        if(exists("R_LIB_PATHS",yaml_file)) {
          yaml_file$R_LIB_PATHS <- NULL
          yaml_file$R_LIB_PATHS <- .libPaths()
        }
        
        # sort
        tag_names <-  sort(names(yaml_file))
        new_yaml_file <- list()
        for(i in 1:length(tag_names)) {
          new_yaml_file[[tag_names[i]]] <- yaml_file[[tag_names[i]]]
        }
        #write in the file
        cat(yaml::as.yaml(new_yaml_file,
                          column.major = F,
                          indent.mapping.sequence = TRUE),
            file=file.path(directory,"meta.yaml"))
      }
    } else {
      cat(yaml::as.yaml(list(R_LIB_PATHS=.libPaths(),
                             R_packages=R_packages,
                             R_system=devtools::session_info()[[1]]$system,
                             R_version=strsplit(devtools::session_info()[[1]]$version," ")[[1]][3]),
                        column.major = F,
                        indent.mapping.sequence = TRUE),
          file=file.path(directory,"meta.yaml"))
    }
  }
}