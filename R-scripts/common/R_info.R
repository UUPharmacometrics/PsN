R_info <- function(directory) {
  #get R version
  if(file.exists(file.path(directory,"meta.yaml"))) {
    yaml_file <- yaml.load_file(file.path(directory,"meta.yaml"))
    if(!exists("R_version",yaml_file)) {
      cat(yaml::as.yaml(list(R_version=strsplit(devtools::session_info()[[1]]$version," ")[[1]][3],
                             R_system=devtools::session_info()[[1]]$system,
                             R_packages=as.data.frame(devtools::session_info()[[2]])[,c("package","version","source")]),
                        column.major = F,
                        indent.mapping.sequence = TRUE),
          file=file.path(directory,"meta.yaml"),
          append = TRUE)
    }
  } else {
    cat(yaml::as.yaml(list(R_version=strsplit(devtools::session_info()[[1]]$version," ")[[1]][3],
                           R_system=devtools::session_info()[[1]]$system,
                           R_packages=as.data.frame(devtools::session_info()[[2]])[,c("package","version","source")]),
                      column.major = F,
                      indent.mapping.sequence = TRUE),
        file=file.path(directory,"meta.yaml"))
  }
}