#' Get additional information from the yaml file about PsN version, Nonmem version, R version and loaded R packages that can be added to the pdf report.
#' 
#' @param yaml_file Yaml file name
#' @return A list of three character elements (PsN_version, NONMEM_version and R_version) and one dataframe element (R_packages).
additional_info <- function(yaml_file) {
  yaml_list <- yaml.load_file(yaml_file)
  R_packages <- as.data.frame(devtools::session_info()[[2]])[,c("package","version","source")]
  out <- list(PsN_version=yaml_list$PsN_version,
              NONMEM_version=yaml_list$NONMEM_version,
              R_version=yaml_list$R_version,
              R_packages=R_packages)
  return(out)
}