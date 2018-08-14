# input arguments from command line
args <- commandArgs(TRUE)

if(length(args) > 1) {
  message(paste0("Error: Too many arguments passed! R library installation will not continue!"))
  quit()
}

if(length(args) == 1) {
  path <- as.character(args[1])
  #check if path exists
  if(is.na(file.info(path)$isdir)) {
    message(paste0("Error: Given path ",path," does not exist! R library installation will not continue!"))
    quit()
  }
  
  # set path where to install libraries
  .libPaths(path)
}

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.r-project.org"
  options(repos = r)
})
path <- .libPaths()[1]
message(paste0("Installing R libraries in ",path,"."))
# install packages
install.packages("Rcpp", dependencies=TRUE, lib=path)
install.packages("caTools", dependencies=TRUE,lib = path)
install.packages("devtools", dependencies=TRUE,lib = path)
install.packages("expm", dependencies=TRUE,lib = path)
install.packages("formatR", dependencies=TRUE,lib = path)
install.packages("ggthemes", dependencies=TRUE,lib = path)
install.packages("gplots", dependencies=TRUE,lib = path)
install.packages("grid", dependencies=TRUE,lib = path)
install.packages("gridExtra", dependencies=TRUE,lib = path)
install.packages("MASS", dependencies=TRUE,lib = path)
install.packages("MCMCpack", dependencies=TRUE,lib = path)
install.packages("mvtnorm", dependencies=TRUE,lib = path)
install.packages("PEIP", dependencies=TRUE,lib = path)
install.packages("PerformanceAnalytics", dependencies=TRUE,lib = path)
install.packages("plotrix", dependencies=TRUE,lib = path)
install.packages("plyr", dependencies=TRUE,lib = path)
install.packages("RColorBrewer", dependencies=TRUE,lib = path)
install.packages("reshape", dependencies=TRUE,lib = path)
install.packages("reshape2", dependencies=TRUE,lib = path)
install.packages("scales", dependencies=TRUE,lib = path)
install.packages("stats4", dependencies=TRUE,lib = path)
install.packages("tidyverse", dependencies=TRUE,lib = path)
install.packages("vpc", dependencies=TRUE,lib = path)
install.packages("xpose4", dependencies=TRUE,lib = path)
install.packages("xpose", dependencies=TRUE,lib = path)
install.packages("yaml", dependencies=TRUE,lib = path)
install.packages("withr", dependencies=TRUE, lib=path)

require(devtools)
require(withr)
withr::with_libpaths(new=path, devtools::install_version("kableExtra", version="0.7.0"))
