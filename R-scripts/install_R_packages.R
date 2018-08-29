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
install.packages("Rcpp", lib=path)
install.packages("kableExtra", lib=path)
install.packages("caTools", lib = path)
install.packages("devtools", lib = path)
install.packages("expm", lib = path)
install.packages("formatR", lib = path)
install.packages("ggthemes", lib = path)
install.packages("gplots", lib = path)
install.packages("grid", lib = path)
install.packages("gridExtra", lib = path)
install.packages("MASS", lib = path)
install.packages("MCMCpack", lib = path)
install.packages("mvtnorm", lib = path)
install.packages("PEIP", lib = path)
install.packages("PerformanceAnalytics", lib = path)
install.packages("plotrix", lib = path)
install.packages("plyr", lib = path)
install.packages("RColorBrewer", lib = path)
install.packages("scales", lib = path)
install.packages("stats4", lib = path)
install.packages("tidyverse", lib = path)
install.packages("vpc", lib = path)
install.packages("xpose4", lib = path)
install.packages("xpose", lib = path)
install.packages("yaml", lib = path)
