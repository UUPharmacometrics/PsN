# input arguments from command line
args <- commandArgs(TRUE)

if(length(args) < 1) {
  message("Error: Please specify path where to install R libraries! R library installation will not continue!")
  quit()
}
path <- as.character(args[1])
#check if path exists
if(is.na(file.info(path)$isdir)) {
  message(paste0("Error: Given path ",path," does not exist! R library installation will not continue!"))
  quit()
}

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.r-project.org"
  options(repos = r)
})

# set path where to install libraries
.libPaths(path)
# install packages
install.packages("caTools", dependencies=TRUE)
install.packages("expm", dependencies=TRUE)
install.packages("gplots", dependencies=TRUE)
install.packages("grid", dependencies=TRUE)
install.packages("gridExtra", dependencies=TRUE)
install.packages("MASS", dependencies=TRUE)
install.packages("MCMCpack", dependencies=TRUE)
install.packages("rmarkdown",dependencies = TRUE)
install.packages("ggthemes", dependencies=TRUE)
install.packages("PEIP", dependencies=TRUE)
install.packages("PerformanceAnalytics", dependencies=TRUE)
install.packages("plotrix", dependencies=TRUE)
install.packages("plyr", dependencies=TRUE)
install.packages("RColorBrewer", dependencies=TRUE)
install.packages("reshape", dependencies=TRUE)
install.packages("reshape2", dependencies=TRUE)
install.packages("scales", dependencies=TRUE)
install.packages("stats4", dependencies=TRUE)
install.packages("tidyverse", dependencies=TRUE)
install.packages("xpose4", dependencies=TRUE)
install.packages("ztable", dependencies=TRUE)
install.packages("devtools", dependencies=TRUE)
install.packages("yaml", dependencies=TRUE)
devtools::install_github("UUPharmacometrics/xpose")
devtools::install_github("ronkeizer/vpc")