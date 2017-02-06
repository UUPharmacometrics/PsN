#input
# file.path <- "C:/Users/svefr809/develope/frem/linearised_hamren/"
# inTable_frem <- paste0(file.path,'frem_ratio.csv')
# covdata <- paste0(file.path,'covdata.csv')
# pardata <- paste0(file.path,'pardata.csv')
# inTable_frem <- read.csv(inTable_frem, header = T, as.is = T)

# pardata <- read.csv(paste0(file.path,'pardata.csv'), header = T, as.is = T)

delete_prefix <- function(covdata) {
  covdata <- read.csv(covdata, header = T, as.is = T, stringsAsFactors = F)
  # names of covariate (names of first column in covdata input table, header = FALSE)
  covariate <- covdata$covname
  
  # delete "LN" prefixes (if they exist) from any of covariates
  for (i in 1:length(covariate)) {
    if (grepl("^LN", covariate[i])) {
      covariate[i] <- gsub("\\LN","",covariate[i])
    }
  }
  out <- list(covdata=covdata,covariate=covariate)
  return(out)
}
