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
