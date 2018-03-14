create.data.full <- function(raw.results.file,skipped.id.file) {
  ## read files
  input_cdd.data <- read.csv(raw.results.file)
  cdd.inds <- read.csv(skipped.id.file, header=F)
  cdd.inds <- unique(cdd.inds)
  
  # Create table cdd.data.all
  # check if exist column method
  if ("method" %in% colnames(input_cdd.data)) {
    # remember which rows don't have method cdd (maybe will be needed)
    not.cdd.method <- which(input_cdd.data$method != "cdd")
    # subset needed data (where method is cdd)
    input_cdd.data <- subset(input_cdd.data, method == "cdd") 
  }
  cdd.inds <- rbind(NA,cdd.inds)
  #add ID numbers to the input_cdd.data
  cdd.data.all <- cbind(cdd.inds,input_cdd.data)
  colnames(cdd.data.all)[1] <- "ID"
  
  no.cook.cov <- FALSE
  if(all(is.na(cdd.data.all$cook.scores[-1])) || (all(cdd.data.all$cov.ratios[-1]==0))) {
    no.cook.cov <- TRUE
  }
  
  # check if there are some cov ratios = 0 (it means that covariance step failed for specifick excluded ID)
  ID_failed_cov <- c()
  if(!all(is.na(cdd.data.all$cook.scores[-1])) && any(cdd.data.all$cov.ratios[-1]==0)) {
    ID_failed_cov <- cdd.data.all$ID[(cdd.data.all$cov.ratios==0)]
    if(is.na(ID_failed_cov[1])) {
      ID_failed_cov <- ID_failed_cov[-1]
    }
  }
  
  # output
  out <- list(input_cdd.data=input_cdd.data,
              cdd.data.all=cdd.data.all,
              no.cook.cov=no.cook.cov,
              ID_failed_cov=ID_failed_cov)
  return(out)
}
