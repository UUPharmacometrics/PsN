format_dofv_data <- function(dofv.raw.results.file,raw.results.file,est.param.names) {
  
  # Read in BOOT results (dOFV)
  raw.results.data <- read.csv(raw.results.file)
  dofv.raw.results.data <- read.csv(dofv.raw.results.file,stringsAsFactors=F)
  dofv.data <- dofv.raw.results.data[-1,] # remove first row=original data
  names(dofv.data)[names(dofv.data) %in% c("model","bs_data_id")] <- c("modelB","model") # to match rawres file
  dofv.data$model <- as.numeric(dofv.data$model)
  
  boot <- dplyr::left_join(raw.results.data,dofv.data[,c("deltaofv","model")])  # keep stats from bootstrap (terminated runs etc) to investigate dOFV distribution
  boot <- boot[order(boot$deltaofv),]  
  boot$rownames <- seq(nrow(boot))/nrow(boot)
  boot$METHOD <- "BOOT"
  
  boot_term <- dplyr::filter(boot, METHOD=="BOOT" & minimization_successful==1) # only runs with min successful
  boot_term$rownames <- seq(nrow(boot_term))/nrow(boot_term)
  boot_term$minimization_successful <- "yes"
  boot$minimization_successful <- "yes+no"
  
  # Create reference chisquare (for dOFV)
  ref <- data.frame("deltaofv"=qchisq(seq(0,0.99,0.01),df=length(est.param.names)),"rownames"=seq(0,0.99,0.01),"METHOD"="REF")
  
  # Merge and format datasets
  all <- plyr::rbind.fill(boot,boot_term,ref)
  all$minimization_successful[is.na(all$minimization_successful)] <- "yes"
  df_est   <- plyr::ddply(all,.(METHOD,minimization_successful), summarise, "df"=round(mean(deltaofv,na.rm=T),1))  # get df for each distribution
  df_est[df_est$METHOD=="REF",]$df <- length(est.param.names) # replace with true df to avoid random noise
  
  out <- list(dofv.raw.results.data=dofv.raw.results.data,
              raw.results.data=raw.results.data,
              dofv.data=dofv.data,
              boot=boot,
              boot_term=boot_term,
              ref=ref,
              all=all,
              df_est=df_est)
  
  return(out)
}
