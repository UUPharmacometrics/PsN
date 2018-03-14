get_eta.etat_values <- function(param_model,theta_values,quiet=F) {
  param_phi_file <- sub("(\\.[^.]+)$",".phi",param_model)
  if(file.exists(param_phi_file)) {
    #which etas to select
    nr <- gsub("\\D","",theta_values$ETA)
    
    eta_table <- read.table(param_phi_file,skip=1,header=T,stringsAsFactors = F) %>%
      dplyr::select(unique(grep(paste(paste0("ETA.",nr,"\\."),collapse="|"),colnames(.))))
    
    for(i in 1:ncol(eta_table)) {
      eta_name <- gsub("[\\(|\\)]","\\.",theta_values$ETA[i])
      if(grepl("boxcox",param_model)) {
        lambda <- as.numeric(theta_values$Lambda[i])
        eta <- eta_table[,eta_name]
        eta_table[,eta_name] <- (exp(eta)^lambda -1)/lambda
      }
      if(grepl("tdist",param_model)) {
        deg_of_freedom <- as.numeric(theta_values$`Degrees of freedom`[i])
        eta <- eta_table[,eta_name]
        eta_table[,eta_name] <- eta*(1+((eta^2 + 1)/(4*deg_of_freedom))+((5*eta^4 + 16*eta^2 + 3)/(96*deg_of_freedom^2))
                                     +((3*eta^6 + 19*eta^4 + 17*eta^2 - 15)/(384*deg_of_freedom^3)))
      }
    }
    eta_table <- eta_table %>% tidyr::gather(ETA_name) %>% 
      dplyr::mutate(ETA_name=sub("\\.","\\(",.$ETA_name)) %>% 
      dplyr::mutate(ETA_name=sub("[.]$","\\)",.$ETA_name)) 
  } else {
    if(!quiet) {
      message("WARNING: File ",param_phi_file," not found!")
    }
    eta_table <- data.frame()
  }
  return(eta_table)
}