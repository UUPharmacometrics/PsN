get_eta_values <- function(working.directory,theta_values,param_model) {
  
  #param_model should be "boxcox" or "tdist" only!
  
  phi_file_path <- file.path(working.directory,paste0(param_model,".phi"))
  if(file.exists(phi_file_path)) {
    eta_table <- read.table(phi_file_path,skip=1,header=T,stringsAsFactors = F) %>%
      select(grep("ETA",colnames(.)))
    for(i in 1:ncol(eta_table)) {
      eta_name <- sub("\\(",".",theta_values$ETA[i])
      eta_name <- sub("\\)",".",eta_name)
      if(param_model=="boxcox") {
        lambda <- as.numeric(theta_values$Lambda[i])
        eta <- eta_table[,eta_name]
        eta_table[,eta_name] <- (exp(eta)^lambda -1)/lambda
      }
      if(param_model=="tdist") {
        deg_of_freedom <- as.numeric(theta_values$`Degrees of freedom`[i])
        eta <- eta_table[,eta_name]
        eta_table[,eta_name] <- eta*(1+((eta^2 + 1)/(4*deg_of_freedom))+((5*eta^4 + 16*eta^2 + 3)/(96*deg_of_freedom^2))
                                     +((3*eta^6 + 19*eta^4 + 17*eta^2 - 15)/(384*deg_of_freedom^3)))
      }
    }
    eta_table <- eta_table %>% gather(ETA_name) %>% 
      mutate(ETA_name=sub("\\.","(",.$ETA_name)) %>% mutate(ETA_name=sub("[.]$",")",.$ETA_name)) 
    } else {
    eta_table <- data.frame()
  }
  return(eta_table)
}