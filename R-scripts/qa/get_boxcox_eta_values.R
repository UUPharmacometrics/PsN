get_eta_values <- function(working.directory,param_model) {
  
  #param_model should be "boxcox" or "tdist" only!
  
  phi_file_path <- file.path(working.directory,paste0(param_model,".phi"))
  if(file.exists(phi_file_path)) {
    phi_table <- read.table(phi_file_path,skip=1,header=T,stringsAsFactors = F)
    eta_table <- phi_table %>% select(grep("ETA",colnames(.))) %>% gather(ETA_name) %>% 
      mutate(ETA_name=sub("\\.","(",.$ETA_name)) %>% mutate(ETA_name=sub("[.]$",")",.$ETA_name)) 
    } else {
    eta_table <- data.frame()
  }
  return(eta_table)
}