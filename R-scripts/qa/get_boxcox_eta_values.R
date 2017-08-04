get_boxcox_eta_values <- function(working.directory) {
  if(file.exists(file.path(working.directory,"boxcox.phi"))) {
    boxcox_phi_table <- read.table(file.path(working.directory,"boxcox.phi"),skip=1,header=T,stringsAsFactors = F)
    eta_table <- boxcox_phi_table %>% select(grep("ETA",colnames(.))) %>% gather(ETA_name) %>% 
      mutate(ETA_name=sub("\\.","(",.$ETA_name)) %>% mutate(ETA_name=sub("[.]$",")",.$ETA_name)) 
    } else {
    eta_table <- data.frame()
  }
  return(eta_table)
}