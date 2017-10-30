get_eta_transf_table <- function(input_table,seq_length.out=1000) {
  fig_height <- 15
  if(any(grepl('ETA',input_table$ETA)) && (any(colnames(input_table)=="Lambda") || any(colnames(input_table)=="Degrees of freedom")))  {
    make_eta_transf_plot <- TRUE
    
    for(i in 1:nrow(input_table)) {
      sd <- as.numeric(input_table[i,3])
      if(any(colnames(input_table)=="Lambda")) {
        lambda <- as.numeric(input_table$Lambda[i])
      } else {
        deg_of_freedom <- as.numeric(input_table$`Degrees of freedom`[i])
      }
      eta <- qnorm(seq(1E-7, 1-1E-7, length.out = seq_length.out), sd=sd)
      density <- dnorm(eta,sd=sd)
      if(any(colnames(input_table)=="Lambda")) {
        ETAT <- (exp(eta)^lambda -1)/lambda
      } else {
        ETAT <- eta*(1+((eta^2 + 1)/(4*deg_of_freedom))+((5*eta^4 + 16*eta^2 + 3)/(96*deg_of_freedom^2))
                     +((3*eta^6 + 19*eta^4 + 17*eta^2 - 15)/(384*deg_of_freedom^3)))
      }
      table_per_eta <- data.frame(ETA_name=input_table$ETA[i],ETA=eta,density=density,ETAT=ETAT,stringsAsFactors = F)
      table_per_eta <- table_per_eta %>% tidyr::gather(type,eta,-ETA_name,-density)
      if(i > 1) {
        eta_transf_table <- rbind(eta_transf_table,table_per_eta)
      } else {
        eta_transf_table <- table_per_eta
      }
    }
    
    #count unique ETAs
    if(length(unique(eta_transf_table$ETA_name)) <= 8) {
      fig_height <- 6.25
    }
    out <- list(eta_transf_table=eta_transf_table,
                make_eta_transf_plot=make_eta_transf_plot,
                fig_height=fig_height)
  } else {
    make_eta_transf_plot <- FALSE
    out <- list(make_eta_transf_plot=make_eta_transf_plot,
                fig_height=fig_height)
  }
  
  return(out)
}