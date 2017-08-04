tables_for_boxcox_shape <- function(boxcox_lambdas_table,seq_length.out=1000) {
  fig_height_boxcox <- 15
  if(any(grepl('ETA',boxcox_lambdas_table$ETA)))  {
    make_boxcox_shape_plot <- TRUE
    
    for(i in 1:nrow(boxcox_lambdas_table)) {
      sd <- as.numeric(boxcox_lambdas_table[i,3])
      lambda <- as.numeric(boxcox_lambdas_table$Lambda[i])
      eta <- qnorm(seq(1E-7, 1-1E-7, length.out = seq_length.out), sd=sd)
      density <- dnorm(eta,sd=sd)
      ETAT <- (exp(eta)^lambda -1)/lambda
      table_per_eta <- data.frame(ETA_name=boxcox_lambdas_table$ETA[i],ETA=eta,density=density,ETAT=ETAT,stringsAsFactors = F)
      table_per_eta <- table_per_eta %>% gather(type,eta,-ETA_name,-density)
      if(i > 1) {
        boxcox_shape_table <- rbind(boxcox_shape_table,table_per_eta)
      } else {
        boxcox_shape_table <- table_per_eta
      }
    }
    
    #count unique ETAs
    if(length(unique(boxcox_shape_table$ETA_name)) <= 8) {
      fig_height_boxcox <- 7
    }
    out <- list(boxcox_shape_table=boxcox_shape_table,
                make_boxcox_shape_plot=make_boxcox_shape_plot,
                fig_height_boxcox=fig_height_boxcox)
  } else {
    make_boxcox_shape_plot <- FALSE
    out <- list(make_boxcox_shape_plot=make_boxcox_shape_plot,
                fig_height_boxcox=fig_height_boxcox)
  }
  
  return(out)
}