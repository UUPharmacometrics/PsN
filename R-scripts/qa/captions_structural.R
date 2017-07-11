captions_structural <- function(idv_all,idv_name,which_idv,perc) {
  #captions
  #order TIME,TAD,PRED
  order <- c(idv_name,"TAD","PRED")
  vpc_captions_all <- c(paste0("VPC of observations (DV) vs. binned time (",idv_name,") before and after correcting for the estimated structural bias by ",idv_name," bin."),
                        "VPC of observations (DV) vs. binned time after dose (TAD) before and after correcting for the estimated structural bias by TAD bin.",
                        "VPC of observations (DV) vs. binned population predictions (PRED) before and after correcting for the estimated structural bias by PRED bin.")
  
  resmod_dofv_table_captions_all <- c(paste0("Dependent (DV) and independent variable (IDV) used for the structural bias estimation per time (",idv_name,") bin as well expected improvement in OFV from addressing remaining biases."),
                                      "Dependent (DV) and independent variable (IDV) used for the structural bias estimation per time after dose (TAD) bin as well expected improvement in OFV from addressing remaining biases.",
                                      "Dependent (DV) and independent variable (IDV) used for the structural bias estimation per population prediction (PRED) bin as well expected improvement in OFV from addressing remaining biases.")
  if(perc) {
    structural_bias_tables_captions_all <- c("Estimated structural bias in conditionally weighted residuals (CWRES) by time bin and corresponding bias on the percent conditional model prediction (%CPRED) scale, obtained by inversion of the FOCE-based CWRES equation.",
                                             "Estimated structural bias in conditionally weighted residuals (CWRES) by time after dose (TAD) bin and corresponding bias on the percent conditional model prediction (%CPRED) scale, obtained by inversion of the FOCE-based CWRES equation.",
                                             "Estimated structural bias in conditionally weighted residuals (CWRES) by population prediction (PRED) bin and corresponding bias on the percent conditional model prediction (%CPRED) scale, obtained by inversion of the FOCE-based CWRES equation.")
    structural_bias_plots_captions_all <- c(paste0("Estimated structural bias on the percent population prediction (%CPRED) scale vs. binned time (",idv_name,")."),
                                            "Estimated structural bias on the percent population prediction (%CPRED) scale vs. binned time after dose (TAD).",
                                            "Estimated structural bias on the percent population prediction (%CPRED) scale vs. binned population predictions (PRED).")
  } else {
    structural_bias_tables_captions_all <- c("Estimated structural bias in conditionally weighted residuals (CWRES) by time bin and corresponding bias on the conditional model prediction (CPRED) scale, obtained by inversion of the FOCE-based CWRES equation.",
                                             "Estimated structural bias in conditionally weighted residuals (CWRES) by time after dose (TAD) bin and corresponding bias on the conditional model prediction (CPRED) scale, obtained by inversion of the FOCE-based CWRES equation.",
                                             "Estimated structural bias in conditionally weighted residuals (CWRES) by population prediction (PRED) bin and corresponding bias on the conditional model prediction (CPRED) scale, obtained by inversion of the FOCE-based CWRES equation.")
    structural_bias_plots_captions_all <- c(paste0("Estimated structural bias on the population prediction (CPRED) scale vs. binned time (",idv_name,")."),
                                            "Estimated structural bias on the population prediction (CPRED) scale vs. binned time after dose (TAD).",
                                            "Estimated structural bias on the population prediction (CPRED) scale vs. binned population predictions (PRED).")
  }
  
  idv <- c()
  vpc_captions <- c()
  resmod_dofv_table_captions <- c()
  structural_bias_tables_captions <- c()
  structural_bias_plots_captions <- c()
  j <- 1
  for (i in 1:length(order)) {
    if(any(idv_all==order[i])) {
      idv[j] <- order[i]
      vpc_captions[j] <- vpc_captions_all[i]
      resmod_dofv_table_captions[j] <- resmod_dofv_table_captions_all[i]
      structural_bias_tables_captions[j] <- structural_bias_tables_captions_all[i]
      structural_bias_plots_captions[j] <- structural_bias_plots_captions_all[i]
      j <- j + 1
    }
  }
  
  #which of idv captions to print out
  nr_of_idv <- which(idv == which_idv)
  idv_resmod_dofv_table_captions <- resmod_dofv_table_captions[nr_of_idv]
  idv_structural_bias_tables_captions <- structural_bias_tables_captions[nr_of_idv]
  idv_structural_bias_plots_captions <- structural_bias_plots_captions[nr_of_idv]
  idv_vpc_captions <- vpc_captions[nr_of_idv]
  
  out <- list(idv_resmod_dofv_table_captions=idv_resmod_dofv_table_captions,
              idv_structural_bias_tables_captions=idv_structural_bias_tables_captions,
              idv_structural_bias_plots_captions=idv_structural_bias_plots_captions,
              idv_vpc_captions=idv_vpc_captions)
  return(out)
}