cook_cov_calcul <- function(raw.results.file,included.ids.file,N.ESTIMATED.PARAMS) {
  #read in needed files
  raw.results.data <- read.csv(raw.results.file)
  included.ids.data <- read.csv(included.ids.file,header=F)
  
  #get all ID numbers
  ID_unique <- unique(unlist(included.ids.data))
  ID <- sort(ID_unique)
  
  #parameter names
  ofv_column_nr <- grep("^ofv$",colnames(raw.results.data))
  parameter_names <- colnames(raw.results.data[,(ofv_column_nr+1):(ofv_column_nr+N.ESTIMATED.PARAMS)])
  parameter_data <- raw.results.data[-1,parameter_names]
  rownames(parameter_data) <- NULL
  
  #calcutate variances of each parameter and set variance values to P_orig
  P_orig <- c()
  for(i in 1:N.ESTIMATED.PARAMS) {
    P_orig[i] <- var(parameter_data[,i])
  }
  
  # variances of parameter values for each not included ID matrix
  var_param_no_ID <- array(NA,c(length(ID),N.ESTIMATED.PARAMS)) #parameter estimates where ID is not included
  colnames(var_param_no_ID) <- parameter_names
  var_param_no_ID <- as.data.frame(var_param_no_ID)
  list_covar_var_matr <- list()
  list_parameter_data_per_no_ID <- list()
  
  for(i in 1:length(ID)) {
    #subset rows which NOT includes specifick ID number
    if(any(apply(included.ids.data,MARGIN=1,function(x) all(x!=ID[i])))) {
      no_ID_data <- included.ids.data[apply(included.ids.data,MARGIN=1,function(x) all(x!=ID[i])),] #margin=1 means rows
      no_ID_data_rownames <- rownames(no_ID_data)
      parameter_data_per_no_ID <- parameter_data[no_ID_data_rownames,]
      rownames(parameter_data_per_no_ID) <- NULL
      # list_covar_var_matr[[i]] <- cov(parameter_data_per_no_ID)
      list_parameter_data_per_no_ID[[i]] <- parameter_data_per_no_ID 
      for(j in 1:ncol(parameter_data_per_no_ID)) {
        var_param_no_ID[i,j] <- var(parameter_data_per_no_ID[,j])
      }
    }
  }
  
  #calculate cook scores and cov ratios
  cook.scores <- c()
  cov.ratios <- c()
  for(i in 1:length(ID)) {
    if(any(is.na(var_param_no_ID[i,]))) {
      cook.scores[i] <- NA
      cov.ratios[i] <- NA
    } else {
      Pk <- var_param_no_ID[i,]
      cov_Pk <- diag(Pk,N.ESTIMATED.PARAMS,N.ESTIMATED.PARAMS)
      cov_P_orig <- diag(P_orig,N.ESTIMATED.PARAMS,N.ESTIMATED.PARAMS)
      # cov_Pk <- list_covar_var_matr[[i]]
      # cov_P_orig <- cov(parameter_data)
      Pk <- matrix(as.numeric(Pk),N.ESTIMATED.PARAMS,1)
      Porig <- matrix(as.numeric(P_orig),N.ESTIMATED.PARAMS,1)
      Pk_Porig <- Pk - P_orig
      cook.scores[i] <- sqrt(t(Pk - Porig)%*%solve(cov_P_orig)%*%(Pk - Porig))
      if(any(P_orig==0)) {
        cov.ratios[i] <- NA
      } else {
        cov.ratios[i] <- sqrt(det(cov_Pk)/(det(cov_P_orig)))
      }
    }
  }
  
  # data for plots
  data_plots <- data.frame(ID,cook.scores=cook.scores,cov.ratios=cov.ratios)
  
  # check if some ID failed cov ratios
  failed_cov_ID <- c()
  if (any(is.na(cov.ratios))) {
    failed_cov_rows <- which(is.na(cov.ratios))
    failed_cov_ID <- ID[failed_cov_rows]
    data_plots <- data_plots[-failed_cov_rows,]
    rownames(data_plots) <- NULL
  } 
  
  out <- list(raw.results.data=raw.results.data,
              included.ids.data=included.ids.data,
              ID=ID,
              parameter_names=parameter_names,
              parameter_data=parameter_data,
              P_orig=P_orig,
              list_parameter_data_per_no_ID=list_parameter_data_per_no_ID,
              var_param_no_ID=var_param_no_ID,
              data_plots=data_plots,
              failed_cov_ID=failed_cov_ID)
  return(out)
  
}