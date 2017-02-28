cook_cov_calcul <- function(raw.results.file,included.ids.file,est.param.names,show.warning=TRUE) {
  #read in needed files
  raw.results.data <- read.csv(raw.results.file)
  included.ids.data_input <- read.csv(included.ids.file,header=F)
  included.ids.data <- included.ids.data_input
  
  #get all ID numbers
  ID_unique <- unique(unlist(included.ids.data_input))
  ID <- sort(ID_unique)
  
  #estimated parameter data
  parameter_data <- raw.results.data[-1,est.param.names]
  rownames(parameter_data) <- NULL
  samples <- nrow(parameter_data)
  
  #check if there are some estimation step failures
  if(any(is.na(parameter_data))) {
    estimation_failure_rows <- which(rowSums(is.na(parameter_data))>0)
    parameter_data <- parameter_data[-estimation_failure_rows,]
    rownames(parameter_data) <- NULL
  }
  estimation_failures <- samples-nrow(parameter_data)
  
  if(estimation_failures > 0) {
    included.ids.data <- included.ids.data_input[-estimation_failure_rows,]
    rownames(included.ids.data) <- NULL
  }
  
  # show warning
  if(estimation_failures > 0) {
    if(show.warning) {
      message(paste0("WARNING! Estimation step failures: ",estimation_failures," of ",samples))
    }
  }

  #calcutate variances of each parameter and set variance values to P_orig
  P_orig <- c()
  P_orig_var <- c()
  for(i in 1:length(est.param.names)) {
    P_orig[i] <- mean(parameter_data[,i])
    P_orig_var[i] <- var(parameter_data[,i])
  }
  
  # variances of parameter values for each not included ID matrix
  var_param_no_ID <- array(NA,c(length(ID),length(est.param.names))) #parameter estimates where ID is not included
  colnames(var_param_no_ID) <- est.param.names
  var_param_no_ID <- as.data.frame(var_param_no_ID)
  mean_param_no_ID <- array(NA,c(length(ID),length(est.param.names))) #parameter estimates where ID is not included
  colnames(mean_param_no_ID) <- est.param.names
  mean_param_no_ID <- as.data.frame(mean_param_no_ID)
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
        mean_param_no_ID[i,j] <- mean(parameter_data_per_no_ID[,j])
      }
    }
  }
  
  #calculate cook scores and cov ratios
  cook.scores <- c()
  cov.ratios <- c()
  for(i in 1:length(ID)) {
    Pk <- mean_param_no_ID[i,]
    Pk <- matrix(as.numeric(Pk),length(est.param.names),1)
    Porig <- matrix(as.numeric(P_orig),length(est.param.names),1)
    cov_P_orig <- diag(P_orig_var,length(est.param.names),length(est.param.names))
    cook.scores[i] <- sqrt(t(Pk - Porig)%*%solve(cov_P_orig)%*%(Pk - Porig))
    if(any(is.na(var_param_no_ID[i,]))) {
      # cook.scores[i] <- NA
      cov.ratios[i] <- NA
    } else {
      Pk_var <- var_param_no_ID[i,]
      cov_Pk <- diag(Pk_var,length(est.param.names),length(est.param.names))
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
  
  # get parameter cook scores
  # formula = abs(p_i,j - p_orig,j)/se(p_orig,j)
  cook.param.data <- array(NA,c(length(ID),length(est.param.names)))
  for (i in 1:length(est.param.names)) {
    cook.param.names <- paste0("cook.par.",est.param.names)
  }
  colnames(cook.param.data) <- cook.param.names
  cook.param.data <- as.data.frame(cook.param.data)
  for (i in 1:length(est.param.names)) {
    for (j in 1:length(ID)) {
      cook.param.data[j,i] <- (abs(mean_param_no_ID[j,i] - P_orig[i])/sqrt(P_orig_var[i]))
    }
  }
  #owerall table
  cook.cov.data <- data.frame(ID,cook.scores=cook.scores,cov.ratios=cov.ratios,cook.param.data)
  #write.csv(cook.cov.data,"cook_cov_data.csv")
  
  out <- list(raw.results.data=raw.results.data,
              included.ids.data_input=included.ids.data_input,
              included.ids.data=included.ids.data,
              ID=ID,
              parameter_data=parameter_data,
              P_orig=P_orig,
              P_orig_var=P_orig_var,
              list_parameter_data_per_no_ID=list_parameter_data_per_no_ID,
              var_param_no_ID=var_param_no_ID,
              mean_param_no_ID=mean_param_no_ID,
              data_plots=data_plots,
              failed_cov_ID=failed_cov_ID,
              cook.param.data=cook.param.data,
              cook.param.names=cook.param.names,
              cook.cov.data=cook.cov.data,
              samples=samples,
              estimation_failures=estimation_failures)
  return(out)
}