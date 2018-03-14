i_ofv_res <- function(all.iofv.file,n.subjects,samples,show.warning=TRUE) {
  
  # iOFV RES
  all.iOFV_sim <- read.csv(all.iofv.file)
  
  # get sample size and number of subjects from the data frame dimentions
  if (missing(samples)) {
    samples <- ncol(all.iOFV_sim)-2
  }
  if (missing(n.subjects)) {
    n.subjects <- nrow(all.iOFV_sim)
  }
  
  # delete individuals with all 0 values (save ID numbers with deleted individuals)
  deleted_rows <- c()
  ID_deleted_ofv <- c()
  for (i in 1:n.subjects) {
    if(all(all.iOFV_sim[i,-1] == 0)) {
      deleted_rows <- c(deleted_rows,i)
      ID_deleted_ofv <- c(ID_deleted_ofv,all.iOFV_sim$ID[i])
    }
  }
  
  #print warning messages
  if(length(deleted_rows) > 0) {
    if(length(ID_deleted_ofv) > 1) {
      ID_deleted_ofv_text <- c()
      for (i in 1:length(ID_deleted_ofv)) {
        if(i == 1) {
          ID_deleted_ofv_text <- ID_deleted_ofv[i]
        } else {
          ID_deleted_ofv_text <- paste0(ID_deleted_ofv_text,", ",ID_deleted_ofv[i])
        }
      }
      if(show.warning) {
        message(paste0("WARNING! No data for ID numbers ",ID_deleted_ofv_text," in the csv file \"",all.iofv.file,"\"."))
      }
    } else {
      if(show.warning) {
        message(paste0("WARNING! No data for ID number ",ID_deleted_ofv," in the csv file \"",all.iofv.file,"\"."))
      }
    }
    all.iOFV_sim <- all.iOFV_sim[-deleted_rows,]
    rownames(all.iOFV_sim ) <- NULL
    n.subjects <- n.subjects - length(deleted_rows)
  }
  
  iOFV_obs <- all.iOFV_sim$ORIGINAL
  
  #find res medians and sort them
  iOFV_res <- array(0,c(samples,n.subjects))
  iOFV_res_median <- array(0,c(n.subjects,1))
  for (i in 1:n.subjects) {
    iOFV_sim <- all.iOFV_sim[i,3:(samples+2)]
    iOFV_sim <- iOFV_sim[!is.na(iOFV_sim)]
    len <- length(iOFV_sim)
    for (j in 1:len) {
      iOFV_res[j,i] <- (iOFV_obs[i]-iOFV_sim[j])/sd(iOFV_sim) # sd() is a standard deviation  
    }   
    iOFV_res_median[i,1] <- median(iOFV_res[,i])
  }
  result <- sort(iOFV_res_median,index.return=TRUE)
  
  # find the place of each ID number as it is sorted in the variable result
  # order iOFV_res values for the right ID numbers
  iOFV_res_ord <- array(0,c(samples,n.subjects))
  id_sorted <- array(0,c(n.subjects,1))
  for (i in 1:n.subjects) {
    iOFV_res_ord[,i] <- iOFV_res[,result$ix[i]]
    id_sorted[i] <- all.iOFV_sim$ID[result$ix[i]]  
  }
  
  # create a text for the plot
  vector_text <- array('',c(n.subjects,1))
  #this are indices in plotted sorted are which are outside lim. always the last few, if any
  if (any(abs(iOFV_res_median[result$ix]) > 3)) {
    index_text <- which(abs(iOFV_res_median[result$ix]) > 3)
    outlier_median <- iOFV_res_median[result$ix][index_text] # medians which abs are > than standard deviation
    outlier_ID <- id_sorted[index_text]
    vector_text[index_text] <- outlier_ID
    # save in one data frame all outliers ID numbers and the value which formed the basis
    # for classifying the ID as outlier 
    ofv_outliertable <- data.frame(ID=outlier_ID,MEDIAN=outlier_median)
  } else {
    outlier_ID <- NULL
    outlier_median <- NULL
    ofv_outliertable <- data.frame()
  }

  #output
  out <- list(all.iOFV_sim=all.iOFV_sim,
              iOFV_res=iOFV_res,
              result=result,
              iOFV_res_ord=iOFV_res_ord,
              id_sorted=id_sorted,
              outlier_ID=outlier_ID,
              ofv_outliertable=ofv_outliertable,
              outlier_median=outlier_median,
              vector_text=vector_text,
              n.subjects=n.subjects,
              ID_deleted_ofv=ID_deleted_ofv)
  return(out)
}