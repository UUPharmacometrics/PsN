outlier.table <- function(residual.outliers.file) {
  outlierframe <- read.csv(residual.outliers.file)
  if(length(outlierframe$ID)<1){
    outlierframe <- data.frame(C = c("No outliers detected"))
    names(outlierframe) <- NULL
    outliers_count <- data.frame(ID=c(),OUTLIERS.IWRES=c(),OUTLIERS.CWRES=c())
  } else {
    #round values
    for (i in 3:6) {
      max_dec_dig <- max(nchar(sub('.*\\.','',outlierframe[,i])))
      if (max_dec_dig >= 4) {
        outlierframe[,i] <-sprintf("%.4f",outlierframe[,i])
      } else {
        sig_text <- paste0("%.",max_dec_dig,"f")
        outlierframe[,i] <- sprintf(sig_text,outlierframe[,i])
      }
    }
    # subset all different ID numbers
    unique_id <- unique(outlierframe$ID)
    # count ountliers in each ID number
    iwres.cwres_count <- c()
    outliers_count <- array("",c(length(unique_id),3))
    for (n in 1:length(unique_id)) {
      id_nr <- unique_id[n]
      data <- subset(outlierframe,ID == id_nr, select = c(ID,OUTLIER.IWRES,OUTLIER.CWRES))
      iwres.cwres_count <- as.vector(colSums(data[,2:3] == 1))
      outliers_count[n,1] <- id_nr
      outliers_count[n,2] <- iwres.cwres_count[1]
      outliers_count[n,3] <- iwres.cwres_count[2]
    }
    # Replace 0 to blank
    outliers_count[outliers_count == 0] <- ""
    # transform to data frame
    outliers_count <- as.data.frame(outliers_count)
    colnames(outliers_count) <- c("ID","OUTLIERS.IWRES","OUTLIERS.CWRES")
  }
  
  #output
  out_list <- list(outlierframe=outlierframe,
                   outliers_count=outliers_count)
  return(out_list)
}