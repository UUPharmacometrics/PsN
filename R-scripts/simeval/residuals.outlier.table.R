outlier.table <- function(residual.outliers.file) {
  if(file.exists(residual.outliers.file)) {
    #get residual names
    residual_names <- read.table(residual.outliers.file,header=F,stringsAsFactors = F)[1,] %>%
      strsplit(.,split=",") %>%
      .[[1]]
    residual_names <- sort(residual_names[grep(paste(paste0("^",c("CWRES","IWRES"),"$"),collapse="|"),residual_names)])
    
    #get outlier table
    outlier_table_input <- read.csv(residual.outliers.file)
    if(nrow(outlier_table_input)<1){
      outlier_table <- data.frame(C = c("No residual outliers detected"))
      names(outlier_table) <- NULL
      outliers_count <- data.frame()
    } else {
      specif_col <- c("DV","PRED","CWRES","IWRES")
      outlier_table <- outlier_table_input
      #round values
      for (i in 1:length(specif_col)) {
        if(any(colnames(outlier_table)==specif_col[i])) {
          max_dec_dig <- max(nchar(sub('.*\\.','',outlier_table[,specif_col[i]])))
          if (max_dec_dig >= 4) {
            outlier_table[,specif_col[i]] <-sprintf("%.4f",outlier_table[,specif_col[i]])
          } else {
            sig_text <- paste0("%.",max_dec_dig,"f")
            outlier_table[,specif_col[i]] <- sprintf(sig_text,outlier_table[,specif_col[i]])
          }
        }
      }
      # subset all different ID numbers
      unique_id <- unique(outlier_table$ID)
      # count ountliers in each ID number
      if(any(colnames(outlier_table)=="OUTLIER.IWRES")) {
        select_col <- c("ID","OUTLIER.CWRES","OUTLIER.IWRES")
      } else {
        select_col <- c("ID","OUTLIER.CWRES")
      }
      outliers_count <- outlier_table %>%
        dplyr::select(select_col) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarize_at(select_col[-1],funs(sum(. == 1))) %>%
        as.data.frame()
      
      # Replace 0 to blank
      outliers_count[outliers_count == 0] <- ""
    }
  } else {
    outlier_table <- data.frame()
    outliers_count <- data.frame()
    residual_names <- NULL
  }
  
  #output
  out_list <- list(outlier_table=outlier_table,
                   outliers_count=outliers_count,
                   residual_names=residual_names)
  return(out_list)
}