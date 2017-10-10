boxplot_i_ofv_res <- function(list_i_ofv_res,n.subjects,add_title=TRUE) {
  # unlist
  iOFV_res_ord <- list_i_ofv_res$iOFV_res_ord
  id_sorted <- list_i_ofv_res$id_sorted
  result <- list_i_ofv_res$result
  vector_text <- list_i_ofv_res$vector_text
  
  # From which position sorted res medians starts crossing zero line 
  #(need for drawing vertical line in the plot)
  flag <- 0
  cross <- 0
  for (i in 1:n.subjects) {
    if(flag == 0){
      if(result$x[i] > 0){
        cross <- i
        flag <- 1
      }
    } 
  }
  
  #make a plot
  boxplot(iOFV_res_ord, outline=FALSE, range=0.001,names=as.character(id_sorted),xlab="ID",ylab="iOFV RES")
  #this are indices in plotted sorted arr which are outside lim. always the last few, if any
  if (any(vector_text != "")) {
    text(1:n.subjects,result$x-1, paste("", vector_text),col="red") # mark outliers
  } 
  abline(h=0, lwd=2, lty=3, col="black")
  abline(h=-3, lwd=2, lty=3, col="black") # mark outlier line
  abline(h=3, lwd=2, lty=3, col="black") # mark outlier line
  if(add_title) {
    title("iOFV RES")
  }
}