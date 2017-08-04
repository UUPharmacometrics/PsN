hist_of_cdd_dofv <- function(data) {
  if(length(data)>0) {
    hist(data,
         breaks=20,
         main="",
         xlab="dOFV")
    abline(v=3.84,col="red")
    axis(1, at=3.84,labels=3.84,col="red",col.axis="red")
  }
}