# create table with all needed information for text
param_plot_text <- function(list_outTable) {
  list_outTable_text <- list()
  for (i in 1:length(list_outTable)) {
    outTable <- list_outTable[[i]]
    outTable <- outTable[-1,]
    outTable_text <- data.frame()
    V1 <- c(colnames(outTable)[9], outTable$COVARIATE, colnames(outTable)[8],outTable$MEAN, colnames(outTable)[5], outTable$EXPECTED)
    V05 <- rep(c(1:3),each = (nrow(outTable) +1) )
    outTable_text <- data.frame(V1,V05,V0 = factor(rep(c(1:(nrow(outTable) +1)),3),levels = c((nrow(outTable) +1):1)))
    list_outTable_text[[i]] <- outTable_text
  }
 return(list_outTable_text) 
}