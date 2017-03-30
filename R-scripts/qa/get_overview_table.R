get_overview_table <- function(overview_list,rgroup) {
  n.rgroup <- c()
  for(i in 1:length(overview_list)) {
    n.rgroup[i] <- nrow(overview_list[[i]])
    if(i == 1) {
      overview_table <- overview_list[[i]]
    } else {
      overview_table <- rbind(overview_table,overview_list[[i]])
    }
  }
  rownames(overview_table) <- NULL
  return(list(overview_table=overview_table,
              n.rgroup=n.rgroup))
}