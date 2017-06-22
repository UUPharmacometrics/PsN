get_overview_table <- function(overview_list,rgroup) {
  n.rgroup <- c()
  for(i in 1:length(overview_list)) {
    if(ncol(overview_list[[i]])!=3) {
      # for(j in 1:nrow(overview_list[[i]])) {
      #   overview_list[[i]][j,1] <- paste("      ",(overview_list[[i]][j,1]))
      # }
      overview_list[[i]] <- cbind(overview_list[[i]],rep("",nrow(overview_list[[i]])))
      colnames(overview_list[[i]]) <- c("","dOFV","Add.params")
    }
    n.rgroup[i] <- nrow(overview_list[[i]])
    if(i == 1) {
      overview_table <- overview_list[[i]]
    } else {
      overview_table <- rbind(overview_table,overview_list[[i]])
    }
  }
  rownames(overview_table) <- NULL
  colnames(overview_table) <- c("","dOFV","Additional parameters")
  return(list(overview_table=overview_table,
              n.rgroup=n.rgroup))
}