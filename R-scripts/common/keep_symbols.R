keep_symbols <- function(table,type) {
  #add needed symbols for ztable
  for (j in 1:ncol(table)) {
    for (i in 1:nrow(table)) {
      if(type=="html") {
        table[i,j] <- gsub("-","$-$ ",table[i,j])
      } else {
        table[i,j] <- gsub("-","$-$",table[i,j])
      }
      table[i,j] <- gsub("_",".",table[i,j])
    }
  }
  
  #change column names
  col_names <- c()
  for(i in 1:ncol(table)) {
    if(type=="html") {
      col_names[i] <- gsub("-","$-$ ",colnames(table)[i])
    } else {
      col_names[i] <- gsub("-","$-$",colnames(table)[i])
    }
    col_names[i] <- gsub("%","\\\\%",colnames(table)[i])
  }
  colnames(table) <- col_names
  return(table)
}