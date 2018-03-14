get_omega_values <- function(ext_file,omegas){
  omega_table <- get_initial_estimates_from_ext(filename=ext_file,select="omega")
  omegas_v <- c()
  col.names_v <- c()
  omegas_c <- c()
  col.names_c <- c()
  if(ncol(omega_table)>0) {
    col_names <- colnames(omega_table)[which(omega_table!=0)]
    omega_table <- as.data.frame(omega_table[,which(omega_table!=0)])
    colnames(omega_table) <- col_names
    for(i in 1:length(omega_table)) {
      numeration <- sub('.*OMEGA\\.','',colnames(omega_table[i]))
      numeration <- substr(numeration, 1, nchar(numeration)-1) # delete last element in string
      first <- sub('\\..*','',numeration)
      second <- sub('.*\\.','',numeration)
      if(first==second && omega_table[i]!=0) { #get omega values from the diagonals (which are not 0)
        omegas_v <- c(omegas_v,omega_table[i])
        col.names_v <- c(col.names_v,colnames(omega_table[i]))
      } 
      if(first!=second && omega_table[i]!=0){ #get omega values outside of the diagonals (which are not 0)
        omegas_c <- c(omegas_c,omega_table[i])
        col.names_c <- c(col.names_c,colnames(omega_table[i]))
      }
      
    }
  } else {
    omega_table <- data.frame()
  }
  
  if(length(omegas_v)>0) {
    omegas_var <- data.frame(omegas_v,stringsAsFactors = F)
    colnames(omegas_var) <- col.names_v
  } else {
    omegas_var <- data.frame()
  }
  if(length(omegas_c)>0) {
    omegas_cov <- data.frame(omegas_c,stringsAsFactors = F)
    colnames(omegas_cov) <- col.names_c
  } else {
    omegas_cov <- data.frame()
  }
  
  #what to print out
  if(omegas=="all") {
    omega_values <- omega_table
  } else if(omegas=="var"){
    omega_values <- omegas_var
  } else if(omegas=="cov"){
    omega_values <- omegas_cov
  }
  
  return(omega_values)
}