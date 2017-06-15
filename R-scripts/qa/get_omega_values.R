get_omega_values <- function(ext_file){
  omega_table <- read.table(ext_file,header=TRUE,skip=1,stringsAsFactors = F) %>%
    filter(ITERATION==-1000000000)
  omega_table <- omega_table[grep("^OMEGA",colnames(omega_table))]
  omegas_v <- c()
  col.names_v <- c()
  omegas_c <- c()
  col.names_c <- c()
  for(i in 1:length(omega_table)) {
    numeration <- sub('.*OMEGA.','',colnames(omega_table[i]))
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
  omegas_var <- data.frame(omegas_v,stringsAsFactors = F)
  colnames(omegas_var) <- col.names_v
  omegas_cov <- data.frame(omegas_c,stringsAsFactors = F)
  colnames(omegas_cov) <- col.names_c
  
  return(list(omegas_var=omegas_var,
              omegas_cov=omegas_cov))
}