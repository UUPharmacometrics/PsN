plot_1 <- function(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                   out_distance,n.subjects,outlying_criteria,do_outlier_plot,model.filename) {
 
  noutlier <- 0
  flag <- 0
  identityline <- seq(1,n.subjects,by=1) #only for plot
  outlier_id_row <- c()
  if(out_distance < outlying_criteria){  #criteria to define outlying individual
    flag <- 1
    vector_text <- array('',c(n.subjects,1))
    index_text <- index_emp_distance$ix[ n.subjects ]
    outlier_ID <- ebenpde_tmp$ID[index_text]
    vector_text[ n.subjects ] <- outlier_ID
    if (do_outlier_plot){
      plot(emp_distance_sort,theor_distance, xlab = "Ordered robust empirical MD^2" ,
           ylab= "Theoretical ChiSq MD^2", main=paste('ChiSq Q-Q plot ',model.filename))
      text(emp_distance_sort,theor_distance,vector_text,col="red",pos=1) # plot the ID of the outlying individual
      matplot(identityline,identityline,type="l",col="red",add=T) 
    }
    noutlier <- noutlier + 1
    outlier_id_row[noutlier] <- index_text # create vector with row numbers of outliers (need for outlier table in the end)
    
    out <- list(noutlier=noutlier,
                flag=flag,
                outlier_id_row=outlier_id_row,
                outlier_ID=outlier_ID,
                identityline=identityline,
                vector_text=vector_text)
  return(out)  
  } else {
    if (do_outlier_plot){
      plot(emp_distance_sort,theor_distance, xlab = "Ordered robust empirical MD^2" ,
           ylab= "Theoretical ChiSq MD^2", main=paste('ChiSq Q-Q plot ',model.filename))
      matplot(identityline,identityline,type="l",col="red",add=T)
    }
    out <- list(noutlier=noutlier,
                flag=flag,
                outlier_id_row=outlier_id_row,
                identityline=identityline)
  return(out)
  }
}