plot_2 <- function(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,noutlier,
                   flag,n.subjects,eta.names,outlying_criteria,outlier_id_row,
                   do_outlier_plot,model.filename) {
  # vector with values of theor_distance to print on the plot
  vector_theor_dist <- array(0,c(1,n.subjects))
  vector_theor_dist[n.subjects] <- theor_distance[n.subjects]
  out_distance <- c()
  i <- 1
  while(i < n.subjects){
    flag1 <- 0
    # create a vector of probability where a Chi^2 random variable with the given degrees of freedom is less than or equal to it
    ebe_npde_quant <- seq( (1-0.5)/(n.subjects-i) , ((n.subjects-i)-0.5)/(n.subjects-i) ,by=(1/(n.subjects-i)))
    # Compute the inverse Chi^2 distribution corresponding to a given probability ebe_npde_quant
    theor_distance <- chi2inv(ebe_npde_quant,length(eta.names)) #keep
    vector_theor_dist[n.subjects-i] <- round(theor_distance[n.subjects-i],digits=6) #keep
    out_distance[i] <- (theor_distance[(n.subjects-i)]-emp_distance_sort[(n.subjects-i)])/sqrt(2)
    if(out_distance[i] < outlying_criteria && flag==1){flag1 <- 1} #criteria to define outlying individual
    if(flag1 == 1){
      identityline <- seq(1,(n.subjects-i),by=1)
      emp_distance_sort1 <- emp_distance_sort[1:(n.subjects-i)]
      theor_distance1 <- theor_distance[1:(n.subjects-i)]
      vector_text <- array('',c(n.subjects-i,1))
      index_text <- index_emp_distance$ix[ n.subjects-i ] # plot the ID of the last 10 subjects (not 10, but 1 last subject)
      vector_text[n.subjects-i ] <- ebenpde_tmp$ID[index_text]
      if (do_outlier_plot){
        plot(emp_distance_sort1,theor_distance1, xlab = "Ordered robust empirical MD^2" ,
             ylab= "Theoretical ChiSq MD^2", main=paste('ChiSq Q-Q plot ',model.filename))
        text(emp_distance_sort1,theor_distance1,vector_text,col="red",pos=1)
        matplot(identityline,identityline,type="l",col="red",add=T)
      }
      noutlier <- noutlier + 1
      outlier_id_row[noutlier] <- index_text # create vector with row numbers of outliers (need for outlier table in the end)
    } else {flag<-0}
    i <- i+1 
  }
  out <- list(vector_theor_dist=vector_theor_dist,
              noutlier=noutlier,
              outlier_id_row=outlier_id_row,
              flag1=flag1,
              out_distance=out_distance)
  return(out)
}