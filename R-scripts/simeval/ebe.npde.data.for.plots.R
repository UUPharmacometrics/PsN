data.for.plots <- function(emp_distance,n.subjects,eta.names) {
  # Sort emp_distance values and remember rows, where they were in the beginning
  index_emp_distance <- sort(emp_distance,index.return=TRUE)
  emp_distance_sort <- index_emp_distance$x # take only sorted values
  # create a vector of probability where a Chi^2 random variable with the given degrees of freedom is less than or equal to it 
  ebe_npde_quant <- seq( (1-0.5)/n.subjects , (n.subjects-0.5)/n.subjects ,by=(1/n.subjects))
  # Compute the inverse Chi^2 distribution corresponding to a given probability ebe_npde_quant
  theor_distance <- PEIP::chi2inv(ebe_npde_quant,length(eta.names))
  out_distance <- (theor_distance[n.subjects]-emp_distance_sort[n.subjects])/sqrt(2)
  out <- list(index_emp_distance=index_emp_distance,
              emp_distance_sort=emp_distance_sort,
              theor_distance=theor_distance,
              out_distance=out_distance,
              ebe_npde_quant=ebe_npde_quant)
  return(out)
}