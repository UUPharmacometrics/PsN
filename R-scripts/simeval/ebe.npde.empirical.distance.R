empirical.distance <- function(ebenpde_obs,n.subjects,n.eta) {
  # Calculate empirical distance (emp_distance)
  emp_distance <- array(0,c(n.subjects,1))
  mean_ebenpde <- array(0,c(1,n.eta))
  var_ebenpde <- diag(1,n.eta,n.eta)
  for(i in 1:n.subjects){ 
    emp_distance[i]<- (as.matrix(ebenpde_obs[i,]-mean_ebenpde)%*%as.matrix(solve(var_ebenpde))%*%as.matrix(t(ebenpde_obs[i,]-mean_ebenpde)))
  }
  return(emp_distance)
}
