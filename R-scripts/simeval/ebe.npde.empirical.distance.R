empirical.distance <- function(ebenpde_obs,n.subjects) {
  # Calculate empirical distance (emp_distance)
  emp_distance <- array(0,c(n.subjects,1))
  id_eta.names <- list()
  for(i in 1:n.subjects){
    if(any(is.na(ebenpde_obs[i,]))) {
      col_nr <- which(is.na(ebenpde_obs[i,]))
      ebenpde_obs_i <- ebenpde_obs[i,-col_nr]
      eta.names <- colnames(ebenpde_obs[-col_nr])
    } else {
      ebenpde_obs_i <- ebenpde_obs[i,]
      eta.names <- colnames(ebenpde_obs)
    }
    mean_ebenpde <- array(0,c(1,length(eta.names)))
    var_ebenpde <- diag(1,length(eta.names),length(eta.names))
    emp_distance[i]<- (as.matrix(ebenpde_obs_i-mean_ebenpde)%*%as.matrix(solve(var_ebenpde))%*%as.matrix(t(ebenpde_obs_i-mean_ebenpde)))
    
    id_eta.names[[i]] <- eta.names
  }
  out <- list(emp_distance=emp_distance,
              id_eta.names=id_eta.names)
  return(out)
}