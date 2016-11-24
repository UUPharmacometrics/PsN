empirical.distance <- function(ebenpde_obs,n.subjects) {
  # Calculate empirical distance (emp_distance)
  emp_distance <- array(0,c(n.subjects,1))
  id_iiv.eta.names <- list()
  for(i in 1:n.subjects){
    if(any(is.na(ebenpde_obs[i,]))) {
      col_nr <- which(is.na(ebenpde_obs[i,]))
      ebenpde_obs_i <- ebenpde_obs[i,-col_nr]
      iiv.eta.names <- colnames(ebenpde_obs[-col_nr])
    } else {
      ebenpde_obs_i <- ebenpde_obs[i,]
      iiv.eta.names <- colnames(ebenpde_obs)
    }
    mean_ebenpde <- array(0,c(1,length(iiv.eta.names)))
    var_ebenpde <- diag(1,length(iiv.eta.names),length(iiv.eta.names))
    emp_distance[i]<- (as.matrix(ebenpde_obs_i-mean_ebenpde)%*%as.matrix(solve(var_ebenpde))%*%as.matrix(t(ebenpde_obs_i-mean_ebenpde)))
    
    id_iiv.eta.names[[i]] <- iiv.eta.names
  }
  out <- list(emp_distance=emp_distance,
              id_iiv.eta.names=id_iiv.eta.names)
  return(out)
}