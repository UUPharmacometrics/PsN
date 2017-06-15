get_tables_for_vpc <- function(obs_table,obs_extra_table,sim_table,idv_all) {
  make_vpc <- (file.exists(obs_table) && file.exists(obs_extra_table) && file.exists(sim_table))
  if(make_vpc) {
    obs <- read_nm_tables(obs_table)
    extra_obs <- read_nm_tables(obs_extra_table)
    if(all(colnames(obs)!="DV")) {
      obs <- cbind(obs,"DV"=extra_obs[,"DV"])
    }
    if(all(colnames(obs)!="MDV")) {
      obs <- cbind(obs,"MDV"=extra_obs[,"MDV"])
    }
    if(any(obs$MDV==1)) {
      obs <- obs[which(obs$MDV==0),]
      rownames(obs) <- NULL
    }
    if(any(extra_obs$MDV==1)) {
      extra_obs <- extra_obs[which(extra_obs$MDV==0),]
      rownames(extra_obs) <- NULL
    }
    
    add_cols <- obs[,c(idv_all)]
    sim <- read_nm_tables(sim_table)
    sim_names <- colnames(sim)

    sim <- cbind(sim,add_cols)
    colnames(sim) <- c(sim_names,idv_all)
    out <- list(obs=obs,
                sim=sim,
                make_vpc=make_vpc)
  } else {
    out <- list(make_vpc=make_vpc)
  }
  return(out)
}