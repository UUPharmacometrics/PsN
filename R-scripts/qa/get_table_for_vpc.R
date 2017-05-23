get_tables_for_vpc <- function(obs_table,obs_extra_table,sim_table,idv_all) {
  make_vpc <- (file.exists(obs_table) && file.exists(obs_extra_table) && file.exists(sim_table))
  if(make_vpc) {
    obs <- read_table_nm_with_NA(obs_table,delete_NA_rows = F)
    extra_obs <- read_table_nm_with_NA(obs_extra_table,delete_NA_rows = F)
    if(all(colnames(obs)!="DV")) {
      obs <- cbind(obs,"DV"=extra_obs[,"DV"])
    }
    if(all(colnames(obs)!="MDV")) {
      obs <- cbind(obs,"MDV"=extra_obs[,"MDV"])
    }
    add_cols <- obs[,c(idv_all)]
    sim <- read_table_nm(sim_table)
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