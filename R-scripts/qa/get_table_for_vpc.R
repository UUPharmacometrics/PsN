get_tables_for_vpc <- function(obs_table,obs_extra_table,sim_table,model.filename,idv) {
  vpc_files_exist <- (file.exists(obs_table) && file.exists(obs_extra_table) && file.exists(sim_table))
  if(vpc_files_exist) {
    obs <- vpc::read_table_nm(obs_table)
    extra_obs <- vpc::read_table_nm(obs_extra_table)
    obs <- cbind(obs,"DV"=extra_obs[,"DV"],"MDV"=extra_obs[,"MDV"])
    add_cols <- obs[,c(idv)]
    sim <- vpc::read_table_nm(sim_table)
    sim <- cbind(sim,add_cols)
    out <- list(obs=obs,
                sim=sim,
                vpc_files_exist=vpc_files_exist)
  } else {
    out <- list(vpc_files_exist=vpc_files_exist)
  }
  return(out)
}