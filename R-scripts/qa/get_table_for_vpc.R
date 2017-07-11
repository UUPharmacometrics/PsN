get_tables_for_vpc <- function(obs_table,obs_extra_table,sim_table,sim_extra_table,idv_all,dvid,dvid_name) {
  make_vpc <- (file.exists(obs_table) && file.exists(obs_extra_table) && file.exists(sim_table))
  if(dvid!="NA"){
    make_vpc <- make_vpc && file.exists(sim_extra_table)
  }
  
  if(make_vpc) {
    obs <- read_nm_tables(obs_table)
    extra_obs <- read_nm_tables(obs_extra_table)
    
    #choose DVID
    if(dvid!='NA') {
      dvid_column_nr <- which(colnames(obs)== dvid_name)
      obs <- obs[which(obs[,dvid_column_nr] == dvid),]
      dvid_column_nr <- which(colnames(extra_obs)== dvid_name)
      extra_obs <- extra_obs[which(extra_obs[,dvid_column_nr] == dvid),]
    }
    
    if(any(colnames(extra_obs)=="DV")) {
      if(all(colnames(obs)!="DV")) {
        obs <- cbind(obs,"DV"=extra_obs[,"DV"])
      }
    } else {
      make_vpc <- FALSE # problem with synonyms in PsN, if in model is DV=MYDV, it will produce table with column MYDV, even if in $TABLE is DV
    }

    if(any(colnames(extra_obs)=="MDV")) {
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
    }
    add_cols <- obs[,c(idv_all)]
    sim <- read_nm_tables(sim_table)
    if(dvid != "NA") {
      sim_extra <- read_nm_tables(sim_extra_table)
      dvid_column_nr <- which(colnames(sim_extra)== dvid_name)
      dvid_sim_col <- sim_extra[,dvid_column_nr]
      sim <- cbind(sim,add_cols,dvid_sim_col)
      dvid_column_nr <- which(colnames(sim)== dvid_name)
      sim <- sim[which(sim[,dvid_column_nr] == dvid),]
    } else {
      sim <- cbind(sim,add_cols)
    }
    
    out <- list(obs=obs,
                sim=sim,
                make_vpc=make_vpc)
  } else {
    out <- list(make_vpc=make_vpc)
  }
  return(out)
}