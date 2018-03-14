get_tables_for_vpc <- function(obs_table,obs_extra_table,sim_table,sim_extra_table,idv_all,dvid,dvid_name,quiet=F) {
  make_vpc <- (file.exists(obs_table) && file.exists(obs_extra_table) && file.exists(sim_table))
  if(dvid!="NA"){
    make_vpc <- make_vpc && file.exists(sim_extra_table)
    if(!file.exists(sim_extra_table) && !quiet) {
      message("WARNING: File ",sim_extra_table," not found!")
    }
  }
  
  if(make_vpc) {
    obs <- as.data.frame(xpose::read_nm_tables(obs_table,quiet = TRUE))
    extra_obs <- xpose::read_nm_tables(obs_extra_table,quiet = TRUE)
    
    #choose DVID
    if(dvid!='NA') {
      dvid_column_nr <- which(colnames(obs)== dvid_name)
      obs <- obs[which(obs[,dvid_column_nr] == dvid),]
      rownames(obs) <- NULL
      dvid_column_nr <- which(colnames(extra_obs)== dvid_name)
      extra_obs <- extra_obs[which(extra_obs[,dvid_column_nr] == dvid),]
      rownames(extra_obs) <- NULL
    }
    
    if(all(colnames(obs)!="DV")) {
      if(any(colnames(extra_obs)=="DV")) {  
        obs <- cbind(obs,"DV"=extra_obs[,"DV"])
      } else {
        if(!quiet) {
          message("WARNING: In the file ",obs_extra_table," DV column not found!")
        }
        make_vpc <- FALSE # problem with synonyms in PsN, if in model is DV=MYDV, it will produce table with column MYDV, even if in $TABLE is DV
        return(list(make_vpc=make_vpc))
      }
    } 


    if(all(colnames(obs)!="MDV")) {
      if(any(colnames(extra_obs)=="MDV")) {
        obs <- cbind(obs,"MDV"=extra_obs[,"MDV"])
      }
    }
    
    # Filter observations in obs table
    if(any(colnames(obs)=="MDV")) {
      if(any(obs$MDV==1)) {
        obs <- obs[which(obs$MDV==0),]
        rownames(obs) <- NULL
      }
    }


    add_cols <- obs[,c(idv_all)]
    sim <- xpose::read_nm_tables(sim_table,quiet = TRUE)
    if(dvid != "NA") {
      if(all(colnames(sim)!=dvid_name)) {
        sim_extra <- xpose::read_nm_tables(sim_extra_table,quiet = TRUE)
        dvid_column_nr <- which(colnames(sim_extra)== dvid_name)
        dvid_sim_col <- sim_extra[,dvid_column_nr]
        sim <- cbind(sim,dvid_sim_col)
      }
      dvid_column_nr <- which(colnames(sim)== dvid_name)
      sim <- sim[which(sim[,dvid_column_nr] == dvid),]
      rownames(sim) <- NULL
    }
    sim <- cbind(sim,add_cols)
    
    out <- list(obs=obs,
                sim=sim,
                make_vpc=make_vpc)
  } else {
    if(!file.exists(obs_table) && !quiet) {
      message("WARNING: File ",obs_table," not found!")
    }
    if(!file.exists(obs_extra_table) && !quiet) {
      message("WARNING: File ",obs_extra_table," not found!")
    }
    if(!file.exists(sim_table) && !quiet) {
      message("WARNING: File ",sim_table," not found!")
    }
    out <- list(make_vpc=make_vpc)
  }
  return(out)
}