get_resmod_ruv_table <- function(directory, idv_name, dvid_name, skip){
  resmod_table_list <- get_resmod_table(directory=directory, idv=idv_name)
  resmod_file_exists <- resmod_table_list$resmod_file_exists
  resmod_ruv_table_list <- list()
  if(resmod_file_exists && all(skip!="resmod")) {
    resmod_table_full <- resmod_table_list$resmod_table
    dvid_nr <- find_dvid_values(directory,idv=idv_name,dvid_name)

    if(length(dvid_nr) == 1 && dvid_nr=="NA"){
      resmod_ruv_overview <- as.data.frame(array(0,c(2,3)))
    } else {
      resmod_ruv_overview <- as.data.frame(array(0,c((2*length(dvid_nr)+length(dvid_nr)),3)))
    }
    
    #get TAD results file if exists
    tad_resmod_table_list <- get_resmod_table(directory=directory, idv="TAD",quiet=T)
    add_tad_varying <- tad_resmod_table_list$resmod_file_exists & idv_name!="TAD"
    if(add_tad_varying) {
      tad_table <- tad_resmod_table_list$resmod_table
    }
    
    k <- 1
    for (j in 1:length(dvid_nr)) {
      resmod_table <- resmod_table_full %>% dplyr::filter(dvid==!!dvid_nr[j]) %>% dplyr::select(-iteration, -dvid)
      non_time_var <- resmod_table %>%
        dplyr::filter(!grepl("idv_varying", model)) %>%
        dplyr::mutate(df = stringr::str_count(parameters, "="))
      time_var_cutoff <- resmod_table %>%
        dplyr::filter(grepl("idv_varying_RUV_cutoff",model)) %>%
        dplyr::mutate(df = 2) %>%
        dplyr::arrange(desc(dOFV)) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(model = "time varying")
      resmod_ruv_table <- dplyr::bind_rows(non_time_var, 
                                           time_var_cutoff)
      #add tad varying row, if exists time after dose
      if(add_tad_varying) {
        tad_varying <- tad_table %>% dplyr::filter(dvid==!!dvid_nr[j]) %>% dplyr::select(-iteration, -dvid) %>%
          dplyr::filter(grepl("idv_varying_RUV_cutoff",model)) %>%
          dplyr::mutate(df = 2) %>%
          dplyr::arrange(desc(dOFV)) %>%
          dplyr::slice(1) %>%
          dplyr::mutate(model = "tad varying")
        resmod_ruv_table <- dplyr::bind_rows(resmod_ruv_table,
                                             tad_varying)
      }
      
      resmod_ruv_table <- resmod_ruv_table[order(resmod_ruv_table$dOFV,decreasing = T),]
      rownames(resmod_ruv_table) <- NULL
      colnames(resmod_ruv_table)[which(colnames(resmod_ruv_table)=="model")] <- "Model"
      
      #choose only 3 columns
      resmod_ruv_table <- resmod_ruv_table[,c("Model","dOFV","df","parameters")]
      if(all(is.na(resmod_ruv_table$dOFV))) {
        resmod_ruv_table$df <- rep("",length(resmod_ruv_table$df))
        resmod_ruv_table$parameters <- rep("",length(resmod_ruv_table$parameters))
      }
      colnames(resmod_ruv_table) <- c("Model","dOFV","Additional parameters","Parameter values")
      
      #replace symbol "_" with the space
      nr_rows <- grep("\\_",resmod_ruv_table[,1])
      for(i in 1:length(nr_rows)) {
        resmod_ruv_table[nr_rows[i],1] <- gsub("\\_"," ",resmod_ruv_table[nr_rows[i],1])
      }
      
      if(length(dvid_nr) == 1 && dvid_nr=="NA") {
        resmod_ruv_overview <- resmod_ruv_table[c(1:2),c("Model","dOFV","Additional parameters")] #the highest ofv values
      } else {
        resmod_ruv_overview[k,] <- c(paste0("(",dvid_name," = ",dvid_nr[j],")"),'','')
        resmod_ruv_overview[c(k+1,k+2),] <- resmod_ruv_table[c(1:2),c("Model","dOFV","Additional parameters")] #the highest ofv values
        k <- k + 3
      }
      if(all(is.na(resmod_ruv_table$dOFV))) {
        resmod_ruv_table$dOFV <- format(resmod_ruv_table$dOFV)
      } else {
        resmod_ruv_table$dOFV <- format(round(resmod_ruv_table$dOFV,2),digits=1,trim=T,nsmall=1,scientific = F)
      }
      resmod_ruv_table_list[[j]] <- resmod_ruv_table
    }
   colnames(resmod_ruv_overview) <- c("","dOFV","Add.params")
   
   #if all dOFV values are NA
   if(all(is.na(resmod_ruv_overview$dOFV))) {
     resmod_ruv_overview$dOFV <- format(resmod_ruv_overview$dOFV)
   }
   
  } else {
    if(any(skip=="resmod")) {
      resmod_ruv_overview <- data.frame("RESMOD","SKIPPED",stringsAsFactors = F)
      colnames(resmod_ruv_overview) <- c("","dOFV")
    } else {
      resmod_ruv_overview <- error_table("RESMOD")
    }
    resmod_ruv_table_list[[1]] <- error_table(col=1)
    dvid_nr <- 'NA'
  }
  
  if(length(resmod_ruv_table_list)>1) {
    resmod_row_groups <- as.data.frame(array(0,c(length(dvid_nr),3)),stringsAsFactors=F)
  j <- 1
    for(i in 1:length(resmod_ruv_table_list)) {
      resmod_row_groups[i,1] <- paste0(dvid_name,"=",dvid_nr[i])
      resmod_row_groups[i,2] <- j
      j <- j + nrow(resmod_ruv_table_list[[i]])
      resmod_row_groups[i,3] <- j - 1
      if(i == 1) {
        resmod_ruv_table_full <- resmod_ruv_table_list[[i]]
      } else {
        resmod_ruv_table_full <- rbind(resmod_ruv_table_full,resmod_ruv_table_list[[i]])
      }
    }
  } else {
    resmod_row_groups <- data.frame()
    resmod_ruv_table_full <- resmod_ruv_table_list[[1]]
  }
  
  return(list(resmod_ruv_table_full=resmod_ruv_table_full,
              resmod_row_groups=resmod_row_groups,
              resmod_ruv_table_list=resmod_ruv_table_list,
              resmod_ruv_overview=resmod_ruv_overview,
              dvid_nr=dvid_nr))
}