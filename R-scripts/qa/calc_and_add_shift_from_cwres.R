.calc_and_add_shift_from_cwres <- function(structural_details_table, working.directory, model.filename, CWRES_table, idv, idv_name){
  # .ext file with the final estimates
  final_estimates <- read.table(paste0(working.directory,"linearize_run/scm_dir1/derivatives.ext"), skip=1, header=T) %>%
    filter(ITERATION==-1000000000)
  
  # populate omega from .ext file
  omega_values <- final_estimates %>% select(starts_with("OMEGA")) 
  # create omega matrix
  numeration <- sub('.*OMEGA.','',colnames(omega_values)) %>% substr(., 1, nchar(.)-1)
  dim_omega <- length(unique(as.numeric(sub('\\..*','',numeration))))
  omega_matrix <- matrix(0, dim_omega, dim_omega)
  for(i in 1:length(numeration)) {
    first <- as.numeric(sub('\\..*','',numeration[i]))
    second <- as.numeric(sub('.*\\.','',numeration[i]))
    if(first==second) {
      omega_matrix[first,second] <- omega_values[1,i]
    } else {
      omega_matrix[first,second] <- omega_values[1,i]
      omega_matrix[second,first] <- omega_values[1,i]
    }
  }
  
  # populate sigma from ext file
  sigma_values <- final_estimates %>% select(starts_with("SIGMA"))
  numeration <- sub('.*SIGMA.','',colnames(sigma_values)) %>% substr(., 1, nchar(.)-1)
  dim_sigma <- length(unique(as.numeric(sub('\\..*','',numeration))))
  sigma_matrix <- matrix(0, dim_sigma, dim_sigma)
  for(i in 1:length(numeration)) {
    first <- as.numeric(sub('\\..*','',numeration[i]))
    second <- as.numeric(sub('.*\\.','',numeration[i]))
    if(first==second) {
      sigma_matrix[first,second] <- sigma_values[1,i]
    } else {
      sigma_matrix[first,second] <- sigma_values[1,i]
      sigma_matrix[second,first] <- sigma_values[1,i]
    }
  }
  
  # get table with IDV values
  idv_df <- read.table(CWRES_table, skip = 1, header=T) %>%
    filter(CWRES!=0) %>%
    select_(idv) %>%
    rename_(idv=idv)
  
  # get derivatives from linearized model
  mean_shifts_table <- read.table(file.path(working.directory, paste0(sub('.mod.*','',model.filename),"_linbase.dta")), skip = 1, header=T)
  mean_shifts <- mean_shifts_table %>%
    filter(MDV==0) %>%
    bind_cols(idv_df) %>%
    mutate(bin_index = findInterval(unlist(.$idv), structural_details_table$bin_min),  # in which bin did observation go
           bin_value = structural_details_table$value[bin_index]) %>%                    # get value for that bin
    group_by(ID)  %>%
    do({
      H <- .[,grepl("^H+[0-9]{3}$",colnames(.))] %>% as.matrix()
      G <- .[,grepl("^G+[0-9]{3}$",colnames(.))] %>% as.matrix()
      ICOV <- diag(diag(H %*% sigma_matrix %*% t(H)), nrow = nrow(.)) + G %*% omega_matrix %*% t(G)
      sqrtm <- expm::sqrtm
      if(nrow(.)==1) sqrtm <- sqrt
      data.frame(shift = sqrtm(ICOV) %*% as.matrix(.$bin_value),
                 bin_index = .$bin_index,
                 ipred = .$OPRED)
    }) %>%
    group_by(bin_index) %>%
    summarise(relative_shift = ifelse((any(mean_shifts_table$OPRED > 0) && any(mean_shifts_table$OPRED < 0)),NA,100*mean(shift/ipred)), shift = mean(shift)) %>%
    mutate(bin_min = structural_details_table$bin_min[bin_index],
           bin_max = structural_details_table$bin_max[bin_index],
           bin_mean=ifelse(bin_min==-Inf,-Inf,bin_min+(bin_max-bin_min)/2))
  
  structural_details_table %>%
    slice(mean_shifts$bin_index) %>%
    bind_cols(mean_shifts %>% select(bin_mean, relative_shift, shift))
}