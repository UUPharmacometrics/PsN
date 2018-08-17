.calc_and_add_shift_from_cwres <- function(structural_details_table, orig_ext_file, base_dataset, extra_table, idv, dvid, dvid_name){
  # .ext file with the final estimates
  final_estimates <- read.table(orig_ext_file, skip=1, header=T) %>%
    dplyr::filter(ITERATION==-1000000000)
  
  # populate omega from .ext file
  omega_values <- final_estimates %>% dplyr::select(starts_with("OMEGA")) 
  # create omega matrix
  numeration <- sub('.*OMEGA\\.','',colnames(omega_values)) %>% substr(., 1, nchar(.)-1)
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
  sigma_values <- final_estimates %>% dplyr::select(starts_with("SIGMA"))
  numeration <- sub('.*SIGMA\\.','',colnames(sigma_values)) %>% substr(., 1, nchar(.)-1)
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
  idv_df <- read.table(extra_table, skip = 1, header=T) #%>%
    #dplyr::filter(CWRES!=0)
  cpred_column <- idv_df$CPRED
  if(any(colnames(idv_df)== dvid_name)) {
    dvid_column_nr <- which(colnames(idv_df)== dvid_name)
    idv_df <- idv_df[which(idv_df[,dvid_column_nr] == dvid),]
  }
  if (any(colnames(idv_df) == "MDV")) {
    idv_df <- dplyr::filter(idv_df, MDV==0)
  }
  idv_df <- idv_df %>%
    dplyr::select_(idv) %>%
    dplyr::rename_(idv=idv)
  
  # get derivatives from linearized model
  #mean_shifts_table <- read.table(base_dataset, header=T) %>%
  mean_shifts_table <- read.table(base_dataset, header=T, skip=1) %>%
    dplyr::mutate(CPRED=cpred_column)
  if(any(colnames(mean_shifts_table)== dvid_name)) {
    dvid_column_nr <- which(colnames(mean_shifts_table)== dvid_name)
    mean_shifts_table<- mean_shifts_table[which(mean_shifts_table[,dvid_column_nr] == dvid),]
  }
  #check if MDV column exists (in PRED models it maybe will not exist)
  mean_shifts <- mean_shifts_table
  if(any(colnames(mean_shifts)=="MDV")) {
    mean_shifts <- mean_shifts %>%
      dplyr::filter(MDV==0)
  }
  mean_shifts <- mean_shifts %>%
    dplyr::bind_cols(idv_df) %>%
    dplyr::mutate(bin_index = findInterval(unlist(.$idv), structural_details_table$bin_min),  # in which bin did observation go
           bin_value = structural_details_table$value[bin_index]) %>%                    # get value for that bin
    dplyr::group_by(ID)  %>%
    dplyr::do({
      H <- .[,grepl("^H+[0-9]{3}$",colnames(.))] %>% as.matrix()
      G <- .[,grepl("^G+[0-9]{3}$",colnames(.))] %>% as.matrix()
      ICOV <- diag(diag(H %*% sigma_matrix %*% t(H)), nrow = nrow(.)) + G %*% omega_matrix %*% t(G)
      sqrtm <- expm::sqrtm
      if(nrow(.)==1) sqrtm <- sqrt
      data.frame(shift = -sqrtm(ICOV) %*% as.matrix(.$bin_value),
                 bin_index = .$bin_index,
                 ipred = .$CPRED)
    }) %>%
    
    dplyr::group_by(bin_index) %>%
    dplyr::summarise(relative_shift = ifelse((any(mean_shifts_table$CPRED > 0) && any(mean_shifts_table$CPRED < 0)),NA,100*mean(shift/ipred)), 
              shift = mean(shift),nobs=n()) %>%
    dplyr::mutate(bin_min = structural_details_table$bin_min[bin_index],
           bin_max = structural_details_table$bin_max[bin_index],
           bin_mean = bin_min+(bin_max-bin_min)/2,
           nobs_pr = .$nobs*100/sum(.$nobs),
           nobs = 12*sqrt(.$nobs/3.14)/max(2*sqrt(.$nobs/3.14)))
  
  structural_details_table %>%
    dplyr::slice(mean_shifts$bin_index) %>%
    dplyr::bind_cols(mean_shifts %>% dplyr::select(bin_mean, relative_shift, shift, nobs, nobs_pr))
}