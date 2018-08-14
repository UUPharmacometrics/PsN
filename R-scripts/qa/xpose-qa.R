qa_data <- function(xpdb, resmod_folder, derivatives_model) {
  
  derivatives_lst <- sub("(\\.[^.]+)$",".lst",derivatives_model)
  
  xpdb_derivatives <- xpose::xpose_data(file = derivatives_lst, quiet = T)
    
  
  resmod_results <- file.path(resmod_folder, "results.csv") %>% 
    readr::read_lines(skip = 1) %>% 
    stringr::str_split(",") %>% 
    purrr::map(~list(dofv = as.numeric(.x[[4]]), 
              name = .x[[3]], 
              parameters = list( param = .x[5:length(.x)]),
              dvid=.x[[2]])) %>% 
    purrr::map(as_tibble) %>% 
    dplyr::bind_rows() %>% 
    tidyr::unnest() %>% 
    dplyr::filter(dvid!="sum") %>%
    tidyr::separate(parameters, c("parameter", "value"), sep = "=", convert = T)
  
  qa_data <- tibble::tibble(problem = NA, method = "qa", type = "default", modified = F, 
                    data = list(list(derivatives = xpdb_derivatives,
                                resmod = list(
                                  summary = resmod_results
                                )
                            ))
  )
  if(exists("special", xpdb)){
    xpdb$special <- dplyr::bind_rows(xpdb$special,
                                     qa_data)
  }else{
    xpdb$special <- qa_data
  }

  class(xpdb) <- c("xpose_data", "uneval")
  return(xpdb)
}

add_resmod_xpdbs <- function(xpdb, resmod_folder,  dvid_value) {
  if(dvid_value[1]!="NA") {
    m1_folder_path <- file.path(resmod_folder,paste0("resmod_DVID_",dvid_value),"m1")
  } else {
    m1_folder_path <- file.path(resmod_folder, "m1")
  }
  resmod_xpdbs <- m1_folder_path %>%
    list.files(pattern = "\\.lst$", full.names = T) %>%
    stringi::stri_subset_regex(., "l2\\.",negate=T) %>%
    purrr::map(~xpose::xpose_data(file = .x, quiet = T, ignore = c('data'), extra_files = c('.ext', '.phi'))) %>%
    purrr::set_names(purrr::map_chr(., ~.x$summary$value[.x$summary$label == "run"]))

  xpdb$special$data[[1]]$resmod$xpdbs <- resmod_xpdbs
  class(xpdb) <- c("xpose_data", "uneval")
  return(xpdb)
}


resmod_variability_attribution <- function(xpdb, idv = quo(TIME), dvid_col_name, dvid_value, smooth = F, conditioning_order = NULL){
  idv_name <- dplyr::quo_name(idv)
  
  resmod_attribution_data <- calculate_variability_attribution(xpdb, idv, dvid_col_name, dvid_value, smooth, conditioning_order, resmod = T)
  original_ruv <- calculate_variability_attribution(xpdb, idv, dvid_col_name, dvid_value, smooth, conditioning_order, resmod = F) %>% 
    dplyr::group_by_(idv_name) %>% 
    dplyr::mutate(value = 1-value/sum(value)) %>% 
    dplyr::filter(source == "RUV")  
  
  if(length(levels(resmod_attribution_data$source))>12) {
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    fct_colors <- gg_color_hue(length(levels(resmod_attribution_data$source))) %>% 
      purrr::set_names(levels(resmod_attribution_data$source))
    fct_colors["RUV"] <- "darkgray"
  } else { # to keep nicer colors if there are less than 11 etas + ruv (because RColorBrewer::brewer.pal has max 12 colors)
    fct_colors <- RColorBrewer::brewer.pal(length(levels(resmod_attribution_data$source)), "Set3") %>% 
      purrr::set_names(levels(resmod_attribution_data$source))
    fct_colors["RUV"] <- "darkgray"
  }

  

  
  ggplot(resmod_attribution_data, aes_string(idv_name, "value", fill = "source"))+
    geom_area(position = position_fill(reverse = T))+
    facet_wrap(~name)+
    annotate("line", x = original_ruv[[idv_name]], y = original_ruv[["value"]], linetype = "dashed") +
    scale_fill_manual("Source", values = fct_colors)+
    scale_y_continuous("Percent variability", labels = scales::percent)+
    theme(legend.position = "bottom")
  
}

calculate_variability_attribution <- function(xpdb, idv, dvid_col_name, dvid_value, smooth, conditioning_order, resmod = F){
  idv_name <- dplyr::quo_name(idv)
  
  idv_data <- get_idv_data(xpdb, idv, dvid_col_name, dvid_value)
  is_frem_model <- exists("FREMTYPE", idv_data$idv[[1]])
  
  omega_matrix <- get_omega_matrix(xpdb)
  if(is.null(conditioning_order))  conditioning_order <- generate_conditioning_order(xpdb, is_frem_model,dvid_col_name,dvid_value)
  
  # set names for conditioning if not provided
  if(is.null(names(conditioning_order))) {
    conditioning_order <- purrr::set_names(conditioning_order, paste0("ETA(", conditioning_order,")"))
  }
  
  smooth_fun <- function(x,y,xout) approx(x,y,xout)$y
  if(smooth) smooth_fun <- function(x,y,xout) ksmooth(x,y, kernel = "n", bandwidth = (max(x)-min(x))/10, x.points = xout) %>% {approx(.$x, .$y, xout=xout)$y}
  
  calc_eta_contribution(xpdb, conditioning_order, dvid_col_name, dvid_value) %>%
  { if(resmod){
    dplyr::left_join(., calc_resmod_ruv_contribution(xpdb,dvid_col_name,dvid_value), by = "ID")
  }else{
    dplyr::left_join(., calc_ruv_contribution(xpdb,dvid_col_name,dvid_value), by = "ID")
  }} %>% 
    dplyr::left_join(idv_data, by = "ID") %>% 
    dplyr::mutate(RUV = purrr::map(var_matrix, diag)) %>% 
    {if(!resmod){
      mutate(., name = "original")
    }else{
      .
    }} %>% 
    dplyr::select(name, idv, RUV, eta_contribution) %>% 
    tidyr::unnest() %>% 
    {if(is_frem_model){
      filter(., FREMTYPE==0) %>% 
        select(-FREMTYPE)
    }else{
      .  
    }
    } %>% 
    dplyr::group_by(name) %>% 
    dplyr::do({
      ls <- purrr::map_at(., c("RUV", names(conditioning_order)), smooth_fun, x = .[[idv_name]], xout = unique(.[[idv_name]])) %>% 
        purrr::map_at(idv_name, unique) 
      ls$name <- head(ls$name, length(ls$RUV))
      ls %>% 
        dplyr::bind_cols()
    }) %>% 
    dplyr::ungroup() %>% 
    tidyr::gather("source", "value", -idv_name, -"name") %>%
    dplyr::mutate(source = factor(source) %>% 
                    forcats::fct_relevel(names(conditioning_order)))
}

get_idv_data <- function(xpdb, idv, dvid_col_name, dvid_value){
  qa_data <- get_qa_data(xpdb)
  qa_data$derivatives %>% 
    xpose::fetch_data(., filter = only_obs_cwres(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>% 
    filter_dvid(., dvid_col_name, dvid_value) %>%
    dplyr::select(ID, !!idv, dplyr::matches("FREMTYPE")) %>% 
    tidyr::nest(-ID, .key = "idv")
}

generate_conditioning_order <- function(xpdb, is_frem_model, dvid_col_name, dvid_value) {
  omega_matrix <- get_omega_matrix(xpdb)
  if(is_frem_model){
    eta <- seq_len(NROW(omega_matrix))
    frem_eta <- get_frem_eta(xpdb,dvid_col_name,dvid_value)
    non_frem_eta <- setdiff(eta, frem_eta) %>% 
      purrr::set_names(paste0("ETA(", .,")"))
    conditioning_order <- c(list(covariates = frem_eta), non_frem_eta)
  }else{
    conditioning_order <- seq_len(NROW(omega_matrix))
  }
  conditioning_order
}

get_frem_eta <- function(xpdb,dvid_col_name,dvid_value){
  qa_data <- get_qa_data(xpdb)
  qa_data$derivatives %>% 
    xpose::fetch_data(., filter = only_obs_cwres(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>%
    filter_dvid(.,dvid_col_name,dvid_value) %>% 
    dplyr::filter(FREMTYPE!=0) %>% 
    dplyr::select(matches("G\\d{3}")) %>% 
    purrr::map_lgl(~any(.x!=0)) %>% 
    seq_along(.)[.]
}

calc_eta_contribution <- function(xpdb, conditioning_order, dvid_col_name, dvid_value){
  qa_data <- get_qa_data(xpdb)
  
  omega_matrix <- get_omega_matrix(xpdb, problem = 1, subprob = 0)
  
  conditioning_args <- conditioning_order %>% 
    purrr::accumulate(~list(eta_index = .y, given_index = unlist(.x, use.names = F)), .init = c()) %>% 
    .[-1] # remove first entry
  
  if(purrr::is_empty(names(conditioning_order))){
    col_names <- paste0("var", seq_along(conditioning_order))
  }else{
    col_names <- names(conditioning_order)
  }
  qa_data$derivatives %>%
    xpose::fetch_data(filter = only_obs_cwres(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>%
    filter_dvid(.,dvid_col_name,dvid_value) %>% 
    dplyr::select(ID, matches("G\\d{3}")) %>% 
    tidyr::nest(-ID, .key = "df_deta") %>% 
    dplyr::mutate(df_deta = purrr::map(df_deta, as.matrix),
                  eta_contribution = purrr::map(df_deta, 
                                                ~purrr::invoke_map(var_from_eta_given_eta, conditioning_args, df_deta = .x, omega = omega_matrix) %>% 
                                                  purrr::set_names(col_names) %>% 
                                                  dplyr::bind_cols())) 
}

var_from_eta_given_eta <- function(df_deta, omega, eta_index, given_index){
  if(length(given_index)==0) return(var_from_eta(df_deta, omega, eta_index))
  
  var_given_eta(df_deta, omega, given_index) - var_given_eta(df_deta, omega, union(eta_index, given_index))
}

var_given_eta <- function(df_deta, omega, eta_index){
  if(length(eta_index)==NROW(omega)) return(0) 
  apply(df_deta, 1, function(a) t(a[-eta_index]) %*% schur_complement(omega, eta_index) %*% a[-eta_index])
}

schur_complement <- function(M, block_index){
  MASS::ginv(M)[-block_index, -block_index] %>% MASS::ginv()
  #  M[-block_index, -block_index] - M[block_index, -block_index] %*% MASS::ginv(M[block_index, block_index]) %*% M[-block_index, block_index]
}

var_from_eta <- function(df_deta, omega, eta_index){
  apply(df_deta, 1, function(a)  {
    x <- a[eta_index]+t(a[-eta_index])%*%omega[-eta_index, eta_index, drop=F]%*%MASS::ginv(omega[eta_index,eta_index, drop=F])
    x %*% omega[eta_index, eta_index] %*% t(x)
  })
  
}












# resmod_parameter_uncertainty <- function(xpdb,dvid_col_name,dvid_value){
#   eta_density <- get_eta_density(xpdb, dvid_col_name, dvid_value)
#   
#   omega_matrix <- get_omega_matrix(xpdb)
#   
#   mu_models <- sprintf("log(THETA%i)", seq(1, min(NROW(omega_matrix),NROW(get_theta_estimates(xpdb))))) 
#   
#   table <- dplyr::left_join(calc_iiv_contribution(xpdb, dvid_col_name, dvid_value), calc_resmod_ruv_contribution(xpdb, dvid_col_name, dvid_value), by = "ID") %>%
#     {dplyr::bind_rows(., dplyr::filter(., name == first(name)) %>% 
#                         dplyr::mutate(name = "original"))} %>% 
#     dplyr::left_join(eta_density, by = "ID") %>% 
#     dplyr::left_join(get_df_eta(xpdb, dvid_col_name, dvid_value), by = "ID") %>% 
#     dplyr::mutate(
#           ruv_var = dplyr::case_when(name == "original" ~ ruv_var,
#                               TRUE ~ resmod_ruv_var),
#            var_y = purrr::map2(iiv_var, ruv_var, ~.x+.y),
#            fim = purrr::map2(var_y, df_deta, ~t(.y) %*% solve(.x) %*% .y)) %>% 
#     dplyr::group_by(name) %>% 
#     dplyr::summarise(pfim = list(purrr::reduce(fim, `+`))) %>% 
#     dplyr::mutate(pfim = purrr::map(pfim, ~.x[seq_along(mu_models), seq_along(mu_models)])) %>% 
#     mutate(
#       dmu_dtheta = list(calc_dmu_dtheta(mu_models, xpdb)),
#       theta_cov = purrr::map2(dmu_dtheta, pfim, ~ t(.x) %*% .y %*% .x %>% MASS::ginv()),
#       theta_se = purrr::map(theta_cov, ~diag(.x) %>% sqrt()),
#       theta_name = purrr::map(dmu_dtheta, colnames)
#     ) %>% 
#     tidyr::unnest(theta_se, theta_name) %>% 
#     dplyr::left_join(get_theta_estimates(xpdb), by = c(theta_name = "name")) %>% 
#     dplyr::mutate(rse = theta_se/value*100) %>% 
#     dplyr::select(-value, -theta_se) %>% 
#     tidyr::spread(theta_name, rse) %>% 
#     dplyr::arrange(name) %>%
#     as.data.frame()
#   
#   orig_row <- table$name %in% "original"
#   table <- rbind(table[orig_row,],table[!orig_row,])
#   for(i in 1:nrow(table)) {
#     if(grepl("^idv_varying_RUV.*",table$name[i])) {
#       table$name[i] <- "time varying"
#     }
#     table$name[i] <- gsub("\\_"," ",table$name[i])
#   }
#   for(i in 2:ncol(table)) {
#     table[,i] <- format(round(table[,i],3),digits=3,trim=T,nsmall=1,scientific = F)
#   }
#   table <- table %>% gather(THETA,values,2:ncol(table)) %>%
#     spread_(names(table)[1],"values")
#   return(table)
# }

resmod_shrinkage <- function(xpdb,dvid_col_name,dvid_value){
  eta_density <- get_eta_density(xpdb, dvid_col_name, dvid_value)
  
  omega_matrix <- get_omega_matrix(xpdb)
  
  eta_names <- paste0("eta",seq(1, NROW(omega_matrix)))
  
  table <- calc_resmod_ruv_contribution(xpdb, dvid_col_name, dvid_value) %>%
  {dplyr::bind_rows(., dplyr::filter(., name == first(name)) %>% 
                      dplyr::mutate(name = "original"))} %>% 
    dplyr::left_join(eta_density, by = "ID") %>% 
    dplyr::left_join(get_df_eta(xpdb, dvid_col_name, dvid_value), by = "ID") %>% 
    dplyr::mutate(
      fim = purrr::map2(var_matrix, df_deta, ~t(.y) %*% MASS::ginv(.x) %*% .y),
      bfim = purrr::map(fim, ~.x + MASS::ginv(omega_matrix))
      ) %>% 
    dplyr::mutate(
      shrinkage = purrr::map(bfim, ~1-sqrt(1-diag(MASS::ginv(.x))/diag(omega_matrix))),
      eta_name = list(eta_names)
    ) %>% 
    tidyr::unnest(shrinkage, eta_name) %>%  
    dplyr::group_by(name, eta_name) %>% 
    dplyr::summarise(shrinkage = 100*mean(shrinkage)) %>% 
    tidyr::spread(eta_name, shrinkage)%>% 
    dplyr::ungroup() %>% 
    #dplyr::arrange(name) %>%
    as.data.frame()
  
  orig_row <- table$name %in% "original"
  table <- rbind(table[orig_row,],table[!orig_row,])
  for(i in 1:nrow(table)) {
    if(grepl("^idv_varying_RUV.*",table$name[i])) {
      table$name[i] <- "time varying"
    }
    table$name[i] <- gsub("\\_"," ",table$name[i])
  }
  for(i in 2:ncol(table)) {
    table[,i] <- format(round(table[,i],3),digits=3,trim=T,nsmall=1,scientific = F)
    colnames(table)[i] <- gsub("eta","ETA",colnames(table)[i])
  }
  table <- table %>% gather(ETA,values,2:ncol(table)) %>%
    spread_(names(table)[1],"values")
  
  return(table)
}

calc_ruv_contribution <- function(xpdb,dvid_col_name,dvid_value){
  qa_data <- get_qa_data(xpdb)
  sigma_matrix <- get_sigma_matrix(xpdb, problem = 1, subprob = 0)
  omega_matrix <- get_omega_matrix(xpdb, problem = 1, subprob = 0)
  dxpdb <- qa_data$derivatives
  dxpdb %>% 
    xpose::fetch_data(., filter = only_obs_cwres(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>%
    filter_dvid(.,dvid_col_name,dvid_value) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::do(var_matrix = {
      df_deps <- dplyr::select(., matches("H\\d{3}")) %>% as.matrix()
      
      df_deps_deta <- dplyr::select(., matches("D_EPSETA")) %>% as.matrix()
      interaction_terms <- diag(diag(df_deps_deta %*% kronecker(omega_matrix, sigma_matrix) %*% t(df_deps_deta)), nrow = NROW(df_deps))
      diag(diag(df_deps %*% sigma_matrix %*% t(df_deps)),  nrow = NROW(df_deps)) + interaction_terms
    },
    TIME = .$TIME,
    IPRED = .$OPRED
    )
}

calc_resmod_ruv_contribution <- function(xpdb, dvid_col_name, dvid_value){
  original_matricies <- calc_ruv_contribution(xpdb, dvid_col_name, dvid_value)

  resmod_data <- get_qa_data(xpdb)$resmod

  best_tvar <- resmod_data$summary %>%
    filter_dvid(.,"dvid",dvid_value) %>%
    dplyr::filter(stringr::str_detect(name, "idv_varying_RUV_cutoff\\d+")) %>%
    dplyr::arrange(dofv) %>%
    dplyr::slice(1) %>%
    purrr::flatten()

  resmod_models <- filter(resmod_data$summary, name %in% c("autocorrelation", "power", "tdist", "IIV_on_RUV", best_tvar$name), !is.na(value)) %>%
    filter_dvid(.,"dvid",dvid_value) %>% 
    .$name
  tvar <- function(params, var_matrix, TIME, ...) {
    m <- var_matrix
    diag(m) <- diag(m)*ifelse(TIME<params["t0"], params["SIGMA(1,1)"], params["SIGMA(2,2)"])
    m
  }
  autocorr <- function(params, var_matrix, TIME, ...) {
    stddev_diag <- sqrt(diag(var_matrix))
    cor_mat <- outer(TIME, TIME, function(x,y) exp(-log(2)/params["half-life"]*abs(x-y)))
    diag(cor_mat) <- 1
    params["SIGMA(1,1)"] * stddev_diag %*% t(stddev_diag) * cor_mat
  }
  power <- function(params, var_matrix, IPRED, ...){
    diag(var_matrix) <- params["SIGMA(1,1)"] * diag(var_matrix)*(IPRED+1E-16)^(params["delta_power"])
    var_matrix
  }
  tdist <- function(params, var_matrix, ...){
    params["df"]/(params["df"]-2)*params["THETA2"]^2*var_matrix
  }

  iiv <-  function(params, var_matrix, ...) {
    diag(var_matrix) <- diag(var_matrix)*params["SIGMA(1,1)"]*params["sd_i"]^2
    return(var_matrix)
  }
  
  resmod_estimates <- resmod_data$xpdbs[resmod_models] %>%
    purrr::map_df(~get_file(.x, ext = "ext") %>% dplyr::filter(`ITERATION`==-1E9) %>% dplyr::select(-ITERATION, -OBJ), .id = "name") %>% 
    tidyr::gather("parameter", "value", -name, na.rm = T)
 
  # collect ruv eta values from phi file
  iiv_on_ruv_sd <- resmod_data$xpdbs %>% 
    purrr::map(purrr::safely(~xpose::get_file(.x, ext = 'phi') %>% dplyr::select(ID,`ETA(2)`))) %>% 
    purrr::map("result") %>% 
    purrr::compact() %>% 
    dplyr::bind_rows(.id = "name") %>% 
    dplyr::filter(ID %in% original_matricies$ID) %>% 
    dplyr::mutate(sd_i = exp(`ETA(2)`),
                  `ETA(2)` = NULL)
  
  res_matricies <- resmod_data$summary %>% 
    filter_dvid(.,"dvid",dvid_value) %>%
    dplyr::select(-dofv) %>%
    dplyr::bind_rows(resmod_estimates) %>%
    #dplyr::mutate(value = purrr::map(value, list)) %>% 
    #dplyr::bind_rows(ruv_etas) %>% 
    dplyr::filter(name %in% resmod_models) %>%
    tidyr::nest(-name, .key = 'params') %>%
    dplyr::mutate(
      params = purrr::map(params, ~purrr::set_names(.x$value, .x$parameter)),
      f = dplyr::case_when(
        stringr::str_detect(name, "idv_varying_RUV") ~ "tvar",
        name == "autocorrelation" ~ "autocorr",
        name == "power" ~ "power",
        name == "tdist" ~ "tdist",
        name == 'IIV_on_RUV' ~ "iiv"
      ),
      data = list(original_matricies)) %>%
    tidyr::unnest(data, .drop = F) %>% 
    dplyr::left_join(iiv_on_ruv_sd, by = c("name","ID")) %>% 
    dplyr::mutate(params = purrr::map2(params, sd_i, ~c(.x, sd_i = .y)),
                  sd_i = NULL) %>% 
    tidyr::nest(-name, -f, -ID, .key = "fargs") %>% 
    dplyr::mutate(fargs = purrr::map(fargs, purrr::flatten))
  
  res_matricies$resmod_ruv_var <- purrr::invoke_map(res_matricies$f, res_matricies$fargs)

  # res_matricies %>% 
  #   dplyr::mutate(ruv_var = purrr::map(fargs, "var_matrix"), ########### check this
  #          time = purrr::map(fargs, "time")) %>% 
  #   dplyr::select(name, ID, time, resmod_ruv_var, ruv_var)
  
  res_matricies %>% 
    dplyr::select(name, ID, var_matrix = resmod_ruv_var)
}

calc_iiv_contribution <- function(xpdb, dvid_col_name, dvid_value, per_eta = F){
  qa_data <- get_qa_data(xpdb)
  
  omega_matrix <- get_omega_matrix(xpdb, problem = 1, subprob = 0)
  
  etas <- seq_len(NROW(omega_matrix)) 
  
  iiv_contribution <- qa_data$derivatives %>%
    xpose::fetch_data(filter = only_obs_cwres(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>%
    filter_dvid(.,dvid_col_name,dvid_value) %>%
    dplyr::select(ID, matches("G\\d{3}")) %>% 
    tidyr::nest(-ID, .key = "df_deta") %>% 
    dplyr::mutate(df_deta = purrr::map(df_deta, as.matrix),
           iiv_var = purrr::map(df_deta, ~.x %*% omega_matrix %*% t(.x)))
  
  if(per_eta) {
 #   browser()
    iiv_contribution <- iiv_contribution %>%  
    {purrr::cross_df(list(data = list(.), eta = etas))} %>% 
    tidyr::unnest() %>% 
    dplyr::mutate(wo_eta_iiv_var = purrr::map2(df_deta, eta, ~.x[,-.y, drop = F] %*% omega_matrix[-.y, -.y, drop = F] %*% t(.x[,-.y, drop = F])))
  }
  iiv_contribution <- iiv_contribution %>% 
    dplyr::select(-df_deta)
  return(iiv_contribution)
}

calc_percent_eta_contribution <- function(xpdb, dvid_col_name, dvid_value){
  qa_data <- get_qa_data(xpdb)
  
  omega_matrix <- get_omega_matrix(xpdb, problem = 1, subprob = 0)
  
  etas <- seq_len(NROW(omega_matrix)) 
  
  omega_matrix <- diag(diag(omega_matrix), nrow = max(etas))
  
  table <- qa_data$derivatives %>%
    xpose::fetch_data(filter = only_obs_cwres(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>%
    filter_dvid(.,dvid_col_name,dvid_value) %>%
    dplyr::select(ID, matches("G\\d{3}")) %>% 
    tidyr::nest(-ID, .key = "df_deta") %>% 
    dplyr::mutate(df_deta = purrr::map(df_deta, as.matrix),
                  iiv_var = purrr::map(df_deta, ~.x %*% omega_matrix %*% t(.x) %>% diag )) %>%  
    {purrr::cross_df(list(data = list(.), eta = etas))} %>% 
      tidyr::unnest() %>% 
      dplyr::mutate(wo_eta_iiv_var = purrr::map2(df_deta, eta, ~.x[,-.y, drop = F] %*% omega_matrix[-.y, -.y, drop = F] %*% t(.x[,-.y, drop = F]) %>% diag),
                    eta_contribution_f = purrr::map2(iiv_var, wo_eta_iiv_var, ~ ifelse(.x==0, 0, (.x - .y)/.x))) %>% 
    dplyr::select(ID, eta, eta_contribution_f)
  return(table)
}

as_resmod_name_factor <- function(resmod_names){
  rename_tvar <- function(l){
    l[stringr::str_detect(l, "idv_varying_RUV.*")] <-  "time varying"
    l
  }
  resmod_names %>% 
    forcats::as_factor() %>% 
    forcats::fct_relabel(rename_tvar) %>% 
    {forcats::fct_relevel(., sort(levels(.)))} %>% 
    forcats::fct_relevel("original")
}

get_qa_data <- function(xpdb){
  #if(!any(xpdb$special$method == "qa")) stop("The xpdb does not contain the necessary data.")

    special <- xpdb$special
    class(xpdb) <- c("xpose_data", "uneval")
  dplyr::filter(special, method == 'qa') %>% 
    purrr::pluck("data", 1)
}

get_eta_density <- function(xpdb, dvid_col_name, dvid_value){
  eta_values <- get_eta_values(xpdb, dvid_col_name, dvid_value) 
  
  omega_matrix <- get_omega_matrix(xpdb)
  eta_density <- eta_values %>% 
    tidyr::spread(eta, eta_value) %>% 
    tidyr::nest(-ID) %>% 
    dplyr::mutate(eta_density = purrr::map_dbl(data, ~as.matrix(.x) %>% mvtnorm::dmvnorm(sigma = omega_matrix))) %>% 
    dplyr::mutate(eta_density = eta_density/sum(eta_density)) %>% 
    dplyr::select(-data)
}

get_eta_values <- function(xpdb,dvid_col_name, dvid_value){
  qa_data <- get_qa_data(xpdb)
  qa_data$derivatives %>% 
    xpose::fetch_data(filter = only_obs_cwres(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>% 
    filter_dvid(.,dvid_col_name,dvid_value) %>%
    dplyr::select(ID, matches("^(ETA\\d|ET\\d+)$")) %>% 
    dplyr::group_by(ID) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    tidyr::gather("eta_name", "eta_value", -ID) %>% 
    tidyr::extract(eta_name, "eta", "(\\d+)", convert = T)
}

get_df_eta <- function(xpdb, dvid_col_name, dvid_value){
  get_qa_data(xpdb)$derivatives %>%
    xpose::fetch_data(filter = only_obs_cwres(., .problem = 1, quiet = T), .problem = 1, .subprob = 0, quiet = T) %>% 
    filter_dvid(.,dvid_col_name,dvid_value) %>%
    dplyr::select(ID, matches("G\\d{3}")) %>% 
    tidyr::nest(-ID, .key = "df_deta") %>% 
    dplyr::mutate(df_deta = purrr::map(df_deta, as.matrix))
}

get_theta_estimates <- function(xpdb){
  xpose::get_file(xpdb, ext = "ext", .problem = 1, .subprob = 0, quiet = T) %>% 
    dplyr::filter(ITERATION==-1000000000) %>% 
    dplyr::select(starts_with("THETA")) %>% 
    tidyr::gather("name", "value")
}

get_sigma_matrix <- function(xpdb, problem = NULL, subprob = NULL,
                            method = NULL){
  xpose::get_file(xpdb, ext = "ext", .problem = problem, .subprob = subprob, .method = method, quiet = T) %>% 
    dplyr::filter(ITERATION==-1000000000) %>% 
    dplyr::select(starts_with("SIGMA")) %>% unlist() %>%
    vector_to_matrix()
}

get_omega_matrix <- function(xpdb, problem = NULL, subprob = NULL,
                             method = NULL){
  xpose::get_file(xpdb, ext = "ext", .problem = problem, .subprob = subprob, .method = method, quiet = T) %>% 
    dplyr::filter(ITERATION==-1000000000) %>% 
    dplyr::select(starts_with("OMEGA")) %>% unlist() %>%
    vector_to_matrix()
}

calc_dmu_dtheta <- function(mu_models, xpdb){
  theta_estimates <- get_theta_estimates(xpdb) %>%
    tidyr::spread(name, value) %>% 
    as.list()
  
  paste0("~", mu_models) %>%
    purrr::map(~as.formula(.x, env = emptyenv()) %>% 
          deriv(names(theta_estimates)) %>% 
          eval(envir = theta_estimates) %>%  
          attr("gradient")
    ) %>% 
    do.call(rbind, .)
}


vector_to_matrix <-  function(vec){
  dim <- (sqrt(1+4*length(vec)*2)-1)/2
  mat <- matrix(0, dim, dim)
  mat[upper.tri(mat, T)] <- vec
  mat[lower.tri(mat, T)] <- t(mat)[lower.tri(mat, T)]
  return(mat)
}

filter_dvid <- function(data,dvid_name,dvid_value) {
  if(dvid_name != '') {
    dvid_column_name <- rlang::sym(dvid_name)
    data %>% dplyr::filter(UQ(dvid_column_name)==!!dvid_value)
  } else {
    return(data)
  }
}

only_obs_cwres <- function(xpdb, .problem, quiet){
  function(x){
    x[x[,"CWRES"]!= 0, ] %>% 
      dplyr::mutate(ID = as.numeric(as.character(ID)))
  }
}
