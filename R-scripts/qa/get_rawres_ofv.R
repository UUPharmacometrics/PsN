# get a ofv value from the raw_results.csv
.get_rawres_ofv <- function(rawres_file, row = 1){
  read.csv(rawres_file) %>%
    dplyr::slice(row) %>%
    dplyr::select(ofv) %>% 
    as.numeric()
}