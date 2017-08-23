# get a ofv value from the ext file
.get_ext_ofv <- function(ext_file,iteration=-1000000000){
  read.table(ext_file,header=TRUE,skip=1,stringsAsFactors = F) %>%
    filter(ITERATION==iteration) %>%
    select(OBJ) %>% 
    as.numeric()
}