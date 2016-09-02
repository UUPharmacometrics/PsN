data.obj.obsi <- function(obj.data.dir,obsi.data.dir) {
  # read in data tables
  OBJ_data <- read.table(obj.data.dir,header = TRUE, skip=1)
  OBSi_data <- read.table(obsi.data.dir,header = TRUE, skip=1)
  
  if (exists("OBJ_data") && (exists("OBSi_data"))) {
    #order data frames by ID numbers
    OBJ_data <- OBJ_data[order(OBJ_data$ID),]
    OBSi_data <- OBSi_data[order(OBSi_data$ID),]
    
    # get OBJ vaslues
    OBJ_vector <- OBJ_data$OBJ
    
    # get OBSi values
    unique_id <- unique(OBSi_data$ID)
    OBSi_vector <- c()
    for (n in 1:length(unique_id)) {
      data <- subset(OBSi_data,ID == unique_id[n], select = c(ID,RES))
      data <- data[data$RES != 0,]
      OBSi_vector[n] <- nrow(data)
    }
    
    # create list of output, just for testing
    list_out <- list(OBJ_data= OBJ_data,
                     OBJ_vector=OBJ_vector,
                     OBSi_vector=OBSi_vector)
    return(list_out)
  } else {
   cat("Input data files are not found!") 
  }
}
