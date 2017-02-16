data.obj.obsi <- function(obj.data.dir,obsi.data.dir) {
  # read in data tables
  OBJ_data_input <- read.table(obj.data.dir,header = TRUE, skip=1)
  OBSi_data_input <- read.table(obsi.data.dir,header = TRUE, skip=1)
  
  if (any(colnames(OBSi_data_input)=="ID") && any(colnames(OBSi_data_input)=="RES") && 
      any(colnames(OBJ_data_input)=="ID") && any(colnames(OBJ_data_input)=="OBJ")) {
    #order data frames by ID numbers
    OBJ_data <- OBJ_data_input[order(OBJ_data_input$ID),]
    rownames(OBJ_data) <- NULL
    OBSi_data <- OBSi_data_input[order(OBSi_data_input$ID),]
    rownames(OBSi_data) <- NULL
      
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
    list_out <- list(OBJ_data_input=OBJ_data_input,
                      OBSi_data_input=OBSi_data_input,
                      OBJ_data= OBJ_data,
                      OBSi_data=OBSi_data,
                      OBJ_vector=OBJ_vector,
                      OBSi_vector=OBSi_vector,
                      have_needed_columns=TRUE)
  } else {
    list_out <- list(OBJ_data_input=OBJ_data_input,
                      OBSi_data_input=OBSi_data_input,
                      have_needed_columns=FALSE)
  }
    
  return(list_out)
}
