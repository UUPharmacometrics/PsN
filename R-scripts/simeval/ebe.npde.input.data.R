input.data <- function(ebe.npde.file,iiv.eta.names) {
  # rename ETA(n) to ETA.n. because in dataframe names of ETA columns are with dots
  iiv.eta.names <- gsub("\\(",".",iiv.eta.names)
  iiv.eta.names <- gsub("\\)",".",iiv.eta.names)
  
  # read in data
  ebenpde_tmp_input <- read.csv(ebe.npde.file) # load csv file
  # check if there are some individuals where all Eta values are NA (delete them)
  n_eta <-length(iiv.eta.names)
  ebenpde_tmp_input_etas <- as.data.frame(ebenpde_tmp_input[,iiv.eta.names])
  colnames(ebenpde_tmp_input_etas) <- iiv.eta.names
  ebenpde_tmp_input <- cbind(ebenpde_tmp_input[,1:2],ebenpde_tmp_input_etas)
  # n_eta <- ncol(ebenpde_tmp_input)-2
  na_each_row <- rowSums(is.na(ebenpde_tmp_input))
  if (any(na_each_row==n_eta)) {
    row_delete <- which(na_each_row == n_eta)
    ID_deleted <- ebenpde_tmp_input$ID[row_delete]
    if(length(ID_deleted) > 1) {
      ID_deleted_text <- c()
      for (i in 1:length(ID_deleted)) {
        if(i == 1) {
          ID_deleted_text <- ID_deleted[i]
        } else {
          ID_deleted_text <- paste0(ID_deleted_text,", ",ID_deleted[i])
        }
      }
      message(paste0("WARNING! Removed individuals from csv file \"",ebe.npde.file,"\". No data for ID numbers ",ID_deleted_text,"."))
    } else {
      message(paste0("WARNING! Removed individual from csv file \"",ebe.npde.file,"\". No data for ID number ",ID_deleted,"."))
    }
    ebenpde_tmp <- ebenpde_tmp_input[-row_delete,]
    rownames(ebenpde_tmp) <- NULL
  }else {
    row_delete <- c()
    ID_deleted <- c()
    ebenpde_tmp <- ebenpde_tmp_input
  }
  n.subjects <- nrow(ebenpde_tmp)
  
  # save needed ETA columns in separate data frame ebenpde_obs
  ebenpde_obs <- ebenpde_tmp[,iiv.eta.names]
  if(class(ebenpde_obs) == "numeric") {
    ebenpde_obs <- as.data.frame(ebenpde_obs)
    colnames(ebenpde_obs) <- iiv.eta.names
  }
  
  #create ebenpde_tmp data frame
  ebenpde_tmp <- ebenpde_tmp[,-grep("ETA.",colnames(ebenpde_tmp))]
  ebenpde_tmp <- cbind(ebenpde_tmp,ebenpde_obs)
  # output
  out <- list(ebenpde_tmp_input=ebenpde_tmp_input,
              ebenpde_tmp=ebenpde_tmp,
              n.subjects=n.subjects,
              ebenpde_obs=ebenpde_obs,
              iiv.eta.names=iiv.eta.names,
              ID_deleted=ID_deleted)
  return(out)
}

  
  
  
# input.data <- function(ebe.npde.file,iiv.eta.names) {
#   # read in data
#   ebenpde_tmp <- read.csv(ebe.npde.file) # load csv file
#   n.subjects <- nrow(ebenpde_tmp)
#   
#   # rename ETA(n) to ETA.n. because in dataframe names of ETA columns are with dots
#   iiv.eta.names <- gsub("\\(",".",iiv.eta.names)
#   iiv.eta.names <- gsub("\\)",".",iiv.eta.names)
#   # save needed ETA columns in separate data frame ebenpde_obs
#   ebenpde_obs <- ebenpde_tmp[,iiv.eta.names]
#   if(class(ebenpde_obs) == "numeric") {
#     ebenpde_obs <- as.data.frame(ebenpde_obs)
#     colnames(ebenpde_obs) <- iiv.eta.names
#   }
# 
#   #create ebenpde_tmp data frame
#   ebenpde_tmp <- ebenpde_tmp[,-grep("ETA.",colnames(ebenpde_tmp))]
#   ebenpde_tmp <- cbind(ebenpde_tmp,ebenpde_obs)
#   # output
#   out <- list(ebenpde_tmp=ebenpde_tmp,
#               n.subjects=n.subjects,
#               ebenpde_obs=ebenpde_obs,
#               iiv.eta.names=iiv.eta.names,
#               case='')
#   return(out)
# }
