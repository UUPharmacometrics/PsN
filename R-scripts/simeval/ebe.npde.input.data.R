input.data <- function(ebe.npde.file,eta.names,show.warning) {
  if(missing(show.warning)){
    show.warning <- TRUE
  }

  # read in data
  ebenpde_tmp_input <- read.csv(ebe.npde.file) # load csv file
  
  if(missing(eta.names)) {
    eta.names <- colnames(ebenpde_tmp_input)[grep("^ETA.\\d+.$",colnames(ebenpde_tmp_input))]
  }
  # check if there are some individuals where all Eta values are NA (delete them)
  n_eta <-length(eta.names)
  ebenpde_tmp_input_etas <- as.data.frame(ebenpde_tmp_input[,eta.names])
  colnames(ebenpde_tmp_input_etas) <- eta.names
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
      if(show.warning){
        message(paste0("WARNING! No data for ID numbers ",ID_deleted_text,"in the csv file \"",ebe.npde.file,"\"."))
      }
    } else {
      if(show.warning) {
        message(paste0("WARNING! No data for ID number ",ID_deleted,"in the csv file \"",ebe.npde.file,"\"."))
      }
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
  ebenpde_obs <- ebenpde_tmp[,eta.names]
  if(class(ebenpde_obs) == "numeric") {
    ebenpde_obs <- as.data.frame(ebenpde_obs)
    colnames(ebenpde_obs) <- eta.names
  }
  
  #create ebenpde_tmp data frame
  ebenpde_tmp <- ebenpde_tmp[,-grep("ETA.",colnames(ebenpde_tmp))]
  ebenpde_tmp <- cbind(ebenpde_tmp,ebenpde_obs)
  # output
  out <- list(ebenpde_tmp_input=ebenpde_tmp_input,
              ebenpde_tmp=ebenpde_tmp,
              n.subjects=n.subjects,
              ebenpde_obs=ebenpde_obs,
              ID_deleted=ID_deleted,
              eta.names=eta.names)
  return(out)
}