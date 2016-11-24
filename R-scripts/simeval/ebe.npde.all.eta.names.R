eta_iiv_iov <- function(iiv.eta.names,iov.eta.names) {

  # rename ETA(n) to ETA.n. because in dataframe names of ETA columns are with dots
  iiv.eta.names <- gsub("\\(",".",iiv.eta.names)
  iiv.eta.names <- gsub("\\)",".",iiv.eta.names)
  
  # add text iiv to eta names
  eta.names_text <- c()
  for (i in 1:length(iiv.eta.names)) {
    eta.names_text[i] <- paste0(iiv.eta.names[i]," IIV")
  }
  
  # add text to iov eta names
  k <- length(iiv.eta.names)
  if(length(iov.eta.names) > 0) {
    for (i in 1:length(iov.eta.names)) {
      iov.eta.names[[i]] <- gsub("\\(",".",iov.eta.names[[i]])
      iov.eta.names[[i]] <- gsub("\\)",".",iov.eta.names[[i]])
      for (j in 1:length(iov.eta.names[[i]])) {
        k <- k + 1
        eta.names_text[k] <- paste0(iov.eta.names[[i]][j]," IOV occ.",i)
      }
    }
  }
  
  #all eta names
  eta.names <- iiv.eta.names
  if(length(iov.eta.names) > 0) {
    for (i in 1:length(iov.eta.names)) {
      eta.names <- c(eta.names,iov.eta.names[[i]])
    }
  }
  
  # explanation (correlation graph iiv)
  iiv.eta.names_text <- c()
  for (n in 1:length(iiv.eta.names)) {
    if(n == 1) {
      iiv.eta.names_text <- iiv.eta.names[n]
    } else {
      iiv.eta.names_text <- paste0(iiv.eta.names_text,",",iiv.eta.names[n])
    }
  }
  iiv.eta.names_text <- paste0("(",iiv.eta.names_text,")")
  iiv.eta.names_text <- paste0("Correlation graph for EBE NPDE IIV:\n",iiv.eta.names_text)
  
  
  # explanation (correlation graph iov)
  iov.eta.names_text <- c()
  if(length(iov.eta.names)>0) {
    iov.eta.names_text_occ <- c()
    for (i in 1:length(iov.eta.names)) {
      iov.eta.names_occ <- iov.eta.names[[i]]
      for (n in 1:length(iov.eta.names_occ)) {
        if(n == 1) {
          iov.eta.names_text_occ <- iov.eta.names_occ[n]
        } else {
          iov.eta.names_text_occ <- paste0(iov.eta.names_text_occ,",",iov.eta.names_occ[n])
        }
      }
      iov.eta.names_text_occ <- paste0("(",iov.eta.names_text_occ,")")
      iov.eta.names_text <- paste0(iov.eta.names_text,"\n",iov.eta.names_text_occ)
    }
    iov.eta.names_text <- paste0("Correlation graph for EBE NPDE IOV:",iov.eta.names_text)
  }
  
  out <- list(eta.names=eta.names,
              iiv.eta.names=iiv.eta.names,
              iov.eta.names=iov.eta.names,
              eta.names_text=eta.names_text,
              iiv.eta.names_text=iiv.eta.names_text,
              iov.eta.names_text=iov.eta.names_text)
  return(out)
}