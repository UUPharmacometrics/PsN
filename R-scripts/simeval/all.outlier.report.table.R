all.outlier.report.table <- function(ofv_outliers,ebe.npde_outliers,cwres.iwres_outliers,residual_names,ID_deleted_ebe=NULL,ID_deleted_ofv=NULL) {
  if(ncol(cwres.iwres_outliers)>0 && length(residual_names)>0) {
    #organize data so that CWRES would always be first
    residual_names <- sort(residual_names)
    cwres.iwres_table_order <- cwres.iwres_outliers
    j <- 0
    for (i in 2:ncol(cwres.iwres_outliers)) {
      j <- j + 1
      cwres.iwres_table_order[,i] <- cwres.iwres_outliers[,grep(residual_names[j],colnames(cwres.iwres_outliers))]
    }
    cwres.iwres_outliers <- cwres.iwres_table_order
  }

  if(ncol(ofv_outliers)>1) {
    ofv_outliers[,2] <- as.character(round(ofv_outliers[,2],3))
    if(ncol(ebe.npde_outliers)>1) {
      all_outlier_table <- dplyr::full_join(ofv_outliers,ebe.npde_outliers,by="ID")
    } else {
      all_outlier_table <- cbind(ofv_outliers,as.character(rep(NA,nrow(ofv_outliers))),stringsAsFactors = F)
    }
  } else {
    if(ncol(ebe.npde_outliers)>1) {
      all_outlier_table <- data.frame(ebe.npde_outliers$ID,as.character(rep(NA,nrow(ebe.npde_outliers))),ebe.npde_outliers[,2],stringsAsFactors = F)
    } else {
      all_outlier_table <- data.frame()
    }
  }
  if(ncol(all_outlier_table)>0) {
    new_names <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)")
    all_outlier_table <- all_outlier_table %>%
      setNames(c("ID","ofv","ebe_npde"))
    if(length(residual_names)>0) {
      new_names <- c(new_names,paste(residual_names,"outliers"))
      if(ncol(cwres.iwres_outliers)>0) {
        all_outlier_table <- dplyr::full_join(all_outlier_table,cwres.iwres_outliers,by="ID",stringsAsFactors=FALSE)
      } else {
        all_outlier_table <- all_outlier_table %>%
          cbind(.,as.data.frame(array("",c(nrow(all_outlier_table),length(residual_names))),stringsAsFactors=FALSE))
      }
    }
    all_outlier_table <- all_outlier_table %>%
      dplyr::mutate_at("ofv",funs(ifelse(ID %in% ID_deleted_ofv,"NA",.))) %>%
      dplyr::mutate_at("ebe_npde",funs(ifelse(ID %in% ID_deleted_ebe,"NA",.))) %>%
      dplyr::mutate_all(funs(ifelse(is.na(.),"",.))) %>%
      setNames(new_names) %>%
      dplyr::arrange(ID) %>%
      dplyr::mutate_all(as.character)
  } else {
    if(ncol(cwres.iwres_outliers)>0 && length(residual_names)>0) {
        new_names <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)",paste(residual_names,"outliers"))
        all_outlier_table <- cbind(cwres.iwres_outliers$ID,
                                   as.data.frame(array("",c(nrow(cwres.iwres_outliers),2)),stringsAsFactors=FALSE)) %>%
          setNames(c("ID","ofv","ebe_npde")) %>%
          cbind(.,cwres.iwres_outliers[2:ncol(cwres.iwres_outliers)],stringsAsFactors=FALSE) %>%
          dplyr::mutate_at("ofv",funs(ifelse(ID %in% ID_deleted_ofv,"NA",.))) %>%
          dplyr::mutate_at("ebe_npde",funs(ifelse(ID %in% ID_deleted_ebe,"NA",.))) %>%
          dplyr::mutate_all(funs(ifelse(is.na(.),"",.))) %>%
          setNames(new_names) %>%
          dplyr::arrange(ID) %>%
          dplyr::mutate_all(as.character)
    } else {
      all_outlier_table <- data.frame(c("No outliers detected"),stringsAsFactors = F)
      colnames(all_outlier_table) <- NULL
    }
  }
  return(all_outlier_table)
}