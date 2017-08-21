dofv_vs_lin_dofv <- function(cdd_folder,lin_cdd_folder,model_name) {
  #cdd
  rawres_data <- read.csv(paste0(cdd_folder,"/raw_results_",model_name,".csv"),stringsAsFactors = F)
  skipped_id_data <- read.csv(paste0(cdd_folder,"/skipped_individuals1.csv"),header = F,stringsAsFactors = F)
  dofv <- rawres_data$cdd.delta.ofv[-1]
  dofv_data <- data.frame(skipped_id_data,dofv)
  colnames(dofv_data) <- c("id","dofv")
  
  #linearized cdd
  lin_rawres_data <- read.csv(paste0(lin_cdd_folder,"/raw_results_",model_name,"_linbase.csv"),stringsAsFactors = F)
  lin_skipped_id_data <- read.csv(paste0(lin_cdd_folder,"/skipped_individuals1.csv"),header = F,stringsAsFactors = F)
  lin_dofv <- lin_rawres_data$cdd.delta.ofv[-1]
  lin_dofv_data <- data.frame(lin_skipped_id_data,lin_dofv)
  colnames(lin_dofv_data) <- c("id","dofv")
  
  # only existing id for both tables
  existing_id <- intersect(dofv_data$id,lin_dofv_data$id)
  dofv_data_new <- dofv_data[(dofv_data$id %in% existing_id),]
  lin_dofv_data_new <- lin_dofv_data[(lin_dofv_data$id %in% existing_id),]
  
  data_plot <- cbind(dofv_data_new,lin_dofv=lin_dofv_data_new$dofv)
  
  #plot
  plot(data_plot$lin_dofv,data_plot$dofv,
        type="n",
        ylab="dofv",
        xlab="linearized dofv",
        ylim=c(min(data_plot$dofv, na.rm=T),max(data_plot$dofv, na.rm=T)),
        xlim=c(min(data_plot$lin_dofv,na.rm=T),max(data_plot$lin_dofv,na.rm=T))
  )
  abline(h=3.84, lwd=2, lty=3, col="black")
  abline(v=3.84, lwd=2, lty=3, col="black")
  text(data_plot$lin_dofv,data_plot$dofv, labels=data_plot$id,cex=.8, col="black")
}