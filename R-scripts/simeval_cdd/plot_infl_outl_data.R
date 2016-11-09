plot_infl_outl_data <- function(table_for_plot,ID,row,row_outl_not_infl,row_infl_not_outl,fail_ID_text,deleted_outliers_text,cutoff_delta.ofv) {
  if (length(row)>0 || length(row_outl_not_infl)>0 || length(row_infl_not_outl)>0) {
    row_ID_nr <- c(row,row_outl_not_infl,row_infl_not_outl)
    simeval_iofv_res <- table_for_plot$simeval_iofv_res[-row_ID_nr]
    cdd_delta.ofv <- table_for_plot$cdd_delta.ofv[-row_ID_nr]
  } else {
    simeval_iofv_res <- table_for_plot$simeval_iofv_res
    cdd_delta.ofv <- table_for_plot$cdd_delta.ofv
  }
  # add more space around the plot
  if(fail_ID_text != '') {
    par(oma=c(3,1,2,1))
  } else {
    par(oma=c(0,1,2,1))
  }
  # plot
  plot (simeval_iofv_res,cdd_delta.ofv,
        type="p",
        ylab="CDD based influential individuals, OFV(cdd-i,orig)-OFV(cdd-i,est)",
        xlab="Simeval based outliers, (OFV(i,orig)-mean(OFV(sim)))/SD(OFV(sim))",
        ylim=c(min(table_for_plot$cdd_delta.ofv, na.rm=T),max(table_for_plot$cdd_delta.ofv, na.rm=T)),
        xlim=c(min(table_for_plot$simeval_iofv_res,na.rm=T),max(table_for_plot$simeval_iofv_res,na.rm=T))
  )
  abline(h=cutoff_delta.ofv, lwd=2, lty=3, col="black")
  abline(v=3, lwd=2, lty=3, col="black")
  title("Cdd influential individuals and simeval outliers",line=3)
  if (length(row) > 0) {
    text(table_for_plot$simeval_iofv_res[row],table_for_plot$cdd_delta.ofv[row], labels=ID,cex=.8, col="red")
  }
  if (length(row_outl_not_infl) > 0) {
    text(table_for_plot$simeval_iofv_res[row_outl_not_infl],table_for_plot$cdd_delta.ofv[row_outl_not_infl], labels=table_for_plot$ID[row_outl_not_infl],cex=.8, col="black")
  }
  if (length(row_infl_not_outl) > 0) {
    text(table_for_plot$simeval_iofv_res[row_infl_not_outl],table_for_plot$cdd_delta.ofv[row_infl_not_outl], labels=table_for_plot$ID[row_infl_not_outl],cex=.8, col="black")
  }
  mtext("In red color are ID numbers of the individuals which are both: outliers and influential individuals",side=3,line=1,col = "red")
  # if some of ID numbers were deleted
  if(fail_ID_text != '') {
    if (deleted_outliers_text != '') {
      if(!(grepl(",",fail_ID_text))) {
        mtext(paste0("In CDD method Nonmem failed estimation for the ID number: ",fail_ID_text),side=1,line=4,adj=0,col = "blue")
        mtext(paste0("In Simeval method the ID number ",deleted_outliers_text," is an outlier"),side=1,line=5,adj=0,col = "blue")
      } else {
        mtext(paste0("In CDD method Nonmem failed estimation for the ID numbers ",fail_ID_text),side=1,line=4,adj=0,col = "blue")
        if (!(grepl(",",deleted_outliers_text))) {
          mtext(paste0("In Simeval method the ID number ",deleted_outliers_text," is an outlier"),side=1,line=5,adj=0,col = "blue")
        } else {
          mtext(paste0("In Simeval method the ID numbers ",deleted_outliers_text," are outliers"),side=1,line=5,adj=0,col = "blue")
        }
      }
    } else {
      if(!(grepl(",",fail_ID_text))) {
        mtext(paste0("In CDD method Nonmem failed estimation for the ID number ",fail_ID_text),side=1,line=4,adj=0,col = "blue")
      } else {
        mtext(paste0("In CDD method Nonmem failed estimation for the ID numbers ",fail_ID_text),side=1,line=4,adj=0,col = "blue")
      }
    }
  }
}
