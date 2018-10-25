plot_dofv <- function(all,df_est,est.param.names) {
  ggplot2::theme_set(ggplot2::theme_bw(base_size=20))
  qdOFV_all <- ggplot2::ggplot(all,ggplot2::aes(x=rownames,y=deltaofv,color=METHOD,linetype=minimization_successful)) + 
    ggplot2::geom_line() +
    ggplot2::geom_text(data=df_est, ggplot2::aes(x = 0.7,y=c(3,2,1)*qchisq(0.7,df=length(est.param.names))/(2*nrow(df_est)),
                               label=paste(df," (",METHOD,"-",minimization_successful,")",sep="")),show_guide=FALSE,hjust=0,size=5) +
    ggplot2::annotate("text",x = 0.7,y=(nrow(df_est)+1)*qchisq(0.7,df=length(est.param.names))/(2*nrow(df_est)),
             label="Estimated df",hjust=0,fontface="italic",size=5) +
    ggplot2::labs(x="Distribution quantiles",y="dOFV",title="dOFV distribution") +
    ggplot2::theme(legend.position="bottom",legend.box="horizontal") +
    ggplot2::guides(colour = ggplot2::guide_legend(title.position="top"),linetype = ggplot2::guide_legend(title.position="top")) +
    ggplot2::coord_cartesian(ylim=c(0,2*qchisq(0.95,df=length(est.param.names)))) 
  
  return(qdOFV_all)
}