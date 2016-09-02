plot.obj.obsi <- function(OBJ_data,OBSi_vector,OBJ_vector) {
  # plot values
  plot(OBSi_vector,OBJ_vector,
       type="n",
       ylab="OBJ",
       xlab="OBSi",
       ylim=c(min(OBJ_vector, na.rm=T),max(OBJ_vector, na.rm=T)),
       xlim=c(min(OBSi_vector, na.rm=T)-0.5,max(OBSi_vector, na.rm=T)+0.5),
       xaxt = "n")
  axis(1, at = 1:max(OBSi_vector))
  title(" ")
  text(OBSi_vector,OBJ_vector, labels=OBJ_data$ID,cex=.8, col="black")
}
