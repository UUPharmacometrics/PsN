##..............................................................................
## Created by Mia Kjellsson
## April 7, 2011
##.............................................................................
## Last modified: 13.01.16 Steve Choy
## Changes introduced: Distribution of ETAs
## Last modified: 12.01.30 Steve Choy
## Changes introduced: Modified to fit with J&J data
##.............................................................................
## Various goodness-of-fit graphs with VPCs, VPConly trigger to print different pdf
##.............................................................................

vpc.dir <- "51" # includes "vpc_run" in front
run.no <- "51"
vpc.file <- "vpctab51"
vpc.table <- "vpc_results.csv"
graph.file <- paste("vpc_",run.no,".pdf",sep="")


#___import data____________________________________
library(xpose4)

#_____VPC____________________________________________

vpc1 =  xpose.VPC(
	   vpctab=vpc.file,
       vpc.info=vpc.table,
       PI="NULL",  #Either "lines", "area" or "both" specifying whether prediction intervals (as lines, a shaded area or both) should be added to the plot. NULL means no prediction interval.
       PI.real="TRUE", #Plot the percentiles of the real data in the various bins. values can be NULL or TRUE. Note that for a bin with few actual observations the percentiles will be approximate. For example, the 95th percentile of 4 data points will always be the largest of the 4 data points.
       PI.mean=T, #Plot the mean values of simulations 
       PI.ci="area", #Plot the prediction interval of the simulated data's percentiles for each bin. Values can be "both", "area" or "lines" This can be thought of as a prediction interval about the PI.real or a confidence interval about the PI. However, note that with increasing number of simulations the CI will not go towards zero because the interval is also dependent on the size of the data set.
       PI.ci.area.smooth=T,
       PI.real.med.lty = 1,
       PI.real.up.col = "blue",
       PI.real.med.col = "red",
       PI.real.down.col = "blue",
       PI.real.up.lwd = 1.8,
       PI.real.med.lwd = 1.9,
       PI.real.down.lwd = 1.8,
       PI.ci.up.arcol = "blue", #The color of the upper PI area
       PI.ci.down.arcol = "blue",
       PI.ci.med.arcol = "red",
       max.plots.per.page=1,
       col ="blue",
	   type="p",
       cex = "0.5",
	   xlb="Time (days)",
     ylb="HbA1c (Abs CFB)", 
	 #logy=TRUE,
	   ylim=c(-1.4,1),
	 #xlim=c(0,4),
       main="Visual Predictive Check (N=500) with PI.mean"
       )


#________print to PDF______________________
pdf(file=graph.file) #,paper="a4",title=paste("Diagnostic plots for run",run.no,sep=""),bg="White",width=6.5,height=10)
	print(vpc1)
dev.off()
