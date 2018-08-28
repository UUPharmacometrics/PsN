pvar_parameter_plot <- function(csv_file_directory,plot_title,units) {
  # load basic libraries
  library(ggplot2)
  library(gridExtra)
  library("grid")
  library("ggthemes")
  
  data <- read.csv(csv_file_directory, header=TRUE, na.strings= ".",comment.char="#")
  # find numbers of the columns of Model and OFV (between these two columns are all parameters)
  col_model <- which(colnames(data) == "Run.number")
  col_ofv <- which(colnames(data) == "OFV")
  # get names of all parameters
  parameter_names <- c(names(data[(col_model+1):(col_ofv-1)]))
  
  # default for title names
  if (missing(plot_title)) {
    plot_title <- parameter_names
  }
  # default for units
  if (missing(units)) {
    units <- rep("",length(parameter_names))
  } else {
    for (i in 1:length(units)) {
      units[i] <- paste0("(",units[i],")")
    }
  }
  
  plot_list <- list()
  left <- 0
  nr <- 1
  j = 1
  p <- list()
  for (i in 1:length(parameter_names)) {
    # get OFV and parameter columns for ploting
    parameter_col <- which(colnames(data) == parameter_names[i])
    # separate data and create a new data frame
    new_data <- data[,c(col_ofv,parameter_col)]
    new_data <- cbind(new_data,data$Type)
    names(new_data) <- c("x","y","Type")
    frameval <- paste0("frame", i)
    assign( paste0("frame", i), new_data )
    
    #start plotting
    p[[i]] <- ggplot(data=get(frameval), aes(x=x, y=y, colour=Type, shape=Type))+
      geom_point(size=6) +
      scale_colour_manual(name="Variability",
                          breaks=c("PV", "UPV", "EPV"),
                          values = c("#000000","#FF3300", "#0000FF")) +   
      scale_shape_manual(name="Variability",
                         breaks=c("PV", "UPV", "EPV"),
                         values = c(15,16,17))+
      theme(axis.title=element_text(size=13, face="bold"),
            axis.text=element_text(size=13, face="bold"),
            panel.background=element_blank(),
            axis.line.y=element_line(color="black"), 
            axis.line.x=element_line(color="black"),
            plot.title=element_text(size=14,face="bold"),
            legend.key=element_rect(fill=NA),
            legend.text=element_text(size=13,margin = margin(), debug = FALSE),
            legend.background= element_rect(linetype="solid",color="black"),
            legend.title=element_text(size=13),
            plot.margin=unit(c(1,1,1,1),"cm"))+
      labs(x = "OFV", y = paste0("Variability (UPV/EPV/PV) ",units[i]))+
      ggtitle(plot_title[i])
    
    # save plots in the tiff files
    # if we have only 1 to 3 parameters
    if (length(parameter_names) == 1) {
      mplot <- do.call(arrangeGrob,p)
      plot_list[[nr]] <- mplot
    } else {
      # save the legend (call function)
      legend <- get_legend(p[[i]])
      # remove the legend from each graf
      p[[i]] <- p[[i]] + theme(legend.position="none")
      if (i %% 4 == 0) {
        mplot <- arrangeGrob(p[[i-3]],p[[i-2]],p[[i-1]],p[[i]],ncol=2)
        plot_final <- arrangeGrob(mplot,legend,ncol=2,widths=c(6,1))
        plot_list[[nr]] <- plot_final
        nr <- nr + 1
        left <- length(parameter_names) - i
      } 
      if (left == 3 || ((length(parameter_names) == 3) && (i == 3))){
        mplot <- arrangeGrob(p[[i-2]],p[[i-1]],p[[i]],ncol=2)
        plot_final <- arrangeGrob(mplot,legend,ncol=2,widths=c(6,1))
        plot_list[[nr]] <- plot_final
      }
      if (left == 2 || ((length(parameter_names) == 2) && (i == 2))){
        mplot <- arrangeGrob(p[[i-1]],p[[i]],ncol=2)
        plot_final <- arrangeGrob(mplot,legend,ncol=2,nrow=2,widths=c(6,1))
        plot_list[[nr]] <- plot_final
      }
      if (left == 1){
        plot_final <- arrangeGrob(p[[i]],legend,ncol=3,nrow=2,widths=c(3,1,3))
        plot_list[[nr]] <- plot_final
      }
      
    }
  }
  return(plot_list)
}
