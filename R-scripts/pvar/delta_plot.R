delta_plot <- function(csv_file_directory,parameter,EPV,UPV,PV,page_title,page_units,palette,model_names){
  library(ggplot2)
  library(gridExtra)
  library(reshape2)
  library("grid")
  library("ggthemes")
  
  if(missing(EPV)){
    EPV <- TRUE
  }
  if(missing(UPV)){
    UPV <- TRUE
  }
  if(missing(PV)){
    PV <- TRUE
  }
  
  data <- read.csv(csv_file_directory, header=TRUE, na.strings= ".",comment.char="#")
  # create type vector with values of type if they are TRUE in the input
  type <- as.character(unique(data$Type))
  TYPE <- c(EPV,UPV,PV)
  for (i in 1:length(TYPE)) {
    if(!TYPE[i]){
      type[i] <- NA
    }
  }
  type <- type[!is.na(type)]
  
  # find numbers of the columns of Model and OFV (between these two columns are all parameters)
  col_model <- which(colnames(data) == "Run.number")
  col_ofv <- which(colnames(data) == "OFV")
  # get names of all parameters
  parameter_names <- c(names(data[(col_model+1):(col_ofv-1)]))
  if (!missing(parameter)) {
    parameter_names <- parameter
  }
  # default for title names
  if (missing(page_title)) {
    page_title <- parameter_names
  }
  # default for units
  if (missing(page_units)) {
    page_units <- rep("",length(parameter_names))
  } else {
    for (i in 1:length(page_units)) {
      page_units[i] <- paste0("(",page_units[i],")")
    }
  }
  
  # if in Model name column names are with ".mod", than delete ".mod"
  if (grepl(".mod", data$Model.name[1])) {
    mod_names <- as.character(data$Model.name)
    mod_names <- gsub("\\.mod", "",mod_names)
    data$Model.name <- as.factor(mod_names)
  }
  
  # save each data based on Type to the separate data frame and calculate delta values for each parameter and ofv
  list_data <- list()
  for (n in 1:length(type)) {
    list_data[[n]] <- data[data$Type == type[n],]
    new_data <- list_data[[n]]
      for (i in 1:nrow(new_data)) {
        new_data[i,paste0("dOFV")] <- new_data[i,col_ofv]-new_data[1,col_ofv]
        for (j in (col_model+1):(col_ofv-1)) {
          new_data[i,paste0("d",new_data[1,1],"_",names(new_data[j]))] <- as.numeric(new_data[i,j])-as.numeric(new_data[1,j])
          list_data[[n]] <- new_data
        }
      }
  }
  
  # set default colors (if there are not more than 20 models)
  colors_for_palette <- c("#000000","#FF3300", "#0066CC", "#006600", "#FF9900", "#330099","#999999","#003366","#339900","#FF6600","#8B008B","#00FFFF","#8B4513","#FF1493","#FFFF00","#C6E2FF","#8B0000","#FFB6C1","#FFEC8D","#191970")
  if(missing(palette) && (nrow(list_data[[1]]) <= length(colors_for_palette))){
    palette <- colors_for_palette
  }
  
  # make a plots
  plot_full <- list()
  for (j in 1:length(parameter_names)) {
    p <- list()
    for (i in 1:length(list_data)) {
      data <- list_data[[i]]
      # choose columns to plot
      column_name_nr <- which(grepl(paste0("d",data$Type[1],"_",parameter_names[j]), names(data)))
      names(data)[column_name_nr] <- "delta"
      # make a plot
      p[[i]] <- ggplot(data,aes(x=dOFV, y=delta, color=data$Model.name))+
        geom_point(size=6,shape=15)+
        theme(axis.title=element_text(size=18, face="bold"),
              axis.text=element_text(size=14, face="bold"),
              legend.text=element_text(size=12),
              legend.background = element_rect(fill=NA),
              legend.title = element_text(size=12, face="bold"),
              panel.background=element_blank(),
              panel.border = element_rect(colour = "black", fill=NA),
              axis.line.y=element_line(color="black"), 
              axis.line.x=element_line(color="black"),
              plot.title=element_text(size=14,face="bold"),
              plot.margin=unit(c(1,1,1,1),"cm"))+
        xlab(expression(Delta~OFV))+
        ylab(bquote(Delta~.(as.character(data$Type[1]))))+
        guides(color=guide_legend(title = "Model"))
      if (!missing(palette) && missing(model_names)) {
        p[[i]] <- p[[i]] + scale_color_manual(values=palette)
      }
      if (!missing(model_names) && missing(palette)) {
        p[[i]] <- p[[i]] + scale_color_discrete(labels=model_names,breaks=data$Model.name)
      } 
      if (!missing(model_names) && !missing(palette)) {
        p[[i]] <- p[[i]] + scale_color_manual(values=palette,labels=model_names,breaks=data$Model.name)
      }  
      #---------------------------------------------------------------------
      if (length(type) == 1) {
        plot_full[[j]] <- do.call(grid.arrange,p)
        plot_full[[j]] <- grid.arrange(plot_full[[j]],top=textGrob((paste(page_title[j]," ",page_units[j])),gp=gpar(fontsize=20)))
      } 
        # save the legend (call function)
        legend <- get_legend(p[[i]])
        legend <- grid.arrange(legend,ncol=2,widths=c(6,1))
        # remove the legend from each graf
        p[[i]] <- p[[i]] + theme(legend.position="none")
        if ((length(type) == 2) && (i == 2)) {
          plot_p <- grid.arrange(p[[i-1]],p[[i]],ncol=1,nrow=2)
          plot_full[[j]] <- grid.arrange(plot_p,legend,nrow=1,ncol=2,widths=c(2,1),top=textGrob((paste(page_title[j]," ",page_units[j])),gp=gpar(fontsize=20)))
        } else if (length(type) == 3 && (i == 3)) {
          p[[4]] <- legend
          plot_full[[j]] <- do.call(grid.arrange,p)
          plot_full[[j]] <- grid.arrange(plot_full[[j]],top=textGrob((paste(page_title[j]," ",page_units[j])),gp=gpar(fontsize=20)))
        }
    }
  }
return(plot_full)
}








