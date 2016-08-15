pvar_pdf <- function(csv_file_directory,pvar_file_name,delta_plot=TRUE,param_plot=TRUE,parameter,EPV=TRUE,UPV=TRUE,PV=TRUE,page_title,page_units,palette,model_names,plot_title,units) {
  if (delta_plot) {
    delta_pl <- delta_plot(csv_file_directory,parameter,EPV,UPV,PV,page_title,page_units,palette,model_names)
  }
  if (param_plot) {
    ml <- pvar_parameter_plot(csv_file_directory,plot_title,units)
  }
  # what to print in the pdf file
  if (exists("delta_pl") && exists("ml")) {
    p <- append(ml,delta_pl) # connect two lists in one list
  }
  if (exists("delta_pl") && !exists("ml")) {
     p <- delta_pl
  }
  if (!exists("delta_pl") && exists("ml")) {
   p <- ml
  } 
pdf(file=pvar_file_name, width=11.3, height=8.3) # open pdf file
final_plot <- marrangeGrob(p, nrow=1, ncol=1,top=NULL) # put list of plots in separate pdf file pages
print(final_plot) # save plots in pdf file
dev.off() # close pdf file
}

