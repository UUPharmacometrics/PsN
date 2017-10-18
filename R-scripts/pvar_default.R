# get libPaths
source(file.path(rscripts.directory,"common/R_info.R"))
R_info(directory=working.directory,only_libPaths=T)
# input
csv_file <- file.path(working.directory, 'result.csv', fsep = .Platform$file.sep)
source(file.path(rscripts.directory,'pvar','pvar_pdf.R', fsep = .Platform$file.sep))
source(file.path(rscripts.directory,'pvar','delta_plot.R', fsep = .Platform$file.sep))
source(file.path(rscripts.directory,'pvar','param_plot.R', fsep = .Platform$file.sep))
source(file.path(rscripts.directory,'pvar','get_legend_fun.R', fsep = .Platform$file.sep))
#add R_info to the meta file
R_info(directory=working.directory)
pvar_pdf(pvar_file_name=pdf.filename,csv_file_directory=csv_file)