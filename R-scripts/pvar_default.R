library(PsNR)
library(magrittr)
library(methods)

# input
csv_file <- file.path(working.directory, 'result.csv', fsep = .Platform$file.sep)
#add R_info to the meta file
R_info(directory=working.directory)
pvar_pdf(pvar_file_name=pdf.filename,csv_file_directory=csv_file)
