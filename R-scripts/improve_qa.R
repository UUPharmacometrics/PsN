library(dplyr)
library(yaml)

source(file.path(rscripts.directory, "/common/find_r_files_in_subdir.R"))
rscript <- find_r_files_in_subdir(toolname, rscripts.directory)
for (i in 1:length(rscript)) {
    source(rscript[i])
}
list_par_var_models <- get_param_var_tables(directory=working.directory,model.filename,skip)
tree <- list(dofv=list(full_block=list_par_var_models$dofv_block, boxcox=list_par_var_models$dofv_box))

decision <- function(tree) {
	dofv <- tree$dofv
	if (dofv$full_block > dofv$boxcox) {
		choice = "full_block";
	} else {
		choice = "boxcox";
	}
	return(choice);
}

tree$choice = decision(tree)

yaml <- as.yaml(tree)
cat(yaml, file="results.yaml")
