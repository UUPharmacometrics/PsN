# get libPaths
library(PsNR)
library(magrittr)
library(methods)
library(yaml)
library(dplyr)
R_info(directory=working.directory,only_libPaths=T)

rscript <- find_r_files_in_subdir(toolname, rscripts.directory)
for (i in 1:length(rscript)) {
    source(rscript[i])
}
#add R_info to the meta file
R_info(directory=working.directory)

list_par_var_models <- get_param_var_tables(directory=working.directory,model.filename,skip)
resmod_table_list <- get_resmod_ruv_table(directory=working.directory,idv_name,dvid_name,skip)
resmod_table <- resmod_table_list$resmod_ruv_table_full

tree <- list(
    dofv=list(
		etas=list(
        	full_block=list_par_var_models$dofv_block,
        	boxcox=list_par_var_models$dofv_box
		),
		resmod=list(
			power=dplyr::filter(resmod_table, Model == "power")$dOFV,
			iiv_on_ruv=dplyr::filter(resmod_table, Model == "IIV on RUV")$dOFV
		)
    )
)

decision <- function(tree) {
	dofv <- tree$dofv
	if (dofv$etas$full_block > dofv$etas$boxcox) {
		eta_action = "full_block"
	} else {
		eta_action = "boxcox"
	}

	if (dofv$resmod$power > dofv$resmod$iiv_on_ruv) {
		resmod_action = "power"
	} else {
		resmod_action = "iiv_on_ruv"
	}
	return(c(eta_action, resmod_action));
}

tree$actions = decision(tree)

yaml <- yaml::as.yaml(tree, indent.mapping.sequence=TRUE)
cat(yaml, file="results.yaml")
