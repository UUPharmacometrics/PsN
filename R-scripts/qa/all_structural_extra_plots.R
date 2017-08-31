all_structural_extra_plots <- function(working.directory,model.filename,resmod_structural_details,CWRES_table,idv_all,idv_name,dvid_name,type) {
  if(length(idv_all)!=0) {
    for(i in 1:length(resmod_structural_details)) {
      #tables for vpc plots
      vpc_tables_list <- get_tables_for_vpc(obs_table=CWRES_table,obs_extra_table=file.path(working.directory,paste0(sub('.([^.]*)$','',model.filename),"_linbase.dta")),sim_table=file.path(working.directory,"/simeval_run/m1/sim_res_table-1.dta"),sim_extra_table=file.path(working.directory,"/simeval_run/m1/orig_pred.dta"),idv_all,resmod_structural_details[[i]]$dvid,dvid_name)
      make_vpc <- vpc_tables_list$make_vpc
      if(make_vpc) {
        obs <- vpc_tables_list$obs
        sim <- vpc_tables_list$sim
      }
      
      perc <- resmod_structural_details[[i]]$perc
      #captions
      all_captions <- captions_structural(idv_all,idv_name,resmod_structural_details[[i]]$idv,perc)
      resmod_dofv_table_captions <- all_captions$idv_resmod_dofv_table_captions
      structural_bias_tables_captions <- all_captions$idv_structural_bias_tables_captions
      structural_bias_plots_captions <- all_captions$idv_structural_bias_plots_captions
      vpc_captions <- all_captions$idv_vpc_captions
      
      #print
      cat(resmod_structural_details[[i]]$idv_text)
      first_table <- keep_symbols(resmod_structural_details[[i]]$first_table,type)
      first_table <- ztable_sub(first_table,type=type,include.colnames = F,include.rownames = F,longtable = T,align="lr")
      print(first_table)
      cat(resmod_dofv_table_captions)
      second_table <- keep_symbols(resmod_structural_details[[i]]$second_table,type)
      second_table <- ztable_sub(second_table,type=type,colnames.bold = T,include.rownames = F,longtable = T,align="rlrrr")
      second_table <- addcgroup(second_table,cgroup=c("","Estimated bias",""),n.cgroup=c(1,2,1))
      print(second_table)
      cat(structural_bias_tables_captions)
      shift_tab <- resmod_structural_details[[i]]$table
      if(ncol(shift_tab)!=1) {
        cat("\n\n")
        idv_plot <- plot_ipred(table=shift_tab,idv=resmod_structural_details[[i]]$idv)
        print(idv_plot)
        cat("\n\n")
        cat(structural_bias_plots_captions)
        if(make_vpc) {
          cat("\n\n")
          #vpc plots
          vpc_plot <- plot_structural_vpc(obs,sim,shift_tab,idv=resmod_structural_details[[i]]$idv)
          print(vpc_plot)
          cat("\n\n")
          cat(vpc_captions)
        }
      }
      cat("\n\\newpage\n")
    }
  } else {
    print(resmod_structural_details)
  }
}