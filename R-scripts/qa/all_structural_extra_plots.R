all_structural_extra_plots <- function(simeval_directory,base_dataset,resmod_structural_details,
                                       extra_table,idv_all,idv_name,dvid_name,type,nonlinear) {
  if(length(idv_all)!=0) {
    for(i in 1:length(resmod_structural_details)) {
      #tables for vpc plots
      if(!nonlinear) {
        vpc_tables_list <- get_tables_for_vpc(obs_table=extra_table,
                                              obs_extra_table=base_dataset,
                                              sim_table=file.path(simeval_directory,"/m1/sim_res_table-1.dta"),
                                              sim_extra_table=file.path(simeval_directory,"/m1/orig_pred.dta"),
                                              idv_all,
                                              dvid=resmod_structural_details[[i]]$dvid,
                                              dvid_name)
        make_vpc <- vpc_tables_list$make_vpc
      } else {
        make_vpc <- FALSE
      }
      perc <- resmod_structural_details[[i]]$perc
      #captions
      all_captions <- captions_structural(idv_all,idv_name,resmod_structural_details[[i]]$idv,perc)
      resmod_dofv_table_captions <- all_captions$idv_resmod_dofv_table_captions
      structural_bias_tables_captions <- all_captions$idv_structural_bias_tables_captions
      structural_bias_plots_captions <- all_captions$idv_structural_bias_plots_captions
      vpc_captions <- all_captions$idv_vpc_captions
      
      #print
      cat(paste0("##",resmod_structural_details[[i]]$idv_text,"\n\n"))
      first_table <- kable_table(resmod_structural_details[[i]]$first_table,format=type,booktabs=T,align = c("l","r"),linesep="") %>%
        kableExtra::kable_styling(position="c",full_width = F) %>%
        column_spec(1,bold=T)
      print(first_table)
      cat(resmod_dofv_table_captions)
      if(!nonlinear) {
        second_table <- kable_table(resmod_structural_details[[i]]$second_table,format=type,booktabs=T,longtable=T,align = c("l","r","r"),linesep="") %>%
          kableExtra::kable_styling(position="c",full_width = F) %>%
          add_header_above(c(" "=1,"Estimated bias"=2),bold=T)
        print(second_table)
        cat(structural_bias_tables_captions)
      }
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
          vpc_plot <- plot_structural_vpc(vpc_tables_list$obs,vpc_tables_list$sim,shift_tab,idv=resmod_structural_details[[i]]$idv)
          print(vpc_plot)
          cat("\n\n")
          cat(vpc_captions)
        }
      }
      cat("\\pagebreak\n\n")
    }
  } else {
    print(resmod_structural_details)
  }
}