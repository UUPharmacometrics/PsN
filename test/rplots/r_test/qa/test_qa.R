library(testthat)
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

tool = 'qa'

#####################    1.Source functions which are going to be testet   ##################
# create a working directory for R scripts
source("../set.working.directory.R")
rscript.w.dir <- fun.rscript.w.dir()

directory_and_script <- find_r_files_in_subdir(toolname = tool,topdir = rscript.w.dir)

#source functions
for (i in 1:length(directory_and_script)) {
  source(directory_and_script[i])
}
#source other tool used functions
source(file.path(rscript.w.dir,"cdd/create.data.full.R"))
source(file.path(rscript.w.dir,"simeval/ofv.i_ofv_res.R"))
###################################    2.Input data    ######################################
source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

###################################     3. Make a test    ###################################
#...........................  (1) Test function get_rawres_ofv.R and get_ext_ofv.R .....................................  
context("qa, get_rawres_ofv and get_ext_ofv")
test_that("gets ofv values from csv and ext files",{
  expect_equal(587.27,.get_rawres_ofv(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv")))
  expect_equal(587.1,.get_rawres_ofv(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv"),row=4))
  expect_equal(-14.174,.get_ext_ofv(ext_file=file.path(files.w.dir,"qa_run1/new.ext")))
  expect_equal(-14.1487,.get_ext_ofv(ext_file=file.path(files.w.dir,"qa_run1/new.ext"),iteration=19))
})

#...........................  (2) Test function which_resmod_folders.R .....................................  
context("qa, which_resmod_folders")
test_that("gets all idv names from resmod folders",{
  expect_equal(c("TIME","PRED"),which_resmod_folders(file.path(files.w.dir,"qa_run1"),idv_name="TIME"))
  expect_equal(c(),which_resmod_folders(file.path(files.w.dir,"qa_missing_folders_files"),idv_name="TIME"))
})

#...........................  (3) Test function ofv_summary_table.R .....................................  
#create expected data
ofv_summary_1 <- data.frame(c("Nonlinear base model","Linearized base model before estimation","Linearized base model after estimation","Sum of individual OFV values"),
                          c("34.47394","23.4659815162199","25.7486537242589","-26.23943825"),
                          stringsAsFactors = F)
colnames(ofv_summary_1) <- c("","OFV")
ofv_summary_2 <- data.frame(c("Nonlinear base model","Linearized base model before estimation","Linearized base model after estimation","Sum of individual OFV values"),
                            c("34.47394","ERROR","ERROR","ERROR"),
                            stringsAsFactors = F)
colnames(ofv_summary_2) <- c("","OFV")
#compare
context("qa, ofv_summary_table")
test_that("make ofv summary table",{
  expect_equal(ofv_summary_1,ofv_summary_table(working.directory=file.path(files.w.dir,"qa_run1"),model.filename="run_100.mod"))
  expect_equal(ofv_summary_2,ofv_summary_table(working.directory=file.path(files.w.dir,"qa_missing_folders_files"),model.filename="run_100.mod"))
})

#...........................  (4) Test function get_resmod_table.R .....................................  
#create expected data
resmod_table_1 <- data.frame(iteration=rep("NA",10),dvid=rep("NA",10),
                             model=c("IIV_on_RUV","autocorrelation","dtbs","idv_varying_RUV","idv_varying_RUV_cutoff0","idv_varying_RUV_cutoff1","idv_varying_combined","idv_varying_theta","power","tdist"),
                             dOFV=c(10.6,0,21.67,12.05,0.27,0.58,31.28,1.33,0.92,43.62),
                             parameters=c("%CV=7.2","half-life=1.000","lambda=-0.136,Zeta=0.09","sdeps_0-t1=0.871,sdeps_t1-t2=1.220,sdeps_t2-inf=0.967,t1=0.64,t2=0.68",
                                          "sdeps_0-t0=0.875,sdeps_t0-inf=1.011,t0=0.64","sdeps_0-t0=1.069,sdeps_t0-inf=0.981,t0=0.68","sdeps_0-t1=0.868,th_0-t1=0.108,sdeps_t1-t2=1.203,sdeps_t2-inf=0.970,t1=0.64,t2=0.68",
                                          "th_0-t1=0.108,th_t1-t2=0.222,th_t2-inf=-0.020,t1=0.64,t2=0.68","delta_power=0.094","Df=2.550"),
                             stringsAsFactors = F)
resmod_table_2 <- data.frame(iteration=rep("NA",25),dvid=c(rep(1,8),rep(2,8),rep("sum",9)),
                             model=c(rep(c("IIV_on_RUV","autocorrelation","idv_varying_RUV","idv_varying_RUV_cutoff0","idv_varying_RUV_cutoff1","idv_varying_combined","idv_varying_theta","tdist"),3),"L2"),
                             dOFV=c(2.68,0,6.62,0.76,0.46,21.86,4.51,5.02,1.05,0,7.31,1.65,0.07,11.17,NA,0.02,5.15,0,29.82,2.41,8.52,43.35,37.18,5.14,3.11),
                             parameters=c("%CV=14.973","half-life=0.011","sdeps_0-t1=0.869,sdeps_t1-inf=0.989,t1=7.00","sdeps_0-t0=0.868,sdeps_t0-inf=0.999,t0=7.00",
                                          "sdeps_0-t0=0.931,sdeps_t0-inf=0.999,T0=18.00","sdeps_0-t1=0.834,th_0-t1=0.212,sdeps_t1-inf=0.982,th_t1-inf=-0.151,t1=7.00",
                                          "th_0-t1=0.212,th_t1-inf=-0.151,t1=7.00","df=9.238",
                                          "%CV=15.214","half-life=0.011","sdeps_0-t1=0.867,sdeps_t1-inf=0.982,t1=7.00","sdeps_0-t0=0.868,sdeps_t0-inf=1.020,t0=7.00",
                                          "sdeps_0-t0=0.925,sdeps_t0-inf=1.028,T0=18.00","sdeps_0-t1=0.820,th_0-t1=-0.234,sdeps_t1-inf=0.976,th_t1-inf=0.165,t1=7.00",
                                          "th_0-t1=-0.234,th_t1-inf=0.165,t1=7.00","df=119.865",
                                          rep("NA",9)),
                             stringsAsFactors = F)
#compare
context("qa, get_resmod_table")
test_that("open and manipulate resmod result table",{
  expect_equal(TRUE,get_resmod_table(directory=file.path(files.w.dir,"qa_run1"), idv="PRED")$resmod_file_exists)
  expect_equal(resmod_table_1,get_resmod_table(directory=file.path(files.w.dir,"qa_run1"), idv="PRED")$resmod_table)
  expect_equal(FALSE,get_resmod_table(directory=file.path(files.w.dir,"qa_run1"), idv="TIME")$resmod_file_exists)
  expect_equal(FALSE,get_resmod_table(directory=file.path(files.w.dir,"qa_run1"), idv="NEW")$resmod_file_exists)
  expect_equal(resmod_table_2,get_resmod_table(directory=file.path(files.w.dir,"qa_run2"), idv="TIME")$resmod_table)
})

#...........................  (5) Test function get_resmod_structural_dofv.R .....................................  
context("qa, get_resmod_structural_dofv")
test_that("get dofv value form the resmod table",{
  expect_equal(1.33,get_resmod_structural_dofv(directory=file.path(files.w.dir,"qa_run1"),idv="PRED",dvid="NA"))
  expect_equal(as.numeric(NA),get_resmod_structural_dofv(directory=file.path(files.w.dir,"qa_run2"),idv="TIME",dvid=2))
  expect_equal("ERROR",get_resmod_structural_dofv(directory=file.path(files.w.dir,"qa_run1"),idv="TIME",dvid=1))
})

#...........................  (6) Test function get_structural_overview_table.R .....................................  
#create expected data
str_overview_1 <- data.frame(c("TIME","PRED"),c(4.314,4.8),c(4,4),stringsAsFactors = F)
colnames(str_overview_1) <- c("","dOFV","Add.params")
str_overview_2 <- data.frame(c("(ORIG = 1)","TIME","PRED","(ORIG = 2)","TIME","PRED"),c("","4.51","4.8","","NA","0.4"),c("",3,3,"",3,3),
                             stringsAsFactors = F)
colnames(str_overview_2) <- c("","dOFV","Add.params")
str_overview_3 <- data.frame(c("(ORIG = 1)","TIME","TAD","PRED","(ORIG = 2)","TIME","TAD","PRED"),c("","4.51","ERROR","4.8","","NA","ERROR","0.4"),c("",3,"",3,"",3,"",3),
                             stringsAsFactors = F)
colnames(str_overview_3) <- c("","dOFV","Add.params")
str_overview_4 <- data.frame(c("TIME","TAD"),c("ERROR","ERROR"),c("",""),stringsAsFactors = F)
colnames(str_overview_4) <- c("","dOFV","Add.params")
str_overview_5 <- data.frame(c("TIME","PRED"),c("NA","4.8"),c(4,4),stringsAsFactors = F)
colnames(str_overview_5) <- c("","dOFV","Add.params")
str_overview_6 <- data.frame(c("TIME","TAD","PRED"),c("NA","ERROR","4.8"),c("4","","4"),stringsAsFactors = F)
colnames(str_overview_6) <- c("","dOFV","Add.params")
str_overview_7 <- data.frame(c("TIME","PRED"),c("NA","NA"),c(4,4),stringsAsFactors = F)
colnames(str_overview_7) <- c("","dOFV","Add.params")
str_overview_8 <- data.frame(c("(DVID = 1)","TIME","PRED","(DVID = 2)","TIME","PRED"),c("","NA","NA","","NA","NA"),c("","4","4","","4","4"),
                             stringsAsFactors = F)
colnames(str_overview_8) <- c("","dOFV","Add.params")
str_overview_9 <- data.frame("RESMOD","ERROR",stringsAsFactors = F)
colnames(str_overview_9) <- c("","dOFV")
str_overview_10 <- data.frame("RESMOD","SKIPPED",stringsAsFactors = F)
colnames(str_overview_10) <- c("","dOFV")
#compare
context("qa, get_structural_overview_table")
test_that("get structural overview table",{
  expect_equal(str_overview_1,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run3"),idv=c("TIME","PRED"),dvid_name="NA",groups=5))
  expect_equal(str_overview_2,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run2"),idv=c("TIME","PRED"),dvid_name="ORIG",groups=4))
  expect_equal(str_overview_3,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run2"),idv=c("TIME","TAD","PRED"),dvid_name="ORIG",groups=4))
  expect_equal(str_overview_4,get_structural_overview_table(directory=file.path(files.w.dir,"qa_missing_folders_files"),idv=c("TIME","TAD"),dvid_name="ORIG",groups=5))
  expect_equal(str_overview_5,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run5"),idv=c("TIME","PRED"),dvid_name="NA",groups=5))
  expect_equal(str_overview_6,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run6"),idv=c("TIME","TAD","PRED"),dvid_name="NA",groups=5))
  expect_equal(str_overview_7,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run7"),idv=c("TIME","PRED"),dvid_name="NA",groups=5))
  expect_equal(str_overview_8,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run8"),idv=c("TIME","PRED"),dvid_name="DVID",groups=5))
  expect_equal(str_overview_9,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run2"),idv=c(),dvid_name="ORIG",groups=4,skip=c('etas','cdd','frem')))
  expect_equal(str_overview_10,get_structural_overview_table(directory=file.path(files.w.dir,"qa_run2"),idv=c(),dvid_name="ORIG",groups=4,skip=c('etas','cdd','frem','resmod')))
})

#...........................  (7) Test function get_omega_values.R .....................................
context("qa, get_omega_values")
test_that("get omega variances and covariances",{
  expect_equal(data.frame("OMEGA.1.1."=c(0.0168598),"OMEGA.2.1."=c(0.229251),"OMEGA.2.2."=c(0.166343),stringsAsFactors = F),
               get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/new.ext"),"all"))
  expect_equal(data.frame("OMEGA.1.1."=c(0.0168598),"OMEGA.2.2."=c(0.229251),"OMEGA.3.3."=c(0.166343),"OMEGA.4.4."=c(0.0154853),stringsAsFactors = F),
               get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox.ext"),"var"))
  expect_equal(data.frame(),
               get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox.ext"),"cov"))
  expect_equal(data.frame(),
               get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/new_2.ext"),"all"))
  expect_equal(data.frame(),
               get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox_2.ext"),"var"))
  expect_equal(data.frame("OMEGA.2.1."=0.36,"OMEGA.3.2."=0.15),
               get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox_2.ext"),"cov"))
})

#...........................  (8) Test function get_param_var_tables.R .....................................
#run function
param_var_list_1 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run2"),model.filename="run_100.mod",skip=c())
param_var_list_2 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run1"),model.filename="run_100.mod",skip=c('cdd','simeval'))
param_var_list_3 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run3"),model.filename="run_100.mod",skip=c())
param_var_list_4 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_missing_folders_files"),model.filename="r.mod",skip=c('resmod'))
param_var_list_5 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run4"),model.filename="run_100.mod",skip=c('frem','resmod','scm','cdd','simeval'))
param_var_list_6 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_missing_folders_files"),model.filename="run_100.mod",skip=c())
param_var_list_7 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run4"),model.filename="run_100.mod",skip=c('frem','etas','scm','simeval'))
#create expected data
par_var_models_1 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution"), 
                             c(-3.73,-0.566,3.41,-1.03),
                             c(4,0,2,0),stringsAsFactors = F)
colnames(par_var_models_1) <- c("","dOFV","Add.params")
par_var_models_2 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution"), 
                               c("ERROR","-0.566000000000001","NA","NA"),
                               c("","3","",""),stringsAsFactors = F)
colnames(par_var_models_2) <- c("","dOFV","Add.params")
par_var_models_3 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution"), 
                               c("ERROR","NA","ERROR","NA"),
                               c("","","",""),stringsAsFactors = F)
colnames(par_var_models_3) <- c("","dOFV","Add.params")
par_var_models_4 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution"), 
                               c("ERROR","ERROR","ERROR","ERROR"),
                               stringsAsFactors = F)
colnames(par_var_models_4) <- c("","dOFV")
par_var_models_5 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution"), 
                               c("-3.73","NA","3.41","NA"),
                               c("0","","0",""),stringsAsFactors = F)
colnames(par_var_models_5) <- c("","dOFV","Add.params")
par_var_models_6 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution"), 
                               c("NA","NA","NA","NA"),
                               c("","","",""),stringsAsFactors = F)
colnames(par_var_models_6) <- c("","dOFV","Add.params")
par_var_models_7 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution"), 
                               c(rep("SKIPPED",4)),stringsAsFactors = F)
colnames(par_var_models_7) <- c("","dOFV")
#compare
context("qa, get_param_var_tables")
test_that("get parameter variability overview part, get delta ofv values",{
  expect_equal(par_var_models_1,param_var_list_1$par_var_models)
  expect_equal(-3.73,param_var_list_1$dofv_block)
  expect_equal(-0.566,param_var_list_1$dofv_box)
  expect_equal(-1.03,param_var_list_1$dofv_tdist)
  expect_equal(TRUE,param_var_list_1$fullblock_mod)
  expect_equal(TRUE,param_var_list_1$boxcox_mod)
  expect_equal(TRUE,param_var_list_1$add_etas_mod)
  expect_equal(TRUE,param_var_list_1$tdist_mod)
  expect_equal(par_var_models_2,param_var_list_2$par_var_models)
  expect_equal("ERROR",param_var_list_2$dofv_block)
  expect_equal(-0.566,param_var_list_2$dofv_box)
  expect_equal("NA",param_var_list_2$dofv_tdist)
  expect_equal(TRUE,param_var_list_2$fullblock_mod)
  expect_equal(TRUE,param_var_list_2$boxcox_mod)
  expect_equal(FALSE,param_var_list_2$add_etas_mod)
  expect_equal(FALSE,param_var_list_2$tdist_mod)
  expect_equal(par_var_models_3,param_var_list_3$par_var_models)
  expect_equal("ERROR",param_var_list_3$dofv_block)
  expect_equal("NA",param_var_list_3$dofv_box)
  expect_equal("NA",param_var_list_3$dofv_tdist)
  expect_equal(TRUE,param_var_list_3$fullblock_mod)
  expect_equal(FALSE,param_var_list_3$boxcox_mod)
  expect_equal(TRUE,param_var_list_3$add_etas_mod)
  expect_equal(FALSE,param_var_list_3$tdist_mod)
  expect_equal(par_var_models_4,param_var_list_4$par_var_models)
  expect_equal("NA",param_var_list_4$dofv_block)
  expect_equal("NA",param_var_list_4$dofv_box)
  expect_equal("NA",param_var_list_4$dofv_tdist)
  expect_equal(FALSE,param_var_list_4$fullblock_mod)
  expect_equal(FALSE,param_var_list_4$boxcox_mod)
  expect_equal(FALSE,param_var_list_4$add_etas_mod)
  expect_equal(FALSE,param_var_list_4$tdist_mod)
  expect_equal(par_var_models_5,param_var_list_5$par_var_models)
  expect_equal(-3.73,param_var_list_5$dofv_block)
  expect_equal("NA",param_var_list_5$dofv_box)
  expect_equal("NA",param_var_list_5$dofv_tdist)
  expect_equal(TRUE,param_var_list_5$fullblock_mod)
  expect_equal(FALSE,param_var_list_5$boxcox_mod)
  expect_equal(TRUE,param_var_list_5$add_etas_mod)
  expect_equal(FALSE,param_var_list_5$tdist_mod)
  expect_equal(par_var_models_6,param_var_list_6$par_var_models)
  expect_equal("NA",param_var_list_6$dofv_block)
  expect_equal("NA",param_var_list_6$dofv_box)
  expect_equal("NA",param_var_list_6$dofv_tdist)
  expect_equal(FALSE,param_var_list_6$fullblock_mod)
  expect_equal(FALSE,param_var_list_6$boxcox_mod)
  expect_equal(FALSE,param_var_list_6$add_etas_mod)
  expect_equal(FALSE,param_var_list_6$tdist_mod)
  expect_equal(par_var_models_7,param_var_list_7$par_var_models)
})

#...........................  (9) Test function get_full_omega_block.R .....................................
#run function
full_omega_block_table_1 <- data.frame(c("sd(1)","corr(2,1)","sd(2)","sd(3)","sd(4)","dOFV"),
                                       c("0.13","0.07","0.75","0.45","0.10","3.0"),
                                       c("0.1","NA","0.8","NA","NA",""),stringsAsFactors = F)
colnames(full_omega_block_table_1) <- c("","New","Old")
full_omega_block_table_2 <- data.frame(c("sd(1)","corr(2,1)","sd(2)","sd(3)","sd(4)"),
                                       c("0.13","0.07","0.75","0.45","0.10"),
                                       c("0.1","NA","0.8","NA","NA"),stringsAsFactors = F)
colnames(full_omega_block_table_2) <- c("","New","Old")
full_omega_block_table_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(full_omega_block_table_3) <- c("")
#compare
context("qa, get_full_omega_block")
test_that("get full omega block, extra table",{
  expect_equal(full_omega_block_table_1,
               get_full_omega_block(directory=file.path(files.w.dir,"qa_run4"),dofv_block=3.0014))
  expect_equal(full_omega_block_table_2,
               get_full_omega_block(directory=file.path(files.w.dir,"qa_run4"),dofv_block="NA"))
  expect_equal(full_omega_block_table_3,
               get_full_omega_block(directory=file.path(files.w.dir,"qa_missing_folders_files"),dofv_block=3.3414))
})

#...........................  (10) Test function get_param_extra_table.R .....................................
#create expected data
boxcox_lambdas_table_1 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)","dOFV"),
                                       c("2.11","NA","-1.14","-0.82","3.0"),
                                       c("0.13","0.48","0.41","0.12",""),
                                       c("0.13","0.77","0.00","0.00",""),stringsAsFactors = F)
colnames(boxcox_lambdas_table_1) <- c("","Lambda","New SD","Old SD")
boxcox_lambdas_table_2 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)"),
                                     c("2.11","NA","-1.14","-0.82"),
                                     c("0.13","0.48","0.41","0.12"),
                                     c("0.13","0.77","0.00","0.00"),stringsAsFactors = F)
colnames(boxcox_lambdas_table_2) <- c("","Lambda","New SD","Old SD")
boxcox_lambdas_table_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(boxcox_lambdas_table_3) <- c("")
boxcox_lambdas_orig_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(boxcox_lambdas_orig_3) <- c("")
#create expected data
tdist_table_1 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)","dOFV"),
                                     c("2.11","NA","-1.14","-0.82","3.0"),
                                     c("0.13","0.48","0.41","0.12",""),
                                     c("0.13","0.77","0.00","0.00",""),stringsAsFactors = F)
colnames(tdist_table_1) <- c("","Degrees of freedom","New SD","Old SD")
tdist_table_2 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)"),
                                     c("2.11","NA","-1.14","-0.82"),
                                     c("0.13","0.48","0.41","0.12"),
                                     c("0.13","0.77","0.00","0.00"),stringsAsFactors = F)
colnames(tdist_table_2) <- c("","Degrees of freedom","New SD","Old SD")
tdist_table_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(tdist_table_3) <- c("")
#compare
context("qa, get_param_extra_table")
test_that("get full omega block or tdist extra table",{
  expect_equal(boxcox_lambdas_table_1,
               get_param_extra_table(directory=file.path(files.w.dir,"qa_run1"),dofv=3.0014,param_model="boxcox")$param_extra_table)
  expect_equal(boxcox_lambdas_table_2,
               get_param_extra_table(directory=file.path(files.w.dir,"qa_run1"),dofv="ERROR",param_model="boxcox")$param_extra_table)
  expect_equal(boxcox_lambdas_table_3,
               get_param_extra_table(directory=file.path(files.w.dir,"qa_missing_folders_files"),dofv=3.3414,param_model="boxcox")$param_extra_table)
  expect_equal(boxcox_lambdas_orig_3,
               get_param_extra_table(directory=file.path(files.w.dir,"qa_missing_folders_files"),dofv=3.3414,param_model="boxcox")$param_extra_table_orig)
  expect_equal(tdist_table_1,
               get_param_extra_table(directory=file.path(files.w.dir,"qa_run1"),dofv=3.0014,param_model="tdist")$param_extra_table)
  expect_equal(tdist_table_2,
               get_param_extra_table(directory=file.path(files.w.dir,"qa_run1"),dofv="ERROR",param_model="tdist")$param_extra_table)
  expect_equal(tdist_table_3,
               get_param_extra_table(directory=file.path(files.w.dir,"qa_missing_folders_files"),dofv=3.3414,param_model="tdist")$param_extra_table)
  expect_equal(boxcox_lambdas_orig_3,
               get_param_extra_table(directory=file.path(files.w.dir,"qa_missing_folders_files"),dofv=3.3414,param_model="tdist")$param_extra_table_orig)
})

#...........................  (11) Test function get_eta_values.R .....................................
#input tables
theta_values_boxcox <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)"),c(2,0.2,0.4,0.1))
colnames(theta_values_boxcox) <- c("ETA","Lambda")
theta_values_tdist <- theta_values_boxcox
colnames(theta_values_tdist) <- c("ETA","Degrees of freedom")
# run function
eta_boxcox <- get_eta_values(working.directory=file.path(files.w.dir,"qa_run1/modelfit_run"),
                             theta_values=theta_values_boxcox,param_model="boxcox")
eta_tdist <- get_eta_values(working.directory=file.path(files.w.dir,"qa_run1/modelfit_run"),
                            theta_values=theta_values_tdist,param_model="tdist")
#compare
context("qa, get_eta_values")
test_that("get eta values from boxcox or tdist phi file",{
  expect_equal(c("ETA_name","value"),colnames(eta_boxcox))
  expect_equal(c(rep("ETA(1)",4),rep("ETA(2)",4),rep("ETA(3)",4),rep("ETA(4)",4)),eta_boxcox$ETA_name)
  expect_equal(c(0.216665,0.276354,0.231142,0.107048,-0.022947,0.101007,0.974127,-0.022947,
                 -0.126678,-0.369641,-0.525548,0.143994,-0.109397,-0.073727,0.034058,0.042088),round(eta_boxcox$value,6))
  expect_equal(data.frame(),
               get_eta_values(working.directory=file.path(files.w.dir,"qa_run2/modelfit_run"),
                              theta_values=theta_values_boxcox,param_model="boxcox"))
  expect_equal(c("ETA_name","value"),colnames(eta_tdist))
  expect_equal(c(rep("ETA(1)",4),rep("ETA(2)",4),rep("ETA(3)",4),rep("ETA(4)",4)),eta_tdist$ETA_name)
  expect_equal(c(0.204035,0.249988,0.215493,0.109567,0.042453,-0.174131,10.684607,0.042453,
                 -0.162517,-0.646372,-1.279824,0.175925,3.482807,2.374547,-1.100379,-1.357668),round(eta_tdist$value,6))
  expect_equal(data.frame(),
               get_eta_values(working.directory=file.path(files.w.dir,"qa_run2/modelfit_run"),
                              theta_values=theta_values_tdist,param_model="tdist"))
  
 })

#...........................  (12) Test function get_eta_transf_table.R .....................................
#create input data
boxcox_lambdas_orig_1 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)"),
                                    c(2.107970,NA,-1.138560),
                                    c(0.1298453,0.4788016,0.4078517),
                                    c(0.1341175,0.7722836,0.0000000),stringsAsFactors = F)
colnames(boxcox_lambdas_orig_1) <- c("ETA","Lambda","New SD","Old SD")
boxcox_lambdas_orig_2 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)","ETA(5)","ETA(6)","ETA(7)","ETA(8)","ETA(9)"),
                                    c(2.1,0,-1.1,0,0.2,0,-0.8,0,0),
                                    c(0.12,0.4,0.4,0.1,0.2,0.14,0.14,0.9,0.1),
                                    c(0.1,0.7,0,0,0,0,0,0,0),stringsAsFactors = F)
colnames(boxcox_lambdas_orig_2) <- c("ETA","Lambda","New SD","Old SD")

tdist_table_orig_1 <- boxcox_lambdas_orig_1
colnames(tdist_table_orig_1) <- c("ETA","Degrees of freedom","New SD","Old SD")
tdist_table_orig_2 <- boxcox_lambdas_orig_2
colnames(tdist_table_orig_2) <- c("ETA","Degrees of freedom","New SD","Old SD")

#run function
list_boxcox_shape_1 <- get_eta_transf_table(input_table=boxcox_lambdas_orig_1,seq_length.out=3)
boxcox_shape_table_1 <- list_boxcox_shape_1$eta_transf_table
make_boxcox_shape_plot_1 <- list_boxcox_shape_1$make_eta_transf_plot
fig_height_boxcox_1 <- list_boxcox_shape_1$fig_height
list_boxcox_shape_2 <- get_eta_transf_table(input_table=boxcox_lambdas_orig_2,seq_length.out=3)
boxcox_shape_table_2 <- list_boxcox_shape_2$eta_transf_table
make_boxcox_shape_plot_2 <- list_boxcox_shape_2$make_eta_transf_plot
fig_height_boxcox_2 <- list_boxcox_shape_2$fig_height
list_boxcox_shape_3 <- get_eta_transf_table(input_table=boxcox_lambdas_orig_3,seq_length.out=3)
make_boxcox_shape_plot_3 <- list_boxcox_shape_3$make_eta_transf_plot
fig_height_boxcox_3 <- list_boxcox_shape_3$fig_height

list_tdist_tables_1 <- get_eta_transf_table(input_table=tdist_table_orig_1,seq_length.out=3)
tdist_table_1 <- list_tdist_tables_1$eta_transf_table
make_tdist_plot_1 <- list_tdist_tables_1$make_eta_transf_plot
fig_height_tdist_1 <- list_tdist_tables_1$fig_height
list_tdist_tables_2 <- get_eta_transf_table(input_table=tdist_table_orig_2,seq_length.out=3)
tdist_table_2 <- list_tdist_tables_2$eta_transf_table
make_tdist_plot_2 <- list_tdist_tables_2$make_eta_transf_plot
fig_height_tdist_2 <- list_tdist_tables_2$fig_height
#compare
context("qa, get_eta_transf_table")
test_that("get create input table for transformed density plot",{
  expect_equal(c("ETA_name","density","type","eta"),colnames(boxcox_shape_table_1))
  expect_equal(c(18,4),dim(boxcox_shape_table_1))
  expect_equal(c(rep("ETA(1)",6),rep("ETA(2)",6),rep("ETA(3)",6)),boxcox_shape_table_1$ETA_name)
  expect_equal(rep(c(rep("ETA",3),rep("ETAT",3)),3),boxcox_shape_table_1$type)
  expect_equal(c(-0.6751095,0,0.6751095,-0.3600795,0,1.4943333,-2.4894512,0,2.4894512,rep(NA,3),
                 -2.1205587,0,2.1205587,-8.9436407,0,0.7997624),
               round(boxcox_shape_table_1$eta),7)
  expect_equal(TRUE,make_boxcox_shape_plot_1)
  expect_equal(7,fig_height_boxcox_1)
  expect_equal(c("ETA_name","density","type","eta"),colnames(boxcox_shape_table_2))
  expect_equal(c(54,4),dim(boxcox_shape_table_2))
  expect_equal(c(rep("ETA(1)",6),rep("ETA(2)",6),rep("ETA(3)",6),
                 rep("ETA(4)",6),rep("ETA(5)",6),rep("ETA(6)",6),
                 rep("ETA(7)",6),rep("ETA(8)",6),rep("ETA(9)",6)),boxcox_shape_table_2$ETA_name)
  expect_equal(rep(c(rep("ETA",3),rep("ETAT",3)),9),boxcox_shape_table_2$type)
  expect_equal(TRUE,make_boxcox_shape_plot_2)
  expect_equal(15,fig_height_boxcox_2)
  expect_equal(15,fig_height_boxcox_2)
  expect_equal(FALSE,make_boxcox_shape_plot_3)
  expect_equal(15,fig_height_boxcox_3)
  
  expect_equal(c("ETA_name","density","type","eta"),colnames(tdist_table_1))
  expect_equal(c(18,4),dim(tdist_table_1))
  expect_equal(c(rep("ETA(1)",6),rep("ETA(2)",6),rep("ETA(3)",6)),tdist_table_1$ETA_name)
  expect_equal(rep(c(rep("ETA",3),rep("ETAT",3)),3),tdist_table_1$type)
  expect_equal(c(-0.6751095,0,0.6751095,-0.3600795,0,1.4943333,-2.4894512,0,2.4894512,rep(NA,3),
                 -2.1205587,0,2.1205587,-8.9436407,0,0.7997624),
               round(tdist_table_1$eta),7)
  expect_equal(TRUE,make_tdist_plot_1)
  expect_equal(7,fig_height_tdist_1)
  expect_equal(c("ETA_name","density","type","eta"),colnames(tdist_table_2))
  expect_equal(c(54,4),dim(tdist_table_2))
  expect_equal(c(rep("ETA(1)",6),rep("ETA(2)",6),rep("ETA(3)",6),
                 rep("ETA(4)",6),rep("ETA(5)",6),rep("ETA(6)",6),
                 rep("ETA(7)",6),rep("ETA(8)",6),rep("ETA(9)",6)),tdist_table_2$ETA_name)
  expect_equal(rep(c(rep("ETA",3),rep("ETAT",3)),9),tdist_table_2$type)
  expect_equal(TRUE,make_tdist_plot_2)
  expect_equal(15,fig_height_tdist_2)
  expect_equal(15,fig_height_tdist_2)
})

#...........................  (13) Test function get_all_covariates.R .....................................
#expected data
frem_table_1 <- data.frame("FREM","ERROR","",stringsAsFactors = F)
colnames(frem_table_1) <- c("","dOFV","Add.params")
frem_table_2 <- data.frame("FREM","NA","",stringsAsFactors = F)
colnames(frem_table_2) <- c("","dOFV","Add.params")
frem_table_3 <- data.frame("ALL",3.6,6,stringsAsFactors = F)
colnames(frem_table_3) <- c("","dOFV","Add.params")
frem_table_4 <- data.frame("ALL",7.1,0,stringsAsFactors = F)
colnames(frem_table_4) <- c("","dOFV","Add.params")
frem_table_5 <- data.frame("FREM","SKIPPED","",stringsAsFactors = F)
colnames(frem_table_5) <- c("","dOFV","Add.params")
#compare
context("qa, get_all_covariates")
test_that("get FREM covariates table",{
  expect_equal(frem_table_1,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_missing_folders_files/frem_run"),covariates=c('AGE'),
                                               categorical=c(),parameters=c(),dofv_full_block=3.5,skip=c('etas','resmod','scm','cdd','simeval'))$frem_table)
  expect_equal(FALSE,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_missing_folders_files/frem_run"),covariates=c('AGE'),
                                        categorical=c(),parameters=c(),dofv_full_block=3.5,skip=c())$frem_files_exists)
  expect_equal(frem_table_2,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),covariates=c(),categorical=c(),
                                               parameters=c(),dofv_full_block=3.5,skip=c('resmod','scm','cdd','simeval'))$frem_table)
  expect_equal(FALSE,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),covariates=c(),categorical=c(),
                                        parameters=C(),dofv_full_block=3.5,skip=c())$frem_files_exists)
  expect_equal(frem_table_3,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),covariates=c('SEX'),
                                               categorical=c('AGE','WGT'),parameters=c('K1','K2'),dofv_full_block=3.5,skip=c())$frem_table)
  expect_equal(TRUE,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),covariates=c('SEX'),categorical=c('AGE','WGT'),
                                       parameters=c('K1','K2'),dofv_full_block=3.5,skip=c())$frem_files_exists)
  expect_equal(frem_table_4,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),covariates=c('SEX'),
                                               categorical=c('AGE','WGT'),parameters=c(),dofv_full_block='NA',skip=c('resmod'))$frem_table)
  expect_equal(TRUE,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),covariates=c('SEX'),categorical=c('AGE','WGT'),
                                       parameters=c(),dofv_full_block='NA',skip=c('etas','resmod','scm','cdd','simeval'))$frem_files_exists)
  expect_equal(frem_table_5,get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),covariates=c('SEX'),
                                               categorical=c('AGE','WGT'),parameters=c(),dofv_full_block='NA',skip=c('resmod','frem','cdd'))$frem_table)
  
})

#...........................  (14) Test function get_scm_table.R .....................................
#expected data
scm_table_1 <- data.frame("SCM","ERROR",stringsAsFactors = F)
colnames(scm_table_1) <- c("","dOFV")
max_scm_table_1 <- data.frame("SCM","ERROR","",stringsAsFactors = F)
colnames(max_scm_table_1) <- c("","dOFV","Add.params")
scm_table_2 <- data.frame("SCM","NA",stringsAsFactors = F)
colnames(scm_table_2) <- c("","dOFV")
max_scm_table_2 <- data.frame("SCM","NA","",stringsAsFactors = F)
colnames(max_scm_table_2) <- c("","dOFV","Add.params")
scm_table_3 <- data.frame(c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4"),c(2.87,0.47,0.17,0.12),c(-0.03,0.05,0.004,-0.012),stringsAsFactors = F)
colnames(scm_table_3) <- c("","dOFV","Coef")
max_scm_table_3 <- data.frame("CLAPGR-4",2.87,1,stringsAsFactors = F)
colnames(max_scm_table_3) <- c("","dOFV","Add.params")
scm_table_4 <- data.frame(c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4"),c(NA,0.47,0.17,0.12),c(NA,0.05,0.004,-0.012),stringsAsFactors = F)
colnames(scm_table_4) <- c("","dOFV","Coef")
max_scm_table_4 <- data.frame("CLWGT-4",0.47,1,stringsAsFactors = F)
colnames(max_scm_table_4) <- c("","dOFV","Add.params")
max_scm_table_5 <- data.frame("SCM","SKIPPED","",stringsAsFactors = F)
colnames(max_scm_table_5) <- c("","dOFV","Add.params")
scm_table_5 <- data.frame("SCM","SKIPPED",stringsAsFactors = F)
colnames(scm_table_5) <- c("","dOFV")

#compare
context("qa, get_scm_table")
test_that("get SCM table",{
  expect_equal(scm_table_1,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_missing_folders_files/scm_run/raw_results_scm.csv"),
                             parameters=c('K1','K2'),covariates=c('AGE'),categorical=c(),skip=c())$scm_table)
  expect_equal(max_scm_table_1,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_missing_folders_files/scm_run/raw_results_scm.csv"),
                             parameters=c('K1','K2'),covariates=c('AGE'),categorical=c(),skip=c())$max_scm_table)
  expect_equal(FALSE,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_missing_folders_files/scm_run/raw_results_scm.csv"),
                             parameters=c('K1','K2'),covariates=c('AGE'),categorical=c(),skip=c())$scm_files_exists)
  expect_equal(scm_table_2,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv"),parameters=c('K1','K2'),
                             covariates=c(),categorical=c(),skip=c())$scm_table)
  expect_equal(max_scm_table_2,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv"),parameters=c('K1','K2'),
                             covariates=c(),categorical=c(),skip=c())$max_scm_table)
  expect_equal(FALSE,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv"),parameters=c('K1','K2'),
                             covariates=c(),categorical=c(),skip=c())$scm_files_exists)
  expect_equal(scm_table_3,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c())$scm_table)
  expect_equal(max_scm_table_3,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c())$max_scm_table)
  expect_equal(TRUE,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c())$scm_files_exists)
  expect_equal(scm_table_3,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run2/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c())$scm_table)
  expect_equal(max_scm_table_3,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run2/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c('frem','resmod','etas','cdd','simeval'))$max_scm_table)
  expect_equal(TRUE,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run2/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c('frem','resmod','etas','cdd','simeval'))$scm_files_exists)
  expect_equal(scm_table_4,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run3/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c('frem','resmod','etas','cdd','simeval'))$scm_table)
  expect_equal(max_scm_table_4,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run3/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c('frem','resmod','etas','cdd','simeval'))$max_scm_table)
  expect_equal(TRUE,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run3/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c('frem','resmod','etas','cdd','simeval'))$scm_files_exists)
  expect_equal(max_scm_table_5,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run3/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c('frem','scm'))$max_scm_table)
  expect_equal(scm_table_5,
               get_scm_table(rawres_file=file.path(files.w.dir,"qa_run3/scm_run/raw_results_scm.csv"),parameters=c('CL','V'),
                             covariates=c('APGR,WGT'),categorical=c(),skip=c('frem','scm'))$scm_table)
})

#...........................  (15) Test function get_covariates_table.R .....................................
#expected data
covariates_table_1 <- data.frame(c("FREM","SCM"),c("ERROR","ERROR"),c("",""),stringsAsFactors = F)
colnames(covariates_table_1) <- c("","dOFV","Add.params")
covariates_extra_table_1 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("ERROR","ERROR"),stringsAsFactors = F)
covariates_table_2 <- data.frame(c("FREM","SCM"),c("NA","NA"),c("",""),stringsAsFactors = F)
colnames(covariates_table_2) <- c("","dOFV","Add.params")
covariates_extra_table_2 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("NA","NA"),stringsAsFactors = F)
covariates_table_3 <- data.frame(c("FREM","CLAPGR-4"),c("NA","2.87"),c("","1"),stringsAsFactors = F)
colnames(covariates_table_3) <- c("","dOFV","Add.params")
covariates_extra_table_3 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                       "dOFV"=c("2.9","0.5","0.2","0.1","3.6","NA"),
                                       "Coefficient"=c("-0.030","0.050","0.004","-0.012","",""),stringsAsFactors = F)
covariates_table_4 <- data.frame(c("FREM","CLAPGR-4"),c(3.6,2.87),c(6,1),stringsAsFactors = F)
colnames(covariates_table_4) <- c("","dOFV","Add.params")
covariates_extra_table_4 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                       "dOFV"=c("2.9","0.5","0.2","0.1","3.6","3.6"),
                                       "Coefficient"=c("-0.030","0.050","0.004","-0.012","",""),stringsAsFactors = F)
covariates_table_5 <- data.frame(c("FREM","CLAPGR-4"),c("ERROR","2.87"),c("","1"),stringsAsFactors = F)
colnames(covariates_table_5) <- c("","dOFV","Add.params")
covariates_extra_table_5 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                       "dOFV"=c("2.9","0.5","0.2","0.1","3.6","ERROR"),
                                       "Coefficient"=c("-0.030","0.050","0.004","-0.012","",""),stringsAsFactors = F)
covariates_table_6 <- data.frame(c("FREM","SCM"),c("7.1","ERROR"),c("0",""),stringsAsFactors = F)
colnames(covariates_table_6) <- c("","dOFV","Add.params")
covariates_extra_table_6 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("ERROR","7.1"),stringsAsFactors = F)
covariates_table_7 <- data.frame(c("FREM","CLWGT-4"),c(3.6,0.47),c(6,1),stringsAsFactors = F)
colnames(covariates_table_7) <- c("","dOFV","Add.params")
covariates_extra_table_7 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                       "dOFV"=c("NA","0.5","0.2","0.1","NA","3.6"),
                                       "Coefficient"=c("NA","0.050","0.004","-0.012","",""),stringsAsFactors = F)
covariates_table_8 <- data.frame(c("FREM","SCM"),c("SKIPPED","SKIPPED"),c("",""),stringsAsFactors = F)
colnames(covariates_table_8) <- c("","dOFV","Add.params")
covariates_extra_table_8 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("SKIPPED","SKIPPED"),stringsAsFactors = F)
#compare
context("qa, get_covariates_table")
test_that("create covariate table (FREM and SCM) and extra table",{
  expect_equal(covariates_table_1,get_covariates_table(frem_table=frem_table_1,scm_table=scm_table_1,max_scm_table=max_scm_table_1)$covariates_table)
  expect_equal(covariates_extra_table_1,get_covariates_table(frem_table=frem_table_1,scm_table=scm_table_1,max_scm_table=max_scm_table_1)$covariates_extra_table)
  expect_equal(covariates_table_2,get_covariates_table(frem_table=frem_table_2,scm_table=scm_table_2,max_scm_table=max_scm_table_2)$covariates_table)
  expect_equal(covariates_extra_table_2,get_covariates_table(frem_table=frem_table_2,scm_table=scm_table_2,max_scm_table=max_scm_table_2)$covariates_extra_table)
  expect_equal(covariates_table_3,get_covariates_table(frem_table=frem_table_2,scm_table=scm_table_3,max_scm_table=max_scm_table_3)$covariates_table)
  expect_equal(covariates_extra_table_3,get_covariates_table(frem_table=frem_table_2,scm_table=scm_table_3,max_scm_table=max_scm_table_3)$covariates_extra_table)
  expect_equal(covariates_table_4,get_covariates_table(frem_table=frem_table_3,scm_table=scm_table_3,max_scm_table=max_scm_table_3)$covariates_table)
  expect_equal(covariates_extra_table_4,get_covariates_table(frem_table=frem_table_3,scm_table=scm_table_3,max_scm_table=max_scm_table_3)$covariates_extra_table)
  expect_equal(covariates_table_5,get_covariates_table(frem_table=frem_table_1,scm_table=scm_table_3,max_scm_table=max_scm_table_3)$covariates_table)
  expect_equal(covariates_extra_table_5,get_covariates_table(frem_table=frem_table_1,scm_table=scm_table_3,max_scm_table=max_scm_table_3)$covariates_extra_table)
  expect_equal(covariates_table_6,get_covariates_table(frem_table=frem_table_4,scm_table=scm_table_1,max_scm_table=max_scm_table_1)$covariates_table)
  expect_equal(covariates_extra_table_6,get_covariates_table(frem_table=frem_table_4,scm_table=scm_table_1,max_scm_table=max_scm_table_1)$covariates_extra_table)
  expect_equal(covariates_table_7,get_covariates_table(frem_table=frem_table_3,scm_table=scm_table_4,max_scm_table=max_scm_table_4)$covariates_table)
  expect_equal(covariates_extra_table_7,get_covariates_table(frem_table=frem_table_3,scm_table=scm_table_4,max_scm_table=max_scm_table_4)$covariates_extra_table)
  expect_equal(covariates_table_8,get_covariates_table(frem_table=frem_table_5,scm_table=scm_table_5,max_scm_table=max_scm_table_5)$covariates_table)
  expect_equal(covariates_extra_table_8,get_covariates_table(frem_table=frem_table_5,scm_table=scm_table_5,max_scm_table=max_scm_table_5)$covariates_extra_table)
})

#...........................  (16) Test function get_resmod_ruv_table.R .....................................
#run function
resmod_list_1 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run1"),idv_name="TIME",dvid_name="ORIG",skip=c())
resmod_list_2 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run3"),idv_name="TIME",dvid_name="NA",skip=c('frem','scm','etas','cdd','simeval'))
resmod_list_3 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run2"),idv_name="TIME",dvid_name="ORIG",skip=c('etas','cdd','simeval'))
resmod_list_4 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run7"),idv_name="TIME",dvid_name="NA",skip=c('frem','simeval'))
resmod_list_5 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run3"),idv_name="TIME",dvid_name="NA",skip=c('resmod'))
#expected data
resmod_ruv_table_list_1 <- data.frame("ERROR",stringsAsFactors = F)
colnames(resmod_ruv_table_list_1) <- ""
resmod_ruv_overview_1 <- data.frame("RESMOD","ERROR",stringsAsFactors = F)
colnames(resmod_ruv_overview_1) <- c("","dOFV")
resmod_ruv_table_list_2 <- data.frame("Model"=c("tdist","dtbs","IIV on RUV","power","time varying","autocorrelation"),
                                      "dOFV"=c("43.6","21.7","10.6","0.9","0.6","0.0"),
                                      "Additional parameters"=c(1,2,1,1,2,1),
                                      "Parameter values"=c("Df=2.550","lambda=-0.136,Zeta=0.09","%CV=7.2","delta_power=0.094","sdeps_0-t0=1.069,sdeps_t0-inf=0.981,t0=0.68","half-life=1.000"),
                                      stringsAsFactors = F)
colnames(resmod_ruv_table_list_2) <- c("Model","dOFV","Additional parameters","Parameter values")
resmod_ruv_overview_2 <- data.frame(c("tdist","dtbs"),c(43.62,21.67),c(1,2),stringsAsFactors = F)
colnames(resmod_ruv_overview_2) <- c("","dOFV","Add.params")
resmod_ruv_table_list_3_a <- data.frame("Model"=c("tdist","IIV on RUV","time varying","autocorrelation"),
                                      "dOFV"=c("5.0","2.7","0.8","0.0"),
                                      "Additional parameters"=c(1,1,2,1),
                                      "Parameter values"=c("df=9.238","%CV=14.973","sdeps_0-t0=0.868,sdeps_t0-inf=0.999,t0=7.00","half-life=0.011"),
                                      stringsAsFactors = F)
colnames(resmod_ruv_table_list_3_a) <- c("Model","dOFV","Additional parameters","Parameter values")
resmod_ruv_table_list_3_b <- data.frame("Model"=c("time varying","IIV on RUV","tdist","autocorrelation"),
                                        "dOFV"=c("1.65","1.05","0.02","0.00"),
                                        "Additional parameters"=c(2,1,1,1),
                                        "Parameter values"=c("sdeps_0-t0=0.868,sdeps_t0-inf=1.020,t0=7.00","%CV=15.214","df=119.865","half-life=0.011"),
                                        stringsAsFactors = F)
colnames(resmod_ruv_table_list_3_b) <- c("Model","dOFV","Additional parameters","Parameter values")
resmod_ruv_overview_3 <- data.frame(c("(ORIG = 1)","tdist","IIV on RUV","(ORIG = 2)","time varying","IIV on RUV"),
                                    c("","5.02","2.68","","1.65","1.05"),
                                    c("","1","1","","2","1"),stringsAsFactors = F)
colnames(resmod_ruv_overview_3) <- c("","dOFV","Add.params")
resmod_ruv_table_list_4 <- data.frame("Model"=c("IIV on RUV","autocorrelation","dtbs","power","tdist","time varying"),
                                      "dOFV"=c("NA","NA","NA","NA","NA","NA"),
                                      "Additional parameters"=c(rep("",6)),
                                      "Parameter values"=c(rep("",6)),
                                      stringsAsFactors = F)
colnames(resmod_ruv_table_list_4) <- c("Model","dOFV","Additional parameters","Parameter values")
resmod_ruv_overview_4 <- data.frame(c("RESMOD"),c("NA"),c(""),stringsAsFactors = F)
colnames(resmod_ruv_overview_4) <- c("","dOFV","Add.params")
resmod_ruv_overview_5 <- data.frame("RESMOD","SKIPPED",stringsAsFactors = F)
colnames(resmod_ruv_overview_5) <- c("","dOFV")
#compare
context("qa, get_resmod_ruv_table")
test_that("create resmod ruv table",{
  expect_equal(1,length(resmod_list_1$resmod_ruv_table_list))
  expect_equal(resmod_ruv_table_list_1,resmod_list_1$resmod_ruv_table_list[[1]])
  expect_equal(resmod_ruv_overview_1,resmod_list_1$resmod_ruv_overview)
  expect_equal('NA',resmod_list_1$dvid_nr)
  expect_equal(1,length(resmod_list_2$resmod_ruv_table_list))
  expect_equal(resmod_ruv_table_list_2,resmod_list_2$resmod_ruv_table_list[[1]])
  expect_equal(resmod_ruv_overview_2,resmod_list_2$resmod_ruv_overview)
  expect_equal('NA',resmod_list_2$dvid_nr)
  expect_equal(2,length(resmod_list_3$resmod_ruv_table_list))
  expect_equal(resmod_ruv_table_list_3_a,resmod_list_3$resmod_ruv_table_list[[1]])
  expect_equal(resmod_ruv_table_list_3_b,resmod_list_3$resmod_ruv_table_list[[2]])
  expect_equal(resmod_ruv_overview_3,resmod_list_3$resmod_ruv_overview)
  expect_equal(c(1,2),resmod_list_3$dvid_nr)
  expect_equal(1,length(resmod_list_4$resmod_ruv_table_list))
  expect_equal(resmod_ruv_table_list_4,resmod_list_4$resmod_ruv_table_list[[1]])
  # expect_equal(resmod_ruv_overview_4,resmod_list_4$resmod_ruv_overview)
  expect_equal('NA',resmod_list_4$dvid_nr)
  expect_equal(resmod_ruv_overview_5,resmod_list_5$resmod_ruv_overview)
})

#...........................  (17) Test function get_ii_table.R .....................................
#fun function
list_ii_1 <- get_ii_table(raw.results.file=file.path(files.w.dir,"qa_missing_folders_files/cdd_run/raw_results_model_linbase.csv"),
                          skipped.id.file=file.path(files.w.dir,"qa_missing_folders_files/cdd_run/skipped_individuals1.csv"),
                          cutoff=3.84,max_rows=3,skip=c('frem','resmod','etas','scm','simeval'))
list_ii_2 <- get_ii_table(raw.results.file=file.path(files.w.dir,"qa_run2/cdd_run/raw_results_linbase.csv"),
                          skipped.id.file=file.path(files.w.dir,"qa_run2/cdd_run/skipped_individuals1.csv"),
                          cutoff=3.84,max_rows=3,skip=c())
list_ii_3 <- get_ii_table(raw.results.file=file.path(files.w.dir,"qa_run3/cdd_run/raw_results_linbase.csv"),
                          skipped.id.file=file.path(files.w.dir,"qa_run3/cdd_run/skipped_individuals1.csv"),
                          cutoff=3.84,max_rows=3,skip=c('scm','simeval'))
list_ii_4 <- get_ii_table(raw.results.file=file.path(files.w.dir,"qa_run1/cdd_run/raw_results_linbase.csv"),
                          skipped.id.file=file.path(files.w.dir,"qa_run1/cdd_run/skipped_individuals1.csv"),
                          cutoff=1,max_rows=3,skip=c('frem','resmod','etas','scm','simeval'))
list_ii_5 <- get_ii_table(raw.results.file=file.path(files.w.dir,"qa_run1/cdd_run/raw_results_linbase.csv"),
                          skipped.id.file=file.path(files.w.dir,"qa_run1/cdd_run/skipped_individuals1.csv"),
                          cutoff=0.1,max_rows=3,skip=c())
list_ii_6 <- get_ii_table(raw.results.file=file.path(files.w.dir,"qa_run1/cdd_run/raw_results_linbase.csv"),
                          skipped.id.file=file.path(files.w.dir,"qa_run1/cdd_run/skipped_individuals1.csv"),
                          cutoff=0.000001,max_rows=2,skip=c('frem','resmod','etas','scm','simeval'))
list_ii_7 <- get_ii_table(raw.results.file=file.path(files.w.dir,"qa_run4/cdd_run/raw_results_linbase.csv"),
                          skipped.id.file=file.path(files.w.dir,"qa_run4/cdd_run/skipped_individuals1.csv"),
                          cutoff=3.84,max_rows=3,skip=c('frem','resmod','etas','scm','simeval'))
list_ii_8 <- get_ii_table(raw.results.file=file.path(files.w.dir,"qa_run4/cdd_run/raw_results_linbase.csv"),
                          skipped.id.file=file.path(files.w.dir,"qa_run4/cdd_run/skipped_individuals1.csv"),
                          cutoff=3.84,max_rows=3,skip=c('cdd','scm','simeval'))
#expected data
ERROR_table <- data.frame("ERROR",stringsAsFactors = F)
colnames(ERROR_table) <- ""
cdd_highest_dofv_1 <- data.frame("CDD","ERROR",stringsAsFactors = F)
colnames(cdd_highest_dofv_1) <- c("","dOFV")
ii_table_3 <- data.frame("All dOFV values are negative",stringsAsFactors = F)
colnames(ii_table_3) <- ""
cdd_highest_dofv_3 <- data.frame("All dOFV values are negative","",stringsAsFactors = F)
colnames(cdd_highest_dofv_3) <- c("","dOFV")
cdd_data_4 <- data.frame(id=c(11,34,14,57,3,7,8),
                         dOFV=c(0.11014,2.00265,0.12541,0.08563,0.00003,0.01568,1.03525))
ii_table_4 <- data.frame(c("Subject 34","Subject 8"),c("2.0","1.0"),stringsAsFactors = F)
colnames(ii_table_4) <- c("Subjects","dOFV")
cdd_highest_dofv_4 <- data.frame("Subject 34",2.00265,stringsAsFactors = F)
colnames(cdd_highest_dofv_4) <- c("","dOFV")
ii_table_5 <- data.frame(c("Subject 34","Subject 8","Subject 14"),c("2.0","1.0","0.1"),stringsAsFactors = F)
colnames(ii_table_5) <- c("Subjects","dOFV")
ii_table_6 <- data.frame(c("Subject 34","Subject 8"),c("2.0","1.0"),stringsAsFactors = F)
colnames(ii_table_6) <- c("Subjects","dOFV")
cdd_data_7 <- data.frame(id=c(11,12,34,14,57),
                         dOFV=c(0.01014,0.06941,0.00265,0.02541,0.08563))
ii_table_7 <- data.frame(c("No influential individuals detected"),stringsAsFactors = F)
colnames(ii_table_7) <- c("")
cdd_highest_dofv_7 <- data.frame("None","",stringsAsFactors = F)
colnames(cdd_highest_dofv_7) <- c("","dOFV")
cdd_data_8 <- data.frame("SKIPPED",stringsAsFactors = F)
colnames(cdd_data_8) <- NULL
ii_table_8 <- data.frame(c("No influential individuals detected"),stringsAsFactors = F)
colnames(ii_table_8) <- c("")
cdd_highest_dofv_8 <- data.frame("CDD","SKIPPED",stringsAsFactors = F)
colnames(cdd_highest_dofv_8) <- c("","dOFV")

#compare
context("qa, get_ii_table")
test_that("create influential individuals table",{
  expect_equal(FALSE,list_ii_1$cdd_files_exist)
  expect_equal(c(),list_ii_1$all_dofv)
  expect_equal(ERROR_table,list_ii_1$cdd.data)
  expect_equal(cdd_highest_dofv_1,list_ii_1$cdd_highest_dofv)
  expect_equal(ERROR_table,list_ii_1$ii_table)
  expect_equal(c(),list_ii_1$infl_id)
  expect_equal(5,list_ii_1$fig_height_infl)
  expect_equal(TRUE,list_ii_2$cdd_files_exist)
  expect_equal(c(),list_ii_2$all_dofv)
  expect_equal(ERROR_table,list_ii_2$cdd.data)
  expect_equal(cdd_highest_dofv_1,list_ii_2$cdd_highest_dofv)
  expect_equal(ERROR_table,list_ii_2$ii_table)
  expect_equal(c(),list_ii_2$infl_id)
  expect_equal(5,list_ii_2$fig_height_infl)
  expect_equal(TRUE,list_ii_3$cdd_files_exist)
  expect_equal(c(-0.01014,-0.06941,-0.00265,-0.02541,-0.08563),list_ii_3$all_dofv)
  expect_equal(data.frame(),list_ii_3$cdd.data)
  expect_equal(cdd_highest_dofv_3,list_ii_3$cdd_highest_dofv)
  expect_equal(ii_table_3,list_ii_3$ii_table)
  expect_equal(c(),list_ii_3$infl_id)
  expect_equal(5,list_ii_3$fig_height_infl)
  expect_equal(TRUE,list_ii_4$cdd_files_exist)
  expect_equal(c(0.11014,-0.06941,2.00265,0.12541,0.08563,0.00003,-2.08563,0.01568,1.03525),list_ii_4$all_dofv)
  expect_equal(cdd_data_4,list_ii_4$cdd.data)
  expect_equal(cdd_highest_dofv_4,list_ii_4$cdd_highest_dofv)
  expect_equal(ii_table_4,list_ii_4$ii_table)
  expect_equal(c(34,8),list_ii_4$infl_id)
  expect_equal(5,list_ii_4$fig_height_infl)
  expect_equal(TRUE,list_ii_5$cdd_files_exist)
  expect_equal(c(0.11014,-0.06941,2.00265,0.12541,0.08563,0.00003,-2.08563,0.01568,1.03525),list_ii_5$all_dofv)
  expect_equal(cdd_data_4,list_ii_5$cdd.data)
  expect_equal(cdd_highest_dofv_4,list_ii_5$cdd_highest_dofv)
  expect_equal(ii_table_5,list_ii_5$ii_table)
  expect_equal(c(34,8,14,11),list_ii_5$infl_id)
  expect_equal(7,list_ii_5$fig_height_infl)
  expect_equal(TRUE,list_ii_6$cdd_files_exist)
  expect_equal(c(0.11014,-0.06941,2.00265,0.12541,0.08563,0.00003,-2.08563,0.01568,1.03525),list_ii_6$all_dofv)
  expect_equal(cdd_data_4,list_ii_6$cdd.data)
  expect_equal(cdd_highest_dofv_4,list_ii_6$cdd_highest_dofv)
  expect_equal(ii_table_6,list_ii_6$ii_table)
  expect_equal(c(34,8,14,11,57,7,3),list_ii_6$infl_id)
  expect_equal(15,list_ii_6$fig_height_infl)
  expect_equal(TRUE,list_ii_7$cdd_files_exist)
  expect_equal(c(0.01014,0.06941,0.00265,0.02541,0.08563),list_ii_7$all_dofv)
  expect_equal(cdd_data_7,list_ii_7$cdd.data)
  expect_equal(cdd_highest_dofv_7,list_ii_7$cdd_highest_dofv)
  expect_equal(ii_table_7,list_ii_7$ii_table)
  expect_equal(c(),list_ii_7$infl_id)
  expect_equal(5,list_ii_7$fig_height_infl)
  expect_equal(cdd_data_8,list_ii_8$cdd.data)
  expect_equal(cdd_highest_dofv_8,list_ii_8$cdd_highest_dofv)
})

#...........................  (18) Test function get_outliers_table.R .....................................
#run function
list_simeval_1 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_missing_folders_files/simeval_run"),
                                     cdd.data=cdd_data_4,skip=c('frem','resmod','etas','scm','cdd'))
list_simeval_2 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run1/simeval_run"),
                                     cdd.data=ERROR_table,skip=c())
list_simeval_3 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run2/simeval_run"),
                                     cdd.data=ERROR_table,skip=c('frem','resmod','etas'))
list_simeval_4 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run1/simeval_run"),
                                     cdd.data=data.frame(),skip=c('frem','cdd'))
list_simeval_5 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run2/simeval_run"),
                                     cdd.data=data.frame(),skip=c('scm'))
list_simeval_6 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run2/simeval_run"),
                                     cdd.data=cdd_data_4,skip=c('frem','resmod','etas','scm'))
list_simeval_7 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run3/simeval_run"),
                                     cdd.data=cdd_data_4,skip=c())
list_simeval_8 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run4/simeval_run"),
                                     cdd.data=cdd_data_4,skip=c('frem','resmod','etas','scm'))
list_simeval_9 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run4/simeval_run"),
                                     cdd.data=cdd_data_4,skip=c('frem','resmod','simeval','scm'))
list_simeval_10 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run4/simeval_run"),
                                     cdd.data=cdd_data_8,skip=c('frem','resmod','simeval','scm'))
list_simeval_11 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run4/simeval_run"),
                                      cdd.data=cdd_data_8,skip=c('frem','resmod','cdd','scm'))
#expected data
max_outlier_table_1 <- data.frame("SIMEVAL","ERROR",stringsAsFactors = F)
colnames(max_outlier_table_1) <- c("","dOFV")
outliers_table_2 <- data.frame("No outliers detected",stringsAsFactors = F)
colnames(outliers_table_2) <- c("")
max_outlier_table_2 <- data.frame("No outliers detected","",stringsAsFactors = F)
colnames(max_outlier_table_2) <- c("","dOFV")
outliers_table_3 <- data.frame(Subjects=c("Subject 1","Subject 12","Subject 8","Subject 34"),
                               dOFV=c(rep("ERROR",4)),stringsAsFactors = F)
max_outlier_table_3 <- data.frame("No dOFV values found","",stringsAsFactors = F)
colnames(max_outlier_table_3) <- c("","dOFV")
outliers_table_5 <- data.frame(Subjects=c("Subject 1","Subject 12","Subject 8","Subject 34"),
                               dOFV=c(rep("NA",4)),stringsAsFactors = F)
outliers_table_6 <- data.frame(Subjects=c("Subject 34","Subject 8","Subject 1","Subject 12"),
                               dOFV=c("2.0","1.0","NA","NA"),stringsAsFactors = F)
max_outlier_table_6 <- data.frame("Subject 34",2.00265,stringsAsFactors = F)
colnames(max_outlier_table_6) <- c("","dOFV")
outliers_table_7 <- data.frame(Subjects=c("Subject 1","Subject 12"),
                               dOFV=c("NA","NA"),stringsAsFactors = F)
max_outlier_table_7 <- data.frame("No dOFV values found","",stringsAsFactors = F)
colnames(max_outlier_table_7) <- c("","dOFV")
outliers_table_8 <- data.frame(Subjects=c("Subject 34","Subject 8","Subject 14","Subject 11","Subject 57","Subject 7","Subject 3"),
                               dOFV=c("2.0","1.0","0.1","0.1","0.1","0.0","0.0"),stringsAsFactors = F)
max_outlier_table_8 <- data.frame(c("Subject 34"),
                                  c(2.00265),stringsAsFactors = F)
colnames(max_outlier_table_8) <- c("","dOFV")
max_outlier_table_9 <- data.frame("SIMEVAL","SKIPPED",stringsAsFactors = F)
colnames(max_outlier_table_9) <- c("","dOFV")
max_outlier_table_10 <- data.frame("SIMEVAL","SKIPPED",stringsAsFactors = F)
colnames(max_outlier_table_10) <- c("","dOFV")
outliers_table_11 <- data.frame(Subjects=c("Subject 14","Subject 7","Subject 8","Subject 3","Subject 11","Subject 57","Subject 34"),
                               dOFV=c(rep("",7)),stringsAsFactors = F)
max_outlier_table_11 <- data.frame("No dOFV values found (skipped CDD)","",stringsAsFactors = F)
colnames(max_outlier_table_11) <- c("","dOFV")
#compare
context("qa, get_outliers_table")
test_that("create outliers table",{
  expect_equal(FALSE,list_simeval_1$simeval_files_exist)
  expect_equal(ERROR_table,list_simeval_1$outliers_table)
  expect_equal(max_outlier_table_1,list_simeval_1$max_outlier_table)
  expect_equal(c(),list_simeval_1$outlier_ids)
  expect_equal(5,list_simeval_1$fig_height_outl)
  expect_equal(TRUE,list_simeval_2$simeval_files_exist)
  expect_equal(outliers_table_2,list_simeval_2$outliers_table)
  expect_equal(max_outlier_table_2,list_simeval_2$max_outlier_table)
  expect_equal(c(),list_simeval_2$outlier_ids)
  expect_equal(5,list_simeval_2$fig_height_outl)
  expect_equal(TRUE,list_simeval_3$simeval_files_exist)
  expect_equal(outliers_table_3,list_simeval_3$outliers_table)
  expect_equal(max_outlier_table_3,list_simeval_3$max_outlier_table)
  expect_equal(c(1,12,8,34),list_simeval_3$outlier_ids)
  expect_equal(7,list_simeval_3$fig_height_outl)
  expect_equal(TRUE,list_simeval_4$simeval_files_exist)
  expect_equal(outliers_table_2,list_simeval_4$outliers_table)
  expect_equal(max_outlier_table_2,list_simeval_4$max_outlier_table)
  expect_equal(c(),list_simeval_4$outlier_ids)
  expect_equal(5,list_simeval_4$fig_height_outl)
  expect_equal(TRUE,list_simeval_5$simeval_files_exist)
  expect_equal(outliers_table_5,list_simeval_5$outliers_table)
  expect_equal(max_outlier_table_3,list_simeval_5$max_outlier_table)
  expect_equal(c(1,12,8,34),list_simeval_5$outlier_ids)
  expect_equal(7,list_simeval_5$fig_height_outl)
  expect_equal(TRUE,list_simeval_6$simeval_files_exist)
  expect_equal(outliers_table_6,list_simeval_6$outliers_table)
  expect_equal(max_outlier_table_6,list_simeval_6$max_outlier_table)
  expect_equal(c(1,12,8,34),list_simeval_6$outlier_ids)
  expect_equal(7,list_simeval_6$fig_height_outl)
  expect_equal(TRUE,list_simeval_7$simeval_files_exist)
  expect_equal(outliers_table_7,list_simeval_7$outliers_table)
  expect_equal(max_outlier_table_7,list_simeval_7$max_outlier_table)
  expect_equal(c(1,12),list_simeval_7$outlier_ids)
  expect_equal(5,list_simeval_7$fig_height_outl)
  expect_equal(TRUE,list_simeval_8$simeval_files_exist)
  expect_equal(outliers_table_8,list_simeval_8$outliers_table)
  expect_equal(max_outlier_table_8,list_simeval_8$max_outlier_table)
  expect_equal(c(14,7,8,3,11,57,34),list_simeval_8$outlier_ids)
  expect_equal(15,list_simeval_8$fig_height_outl)
  expect_equal(ERROR_table,list_simeval_9$outliers_table)
  expect_equal(max_outlier_table_9,list_simeval_9$max_outlier_table)
  expect_equal(ERROR_table,list_simeval_10$outliers_table)
  expect_equal(max_outlier_table_10,list_simeval_10$max_outlier_table)
  expect_equal(outliers_table_11,list_simeval_11$outliers_table)
  expect_equal(max_outlier_table_11,list_simeval_11$max_outlier_table)
})

#...........................  (19) Test function get_overview_table.R .....................................
#input data
cdd_highest_dofv_extra <- data.frame("Subject 34",0.01265,stringsAsFactors = F)
colnames(cdd_highest_dofv_extra) <- c("","dOFV")
cdd_highest_dofv_extra_1 <- data.frame("Subject 34",0.00265,stringsAsFactors = F)
colnames(cdd_highest_dofv_extra_1) <- c("","dOFV")
#run function
overview_list_1 <- get_overview_table(structural_overview=str_overview_4,
                                      param_var_overview=par_var_models_4,
                                      covariates_overview=covariates_table_1,
                                      resmod_ruv_overview=resmod_ruv_overview_1,
                                      infl_indiv_overview=cdd_highest_dofv_1,
                                      outliers_overview=max_outlier_table_1)
overview_list_2 <- get_overview_table(structural_overview=str_overview_7,
                                      param_var_overview=par_var_models_6,
                                      covariates_overview=covariates_table_2,
                                      resmod_ruv_overview=resmod_ruv_overview_4,
                                      infl_indiv_overview=cdd_highest_dofv_1,
                                      outliers_overview=max_outlier_table_1)
overview_list_3 <- get_overview_table(structural_overview=str_overview_5,
                                      param_var_overview=par_var_models_2,
                                      covariates_overview=covariates_table_3,
                                      resmod_ruv_overview=resmod_ruv_overview_2,
                                      infl_indiv_overview=cdd_highest_dofv_3,
                                      outliers_overview=max_outlier_table_3)
overview_list_4 <- get_overview_table(structural_overview=str_overview_1,
                                      param_var_overview=par_var_models_1,
                                      covariates_overview=covariates_table_4,
                                      resmod_ruv_overview=resmod_ruv_overview_2,
                                      infl_indiv_overview=cdd_highest_dofv_4,
                                      outliers_overview=max_outlier_table_6)
overview_list_5 <- get_overview_table(structural_overview=str_overview_1,
                                      param_var_overview=par_var_models_1,
                                      covariates_overview=covariates_table_4,
                                      resmod_ruv_overview=resmod_ruv_overview_2,
                                      infl_indiv_overview=cdd_highest_dofv_extra,
                                      outliers_overview=max_outlier_table_6)
overview_list_6 <- get_overview_table(structural_overview=str_overview_1,
                                      param_var_overview=par_var_models_1,
                                      covariates_overview=covariates_table_4,
                                      resmod_ruv_overview=resmod_ruv_overview_2,
                                      infl_indiv_overview=cdd_highest_dofv_extra_1,
                                      outliers_overview=max_outlier_table_6)
overview_list_7 <- get_overview_table(structural_overview=str_overview_10,
                                      param_var_overview=par_var_models_7,
                                      covariates_overview=covariates_table_8,
                                      resmod_ruv_overview=resmod_ruv_overview_5,
                                      infl_indiv_overview=cdd_highest_dofv_8,
                                      outliers_overview=max_outlier_table_9)
overview_list_8 <- get_overview_table(structural_overview=str_overview_10,
                                      param_var_overview=par_var_models_3,
                                      covariates_overview=covariates_table_4,
                                      resmod_ruv_overview=resmod_ruv_overview_5,
                                      infl_indiv_overview=cdd_highest_dofv_8,
                                      outliers_overview=max_outlier_table_11)
#expected data
overview_table_1 <- data.frame(c("TIME","TAD","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "FREM","SCM","RESMOD","CDD","SIMEVAL"),
                               c(rep("ERROR",11)),c(rep("",11)),
                               stringsAsFactors = F)
colnames(overview_table_1) <- c("","dOFV","Additional parameters")
rgroup_names <- c("Structural Model","Parameter Variability Model","Covariates",
                  "Residual Error Model","Influential Individuals","Outliers")
overview_table_2 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "FREM","SCM","RESMOD","CDD","SIMEVAL"),
                               c(rep("NA",9),rep("ERROR",2)),c("4","4",rep("",9)),
                               stringsAsFactors = F)
colnames(overview_table_2) <- c("","dOFV","Additional parameters")
overview_table_3 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "FREM","CLAPGR-4","tdist","dtbs","All dOFV values are negative","No dOFV values found"),
                               c("NA","4.8","ERROR","-0.6","NA","NA","NA","2.9","43.6","21.7","",""),
                               c("4","4","","3","","","","1","1","2","",""),
                               stringsAsFactors = F)
colnames(overview_table_3) <- c("","dOFV","Additional parameters")
overview_table_4 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "FREM","CLAPGR-4","tdist","dtbs","Subject 34","Subject 34"),
                               c("4.3","4.8","-3.7","-0.6","3.4","-1.0","3.6","2.9","43.6","21.7","2.0","2.0"),
                               c(rep("4",3),"0","2","0","6","1","1","2","",""),
                               stringsAsFactors = F)
colnames(overview_table_4) <- c("","dOFV","Additional parameters")
overview_table_5 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "FREM","CLAPGR-4","tdist","dtbs","Subject 34","Subject 34"),
                               c("4.31","4.80","-3.73","-0.57","3.41","-1.03","3.60","2.87","43.62","21.67","0.01","2.00"),
                               c(rep("4",3),"0","2","0","6","1","1","2","",""),
                               stringsAsFactors = F)
colnames(overview_table_5) <- c("","dOFV","Additional parameters")
overview_table_6 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "FREM","CLAPGR-4","tdist","dtbs","Subject 34","Subject 34"),
                               c("4.3","4.8","-3.7","-0.6","3.4","-1.0","3.6","2.9","43.6","21.7","0.0","2.0"),
                               c(rep("4",3),"0","2","0","6","1","1","2","",""),
                               stringsAsFactors = F)
colnames(overview_table_6) <- c("","dOFV","Additional parameters")
overview_table_7 <- data.frame(c("RESMOD","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "FREM","SCM","RESMOD","CDD","SIMEVAL"),
                               c(rep("SKIPPED",10)),c(rep("",10)),
                               stringsAsFactors = F)
colnames(overview_table_7) <- c("","dOFV","Additional parameters")
overview_table_8 <- data.frame(c("RESMOD","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "FREM","CLAPGR-4","RESMOD","CDD","No dOFV values found (skipped CDD)"),
                               c("SKIPPED","ERROR","NA","ERROR","NA","3.6","2.9","SKIPPED","SKIPPED",""),
                               c(rep("",5),"6","1",rep("",3)),
                               stringsAsFactors = F)
colnames(overview_table_8) <- c("","dOFV","Additional parameters")
#compare
context("qa, get_overview_table")
test_that("create an overview table",{
  expect_equal(overview_table_1,overview_list_1$overview_table)
  expect_equal(c(2,4,2,1,1,1),overview_list_1$n.rgroup)
  expect_equal(rgroup_names,overview_list_1$rgroup_names)
  expect_equal(overview_table_2,overview_list_2$overview_table)
  expect_equal(c(2,4,2,1,1,1),overview_list_2$n.rgroup)
  expect_equal(rgroup_names,overview_list_2$rgroup_names)
  expect_equal(overview_table_3,overview_list_3$overview_table)
  expect_equal(c(2,4,2,2,1,1),overview_list_3$n.rgroup)
  expect_equal(rgroup_names,overview_list_3$rgroup_names)
  expect_equal(overview_table_4,overview_list_4$overview_table)
  expect_equal(c(2,4,2,2,1,1),overview_list_4$n.rgroup)
  expect_equal(rgroup_names,overview_list_4$rgroup_names)
  expect_equal(overview_table_5,overview_list_5$overview_table)
  expect_equal(c(2,4,2,2,1,1),overview_list_5$n.rgroup)
  expect_equal(rgroup_names,overview_list_5$rgroup_names)
  expect_equal(overview_table_6,overview_list_6$overview_table)
  expect_equal(overview_table_7,overview_list_7$overview_table)
  expect_equal(overview_table_8,overview_list_8$overview_table)
})

#...........................  (20) Test function get_tables_for_vpc.R .....................................  
#run function
list_vpc_tables_1 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_2 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_missing_folders_files/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_3 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_missing_folders_files/linearize_run/scm_dir1/extra_table"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_4 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_missing_folders_files/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="1",dvid_name="ORIG")
list_vpc_tables_5 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run2/linearize_run/scm_dir1/extra_table"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run2/linbase.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run2/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_missing_folders_files/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_6 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table_5"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase_7.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-3.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="2",dvid_name="ORIG")
list_vpc_tables_7 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table_2"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase_2.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_8 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table_3"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase_2.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_9 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table_4"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase_3.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_10 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table_4"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_11 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table_2"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase_4.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-1.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_12 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table"),
                                         obs_extra_table=file.path(files.w.dir,"qa_run1/linbase_5.dta"),
                                         sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-2.dta"),
                                         sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                         idv_all=c("TIME","PRED"),dvid="NA",dvid_name="DVID")
list_vpc_tables_13 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table_6"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase_6.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-4.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred_1.dta"),
                                        idv_all=c("TIME","PRED"),dvid="2",dvid_name="ORIG")
list_vpc_tables_14 <- get_tables_for_vpc(obs_table=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/extra_table_5"),
                                        obs_extra_table=file.path(files.w.dir,"qa_run1/linbase_7.dta"),
                                        sim_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/sim_res_table-5.dta"),
                                        sim_extra_table=file.path(files.w.dir,"qa_run1/simeval_run/m1/orig_pred.dta"),
                                        idv_all=c("TIME","PRED"),dvid="2",dvid_name="ORIG")
#expected_data
obs_1 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    DV=c(17.3,31,9.7,24.6,24.3,23.9,0,31.7,14.2,18.2,20.3),
                    MDV=c(rep(0,11)))
sim_1 <- data.frame(ID=as.factor(rep(c(1,1,2,2,3,4,4,4,5,5,5),3)),
                    DV=c(25,34,10.9,28,27,20,21,34,22,29,40,20,27.8,25,32,36,19,26,39.1,21,19,34,16,30.1,17,30.8,26,18,19,28,21,24.1,28),
                    MDV=c(rep(0,33)),
                    CWRES=c(1.8,0.73,-0.18,0.42,-0.9,0.13,-1.2,1.07,0.27,1.1,2.4,0.84,-0.37,1.2,0.87,1,-0.85,0.047,1.8,0.96,-1.3,1.4,-0.503,0.45,-1.06,1.1,1.4,-0.045,-1.3,-0.16,0.82,0.2,-0.13),
                    IPRED=c(22,33,11,28,29,20.2,24,30.5,22,27,33,18,29,23,32,33,21,26,32,20,24,29,17,28,20.8,28,23,17,21,27,19,23,29),
                    TIME=rep(c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),3),
                    PRED=rep(c(17,28,10,18,31,20,24,30,19,23,28),3))
obs_8 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    MDV=c(rep(0,11)),
                    DV=c(17.3,31,9.7,24.6,24.3,23.9,0,31.7,14.2,18.2,20.3))
obs_9 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    MDV=c(rep(0,11)))
obs_11 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    DV=c(17.3,31,9.7,24.6,24.3,20.8,23.9,31.7,14.2,18.2,20.3))
obs_12 <- data.frame(ID=factor(c(1,1,3,5,5,5),levels = c(1,2,3,4,5)),
                    TIME=c(2,112.5,134.3,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.5,-0.92,-0.702,-1),
                    PRED=c(17,28,31,19,23,28),
                    CIPREDI=c(17,28,28,14,18,23),
                    DV=c(17.3,31,24.3,14.2,18.2,20.3),
                    MDV=c(rep(0,6)))
sim_12 <- data.frame(ID=as.factor(rep(c(1,1,3,5,5,5),3)),
                    DV=c(25,34,27,22,29,40,20,27.8,36,21,19,34,16,30.1,26,21,24.1,28),
                    MDV=c(rep(0,18)),
                    CWRES=c(1.8,0.73,-0.9,0.27,1.1,2.4,0.84,-0.37,1,0.96,-1.3,1.4,-0.503,0.45,1.4,0.82,0.2,-0.13),
                    IPRED=c(22,33,29,22,27,33,18,29,33,20,24,29,17,28,23,19,23,29),
                    TIME=rep(c(2,112.5,134.3,2,59.5,132),3),
                    PRED=rep(c(17,28,31,19,23,28),3))
obs_6 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    ORIG=c(rep(2,11)),
                    DV=c(17.3,31,9.7,24.6,24.3,23.9,0,31.7,14.2,18.2,20.3),
                    MDV=c(rep(0,11)))
sim_6 <- data.frame(ID=as.factor(rep(c(1,1,2,2,3,4,4,4,5,5,5),3)),
                    DV=c(25,34,10.9,28,27,20,21,34,22,29,40,20,27.8,25,32,36,19,26,39.1,21,19,34,16,30.1,17,30.8,26,18,19,28,21,24.1,28),
                    MDV=c(rep(0,33)),
                    CWRES=c(1.8,0.73,-0.18,0.42,-0.9,0.13,-1.2,1.07,0.27,1.1,2.4,0.84,-0.37,1.2,0.87,1,-0.85,0.047,1.8,0.96,-1.3,1.4,-0.503,0.45,-1.06,1.1,1.4,-0.045,-1.3,-0.16,0.82,0.2,-0.13),
                    IPRED=c(22,33,11,28,29,20.2,24,30.5,22,27,33,18,29,23,32,33,21,26,32,20,24,29,17,28,20.8,28,23,17,21,27,19,23,29),
                    ORIG=rep(2,33),
                    TIME=rep(c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),3),
                    PRED=rep(c(17,28,10,18,31,20,24,30,19,23,28),3))
obs_13 <- data.frame(ID=as.factor(c(1,1,3,5,5,5)),
                     TIME=c(2,112.5,134.3,2,59.5,132),
                     CWRES=c(-0.401,0.58,-1.5,-0.92,-0.702,-1),
                     PRED=c(17,28,31,19,23,28),
                     CIPREDI=c(17,28,28,14,18,23),
                     ORIG=rep(2,6),
                     DV=c(17.3,31,24.3,14.2,18.2,20.3),
                     MDV=c(rep(0,6)))
sim_13 <- data.frame(ID=as.factor(rep(c(1,1,3,5,5,5),3)),
                     DV=c(25,34,27,22,29,40,20,27.8,36,21,19,34,16,30.1,26,21,24.1,28),
                     MDV=c(rep(0,18)),
                     CWRES=c(1.8,0.73,-0.9,0.27,1.1,2.4,0.84,-0.37,1,0.96,-1.3,1.4,-0.503,0.45,1.4,0.82,0.2,-0.13),
                     IPRED=c(22,33,29,22,27,33,18,29,33,20,24,29,17,28,23,19,23,29),
                     ORIG=rep(2,18),
                     TIME=rep(c(2,112.5,134.3,2,59.5,132),3),
                     PRED=rep(c(17,28,31,19,23,28),3))
#compare
context("qa, get_tables_for_vpc")
test_that("get tables for vpc plots",{
  expect_equal(TRUE,list_vpc_tables_1$make_vpc)
  expect_equal(obs_1,list_vpc_tables_1$obs)
  expect_equal(sim_1,list_vpc_tables_1$sim)
  expect_equal(TRUE,list_vpc_tables_2$make_vpc)
  expect_equal(obs_1,list_vpc_tables_2$obs)
  expect_equal(sim_1,list_vpc_tables_2$sim)
  expect_equal(FALSE,list_vpc_tables_3$make_vpc)
  expect_equal(FALSE,list_vpc_tables_4$make_vpc)
  expect_equal(FALSE,list_vpc_tables_5$make_vpc)
  expect_equal(TRUE,list_vpc_tables_6$make_vpc)
  expect_equal(obs_6,list_vpc_tables_6$obs)
  expect_equal(sim_6,list_vpc_tables_6$sim)
  expect_equal(TRUE,list_vpc_tables_7$make_vpc)
  expect_equal(obs_1,list_vpc_tables_7$obs)
  expect_equal(sim_1,list_vpc_tables_7$sim)
  expect_equal(TRUE,list_vpc_tables_8$make_vpc)
  expect_equal(obs_8,list_vpc_tables_8$obs)
  expect_equal(sim_1,list_vpc_tables_8$sim)
  expect_equal(FALSE,list_vpc_tables_9$make_vpc)
  expect_equal(obs_9,list_vpc_tables_9$obs)
  expect_equal(sim_1,list_vpc_tables_9$sim)
  expect_equal(TRUE,list_vpc_tables_10$make_vpc)
  expect_equal(obs_8,list_vpc_tables_10$obs)
  expect_equal(sim_1,list_vpc_tables_10$sim)
  expect_equal(TRUE,list_vpc_tables_11$make_vpc)
  expect_equal(obs_11,list_vpc_tables_11$obs)
  expect_equal(sim_1,list_vpc_tables_11$sim)
  expect_equal(TRUE,list_vpc_tables_12$make_vpc)
  expect_equal(obs_12,list_vpc_tables_12$obs)
  expect_equal(sim_12,list_vpc_tables_12$sim)
  expect_equal(TRUE,list_vpc_tables_13$make_vpc)
  expect_equal(obs_13,list_vpc_tables_13$obs)
  expect_equal(sim_13,list_vpc_tables_13$sim)
  expect_equal(TRUE,list_vpc_tables_14$make_vpc)
  expect_equal(obs_6,list_vpc_tables_14$obs)
  expect_equal(sim_6,list_vpc_tables_14$sim)
})