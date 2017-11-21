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
  expect_equal(.get_rawres_ofv(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv")),
               587.27) #default row 1
  expect_equal(.get_rawres_ofv(rawres_file=file.path(files.w.dir,"qa_run1/scm_run/raw_results_scm.csv"),row=4),
               587.1) # different row
  expect_equal(.get_ext_ofv(ext_file=file.path(files.w.dir,"qa_run1/new.ext")),
               -14.174) # default interaction -1000000000
  expect_equal(.get_ext_ofv(ext_file=file.path(files.w.dir,"qa_run1/new.ext"),iteration=19),
               -14.1487) #different interaction
})

#...........................  (2) Test function which_resmod_folders.R .....................................  
context("qa, which_resmod_folders")
test_that("gets all idv names from resmod folders",{
  expect_equal(which_resmod_folders(file.path(files.w.dir,"qa_run1"),idv_name="TIME"),
               c("TIME","PRED")) # when resmod folders found
  expect_equal(which_resmod_folders(file.path(files.w.dir,"qa_missing_folders_files"),idv_name="TIME"),
               c()) # none resmod folder in the directory
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
  expect_equal(ofv_summary_table(working.directory=file.path(files.w.dir,"qa_run1"),model.filename="run_100.mod"),
               ofv_summary_1) # derivatives.ext, .._linbase.ext and .._linvbase.phi files exist
  expect_equal(ofv_summary_table(working.directory=file.path(files.w.dir,"qa_missing_folders_files"),model.filename="run_100.mod"),
               ofv_summary_2) # derivatives.ext exists, but .._linbase.ext and .._linvbase.phi dosn't exist
})

#...........................  (4) Test function get_resmod_table.R .....................................  
# run function
resmod_table_fun_run_1 <- get_resmod_table(directory=file.path(files.w.dir,"qa_run1"), idv="PRED")
resmod_file_exists_1 <- resmod_table_fun_run_1$resmod_file_exists
resmod_table_1 <- resmod_table_fun_run_1$resmod_table

resmod_table_fun_run_2 <- get_resmod_table(directory=file.path(files.w.dir,"qa_run2"), idv="TIME")
resmod_table_2 <- resmod_table_fun_run_2$resmod_table

#create expected data
exp_resmod_table_1 <- data.frame(iteration=rep("NA",10),dvid=rep("NA",10),
                             model=c("IIV_on_RUV","autocorrelation","dtbs","idv_varying_RUV","idv_varying_RUV_cutoff0","idv_varying_RUV_cutoff1","idv_varying_combined","idv_varying_theta","power","tdist"),
                             dOFV=c(10.6,0,21.67,12.05,0.27,0.58,31.28,1.33,0.92,43.62),
                             parameters=c("%CV=7.2","half-life=1.000","lambda=-0.136,Zeta=0.09","sdeps_0-t1=0.871,sdeps_t1-t2=1.220,sdeps_t2-inf=0.967,t1=0.64,t2=0.68",
                                          "sdeps_0-t0=0.875,sdeps_t0-inf=1.011,t0=0.64","sdeps_0-t0=1.069,sdeps_t0-inf=0.981,t0=0.68","sdeps_0-t1=0.868,th_0-t1=0.108,sdeps_t1-t2=1.203,sdeps_t2-inf=0.970,t1=0.64,t2=0.68",
                                          "th_0-t1=0.108,th_t1-t2=0.222,th_t2-inf=-0.020,t1=0.64,t2=0.68","delta_power=0.094","Df=2.550"),
                             stringsAsFactors = F)
exp_resmod_table_2 <- data.frame(iteration=rep("NA",25),dvid=c(rep(1,8),rep(2,8),rep("sum",9)),
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
  expect_true(resmod_file_exists_1)
  expect_equal(resmod_table_1,exp_resmod_table_1) # replace "NA" in dOFV to NA as numeric, no dvids
  expect_false(get_resmod_table(directory=file.path(files.w.dir,"qa_run1"), idv="TIME")$resmod_file_exists)
  expect_false(get_resmod_table(directory=file.path(files.w.dir,"qa_run1"), idv="NEW")$resmod_file_exists)
  expect_equal(resmod_table_2,exp_resmod_table_2) # with dvids, with sum rows
})

#...........................  (5) Test function get_resmod_structural_dofv.R .....................................  
context("qa, get_resmod_structural_dofv")
test_that("get dofv value form the resmod table",{
  expect_equal(get_resmod_structural_dofv(directory=file.path(files.w.dir,"qa_run1"),idv="PRED",dvid="NA"),
               1.33) # file exists, dvid is "NA"
  expect_equal(get_resmod_structural_dofv(directory=file.path(files.w.dir,"qa_run1"),idv="PRED"),
               1.33) # file exists, dvid value as default "NA"
  expect_equal(get_resmod_structural_dofv(directory=file.path(files.w.dir,"qa_run2"),idv="TIME",dvid=2),
               as.numeric(NA)) # no dvid, ofv values is NA
  expect_equal(get_resmod_structural_dofv(directory=file.path(files.w.dir,"qa_run1"),idv="TIME",dvid=1),
               "ERROR") # file, does not exist
})

#...........................  (6) Test function get_structural_overview_table.R .....................................  
#create expected data
exp_str_overview_1 <- data.frame(c("TIME","PRED"),c(4.314,4.8),c(2,2),stringsAsFactors = F)
colnames(exp_str_overview_1) <- c("","dOFV","Add.params")
exp_str_overview_2 <- data.frame(c("(ORIG = 1)","TIME","PRED","(ORIG = 2)","TIME","PRED"),c("","4.51","4.8","","NA","0.4"),c("",1,1,"",1,1),
                             stringsAsFactors = F)
colnames(exp_str_overview_2) <- c("","dOFV","Add.params")
exp_str_overview_3 <- data.frame(c("(ORIG = 1)","TIME","TAD","PRED","(ORIG = 2)","TIME","TAD","PRED"),c("","4.51","ERROR","4.8","","NA","ERROR","0.4"),
                             c("",1,"",1,"",1,"",1),stringsAsFactors = F)
colnames(exp_str_overview_3) <- c("","dOFV","Add.params")
exp_str_overview_4 <- data.frame(c("TIME","TAD"),c("ERROR","ERROR"),c("",""),stringsAsFactors = F)
colnames(exp_str_overview_4) <- c("","dOFV","Add.params")
exp_str_overview_5 <- data.frame(c("TIME","PRED"),c("NA","4.8"),c(2,2),stringsAsFactors = F)
colnames(exp_str_overview_5) <- c("","dOFV","Add.params")
exp_str_overview_6 <- data.frame(c("TIME","TAD","PRED"),c("NA","ERROR","4.8"),c("2","","2"),stringsAsFactors = F)
colnames(exp_str_overview_6) <- c("","dOFV","Add.params")
exp_str_overview_7 <- data.frame(c("TIME","PRED"),c("NA","NA"),c(2,2),stringsAsFactors = F)
colnames(exp_str_overview_7) <- c("","dOFV","Add.params")
exp_str_overview_8 <- data.frame(c("(DVID = 1)","TIME","PRED","(DVID = 2)","TIME","PRED"),c("","NA","NA","","NA","NA"),c("","","","","",""),
                             stringsAsFactors = F)
colnames(exp_str_overview_8) <- c("","dOFV","Add.params")
exp_str_overview_9 <- data.frame("RESMOD","ERROR",stringsAsFactors = F)
colnames(exp_str_overview_9) <- c("","dOFV")
exp_str_overview_10 <- data.frame("RESMOD","SKIPPED",stringsAsFactors = F)
colnames(exp_str_overview_10) <- c("","dOFV")
#compare
context("qa, get_structural_overview_table")
test_that("get structural overview table",{
  expect_equal(exp_str_overview_1,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run3"),idv=c("TIME","PRED"),dvid_name="NA"))  # no dvid values, all resmod runs found, all dofv nubers
  expect_equal(exp_str_overview_2,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run2"),idv=c("TIME","PRED"),dvid_name="ORIG")) # with dvid, dofv one NA
  expect_equal(exp_str_overview_3,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run2"),idv=c("TIME","TAD","PRED"),dvid_name="ORIG")) #with dvid, TAD resmod run not exist
  expect_equal(exp_str_overview_4,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_missing_folders_files"),idv=c("TIME","TAD"),dvid_name="ORIG")) #resmod runs not found, with dvid
  expect_equal(exp_str_overview_5,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run5"),idv=c("TIME","PRED"),dvid_name="NA")) # no dvid values, all resmod runs found, some NA dofv
  expect_equal(exp_str_overview_6,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run6"),idv=c("TIME","TAD","PRED"),dvid_name="NA")) # resmod TAD run not exist, no dvids
  expect_equal(exp_str_overview_7,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run7"),idv=c("TIME","PRED"),dvid_name="NA")) # all ofv values NA, no dvid
  expect_equal(exp_str_overview_8,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run8"),idv=c("TIME","PRED"),dvid_name="DVID")) # all ofv values NA, with dvid
  expect_equal(exp_str_overview_9,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run2"),idv=c(),dvid_name="ORIG",skip=c('transform','cdd','frem'))) # no idv, produces error table
  expect_equal(exp_str_overview_10,
               get_structural_overview_table(directory=file.path(files.w.dir,"qa_run2"),idv=c(),dvid_name="ORIG",skip=c('transform','cdd','frem','resmod'))) # no idv, produces Skipped table
})

#...........................  (7) Test function find_dvid_values.R .....................................
context("qa, find_dvid_values")
test_that("finds dvid values",{
  expect_equal(find_dvid_values(directory=file.path(files.w.dir,"qa_run1"),idv="PRED",dvid_name="DVID"),
               "NA") # file exists, no dvids
  expect_equal(find_dvid_values(directory=file.path(files.w.dir,"qa_run1"),idv="TIME",dvid_name="DVID"),
               "NA") # file does not exist
  expect_equal(find_dvid_values(directory=file.path(files.w.dir,"qa_run2"),idv="TIME",dvid_name="DVID"),
               c(1,2)) # file exists, some dvids, sum rows exists
  expect_equal(find_dvid_values(directory=file.path(files.w.dir,"qa_run8"),idv="TIME",dvid_name="DVID"),
               c(1,2)) # file esists, some dvids, no sum rows
})

#...........................  (8) Test function added_structural_param.R .....................................
context("qa, added_structural_param")
test_that("calculates how many structural parameter where added",{
  expect_equal(added_structural_param(directory=file.path(files.w.dir,"qa_run1"), idv="TIME", dvid="NA"),
               "") # file does not exist
  expect_equal(added_structural_param(directory=file.path(files.w.dir,"qa_run1"), idv="PRED"),
               2) # file exists, dvid="NA" (by defaylt), exists not NA parameters
  expect_equal(added_structural_param(directory=file.path(files.w.dir,"qa_run2"), idv="TIME", dvid=1),
               1) # file exists, dvid=1, exists not NA parameters
  expect_equal(added_structural_param(directory=file.path(files.w.dir,"qa_run8"), idv="TIME", dvid=2),
               "") # file exists, dvid=2, parameter is NA
})

#...........................  (9) Test function get_omega_values.R .....................................
context("qa, get_omega_values")
test_that("finds omega variances and covariances",{
  expect_equal(get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/new.ext"),"all"),
               data.frame("OMEGA.1.1."=c(0.0168598),
                          "OMEGA.2.1."=c(0.229251),
                          "OMEGA.2.2."=c(0.166343),stringsAsFactors = F)) # find both - variances and covariances
  expect_equal(get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox.ext"),"var"),
               data.frame("OMEGA.1.1."=c(0.0168598),
                          "OMEGA.2.2."=c(0.229251),
                          "OMEGA.3.3."=c(0.166343),
                          "OMEGA.4.4."=c(0.0154853),stringsAsFactors = F)) # find only variances
  expect_equal(get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox.ext"),"cov"),
               data.frame()) # no covariances found
  expect_equal(get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/new_2.ext"),"all"),
               data.frame()) # Empty file
  expect_equal(get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox_2.ext"),"var"),
               data.frame()) # no variances found
  expect_equal(get_omega_values(ext_file=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox_2.ext"),"cov"),
               data.frame("OMEGA.2.1."=0.36,"OMEGA.3.2."=0.15)) # find covariances only
})

#...........................  (10) Test function get_param_var_tables.R .....................................
#run function
param_var_list_1 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run2"),base_model=file.path(files.w.dir,"qa_run2/run_100_linbase.mod"),skip=c())
par_var_models_1 <- param_var_list_1$par_var_models
dofv_block_1 <- param_var_list_1$dofv_block
dofv_box_1 <- param_var_list_1$dofv_box
dofv_tdist_1 <- param_var_list_1$dofv_tdist
fullblock_mod_1 <- param_var_list_1$fullblock_mod
boxcox_mod_1 <- param_var_list_1$boxcox_mod
add_etas_mod_1 <- param_var_list_1$add_etas_mod
tdist_mod_1 <- param_var_list_1$tdist_mod
param_var_list_2 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run1"),base_model=file.path(files.w.dir,"qa_run1/run_100_linbase.mod"),skip=c('cdd','simeval'))
par_var_models_2 <- param_var_list_2$par_var_models
dofv_block_2 <- param_var_list_2$dofv_block
dofv_box_2 <- param_var_list_2$dofv_box
dofv_tdist_2 <- param_var_list_2$dofv_tdist
fullblock_mod_2 <- param_var_list_2$fullblock_mod
boxcox_mod_2 <- param_var_list_2$boxcox_mod
add_etas_mod_2 <- param_var_list_2$add_etas_mod
tdist_mod_2 <- param_var_list_2$tdist_mod
param_var_list_3 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run3"),base_model=file.path(files.w.dir,"qa_run3/run_100_linbase.mod"),skip=c())
par_var_models_3 <- param_var_list_3$par_var_models
dofv_block_3 <- param_var_list_3$dofv_block
dofv_box_3 <- param_var_list_3$dofv_box
dofv_tdist_3 <- param_var_list_3$dofv_tdist
fullblock_mod_3 <- param_var_list_3$fullblock_mod
boxcox_mod_3 <- param_var_list_3$boxcox_mod
add_etas_mod_3 <- param_var_list_3$add_etas_mod
tdist_mod_3 <- param_var_list_3$tdist_mod
param_var_list_4 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_missing_folders_files"),base_model=file.path(files.w.dir,"qa_missing_folders_files/r_linbase.mod"),skip=c('resmod'))
par_var_models_4 <- param_var_list_4$par_var_models
dofv_block_4 <- param_var_list_4$dofv_block
dofv_box_4 <- param_var_list_4$dofv_box
dofv_tdist_4 <- param_var_list_4$dofv_tdist
fullblock_mod_4 <- param_var_list_4$fullblock_mod
boxcox_mod_4 <- param_var_list_4$boxcox_mod
add_etas_mod_4 <- param_var_list_4$add_etas_mod
tdist_mod_4 <- param_var_list_4$tdist_mod
param_var_list_5 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run4"),base_model=file.path(files.w.dir,"qa_run4/run_100_linbase.mod"),skip=c('frem','resmod','scm','cdd','simeval'))
par_var_models_5 <- param_var_list_5$par_var_models
dofv_block_5 <- param_var_list_5$dofv_block
dofv_box_5 <- param_var_list_5$dofv_box
dofv_tdist_5 <- param_var_list_5$dofv_tdist
fullblock_mod_5 <- param_var_list_5$fullblock_mod
boxcox_mod_5 <- param_var_list_5$boxcox_mod
add_etas_mod_5 <- param_var_list_5$add_etas_mod
tdist_mod_5 <- param_var_list_5$tdist_mod
param_var_list_6 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run6"),base_model=file.path(files.w.dir,"qa_run6/run_100_linbase.mod"),skip=c())
par_var_models_6 <- param_var_list_6$par_var_models
dofv_block_6 <- param_var_list_6$dofv_block
dofv_box_6 <- param_var_list_6$dofv_box
dofv_tdist_6 <- param_var_list_6$dofv_tdist
fullblock_mod_6 <- param_var_list_6$fullblock_mod
boxcox_mod_6 <- param_var_list_6$boxcox_mod
add_etas_mod_6 <- param_var_list_6$add_etas_mod
tdist_mod_6 <- param_var_list_6$tdist_mod
param_var_list_7 <- get_param_var_tables(directory=file.path(files.w.dir,"qa_run2"),base_model=file.path(files.w.dir,"qa_run2/run_100_linbase.mod"),skip=c('frem','transform','scm','simeval'))
par_var_models_7 <- param_var_list_7$par_var_models
#create expected data
exp_par_var_models_1 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"),
                             c(-3.73,-0.566,3.41,-1.03,-2.03),
                             c(4,0,2,0,3),stringsAsFactors = F)
colnames(exp_par_var_models_1) <- c("","dOFV","Add.params")
exp_par_var_models_2 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"),
                               c("ERROR","-0.566000000000001","NA","NA","NA"),
                               c("","3","","",""),stringsAsFactors = F)
colnames(exp_par_var_models_2) <- c("","dOFV","Add.params")
exp_par_var_models_3 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"),
                               c("ERROR","NA","ERROR","NA","NA"),
                               c("","","","",""),stringsAsFactors = F)
colnames(exp_par_var_models_3) <- c("","dOFV","Add.params")
exp_par_var_models_4 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"),
                               c("ERROR","ERROR","ERROR","ERROR","ERROR"),
                               stringsAsFactors = F)
colnames(exp_par_var_models_4) <- c("","dOFV")
exp_par_var_models_5 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"),
                               c("-3.73","NA","3.41","NA","0"),
                               c("0","","2","","3"),stringsAsFactors = F)
colnames(exp_par_var_models_5) <- c("","dOFV","Add.params")
exp_par_var_models_6 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"),
                               c("NA","NA","NA","NA","NA"),stringsAsFactors = F)
colnames(exp_par_var_models_6) <- c("","dOFV")
exp_par_var_models_7 <- data.frame(c("Full OMEGA Block", "Box-Cox Transformation","Additional ETA","t-distribution","Interoccasion variability"),
                               c(rep("SKIPPED",5)),stringsAsFactors = F)
colnames(exp_par_var_models_7) <- c("","dOFV")
#compare
context("qa, get_param_var_tables")
test_that("get parameter variability overview part, get delta ofv values",{
  expect_equal(par_var_models_1,exp_par_var_models_1) # All models created and run
  expect_equal(dofv_block_1,-3.73)
  expect_equal(dofv_box_1,-0.566)
  expect_equal(dofv_tdist_1,-1.03)
  expect_true(fullblock_mod_1)
  expect_true(boxcox_mod_1)
  expect_true(add_etas_mod_1)
  expect_true(tdist_mod_1)
  expect_equal(par_var_models_2,exp_par_var_models_2) # Some created and run, some ERROR, some not created
  expect_equal(dofv_block_2,"ERROR")
  expect_equal(dofv_box_2,-0.566)
  expect_equal(dofv_tdist_2,"NA")
  expect_true(fullblock_mod_2)
  expect_true(boxcox_mod_2)
  expect_false(add_etas_mod_2)
  expect_false(tdist_mod_2)
  expect_equal(par_var_models_3,exp_par_var_models_3) # some models not created, some created but not run
  expect_equal(dofv_block_3,"ERROR")
  expect_equal(dofv_box_3,"NA")
  expect_equal(dofv_tdist_3,"NA")
  expect_true(fullblock_mod_3)
  expect_false(boxcox_mod_3)
  expect_true(add_etas_mod_3)
  expect_false(tdist_mod_3)
  expect_equal(par_var_models_4,exp_par_var_models_4) # all models created but not run (all ERROR)
  expect_equal(dofv_block_4,"ERROR")
  expect_equal(dofv_box_4,"ERROR")
  expect_equal(dofv_tdist_4,"ERROR")
  expect_true(fullblock_mod_4)
  expect_true(boxcox_mod_4)
  expect_true(add_etas_mod_4)
  expect_true(tdist_mod_4)
  expect_equal(par_var_models_5,exp_par_var_models_5) # no Error
  expect_equal(dofv_block_5,-3.73)
  expect_equal(dofv_box_5,"NA")
  expect_equal(dofv_tdist_5,"NA")
  expect_true(fullblock_mod_5)
  expect_false(boxcox_mod_5)
  expect_true(add_etas_mod_5)
  expect_false(tdist_mod_5)
  expect_equal(par_var_models_6,exp_par_var_models_6) # non of all 5 model where created (all NA)
  expect_equal(dofv_block_6,"NA")
  expect_equal(dofv_box_6,"NA")
  expect_equal(dofv_tdist_6,"NA")
  expect_false(fullblock_mod_6)
  expect_false(boxcox_mod_6)
  expect_false(add_etas_mod_6)
  expect_false(tdist_mod_6)
  expect_equal(par_var_models_7,exp_par_var_models_7) #skipped
})

#...........................  (11) Test function get_full_omega_block.R .....................................
#run function
full_omega_block_fun_run_1 <- get_full_omega_block(original_max0_model=file.path(files.w.dir,"qa_run4/linearize_run/scm_dir1/derivatives.mod"),fullblock_model=file.path(files.w.dir,"qa_run4/modelfit_run/fullblock.ext"),dofv_block=3.0014)
full_omega_block_table_1 <- full_omega_block_fun_run_1$full_omega_block_table
full_omega_block_error_1 <- full_omega_block_fun_run_1$full_omega_block_error
full_omega_block_fun_run_2 <- get_full_omega_block(original_max0_model=file.path(files.w.dir,"qa_run4/linearize_run/scm_dir1/derivatives.mod"),fullblock_model=file.path(files.w.dir,"qa_run4/modelfit_run/fullblock.ext"),dofv_block="NA")
full_omega_block_table_2 <- full_omega_block_fun_run_2$full_omega_block_table
full_omega_block_error_2 <- full_omega_block_fun_run_2$full_omega_block_error
full_omega_block_fun_run_3 <- get_full_omega_block(original_max0_model=file.path(files.w.dir,"qa_missing_folders_files/linearize_run/scm_dir1/derivatives.mod"),fullblock_model=file.path(files.w.dir,"qa_missing_folders_files/modelfit_run/fullblock.mod"),dofv_block=3.3414)
full_omega_block_table_3 <- full_omega_block_fun_run_3$full_omega_block_table
full_omega_block_error_3 <- full_omega_block_fun_run_3$full_omega_block_error

#expected data
exp_full_omega_block_table_1 <- data.frame(c("sd(1)","corr(2,1)","sd(2)","sd(3)","sd(4)","dOFV"),
                                       c("0.13","0.07","0.75","0.45","0.10","3.0"),
                                       c("0.10","NA","0.77","NA","NA",""),stringsAsFactors = F)
colnames(exp_full_omega_block_table_1) <- c("","New","Old")
exp_full_omega_block_table_2 <- data.frame(c("sd(1)","corr(2,1)","sd(2)","sd(3)","sd(4)"),
                                       c("0.13","0.07","0.75","0.45","0.10"),
                                       c("0.10","NA","0.77","NA","NA"),stringsAsFactors = F)
colnames(exp_full_omega_block_table_2) <- c("","New","Old")
exp_full_omega_block_table_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(exp_full_omega_block_table_3) <- c("")
#compare
context("qa, get_full_omega_block")
test_that("get full omega block, extra table",{
  expect_equal(full_omega_block_table_1,exp_full_omega_block_table_1) #file exists, adds dofv value
  expect_false(full_omega_block_error_1)
  expect_equal(full_omega_block_table_2,exp_full_omega_block_table_2) # file exists, dofv value not added
  expect_false(full_omega_block_error_2)
  expect_equal(full_omega_block_table_3,exp_full_omega_block_table_3) # file does not exist
  expect_true(full_omega_block_error_3)
})

#...........................  (12) Test function get_param_extra_table.R .....................................
#run function
param_extra_fun_run_1 <- get_param_extra_table(original_max0_model=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/derivatives.mod"),param_model=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox.mod"),dofv=3.0014)
boxcox_lambdas_table_1 <- param_extra_fun_run_1$param_extra_table
boxcox_lambdas_table_orig_1 <- param_extra_fun_run_1$param_extra_table_orig

param_extra_fun_run_2 <- get_param_extra_table(original_max0_model=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/derivatives.mod"),param_model=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox.mod"),dofv="ERROR")
boxcox_lambdas_table_2 <- param_extra_fun_run_2$param_extra_table
boxcox_lambdas_table_orig_2 <- param_extra_fun_run_2$param_extra_table_orig

param_extra_fun_run_3 <- get_param_extra_table(original_max0_model=file.path(files.w.dir,"qa_missing_folders_files/linearize_run/scm_dir1/derivatives.mod"),param_model=file.path(files.w.dir,"qa_missing_folders_files/modelfit_run/boxcox.mod"),dofv=3.3414)
boxcox_lambdas_table_3 <- param_extra_fun_run_3$param_extra_table
boxcox_lambdas_table_orig_3 <- param_extra_fun_run_3$param_extra_table_orig

param_extra_fun_run_4 <- get_param_extra_table(original_max0_model=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/derivatives.mod"),param_model=file.path(files.w.dir,"qa_run1/modelfit_run/tdist.mod"),dofv=3.0014)
tdist_table_1 <- param_extra_fun_run_4$param_extra_table
tdist_table_orig_1 <- param_extra_fun_run_4$param_extra_table_orig

param_extra_fun_run_5 <- get_param_extra_table(original_max0_model=file.path(files.w.dir,"qa_run1/linearize_run/scm_dir1/derivatives.mod"),param_model=file.path(files.w.dir,"qa_run1/modelfit_run/tdist.mod"),dofv="ERROR")
tdist_table_2 <- param_extra_fun_run_5$param_extra_table
tdist_table_orig_2 <- param_extra_fun_run_5$param_extra_table_orig

param_extra_fun_run_6 <- get_param_extra_table(original_max0_model=file.path(files.w.dir,"qa_missing_folders_files/linearize_run/scm_dir1/derivatives.mod"),param_model=file.path(files.w.dir,"qa_missing_folders_files/modelfit_run/tdist.mod"),dofv=3.3414)
tdist_table_3 <- param_extra_fun_run_6$param_extra_table
tdist_table_orig_3 <- param_extra_fun_run_6$param_extra_table_orig

#create expected data
exp_boxcox_lambdas_table_1 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)","dOFV"),
                                       c("2.11","NA","-1.14","-0.82","3.0"),
                                       c("0.13","0.48","0.41","0.12",""),
                                       c("0.13","0.77","0.00","0.00",""),stringsAsFactors = F)
colnames(exp_boxcox_lambdas_table_1) <- c("","Lambda","New SD","Old SD")
exp_boxcox_lambdas_table_orig_1 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)"),
                                          c(2.10797,NA,-1.13856,-0.819117),
                                          c(0.129845,0.478802,0.407852,0.1244399),
                                          c(0.134118,0.772284,0,0),stringsAsFactors = F)
colnames(exp_boxcox_lambdas_table_orig_1) <- c("ETA","Lambda","New SD","Old SD")
exp_boxcox_lambdas_table_2 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)"),
                                     c("2.11","NA","-1.14","-0.82"),
                                     c("0.13","0.48","0.41","0.12"),
                                     c("0.13","0.77","0.00","0.00"),stringsAsFactors = F)
colnames(exp_boxcox_lambdas_table_2) <- c("","Lambda","New SD","Old SD")
exp_boxcox_lambdas_table_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(exp_boxcox_lambdas_table_3) <- c("")
exp_boxcox_lambdas_orig_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(exp_boxcox_lambdas_orig_3) <- c("")

exp_tdist_table_1 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)","dOFV"),
                                     c("2.11","NA","-1.14","-0.82","3.0"),
                                     c("0.13","0.48","0.41","0.12",""),
                                     c("0.13","0.77","0.00","0.00",""),stringsAsFactors = F)
colnames(exp_tdist_table_1) <- c("","Degrees of freedom","New SD","Old SD")
exp_tdist_table_orig_1 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)"),
                                          c(2.10797,NA,-1.13856,-0.819117),
                                          c(0.129845,0.478802,0.407852,0.1244399),
                                          c(0.134118,0.772284,0,0),stringsAsFactors = F)
colnames(exp_tdist_table_orig_1) <- c("ETA","Degrees of freedom","New SD","Old SD")
exp_tdist_table_2 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","ETA(4)"),
                                     c("2.11","NA","-1.14","-0.82"),
                                     c("0.13","0.48","0.41","0.12"),
                                     c("0.13","0.77","0.00","0.00"),stringsAsFactors = F)
colnames(exp_tdist_table_2) <- c("","Degrees of freedom","New SD","Old SD")
exp_tdist_table_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(exp_tdist_table_3) <- c("")
#compare
context("qa, get_param_extra_table")
test_that("get full omega block or tdist extra table",{
  expect_equal(boxcox_lambdas_table_1,exp_boxcox_lambdas_table_1) # boxcox files exists, adds dOFV value, rounds values
  expect_equal(boxcox_lambdas_table_orig_1,exp_boxcox_lambdas_table_orig_1,tolerance=6) 
  expect_equal(boxcox_lambdas_table_2,exp_boxcox_lambdas_table_2) # boxcox files exists, rounds values
  expect_equal(boxcox_lambdas_table_orig_2,exp_boxcox_lambdas_table_orig_1,tolerance=6) 
  expect_equal(boxcox_lambdas_table_3,exp_boxcox_lambdas_table_3) # boxcox files does not exist
  expect_equal(boxcox_lambdas_table_orig_3,exp_boxcox_lambdas_orig_3)
  expect_equal(tdist_table_1,exp_tdist_table_1) # tdist files exists, adds dOFV value, rounds values
  expect_equal(tdist_table_orig_1,exp_tdist_table_orig_1,tolerance=6) 
  expect_equal(tdist_table_2,exp_tdist_table_2) # tdist files exists, rounds values
  expect_equal(tdist_table_orig_2,exp_tdist_table_orig_1,tolerance=6) 
  expect_equal(tdist_table_3,exp_tdist_table_3) # tdist files does not exist
  expect_equal(tdist_table_orig_3,exp_boxcox_lambdas_orig_3)
})

#...........................  (13) Test function get_add_etas_table.R .....................................
#run function
add_etas_fun_run_1 <- get_add_etas_table(original_max0_model=file.path(files.w.dir,"qa_run2/linearize_run/scm_dir1/derivatives.mod"),add_etas_dir=file.path(files.w.dir,"qa_run2/add_etas_run"),added_etas=list(A=6,D=NULL,C=4,E=NULL),dofv_add.etas=3.481502,nonlinear=F)
add_etas_table_1 <- add_etas_fun_run_1$add_etas_table
add_etas_error_1 <- add_etas_fun_run_1$add_etas_error

add_etas_fun_run_2 <- get_add_etas_table(original_max0_model=file.path(files.w.dir,"qa_run5/linearize_run/scm_dir1/derivatives.mod"),add_etas_dir=file.path(files.w.dir,"qa_run5/add_etas_run"),added_etas=list(A=6,D=NULL,C=4,E=NULL),dofv_add.etas=3.481502,nonlinear=F)
add_etas_table_2 <- add_etas_fun_run_2$add_etas_table
add_etas_error_2 <- add_etas_fun_run_2$add_etas_error

add_etas_fun_run_3 <- get_add_etas_table(original_max0_model=file.path(files.w.dir,"qa_run2/linearize_run/scm_dir1/derivatives.mod"),add_etas_dir=file.path(files.w.dir,"qa_run2/add_etas_run"),added_etas=list(A=6,C=4),dofv_add.etas="ERROR",nonlinear=F)
add_etas_table_3 <- add_etas_fun_run_3$add_etas_table
add_etas_error_3 <- add_etas_fun_run_3$add_etas_error

add_etas_fun_run_4 <- get_add_etas_table(original_max0_model=file.path(files.w.dir,"qa_run2/linearize_run/scm_dir1/derivatives.mod"),add_etas_dir=file.path(files.w.dir,"qa_run2/add_etas_run"),added_etas=list(),dofv_add.etas=3.481502,nonlinear=F)
add_etas_table_4 <- add_etas_fun_run_4$add_etas_table
add_etas_error_4 <- add_etas_fun_run_4$add_etas_error

#expected data
exp_add_etas_table_1 <- data.frame(c("ETA(1)","ETA(3)","C","A","D","E","dOFV"),
                               c("No","No","Yes","Yes","Not found","Not found","3.5"),
                               c(0.32,0.37,0.77,0.51,"","",""),
                               c(0.42,0.48,"","","","",""),
                               stringsAsFactors = F)
colnames(exp_add_etas_table_1) <- c("","Added","New SD","Old SD")
exp_add_etas_table_2 <- data.frame("ERROR",stringsAsFactors = F)
colnames(exp_add_etas_table_2) <- c("")
exp_add_etas_table_3 <- data.frame(c("ETA(1)","ETA(3)","C","A"),
                               c("No","No","Yes","Yes"),
                               c("0.32","0.37","0.77","0.51"),
                               c(0.42,0.48,"",""),
                               stringsAsFactors = F)
colnames(exp_add_etas_table_3) <- colnames(exp_add_etas_table_1)
#compare
context("qa, get_add_etas_table")
test_that("creates add estas table",{
  expect_equal(add_etas_table_1,exp_add_etas_table_1) #file exists, adds dofv value, all combinations in column 'Added'
  expect_false(add_etas_error_1)
  expect_equal(add_etas_table_2,exp_add_etas_table_2) #files not exist
  expect_true(add_etas_error_2)
  expect_equal(add_etas_table_3,exp_add_etas_table_3) #file exists, all added etas found, no dofv value
  expect_false(add_etas_error_3)
  expect_equal(add_etas_table_4,exp_add_etas_table_2) #files exists, but list of added etas are empty
  expect_true(add_etas_error_4)
})

#...........................  (14) Test function get_iov_table.R .....................................
#run function
iov_fun_run_1 <- get_iov_table(original_max0_model=file.path(files.w.dir,"qa_run5/linearize_run/scm_dir1/derivatives.mod"),iov_model=file.path(files.w.dir,"qa_run5/modelfit_run/iov.mod"),iov_etas=list(o1=c(4,6,7),o2=c(5,8,9)),dofv_iov=-0.002234)
iov_table_1 <- iov_fun_run_1$iov_table
iov_error_1 <- iov_fun_run_1$iov_error

iov_fun_run_2 <- get_iov_table(original_max0_model=file.path(files.w.dir,"qa_run7/linearize_run/scm_dir1/derivatives.mod"),iov_model=file.path(files.w.dir,"qa_run7/modelfit_run/iov.mod"),iov_etas=list(o1=c(4,7),o2=c(5,9)),dofv_iov="NA")
iov_table_2 <- iov_fun_run_2$iov_table
iov_error_2 <- iov_fun_run_2$iov_error

iov_fun_run_3 <- get_iov_table(original_max0_model=file.path(files.w.dir,"qa_run5/linearize_run/scm_dir1/derivatives.mod"),iov_model=file.path(files.w.dir,"qa_run5/modelfit_run/iov.mod"),iov_etas=list(),dofv_iov=-0.002234)
iov_table_3 <- iov_fun_run_3$iov_table
iov_error_3 <- iov_fun_run_3$iov_error

#expected data
exp_iov_table_1 <- data.frame(c("ETA(1)","ETA(2)","ETA(3)","dOFV"),
                          c("0.13","1.00","0.45","0.0"),
                          c(0.11,0.14,"0.10",""),
                          c(0.42,0.36,0.48,""),stringsAsFactors = F)
colnames(exp_iov_table_1) <- c("Linked to","IIV","IOV","Old SD")
exp_iov_table_2 <- data.frame(c("ETA(1)","ETA(3)"),
                          c("0.13","0.45"),
                          c(0.11,"0.10"),
                          c(0.42,"0.48"),stringsAsFactors = F)
colnames(exp_iov_table_2) <- colnames(exp_iov_table_1)
exp_iov_table_3 <- data.frame("ERROR",stringsAsFactors = F)
colnames(exp_iov_table_3) <- ""

#compare
context("qa, get_iov_table")
test_that("creates iov table",{
  expect_equal(iov_table_1,exp_iov_table_1) # files exist, iov not an empty list, round values, dofv is a value 
  expect_false(iov_error_1)
  expect_equal(iov_table_2,exp_iov_table_2) # files exist, iov not an empty list, round values, no dofv value will be added 
  expect_false(iov_error_2)
  expect_equal(iov_table_3,exp_iov_table_3) # iov is an empty list
  expect_true(iov_error_3)
})

#...........................  (15) Test function get_eta_values.R .....................................
#input tables
theta_values_boxcox <- data.frame("ETA"=c("ETA(1)","ETA(2)","ETA(3)","ETA(4)"),"Lambda"=c(2,0.2,0.4,0.1))
theta_values_tdist <- theta_values_boxcox
colnames(theta_values_tdist) <- c("ETA","Degrees of freedom")
# run function
eta_boxcox <- get_eta_values(param_model=file.path(files.w.dir,"qa_run1/modelfit_run/boxcox.mod"),
                             theta_values=theta_values_boxcox)
eta_tdist <- get_eta_values(param_model=file.path(files.w.dir,"qa_run1/modelfit_run/tdist.mod"),
                            theta_values=theta_values_tdist)
#compare
context("qa, get_eta_values")
test_that("get eta values from boxcox or tdist phi file",{
  expect_equal(eta_boxcox,
               data.frame("ETA_name"=c(rep("ETA(1)",4),rep("ETA(2)",4),rep("ETA(3)",4),rep("ETA(4)",4)),
                          "value"=c(0.216665,0.276354,0.231142,0.107048,-0.022947,0.101007,0.974127,-0.022947,
                                    -0.126678,-0.369641,-0.525548,0.143994,-0.109397,-0.073727,0.034058,0.042088),
                          stringsAsFactors = F),
               tolerance=6)
  expect_equal(get_eta_values(param_model=file.path(files.w.dir,"qa_run2/modelfit_run/boxcox.mod"),
                              theta_values=theta_values_boxcox),
               data.frame()) # boxcox file does not exist
  
  expect_equal(data.frame("ETA_name"=c(rep("ETA(1)",4),rep("ETA(2)",4),rep("ETA(3)",4),rep("ETA(4)",4)),
                          "value"=c(0.204035,0.249988,0.215493,0.109567,0.042453,-0.174131,10.684607,0.042453,
                                    -0.162517,-0.646372,-1.279824,0.175925,3.482807,2.374547,-1.100379,-1.357668),
                          stringsAsFactors = F),
               eta_tdist,
               tolerance=6)

  expect_equal(get_eta_values(param_model=file.path(files.w.dir,"qa_run2/modelfit_run/tdist.mod"),
                              theta_values=theta_values_tdist),
               data.frame()) # tdist file does not exist
  
 })

#...........................  (16) Test function get_eta_transf_table.R .....................................
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
boxcox_lambdas_orig_3 <- data.frame()

tdist_table_orig_1 <- boxcox_lambdas_orig_1
colnames(tdist_table_orig_1) <- c("ETA","Degrees of freedom","New SD","Old SD")
tdist_table_orig_2 <- boxcox_lambdas_orig_2
colnames(tdist_table_orig_2) <- c("ETA","Degrees of freedom","New SD","Old SD")
tdist_table_orig_3 <- boxcox_lambdas_orig_1
colnames(tdist_table_orig_3) <- c("ETA","OTHER","New SD","Old SD")

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
list_tdist_tables_3 <- get_eta_transf_table(input_table=tdist_table_orig_3)
make_tdist_plot_3 <- list_tdist_tables_3$make_eta_transf_plot
fig_height_tdist_3 <- list_tdist_tables_3$fig_height

#compare
context("qa, get_eta_transf_table")
test_that("get create input table for transformed density plot",{
  expect_equal(colnames(boxcox_shape_table_1),c("ETA_name","density","type","eta")) #boxcox, etat with lambda calculation, plot hight 6.25
  expect_equal(dim(boxcox_shape_table_1),c(18,4))
  expect_equal(c(rep("ETA(1)",6),rep("ETA(2)",6),rep("ETA(3)",6)),boxcox_shape_table_1$ETA_name)
  expect_equal(rep(c(rep("ETA",3),rep("ETAT",3)),3),boxcox_shape_table_1$type)
  expect_equal(boxcox_shape_table_1$eta,
               c(-0.6751095,0,0.6751095,-0.3600795,0,1.4943333,-2.4894512,0,2.4894512,rep(NA,3),
                 -2.1205587,0,2.1205587,-8.9436407,0,0.7997624),
               tolerance=7)
  expect_true(make_boxcox_shape_plot_1)
  expect_equal(fig_height_boxcox_1,6.25)
  expect_equal(colnames(boxcox_shape_table_2),c("ETA_name","density","type","eta")) #boxcox, etat with lambda calculation, plot hight 15
  expect_equal(dim(boxcox_shape_table_2),c(54,4))
  expect_equal(boxcox_shape_table_2$ETA_name,c(rep("ETA(1)",6),rep("ETA(2)",6),rep("ETA(3)",6),
                 rep("ETA(4)",6),rep("ETA(5)",6),rep("ETA(6)",6),
                 rep("ETA(7)",6),rep("ETA(8)",6),rep("ETA(9)",6)))
  expect_equal(boxcox_shape_table_2$type,rep(c(rep("ETA",3),rep("ETAT",3)),9))
  expect_true(make_boxcox_shape_plot_2)
  expect_equal(fig_height_boxcox_2,15)
  expect_equal(fig_height_boxcox_2,15)
  expect_false(make_boxcox_shape_plot_3) # input table is an empty dataframe
  expect_equal(fig_height_boxcox_3,15)

  expect_equal(colnames(tdist_table_1),c("ETA_name","density","type","eta")) #tdist, etat with degries of freedom calculation, plot hight 6.25
  expect_equal(dim(tdist_table_1),c(18,4))
  expect_equal(tdist_table_1$ETA_name,c(rep("ETA(1)",6),rep("ETA(2)",6),rep("ETA(3)",6)))
  expect_equal(tdist_table_1$type,rep(c(rep("ETA",3),rep("ETAT",3)),3))
  expect_equal(tdist_table_1$eta,
               c(-0.6751095,0,0.6751095,-0.3600795,0,1.4943333,-2.4894512,0,2.4894512,rep(NA,3),
                 -2.1205587,0,2.1205587,-8.9436407,0,0.7997624),
               tolerance=7)
  expect_true(make_tdist_plot_1)
  expect_equal(fig_height_tdist_1,6.25)
  expect_equal(colnames(tdist_table_2),c("ETA_name","density","type","eta")) #tdist, etat with degries of freedom calculation, plot hight 15
  expect_equal(dim(tdist_table_2),c(54,4))
  expect_equal(tdist_table_2$ETA_name,
               c(rep("ETA(1)",6),rep("ETA(2)",6),rep("ETA(3)",6),
                 rep("ETA(4)",6),rep("ETA(5)",6),rep("ETA(6)",6),
                 rep("ETA(7)",6),rep("ETA(8)",6),rep("ETA(9)",6)))
  expect_equal(tdist_table_2$type,rep(c(rep("ETA",3),rep("ETAT",3)),9))
  expect_true(make_tdist_plot_2)
  expect_equal(fig_height_tdist_2,15)
  expect_false(make_tdist_plot_3) # Column "Lambdas" of "Degries of freedom" not found
  expect_equal(fig_height_tdist_3,15)
})

#...........................  (15) Test function get_all_covariates.R .....................................
# run function
list_all_cov1 <- get_all_covariates(frem_directory=file.path(files.w.dir,"qa_missing_folders_files/frem_run"),continuous=c('AGE'),
                                    categorical=c(),parameters=c(),dofv_full_block=3.5,skip=c('transform','resmod','scm','cdd','simeval'))
frem_files_exists_1 <- list_all_cov1$frem_files_exists
frem_table_1 <- list_all_cov1$frem_table

list_all_cov2 <- get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),continuous=c(),categorical=c(),
                                    parameters=c(),dofv_full_block=3.5,skip=c('resmod','scm','cdd','simeval'))
frem_files_exists_2 <- list_all_cov2$frem_files_exists
frem_table_2 <- list_all_cov2$frem_table

list_all_cov3 <- get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),continuous=c('SEX'),
                                    categorical=c('AGE','WGT'),parameters=c('K1','K2'),dofv_full_block=3.5,skip=c())
frem_files_exists_3 <- list_all_cov3$frem_files_exists
frem_table_3 <- list_all_cov3$frem_table

list_all_cov4 <- get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),continuous=c('SEX'),
                                    categorical=c('AGE','WGT'),parameters=c(),dofv_full_block='NA',skip=c('resmod'))
frem_files_exists_4 <- list_all_cov4$frem_files_exists
frem_table_4 <- list_all_cov4$frem_table

list_all_cov5 <- get_all_covariates(frem_directory=file.path(files.w.dir,"qa_run1/frem_run"),continuous=c('SEX'),
                                    categorical=c('AGE','WGT'),parameters=c(),dofv_full_block='NA',skip=c('resmod','frem','cdd'))
frem_files_exists_5 <- list_all_cov5$frem_files_exists
frem_table_5 <- list_all_cov5$frem_table
#expected data
exp_frem_table_1 <- data.frame("FREM","ERROR","",stringsAsFactors = F)
colnames(exp_frem_table_1) <- c("","dOFV","Add.params")
exp_frem_table_2 <- data.frame("FREM","NA","",stringsAsFactors = F)
colnames(exp_frem_table_2) <- c("","dOFV","Add.params")
exp_frem_table_3 <- data.frame("ALL",3.6,6,stringsAsFactors = F)
colnames(exp_frem_table_3) <- c("","dOFV","Add.params")
exp_frem_table_4 <- data.frame("ALL",7.1,0,stringsAsFactors = F)
colnames(exp_frem_table_4) <- c("","dOFV","Add.params")
exp_frem_table_5 <- data.frame("FREM","SKIPPED","",stringsAsFactors = F)
colnames(exp_frem_table_5) <- c("","dOFV","Add.params")
#compare
context("qa, get_all_covariates")
test_that("get FREM covariates table",{
  expect_false(frem_files_exists_1) # files does not exist
  expect_equal(frem_table_1,exp_frem_table_1)
  expect_false(frem_files_exists_2) # covariate length are 0
  expect_equal(frem_table_2,exp_frem_table_2)
  expect_true(frem_files_exists_3) # files exist, add dofv value, round values
  expect_equal(frem_table_3,exp_frem_table_3)
  expect_true(frem_files_exists_4) # files exist, not add dofv value, round values
  expect_equal(frem_table_4,exp_frem_table_4)
  expect_false(frem_files_exists_5) # frem skilpped
  expect_equal(frem_table_5,exp_frem_table_5)
})

#...........................  (16) Test function get_scm_table.R .....................................
#run functions
list_scm_run_1 <- get_scm_table(scm_directory=file.path(files.w.dir,"qa_missing_folders_files/scm_run"),
                                parameters=c('K1','K2'),continuous=c('AGE'),categorical=c(),skip=c())
scm_table_1 <- list_scm_run_1$scm_table
max_scm_table_1 <- list_scm_run_1$max_scm_table
scm_files_exists_1 <- list_scm_run_1$scm_files_exists

list_scm_run_2 <- get_scm_table(scm_directory=file.path(files.w.dir,"qa_run1/scm_run"),
                                parameters=c('K1','K2'),
                                continuous=c(),categorical=c(),skip=c())
scm_table_2 <- list_scm_run_2$scm_table
max_scm_table_2 <- list_scm_run_2$max_scm_table
scm_files_exists_2 <- list_scm_run_2$scm_files_exists

list_scm_run_3 <- get_scm_table(scm_directory=file.path(files.w.dir,"qa_run2/scm_run"),
                                parameters=c('CL','V'),
                                continuous=c('APGR','WGT'),categorical=c(),skip=c())
scm_table_3 <- list_scm_run_3$scm_table
max_scm_table_3 <- list_scm_run_3$max_scm_table
scm_files_exists_3 <- list_scm_run_3$scm_files_exists

list_scm_run_4 <- get_scm_table(scm_directory=file.path(files.w.dir,"qa_run3/scm_run"),
                                parameters=c('CL','V'),
                                continuous=c('APGR','WGT'),categorical=c(),skip=c('frem','resmod','transform','cdd','simeval'))
scm_table_4 <- list_scm_run_4$scm_table
max_scm_table_4 <- list_scm_run_4$max_scm_table
scm_files_exists_4 <- list_scm_run_4$scm_files_exists

list_scm_run_5 <- get_scm_table(scm_directory=file.path(files.w.dir,"qa_run3/scm_run"),
                                parameters=c('CL','V'),
                                continuous=c('APGR','WGT'),categorical=c(),skip=c('frem','scm'))
scm_table_5 <- list_scm_run_5$scm_table
max_scm_table_5 <- list_scm_run_5$max_scm_table
scm_files_exists_5 <- list_scm_run_5$scm_files_exists

#expected data
exp_scm_table_1 <- data.frame("SCM","ERROR",stringsAsFactors = F)
colnames(exp_scm_table_1) <- c("","dOFV")
exp_max_scm_table_1 <- data.frame("SCM","ERROR","",stringsAsFactors = F)
colnames(exp_max_scm_table_1) <- c("","dOFV","Add.params")
exp_scm_table_2 <- data.frame("SCM","NA",stringsAsFactors = F)
colnames(exp_scm_table_2) <- c("","dOFV")
exp_max_scm_table_2 <- data.frame("SCM","NA","",stringsAsFactors = F)
colnames(exp_max_scm_table_2) <- c("","dOFV","Add.params")
exp_scm_table_3 <- data.frame(c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4"),c(2.87,0.47,0.17,0.12),c(-0.03,0.05,0.004,-0.012),stringsAsFactors = F)
colnames(exp_scm_table_3) <- c("","dOFV","Coef")
exp_max_scm_table_3 <- data.frame("CLAPGR-4",2.87,1,stringsAsFactors = F)
colnames(exp_max_scm_table_3) <- c("","dOFV","Add.params")
exp_scm_table_4 <- data.frame(c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4"),c(NA,0.47,0.17,0.12),c(NA,0.05,0.004,-0.012),stringsAsFactors = F)
colnames(exp_scm_table_4) <- c("","dOFV","Coef")
exp_max_scm_table_4 <- data.frame("CLWGT-4",0.47,1,stringsAsFactors = F)
colnames(exp_max_scm_table_4) <- c("","dOFV","Add.params")
exp_max_scm_table_5 <- data.frame("SCM","SKIPPED","",stringsAsFactors = F)
colnames(exp_max_scm_table_5) <- c("","dOFV","Add.params")
exp_scm_table_5 <- data.frame("SCM","SKIPPED",stringsAsFactors = F)
colnames(exp_scm_table_5) <- c("","dOFV")

#compare
context("qa, get_scm_table")
test_that("get SCM table",{
  expect_equal(scm_table_1,exp_scm_table_1) # files not exist
  expect_equal(max_scm_table_1,exp_max_scm_table_1)
  expect_false(scm_files_exists_1)
  expect_equal(scm_table_2,exp_scm_table_2) # files exist, no covariates
  expect_equal(max_scm_table_2,exp_max_scm_table_2)
  expect_false(scm_files_exists_2)
  expect_equal(scm_table_3,exp_scm_table_3) # files exist, covariates exist, round values
  expect_equal(max_scm_table_3,exp_max_scm_table_3) 
  expect_true(scm_files_exists_3)
  expect_equal(scm_table_4,exp_scm_table_4) # files exist, covariates exist, first value is NA,round values
  expect_equal(max_scm_table_4,exp_max_scm_table_4)
  expect_true(scm_files_exists_4)
  expect_equal(scm_table_5,exp_scm_table_5) # skipped
  expect_equal(max_scm_table_5,exp_max_scm_table_5)
})

#...........................  (17) Test function get_covariates_table.R .....................................
#input data (not using calculated frem_table_4 because of floating point missmatch)
frem_table_4 <- data.frame("ALL",7.1,0,stringsAsFactors = F)
colnames(frem_table_4) <- c("","dOFV","Add.params")
#run function
list_covariate_table_1 <- get_covariates_table(frem_table=frem_table_1,scm_table=scm_table_1,max_scm_table=max_scm_table_1)
covariates_table_1 <- list_covariate_table_1$covariates_table
covariates_extra_table_1 <- list_covariate_table_1$covariates_extra_table

list_covariate_table_2 <- get_covariates_table(frem_table=frem_table_2,scm_table=scm_table_2,max_scm_table=max_scm_table_2)
covariates_table_2 <- list_covariate_table_2$covariates_table
covariates_extra_table_2 <- list_covariate_table_2$covariates_extra_table

list_covariate_table_3 <- get_covariates_table(frem_table=frem_table_2,scm_table=scm_table_3,max_scm_table=max_scm_table_3)
covariates_table_3 <- list_covariate_table_3$covariates_table
covariates_extra_table_3 <- list_covariate_table_3$covariates_extra_table

list_covariate_table_4 <- get_covariates_table(frem_table=frem_table_3,scm_table=scm_table_3,max_scm_table=max_scm_table_3)
covariates_table_4 <- list_covariate_table_4$covariates_table
covariates_extra_table_4 <- list_covariate_table_4$covariates_extra_table

list_covariate_table_5 <- get_covariates_table(frem_table=frem_table_1,scm_table=scm_table_3,max_scm_table=max_scm_table_3)
covariates_table_5 <- list_covariate_table_5$covariates_table
covariates_extra_table_5 <- list_covariate_table_5$covariates_extra_table

list_covariate_table_6 <- get_covariates_table(frem_table=frem_table_4,scm_table=scm_table_1,max_scm_table=max_scm_table_1)
covariates_table_6 <- list_covariate_table_6$covariates_table
covariates_extra_table_6 <- list_covariate_table_6$covariates_extra_table

list_covariate_table_7 <- get_covariates_table(frem_table=frem_table_3,scm_table=scm_table_4,max_scm_table=max_scm_table_4)
covariates_table_7 <- list_covariate_table_7$covariates_table
covariates_extra_table_7 <- list_covariate_table_7$covariates_extra_table

list_covariate_table_8 <- get_covariates_table(frem_table=frem_table_5,scm_table=scm_table_5,max_scm_table=max_scm_table_5)
covariates_table_8 <- list_covariate_table_8$covariates_table
covariates_extra_table_8 <- list_covariate_table_8$covariates_extra_table

list_covariate_table_9 <- get_covariates_table(frem_table=frem_table_5,scm_table=scm_table_3,max_scm_table=max_scm_table_3)
covariates_table_9 <- list_covariate_table_9$covariates_table
covariates_extra_table_9 <- list_covariate_table_9$covariates_extra_table

list_covariate_table_10 <- get_covariates_table(frem_table=frem_table_4,scm_table=scm_table_5,max_scm_table=max_scm_table_5)
covariates_table_10 <- list_covariate_table_10$covariates_table
covariates_extra_table_10 <- list_covariate_table_10$covariates_extra_table

#expected data
exp_covariates_table_1 <- data.frame(c("FREM","SCM"),c("ERROR","ERROR"),c("",""),stringsAsFactors = F)
colnames(exp_covariates_table_1) <- c("","dOFV","Add.params")
exp_covariates_extra_table_1 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("ERROR","ERROR"),stringsAsFactors = F)
exp_covariates_table_2 <- data.frame(c("FREM","SCM"),c("NA","NA"),c("",""),stringsAsFactors = F)
colnames(exp_covariates_table_2) <- c("","dOFV","Add.params")
exp_covariates_extra_table_2 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("NA","NA"),stringsAsFactors = F)
exp_covariates_table_3 <- data.frame(c("FREM","CLAPGR-4"),c("NA","2.87"),c("","1"),stringsAsFactors = F)
colnames(exp_covariates_table_3) <- c("","dOFV","Add.params")
exp_covariates_extra_table_3 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                       "dOFV"=c("2.9","0.5","0.2","0.1","3.6","NA"),
                                       "Coefficient"=c("-0.030","0.050","0.004","-0.012","",""),stringsAsFactors = F)
exp_covariates_table_4 <- data.frame(c("FREM","CLAPGR-4"),c(3.6,2.87),c(6,1),stringsAsFactors = F)
colnames(exp_covariates_table_4) <- c("","dOFV","Add.params")
exp_covariates_extra_table_4 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                       "dOFV"=c("2.9","0.5","0.2","0.1","3.6","3.6"),
                                       "Coefficient"=c("-0.030","0.050","0.004","-0.012","",""),stringsAsFactors = F)
exp_covariates_table_5 <- data.frame(c("FREM","CLAPGR-4"),c("ERROR","2.87"),c("","1"),stringsAsFactors = F)
colnames(exp_covariates_table_5) <- c("","dOFV","Add.params")
exp_covariates_extra_table_5 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                       "dOFV"=c("2.9","0.5","0.2","0.1","3.6","ERROR"),
                                       "Coefficient"=c("-0.030","0.050","0.004","-0.012","",""),stringsAsFactors = F)
exp_covariates_table_6 <- data.frame(c("FREM","SCM"),c("7.1","ERROR"),c("0",""),stringsAsFactors = F)
colnames(exp_covariates_table_6) <- c("","dOFV","Add.params")
exp_covariates_extra_table_6 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("ERROR","7.1"),stringsAsFactors = F)
exp_covariates_table_7 <- data.frame(c("FREM","CLWGT-4"),c(3.6,0.47),c(6,1),stringsAsFactors = F)
colnames(exp_covariates_table_7) <- c("","dOFV","Add.params")
exp_covariates_extra_table_7 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                       "dOFV"=c("NA","0.5","0.2","0.1","0.8","3.6"),
                                       "Coefficient"=c("NA","0.050","0.004","-0.012","",""),stringsAsFactors = F)
exp_covariates_table_8 <- data.frame(c("FREM","SCM"),c("SKIPPED","SKIPPED"),c("",""),stringsAsFactors = F)
colnames(exp_covariates_table_8) <- c("","dOFV","Add.params")
exp_covariates_extra_table_8 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("SKIPPED","SKIPPED"),stringsAsFactors = F)
exp_covariates_table_9 <- data.frame(c("FREM","CLAPGR-4"),c("SKIPPED","2.87"),c("","1"),stringsAsFactors = F)
colnames(exp_covariates_table_9) <- c("","dOFV","Add.params")
exp_covariates_extra_table_9 <- data.frame("Covariate"=c("CLAPGR-4","CLWGT-4","VAPGR-4","VWGT-4","sum(SCMu)","FREM"),
                                           "dOFV"=c("2.9","0.5","0.2","0.1","3.6","SKIPPED"),
                                           "Coefficient"=c("-0.030","0.050","0.004","-0.012","",""),stringsAsFactors = F)
exp_covariates_table_10 <- data.frame(c("FREM","SCM"),c("7.1","SKIPPED"),c("0",""),stringsAsFactors = F)
colnames(exp_covariates_table_10) <- c("","dOFV","Add.params")
exp_covariates_extra_table_10 <- data.frame("Covariate"=c("SCM","FREM"),"dOFV"=c("SKIPPED","7.1"),stringsAsFactors = F)
#compare
context("qa, get_covariates_table")
test_that("create covariate table (FREM and SCM) and extra table",{
  expect_equal(covariates_table_1,exp_covariates_table_1) #both scm and frem are errors
  expect_equal(covariates_extra_table_1,exp_covariates_extra_table_1)
  expect_equal(covariates_table_2,exp_covariates_table_2) # both NA (no covar and param where passes to the run)
  expect_equal(covariates_extra_table_2,exp_covariates_extra_table_2) 
  expect_equal(covariates_table_3,exp_covariates_table_3) # Frem NA, SCM a number
  expect_equal(covariates_extra_table_3,exp_covariates_extra_table_3)
  expect_equal(covariates_table_4,exp_covariates_table_4) # both numbers, round values
  expect_equal(covariates_extra_table_4,exp_covariates_extra_table_4)
  expect_equal(covariates_table_5,exp_covariates_table_5) # Frem error, scm number
  expect_equal(covariates_extra_table_5,exp_covariates_extra_table_5)
  expect_equal(covariates_table_6,exp_covariates_table_6) # frem number, scm error
  expect_equal(covariates_extra_table_6,exp_covariates_extra_table_6)
  expect_equal(covariates_table_7,exp_covariates_table_7) # both numbers, scm has NA value (first)
  expect_equal(covariates_extra_table_7,exp_covariates_extra_table_7)
  expect_equal(covariates_table_8,exp_covariates_table_8) # both skipped
  expect_equal(covariates_extra_table_8,exp_covariates_extra_table_8)
  expect_equal(covariates_table_9,exp_covariates_table_9) # only frem skipped
  expect_equal(covariates_extra_table_9,exp_covariates_extra_table_9)
  expect_equal(covariates_table_10,exp_covariates_table_10) # only scm skipped
  expect_equal(covariates_extra_table_10,exp_covariates_extra_table_10)
})

#...........................  (18) Test function get_resmod_ruv_table.R .....................................
#run function
resmod_list_1 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run1"),idv_name="TIME",dvid_name="ORIG",skip=c())
resmod_list_2 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run3"),idv_name="TIME",dvid_name="NA",skip=c('frem','scm','transform','cdd','simeval'))
resmod_list_3 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run2"),idv_name="TIME",dvid_name="ORIG",skip=c('transform','cdd','simeval'))
resmod_list_4 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run7"),idv_name="TIME",dvid_name="NA",skip=c('frem','simeval'))
resmod_list_5 <- get_resmod_ruv_table(directory=file.path(files.w.dir,"qa_run3"),idv_name="TIME",dvid_name="NA",skip=c('resmod'))
#expected data
exp_resmod_ruv_table_list_1 <- data.frame("ERROR",stringsAsFactors = F)
colnames(exp_resmod_ruv_table_list_1) <- ""
exp_resmod_ruv_overview_1 <- data.frame("RESMOD","ERROR",stringsAsFactors = F)
colnames(exp_resmod_ruv_overview_1) <- c("","dOFV")
exp_resmod_ruv_table_list_2 <- data.frame("Model"=c("tdist","dtbs","IIV on RUV","power","time varying","autocorrelation"),
                                      "dOFV"=c("43.6","21.7","10.6","0.9","0.6","0.0"),
                                      "Additional parameters"=c(1,2,1,1,2,1),
                                      "Parameter values"=c("Df=2.550","lambda=-0.136,Zeta=0.09","%CV=7.2","delta_power=0.094","sdeps_0-t0=1.069,sdeps_t0-inf=0.981,t0=0.68","half-life=1.000"),
                                      stringsAsFactors = F)
colnames(exp_resmod_ruv_table_list_2) <- c("Model","dOFV","Additional parameters","Parameter values")
exp_resmod_ruv_overview_2 <- data.frame(c("tdist","dtbs"),c(43.62,21.67),c(1,2),stringsAsFactors = F)
colnames(exp_resmod_ruv_overview_2) <- c("","dOFV","Add.params")
exp_resmod_ruv_table_list_3_a <- data.frame("Model"=c("tdist","IIV on RUV","time varying","autocorrelation"),
                                      "dOFV"=c("5.0","2.7","0.8","0.0"),
                                      "Additional parameters"=c(1,1,2,1),
                                      "Parameter values"=c("df=9.238","%CV=14.973","sdeps_0-t0=0.868,sdeps_t0-inf=0.999,t0=7.00","half-life=0.011"),
                                      stringsAsFactors = F)
colnames(exp_resmod_ruv_table_list_3_a) <- c("Model","dOFV","Additional parameters","Parameter values")
exp_resmod_ruv_table_list_3_b <- data.frame("Model"=c("time varying","IIV on RUV","tdist","autocorrelation"),
                                        "dOFV"=c("1.65","1.05","0.02","0.00"),
                                        "Additional parameters"=c(2,1,1,1),
                                        "Parameter values"=c("sdeps_0-t0=0.868,sdeps_t0-inf=1.020,t0=7.00","%CV=15.214","df=119.865","half-life=0.011"),
                                        stringsAsFactors = F)
colnames(exp_resmod_ruv_table_list_3_b) <- c("Model","dOFV","Additional parameters","Parameter values")
exp_resmod_ruv_overview_3 <- data.frame(c("(ORIG = 1)","tdist","IIV on RUV","(ORIG = 2)","time varying","IIV on RUV"),
                                    c("","5.02","2.68","","1.65","1.05"),
                                    c("","1","1","","2","1"),stringsAsFactors = F)
colnames(exp_resmod_ruv_overview_3) <- c("","dOFV","Add.params")
exp_resmod_ruv_table_list_4 <- data.frame("Model"=c("IIV on RUV","autocorrelation","dtbs","power","tdist","time varying"),
                                      "dOFV"=c("NA","NA","NA","NA","NA","NA"),
                                      "Additional parameters"=c(rep("",6)),
                                      "Parameter values"=c(rep("",6)),
                                      stringsAsFactors = F)
colnames(exp_resmod_ruv_table_list_4) <- c("Model","dOFV","Additional parameters","Parameter values")
exp_resmod_ruv_overview_4 <- data.frame(c("IIV on RUV","autocorrelation"),c("NA","NA"),c("",""),stringsAsFactors = F)
colnames(exp_resmod_ruv_overview_4) <- c("","dOFV","Add.params")
exp_resmod_ruv_overview_5 <- data.frame("RESMOD","SKIPPED",stringsAsFactors = F)
colnames(exp_resmod_ruv_overview_5) <- c("","dOFV")
#compare
context("qa, get_resmod_ruv_table")
test_that("create resmod ruv table",{
  expect_equal(length(resmod_list_1$resmod_ruv_table_list),1) # files does not exist
  expect_equal(resmod_list_1$resmod_ruv_table_list[[1]],exp_resmod_ruv_table_list_1) 
  expect_equal(resmod_list_1$resmod_ruv_overview,exp_resmod_ruv_overview_1) 
  expect_equal(resmod_list_1$dvid_nr,"NA")
  expect_equal(length(resmod_list_2$resmod_ruv_table_list),1) # file exists, dvid is NA
  expect_equal(resmod_list_2$resmod_ruv_table_list[[1]],exp_resmod_ruv_table_list_2)
  expect_equal(resmod_list_2$resmod_ruv_overview,exp_resmod_ruv_overview_2)
  expect_equal(resmod_list_2$dvid_nr,"NA")
  expect_equal(length(resmod_list_3$resmod_ruv_table_list),2) # files exist, dvid is 1,2 
  expect_equal(resmod_list_3$resmod_ruv_table_list[[1]],exp_resmod_ruv_table_list_3_a)
  expect_equal(resmod_list_3$resmod_ruv_table_list[[2]],exp_resmod_ruv_table_list_3_b)
  expect_equal(resmod_list_3$resmod_ruv_overview,exp_resmod_ruv_overview_3)
  expect_equal(resmod_list_3$dvid_nr,c(1,2))
  expect_equal(length(resmod_list_4$resmod_ruv_table_list),1) # file exists, dvid is NA, all dOFV values ar NA
  expect_equal(resmod_list_4$resmod_ruv_table_list[[1]],exp_resmod_ruv_table_list_4)
  expect_equal(resmod_list_4$resmod_ruv_overview,exp_resmod_ruv_overview_4)
  expect_equal(resmod_list_4$dvid_nr,'NA')
  expect_equal(resmod_list_5$resmod_ruv_overview,exp_resmod_ruv_overview_5) #skipped
})

#...........................  (19) Test function get_ii_table.R .....................................
#fun function
list_ii_1 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_missing_folders_files/cdd_run/"),
                          model.filename = "lr.mod",
                          cutoff=3.84,max_rows=3,skip=c('frem','resmod','transform','scm','simeval'),
                          nonlinear=FALSE)
list_ii_2 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_run2/cdd_run/"),
                          model.filename = "r.mod",
                          cutoff=3.84,max_rows=3,skip=c(),
                          nonlinear=FALSE)
list_ii_3 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_run3/cdd_run/"),
                          model.filename = "r.mod",
                          cutoff=3.84,max_rows=3,skip=c('scm','simeval'),
                          nonlinear=FALSE)
list_ii_4 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_run1/cdd_run/"),
                          model.filename = "r.mod",
                          cutoff=1,max_rows=3,skip=c('frem','resmod','transform','scm','simeval'),
                          nonlinear=T)
list_ii_5 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_run1/cdd_run/"),
                          model.filename = "r.mod",
                          cutoff=0.1,max_rows=3,skip=c(),
                          nonlinear=TRUE)
list_ii_6 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_run1/cdd_run/"),
                          model.filename = "r.mod",
                          cutoff=0.000001,max_rows=2,skip=c('frem','resmod','transform','scm','simeval'),
                          nonlinear=T)
list_ii_7 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_run4/cdd_run/"),
                          model.filename = "r.mod",
                          cutoff=3.84,max_rows=3,skip=c('frem','resmod','transform','scm','simeval'),
                          nonlinear=FALSE)
list_ii_8 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_run4/cdd_run/"),
                          model.filename = "r.mod",
                          cutoff=3.84,max_rows=3,skip=c('cdd','scm','simeval'),
                          nonlinear=FALSE)
list_ii_9 <- get_ii_table(cdd_directory=file.path(files.w.dir,"qa_missing_folders_files/cdd_run/"),
                          model.filename = "r.mod",
                          cutoff=3.84,max_rows=3,skip=c('cdd'),
                          nonlinear=FALSE)
#expected data
ERROR_table <- data.frame("ERROR",stringsAsFactors = F)
colnames(ERROR_table) <- ""
exp_cdd_highest_dofv_1 <- data.frame("CDD","ERROR",stringsAsFactors = F)
colnames(exp_cdd_highest_dofv_1) <- c("","dOFV")
exp_ii_table_3 <- data.frame("All dOFV values are negative",stringsAsFactors = F)
colnames(exp_ii_table_3) <- ""
exp_cdd_highest_dofv_3 <- data.frame("All dOFV values are negative","",stringsAsFactors = F)
colnames(exp_cdd_highest_dofv_3) <- c("","dOFV")
exp_cdd_data_4 <- data.frame(id=c(11,34,14,57,3,7,8),
                         dOFV=c(0.11014,2.00265,0.12541,0.08563,0.00003,0.01568,1.03525))
exp_ii_table_4 <- data.frame(c("Subject 34","Subject 8"),c("2.0","1.0"),stringsAsFactors = F)
colnames(exp_ii_table_4) <- c("Subjects","dOFV")
exp_cdd_highest_dofv_4 <- data.frame("Subject 34",2.00265,stringsAsFactors = F)
colnames(exp_cdd_highest_dofv_4) <- c("","dOFV")
exp_ii_table_5 <- data.frame(c("Subject 34","Subject 8","Subject 14"),c("2.0","1.0","0.1"),stringsAsFactors = F)
colnames(exp_ii_table_5) <- c("Subjects","dOFV")
exp_ii_table_6 <- data.frame(c("Subject 34","Subject 8"),c("2.0","1.0"),stringsAsFactors = F)
colnames(exp_ii_table_6) <- c("Subjects","dOFV")
exp_cdd_data_7 <- data.frame(id=c(11,12,34,14,57),
                         dOFV=c(0.01014,0.06941,0.00265,0.02541,0.08563))
exp_ii_table_7 <- data.frame(c("No influential individuals detected"),stringsAsFactors = F)
colnames(exp_ii_table_7) <- c("")
exp_cdd_highest_dofv_7 <- data.frame("None","",stringsAsFactors = F)
colnames(exp_cdd_highest_dofv_7) <- c("","dOFV")
exp_cdd_data_8 <- data.frame("SKIPPED",stringsAsFactors = F)
colnames(exp_cdd_data_8) <- NULL
exp_ii_table_8 <- data.frame(c("No influential individuals detected"),stringsAsFactors = F)
colnames(exp_ii_table_8) <- c("")
exp_cdd_highest_dofv_8 <- data.frame("CDD","SKIPPED",stringsAsFactors = F)
colnames(exp_cdd_highest_dofv_8) <- c("","dOFV")

#compare
context("qa, get_ii_table")
test_that("create influential individuals table",{
  expect_false(list_ii_1$cdd_files_exist) # files not exist
  expect_equal(list_ii_1$all_dofv,c())
  expect_equal(list_ii_1$cdd.data,ERROR_table)
  expect_equal(list_ii_1$cdd_highest_dofv,exp_cdd_highest_dofv_1)
  expect_equal(list_ii_1$ii_table,ERROR_table)
  expect_equal(list_ii_1$infl_id,c())
  expect_equal(list_ii_1$fig_height_infl,5)
  expect_true(list_ii_2$cdd_files_exist) # no dofv column found in csv file
  expect_equal(list_ii_2$all_dofv,c())
  expect_equal(list_ii_2$cdd.data,ERROR_table)
  expect_equal(list_ii_2$cdd_highest_dofv,exp_cdd_highest_dofv_1)
  expect_equal(list_ii_2$ii_table,ERROR_table)
  expect_equal(list_ii_2$infl_id,c())
  expect_equal(list_ii_2$fig_height_infl,5)
  expect_true(list_ii_3$cdd_files_exist) # all dofv values are negative
  expect_equal(list_ii_3$all_dofv,c(-0.01014,-0.06941,-0.00265,-0.02541,-0.08563))
  expect_equal(list_ii_3$cdd.data,data.frame())
  expect_equal(list_ii_3$cdd_highest_dofv,exp_cdd_highest_dofv_3)
  expect_equal(list_ii_3$ii_table,exp_ii_table_3)
  expect_equal(list_ii_3$infl_id,c())
  expect_equal(list_ii_3$fig_height_infl,5)
  expect_true(list_ii_4$cdd_files_exist) # files exist, some dofv values are negative, plot hight 5,nonlinear=TRUE
  expect_equal(list_ii_4$all_dofv,c(0.11014,-0.06941,2.00265,0.12541,0.08563,0.00003,-2.08563,0.01568,1.03525))
  expect_equal(list_ii_4$cdd.data,exp_cdd_data_4)
  expect_equal(list_ii_4$cdd_highest_dofv,exp_cdd_highest_dofv_4)
  expect_equal(list_ii_4$ii_table,exp_ii_table_4)
  expect_equal(list_ii_4$infl_id,c(34,8))
  expect_equal(list_ii_4$fig_height_infl,5)
  expect_true(list_ii_5$cdd_files_exist) # files exist, some dofv values are negative, plot hight 7, nonlinear=TRUE
  expect_equal(list_ii_5$all_dofv,c(0.11014,-0.06941,2.00265,0.12541,0.08563,0.00003,-2.08563,0.01568,1.03525))
  expect_equal(list_ii_5$cdd.data,exp_cdd_data_4)
  expect_equal(list_ii_5$cdd_highest_dofv,exp_cdd_highest_dofv_4)
  expect_equal(list_ii_5$ii_table,exp_ii_table_5)
  expect_equal(list_ii_5$infl_id,c(34,8,14,11))
  expect_equal(list_ii_5$fig_height_infl,7)
  expect_true(list_ii_6$cdd_files_exist) # files exist, some dofv values are negative, plot hight full page, nonlinear=TRUE
  expect_equal(list_ii_6$all_dofv,c(0.11014,-0.06941,2.00265,0.12541,0.08563,0.00003,-2.08563,0.01568,1.03525))
  expect_equal(list_ii_6$cdd.data,exp_cdd_data_4)
  expect_equal(list_ii_6$cdd_highest_dofv,exp_cdd_highest_dofv_4)
  expect_equal(list_ii_6$ii_table,exp_ii_table_6)
  expect_equal(list_ii_6$infl_id,c(34,8,14,11,57,7,3))
  expect_equal(list_ii_6$fig_height_infl,15)
  expect_true(list_ii_7$cdd_files_exist) # files exist, no influential id detected
  expect_equal(list_ii_7$all_dofv,c(0.01014,0.06941,0.00265,0.02541,0.08563))
  expect_equal(list_ii_7$cdd.data,exp_cdd_data_7)
  expect_equal(list_ii_7$cdd_highest_dofv,exp_cdd_highest_dofv_7)
  expect_equal(list_ii_7$ii_table,exp_ii_table_7)
  expect_equal(list_ii_7$infl_id,c())
  expect_equal(list_ii_7$fig_height_infl,5)
  expect_equal(list_ii_8$cdd.data,exp_cdd_data_8) # files exist but cdd skipped
  expect_equal(list_ii_8$cdd_highest_dofv,exp_cdd_highest_dofv_8)
  expect_equal(list_ii_9$cdd.data,exp_cdd_data_8) # files does not exist and cdd skipped
  expect_equal(list_ii_9$cdd_highest_dofv,exp_cdd_highest_dofv_8)
})

#...........................  (20) Test function get_outliers_table.R .....................................
#run function
list_simeval_1 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_missing_folders_files/simeval_run"),
                                     cdd.data=exp_cdd_data_4,skip=c('frem','resmod','transform','scm','cdd'))
list_simeval_2 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run1/simeval_run"),
                                     cdd.data=ERROR_table,skip=c())
list_simeval_3 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run2/simeval_run"),
                                     cdd.data=ERROR_table,skip=c('frem','resmod','transform'))
list_simeval_4 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run1/simeval_run"),
                                     cdd.data=data.frame(),skip=c('frem','cdd'))
list_simeval_5 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run2/simeval_run"),
                                     cdd.data=data.frame(),skip=c('scm'))
list_simeval_6 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run2/simeval_run"),
                                     cdd.data=exp_cdd_data_4,skip=c('frem','resmod','transform','scm'))
list_simeval_7 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run3/simeval_run"),
                                     cdd.data=exp_cdd_data_4,skip=c())
list_simeval_8 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run4/simeval_run"),
                                     cdd.data=exp_cdd_data_4,skip=c('frem','resmod','transform','scm'))
list_simeval_9 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run4/simeval_run"),
                                     cdd.data=exp_cdd_data_4,skip=c('frem','resmod','simeval','scm'))
list_simeval_10 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run4/simeval_run"),
                                     cdd.data=exp_cdd_data_8,skip=c('frem','resmod','simeval','scm'))
list_simeval_11 <- get_outliers_table(simeval_directory=file.path(files.w.dir,"qa_run4/simeval_run"),
                                      cdd.data=exp_cdd_data_8,skip=c('frem','resmod','cdd','scm'))
#expected data
exp_max_outlier_table_1 <- data.frame("SIMEVAL","ERROR",stringsAsFactors = F)
colnames(exp_max_outlier_table_1) <- c("","dOFV")
exp_outliers_table_2 <- data.frame("No outliers detected",stringsAsFactors = F)
colnames(exp_outliers_table_2) <- c("")
exp_max_outlier_table_2 <- data.frame("No outliers detected","",stringsAsFactors = F)
colnames(exp_max_outlier_table_2) <- c("","dOFV")
exp_outliers_table_3 <- data.frame(Subjects=c("Subject 1","Subject 12","Subject 8","Subject 34"),
                               dOFV=c(rep("ERROR",4)),stringsAsFactors = F)
exp_max_outlier_table_3 <- data.frame("No dOFV values found","",stringsAsFactors = F)
colnames(exp_max_outlier_table_3) <- c("","dOFV")
exp_outliers_table_5 <- data.frame(Subjects=c("Subject 1","Subject 12","Subject 8","Subject 34"),
                               dOFV=c(rep("NA",4)),stringsAsFactors = F)
exp_outliers_table_6 <- data.frame(Subjects=c("Subject 34","Subject 8","Subject 1","Subject 12"),
                               dOFV=c("2.0","1.0","NA","NA"),stringsAsFactors = F)
exp_max_outlier_table_6 <- data.frame("Subject 34",2.00265,stringsAsFactors = F)
colnames(exp_max_outlier_table_6) <- c("","dOFV")
exp_outliers_table_7 <- data.frame(Subjects=c("Subject 1","Subject 12"),
                               dOFV=c("NA","NA"),stringsAsFactors = F)
exp_max_outlier_table_7 <- data.frame("No dOFV values found","",stringsAsFactors = F)
colnames(exp_max_outlier_table_7) <- c("","dOFV")
exp_outliers_table_8 <- data.frame(Subjects=c("Subject 34","Subject 8","Subject 14","Subject 11","Subject 57","Subject 7","Subject 3"),
                               dOFV=c("2.0","1.0","0.1","0.1","0.1","0.0","0.0"),stringsAsFactors = F)
exp_max_outlier_table_8 <- data.frame(c("Subject 34"),
                                  c(2.00265),stringsAsFactors = F)
colnames(exp_max_outlier_table_8) <- c("","dOFV")
exp_max_outlier_table_9 <- data.frame("SIMEVAL","SKIPPED",stringsAsFactors = F) 
colnames(exp_max_outlier_table_9) <- c("","dOFV")
exp_max_outlier_table_10 <- data.frame("SIMEVAL","SKIPPED",stringsAsFactors = F)
colnames(exp_max_outlier_table_10) <- c("","dOFV")
exp_outliers_table_11 <- data.frame(Subjects=c("Subject 14","Subject 7","Subject 8","Subject 3","Subject 11","Subject 57","Subject 34"),
                               dOFV=c(rep("",7)),stringsAsFactors = F)
exp_max_outlier_table_11 <- data.frame("No dOFV values found (skipped CDD)","",stringsAsFactors = F)
colnames(exp_max_outlier_table_11) <- c("","dOFV")
#compare
context("qa, get_outliers_table")
test_that("create outliers table",{
  expect_false(list_simeval_1$simeval_files_exist) # files not exist
  expect_equal(list_simeval_1$outliers_table,ERROR_table)
  expect_equal(list_simeval_1$max_outlier_table,exp_max_outlier_table_1)
  expect_equal(list_simeval_1$outlier_ids,c())
  expect_equal(list_simeval_1$fig_height_outl,5)
  expect_true(list_simeval_2$simeval_files_exist) # files exist, no outliers detected, cdd error
  expect_equal(list_simeval_2$outliers_table,exp_outliers_table_2)
  expect_equal(list_simeval_2$max_outlier_table,exp_max_outlier_table_2)
  expect_equal(list_simeval_2$outlier_ids,c())
  expect_equal(list_simeval_2$fig_height_outl,5)
  expect_true(list_simeval_3$simeval_files_exist) # files exist, outliers found, cdd error
  expect_equal(list_simeval_3$outliers_table,exp_outliers_table_3)
  expect_equal(list_simeval_3$max_outlier_table,exp_max_outlier_table_3)
  expect_equal(list_simeval_3$outlier_ids,c(1,12,8,34))
  expect_equal(list_simeval_3$fig_height_outl,7)
  expect_true(list_simeval_4$simeval_files_exist) # files exist, no outliers detected, cdd empty
  expect_equal(list_simeval_4$outliers_table,exp_outliers_table_2)
  expect_equal(list_simeval_4$max_outlier_table,exp_max_outlier_table_2)
  expect_equal(list_simeval_4$outlier_ids,c())
  expect_equal(list_simeval_4$fig_height_outl,5)
  expect_true(list_simeval_5$simeval_files_exist) # files exist, outliers found, cdd empty
  expect_equal(list_simeval_5$outliers_table,exp_outliers_table_5)
  expect_equal(list_simeval_5$max_outlier_table,exp_max_outlier_table_3)
  expect_equal(list_simeval_5$outlier_ids,c(1,12,8,34))
  expect_equal(list_simeval_5$fig_height_outl,7)
  expect_true(list_simeval_6$simeval_files_exist) # files exist, ouliers found, cdd not empty, but some dofv values NA (because negative)
  expect_equal(list_simeval_6$outliers_table,exp_outliers_table_6)
  expect_equal(list_simeval_6$max_outlier_table,exp_max_outlier_table_6)
  expect_equal(list_simeval_6$outlier_ids,c(1,12,8,34))
  expect_equal(list_simeval_6$fig_height_outl,7)
  expect_true(list_simeval_7$simeval_files_exist) # files exist, ouliers found, cdd not empty, but all dofv values NA (because negative)
  expect_equal(list_simeval_7$outliers_table,exp_outliers_table_7)
  expect_equal(list_simeval_7$max_outlier_table,exp_max_outlier_table_7)
  expect_equal(list_simeval_7$outlier_ids,c(1,12))
  expect_equal(list_simeval_7$fig_height_outl,5)
  expect_true(list_simeval_8$simeval_files_exist) # files exist, ouliers found, cdd not empty, all dofv values positive
  expect_equal(list_simeval_8$outliers_table,exp_outliers_table_8)
  expect_equal(list_simeval_8$max_outlier_table,exp_max_outlier_table_8)
  expect_equal(list_simeval_8$outlier_ids,c(14,7,8,3,11,57,34))
  expect_equal(list_simeval_8$fig_height_outl,15)
  expect_equal(list_simeval_9$outliers_table,ERROR_table) # simeval skipped, but cdd not skipped
  expect_equal(list_simeval_9$max_outlier_table,exp_max_outlier_table_9)
  expect_equal(list_simeval_10$outliers_table,ERROR_table) # simeval skipped and cdd skipped
  expect_equal(list_simeval_10$max_outlier_table,exp_max_outlier_table_10)
  expect_equal(list_simeval_11$outliers_table,exp_outliers_table_11) # files exist, outliers found, but cdd skipped
  expect_equal(list_simeval_11$max_outlier_table,exp_max_outlier_table_11)
})

#...........................  (21) Test function get_overview_table.R .....................................
#input data
cdd_highest_dofv_extra <- data.frame("Subject 34",0.01265,stringsAsFactors = F)
colnames(cdd_highest_dofv_extra) <- c("","dOFV")
cdd_highest_dofv_extra_1 <- data.frame("Subject 34",0.00265,stringsAsFactors = F)
colnames(cdd_highest_dofv_extra_1) <- c("","dOFV")
#run function
overview_list_1 <- get_overview_table(structural_overview=exp_str_overview_4,
                                      param_var_overview=exp_par_var_models_4,
                                      covariates_overview=exp_covariates_table_1,
                                      resmod_ruv_overview=exp_resmod_ruv_overview_1,
                                      infl_indiv_overview=exp_cdd_highest_dofv_1,
                                      outliers_overview=exp_max_outlier_table_1)
overview_list_2 <- get_overview_table(structural_overview=exp_str_overview_7,
                                      param_var_overview=exp_par_var_models_6,
                                      covariates_overview=exp_covariates_table_2,
                                      resmod_ruv_overview=exp_resmod_ruv_overview_4,
                                      infl_indiv_overview=exp_cdd_highest_dofv_1,
                                      outliers_overview=exp_max_outlier_table_1)
overview_list_3 <- get_overview_table(structural_overview=exp_str_overview_5,
                                      param_var_overview=exp_par_var_models_2,
                                      covariates_overview=exp_covariates_table_3,
                                      resmod_ruv_overview=exp_resmod_ruv_overview_2,
                                      infl_indiv_overview=exp_cdd_highest_dofv_3,
                                      outliers_overview=exp_max_outlier_table_3)
overview_list_4 <- get_overview_table(structural_overview=exp_str_overview_1,
                                      param_var_overview=exp_par_var_models_1,
                                      covariates_overview=exp_covariates_table_4,
                                      resmod_ruv_overview=exp_resmod_ruv_overview_2,
                                      infl_indiv_overview=exp_cdd_highest_dofv_4,
                                      outliers_overview=exp_max_outlier_table_6)
overview_list_5 <- get_overview_table(structural_overview=exp_str_overview_1,
                                      param_var_overview=exp_par_var_models_1,
                                      covariates_overview=exp_covariates_table_4,
                                      resmod_ruv_overview=exp_resmod_ruv_overview_2,
                                      infl_indiv_overview=cdd_highest_dofv_extra,
                                      outliers_overview=exp_max_outlier_table_6)
overview_list_6 <- get_overview_table(structural_overview=exp_str_overview_1,
                                      param_var_overview=exp_par_var_models_1,
                                      covariates_overview=exp_covariates_table_4,
                                      resmod_ruv_overview=exp_resmod_ruv_overview_2,
                                      infl_indiv_overview=cdd_highest_dofv_extra_1,
                                      outliers_overview=exp_max_outlier_table_6)
overview_list_7 <- get_overview_table(structural_overview=exp_str_overview_10,
                                      param_var_overview=exp_par_var_models_7,
                                      covariates_overview=exp_covariates_table_8,
                                      resmod_ruv_overview=exp_resmod_ruv_overview_5,
                                      infl_indiv_overview=exp_cdd_highest_dofv_8,
                                      outliers_overview=exp_max_outlier_table_9)
overview_list_8 <- get_overview_table(structural_overview=exp_str_overview_10,
                                      param_var_overview=exp_par_var_models_3,
                                      covariates_overview=exp_covariates_table_4,
                                      resmod_ruv_overview=exp_resmod_ruv_overview_5,
                                      infl_indiv_overview=exp_cdd_highest_dofv_8,
                                      outliers_overview=exp_max_outlier_table_11)
#expected data
exp_overview_table_1 <- data.frame(c("TIME","TAD","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                 "Interoccasion variability","FREM","SCM","RESMOD","CDD","SIMEVAL"),
                               c(rep("ERROR",12)),c(rep("",12)),
                               stringsAsFactors = F)
colnames(exp_overview_table_1) <- c("","dOFV","Additional parameters")
exp_rgroup_names <- c("Structural Model","Parameter Variability Model","Covariates",
                  "Residual Error Model","Influential Individuals","Outliers")
exp_overview_table_2 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                     "Interoccasion variability","FREM","SCM","IIV on RUV","autocorrelation","CDD","SIMEVAL"),
                               c(rep("NA",11),rep("ERROR",2)),c("2","2",rep("",11)),
                               stringsAsFactors = F)
colnames(exp_overview_table_2) <- c("","dOFV","Additional parameters")
exp_overview_table_3 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                     "Interoccasion variability","FREM","CLAPGR-4","tdist","dtbs","All dOFV values are negative","No dOFV values found"),
                               c("NA","4.8","ERROR","-0.6","NA","NA","NA","NA","2.9","43.6","21.7","",""),
                               c("2","2","","3","","","","","1","1","2","",""),
                               stringsAsFactors = F)
colnames(exp_overview_table_3) <- c("","dOFV","Additional parameters")
exp_overview_table_4 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                     "Interoccasion variability","FREM","CLAPGR-4","tdist","dtbs","Subject 34","Subject 34"),
                               c("4.3","4.8","-3.7","-0.6","3.4","-1.0","-2.0","3.6","2.9","43.6","21.7","2.0","2.0"),
                               c(2,2,4,"0","2","0",3,"6","1","1","2","",""),
                               stringsAsFactors = F)
colnames(exp_overview_table_4) <- c("","dOFV","Additional parameters")
exp_overview_table_5 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                     "Interoccasion variability","FREM","CLAPGR-4","tdist","dtbs","Subject 34","Subject 34"),
                               c("4.31","4.80","-3.73","-0.57","3.41","-1.03","-2.03","3.60","2.87","43.62","21.67","0.01","2.00"),
                               c(2,2,4,"0","2","0",3,"6","1","1","2","",""),
                               stringsAsFactors = F)
colnames(exp_overview_table_5) <- c("","dOFV","Additional parameters")
exp_overview_table_6 <- data.frame(c("TIME","PRED","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                     "Interoccasion variability","FREM","CLAPGR-4","tdist","dtbs","Subject 34","Subject 34"),
                               c("4.3","4.8","-3.7","-0.6","3.4","-1.0","-2.0","3.6","2.9","43.6","21.7","0.0","2.0"),
                               c(2,2,4,"0","2","0",3,"6","1","1","2","",""),
                               stringsAsFactors = F)
colnames(exp_overview_table_6) <- c("","dOFV","Additional parameters")
exp_overview_table_7 <- data.frame(c("RESMOD","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                     "Interoccasion variability","FREM","SCM","RESMOD","CDD","SIMEVAL"),
                               c(rep("SKIPPED",11)),c(rep("",11)),
                               stringsAsFactors = F)
colnames(exp_overview_table_7) <- c("","dOFV","Additional parameters")
exp_overview_table_8 <- data.frame(c("RESMOD","Full OMEGA Block","Box-Cox Transformation","Additional ETA","t-distribution",
                                     "Interoccasion variability","FREM","CLAPGR-4","RESMOD","CDD","No dOFV values found (skipped CDD)"),
                               c("SKIPPED","ERROR","NA","ERROR","NA","NA","3.6","2.9","SKIPPED","SKIPPED",""),
                               c(rep("",6),"6","1",rep("",3)),
                               stringsAsFactors = F)
colnames(exp_overview_table_8) <- c("","dOFV","Additional parameters")
#compare
context("qa, get_overview_table")
test_that("create an overview table",{
  expect_equal(overview_list_1$overview_table,exp_overview_table_1) # all Error
  expect_equal(overview_list_1$n.rgroup,c(2,5,2,1,1,1))
  expect_equal(overview_list_1$rgroup_names,exp_rgroup_names)
  expect_equal(overview_list_2$overview_table,exp_overview_table_2) # Error and NA
  expect_equal(overview_list_2$n.rgroup,c(2,5,2,2,1,1))
  expect_equal(overview_list_2$rgroup_names,exp_rgroup_names)
  expect_equal(overview_list_3$overview_table,exp_overview_table_3) # Error, NA, values, empty space
  expect_equal(overview_list_3$n.rgroup,c(2,5,2,2,1,1))
  expect_equal(overview_list_3$rgroup_names,exp_rgroup_names)
  expect_equal(overview_list_4$overview_table,exp_overview_table_4) # all values, rounded to one dec digit
  expect_equal(overview_list_4$n.rgroup,c(2,5,2,2,1,1))
  expect_equal(overview_list_4$rgroup_names,exp_rgroup_names)
  expect_equal(overview_list_5$overview_table,exp_overview_table_5) # all values, rounded to two dec digits
  expect_equal(overview_list_5$n.rgroup,c(2,5,2,2,1,1))
  expect_equal(overview_list_5$rgroup_names,exp_rgroup_names)
  expect_equal(overview_list_6$overview_table,exp_overview_table_6) # all values, rounded to one dec digit 
  expect_equal(overview_list_7$overview_table,exp_overview_table_7) # all Skipped
  expect_equal(overview_list_8$overview_table,exp_overview_table_8) # mix of all
})

#...........................  (22) Test function get_tables_for_vpc.R .....................................
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
exp_obs_1 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    DV=c(17.3,31,9.7,24.6,24.3,23.9,0,31.7,14.2,18.2,20.3),
                    MDV=c(rep(0,11)))
exp_sim_1 <- data.frame(ID=as.factor(rep(c(1,1,2,2,3,4,4,4,5,5,5),3)),
                    DV=c(25,34,10.9,28,27,20,21,34,22,29,40,20,27.8,25,32,36,19,26,39.1,21,19,34,16,30.1,17,30.8,26,18,19,28,21,24.1,28),
                    MDV=c(rep(0,33)),
                    CWRES=c(1.8,0.73,-0.18,0.42,-0.9,0.13,-1.2,1.07,0.27,1.1,2.4,0.84,-0.37,1.2,0.87,1,-0.85,0.047,1.8,0.96,-1.3,1.4,-0.503,0.45,-1.06,1.1,1.4,-0.045,-1.3,-0.16,0.82,0.2,-0.13),
                    IPRED=c(22,33,11,28,29,20.2,24,30.5,22,27,33,18,29,23,32,33,21,26,32,20,24,29,17,28,20.8,28,23,17,21,27,19,23,29),
                    TIME=rep(c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),3),
                    PRED=rep(c(17,28,10,18,31,20,24,30,19,23,28),3))
exp_obs_8 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    MDV=c(rep(0,11)),
                    DV=c(17.3,31,9.7,24.6,24.3,23.9,0,31.7,14.2,18.2,20.3))
exp_obs_11 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    DV=c(17.3,31,9.7,24.6,24.3,20.8,23.9,31.7,14.2,18.2,20.3))
exp_obs_12 <- data.frame(ID=factor(c(1,1,3,5,5,5),levels = c(1,2,3,4,5)),
                    TIME=c(2,112.5,134.3,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.5,-0.92,-0.702,-1),
                    PRED=c(17,28,31,19,23,28),
                    CIPREDI=c(17,28,28,14,18,23),
                    DV=c(17.3,31,24.3,14.2,18.2,20.3),
                    MDV=c(rep(0,6)))
exp_sim_12 <- data.frame(ID=as.factor(rep(c(1,1,3,5,5,5),3)),
                    DV=c(25,34,27,22,29,40,20,27.8,36,21,19,34,16,30.1,26,21,24.1,28),
                    MDV=c(rep(0,18)),
                    CWRES=c(1.8,0.73,-0.9,0.27,1.1,2.4,0.84,-0.37,1,0.96,-1.3,1.4,-0.503,0.45,1.4,0.82,0.2,-0.13),
                    IPRED=c(22,33,29,22,27,33,18,29,33,20,24,29,17,28,23,19,23,29),
                    TIME=rep(c(2,112.5,134.3,2,59.5,132),3),
                    PRED=rep(c(17,28,31,19,23,28),3))
exp_obs_6 <- data.frame(ID=as.factor(c(1,1,2,2,3,4,4,4,5,5,5)),
                    TIME=c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),
                    CWRES=c(-0.401,0.58,-1.27,1.6,-1.5,0.0206,-0.34,0.31,-0.92,-0.702,-1),
                    PRED=c(17,28,10,18,31,20,24,30,19,23,28),
                    CIPREDI=c(17,28,11,21,28,20,24,30,14,18,23),
                    ORIG=c(rep(2,11)),
                    DV=c(17.3,31,9.7,24.6,24.3,23.9,0,31.7,14.2,18.2,20.3),
                    MDV=c(rep(0,11)))
exp_sim_6 <- data.frame(ID=as.factor(rep(c(1,1,2,2,3,4,4,4,5,5,5),3)),
                    DV=c(25,34,10.9,28,27,20,21,34,22,29,40,20,27.8,25,32,36,19,26,39.1,21,19,34,16,30.1,17,30.8,26,18,19,28,21,24.1,28),
                    MDV=c(rep(0,33)),
                    CWRES=c(1.8,0.73,-0.18,0.42,-0.9,0.13,-1.2,1.07,0.27,1.1,2.4,0.84,-0.37,1.2,0.87,1,-0.85,0.047,1.8,0.96,-1.3,1.4,-0.503,0.45,-1.06,1.1,1.4,-0.045,-1.3,-0.16,0.82,0.2,-0.13),
                    IPRED=c(22,33,11,28,29,20.2,24,30.5,22,27,33,18,29,23,32,33,21,26,32,20,24,29,17,28,20.8,28,23,17,21,27,19,23,29),
                    ORIG=rep(2,33),
                    TIME=rep(c(2,112.5,2,63.5,134.3,1.8,59.3,130.8,2,59.5,132),3),
                    PRED=rep(c(17,28,10,18,31,20,24,30,19,23,28),3))
exp_obs_13 <- data.frame(ID=as.factor(c(1,1,3,5,5,5)),
                     TIME=c(2,112.5,134.3,2,59.5,132),
                     CWRES=c(-0.401,0.58,-1.5,-0.92,-0.702,-1),
                     PRED=c(17,28,31,19,23,28),
                     CIPREDI=c(17,28,28,14,18,23),
                     ORIG=rep(2,6),
                     DV=c(17.3,31,24.3,14.2,18.2,20.3),
                     MDV=c(rep(0,6)))
exp_sim_13 <- data.frame(ID=as.factor(rep(c(1,1,3,5,5,5),3)),
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
  expect_true(list_vpc_tables_1$make_vpc)
  expect_equal(list_vpc_tables_1$obs,exp_obs_1)
  expect_equal(list_vpc_tables_1$sim,exp_sim_1)
  expect_true(list_vpc_tables_2$make_vpc)
  expect_equal(list_vpc_tables_2$obs,exp_obs_1)
  expect_equal(list_vpc_tables_2$sim,exp_sim_1)
  expect_false(list_vpc_tables_3$make_vpc)
  expect_false(list_vpc_tables_4$make_vpc)
  expect_false(list_vpc_tables_5$make_vpc)
  expect_true(list_vpc_tables_6$make_vpc)
  expect_equal(list_vpc_tables_6$obs,exp_obs_6)
  expect_equal(list_vpc_tables_6$sim,exp_sim_6)
  expect_true(list_vpc_tables_7$make_vpc)
  expect_equal(list_vpc_tables_7$obs,exp_obs_1)
  expect_equal(list_vpc_tables_7$sim,exp_sim_1)
  expect_true(list_vpc_tables_8$make_vpc)
  expect_equal(list_vpc_tables_8$obs,exp_obs_8)
  expect_equal(list_vpc_tables_8$sim,exp_sim_1)
  expect_false(list_vpc_tables_9$make_vpc)
  expect_true(list_vpc_tables_10$make_vpc)
  expect_equal(list_vpc_tables_10$obs,exp_obs_8)
  expect_equal(list_vpc_tables_10$sim,exp_sim_1)
  expect_true(list_vpc_tables_11$make_vpc)
  expect_equal(list_vpc_tables_11$obs,exp_obs_11)
  expect_equal(list_vpc_tables_11$sim,exp_sim_1)
  expect_true(list_vpc_tables_12$make_vpc)
  expect_equal(list_vpc_tables_12$obs,exp_obs_12)
  expect_equal(list_vpc_tables_12$sim,exp_sim_12)
  expect_true(list_vpc_tables_13$make_vpc)
  expect_equal(list_vpc_tables_13$obs,exp_obs_13)
  expect_equal(list_vpc_tables_13$sim,exp_sim_13)
  expect_true(list_vpc_tables_14$make_vpc)
  expect_equal(list_vpc_tables_14$obs,exp_obs_6)
  expect_equal(list_vpc_tables_14$sim,exp_sim_6)
})