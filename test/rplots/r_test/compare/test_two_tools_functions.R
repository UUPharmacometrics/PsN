library(testthat)

tool = 'compare'

#####################    1.Source functions which are going to be testet   ##################
# create a working directory for R scripts
source("../set.working.directory.R")
rscript.w.dir <- fun.rscript.w.dir()

directory_and_script <- find_r_files_in_subdir(toolname = tool,topdir = rscript.w.dir)

#source functions
for (i in 1:length(directory_and_script)) {
  if(grepl("two_tools_functions.R$",directory_and_script[i])) {
    source(directory_and_script[i])
  }
}

###################################    2.Input data    ######################################
source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

###################################     3. Make a test    ###################################
#...........................  (1) Test if message shows up .....................................  
context("Compare, test if messages shows up")
test_that("messages shows up",{
  expect_message(check_tool_names("first_tool","first_tool",quit_opt=FALSE))
  expect_message(folder_existence(files.w.dir,"sec_tool",quit_opt=FALSE))
  expect_message(folder_existence(files.w.dir,"cdd_dir1",quit_opt=FALSE))
  expect_message(get_model_name(files.w.dir,"other_tool",quit_opt=FALSE))
  expect_message(get_toolname_foldername(files.w.dir,"other_tool",quit_opt=FALSE))
  expect_message(get_raw_results_file_name(files.w.dir,"cdd_dir2","first_tool",quit_opt=FALSE))
  expect_message(get_raw_results_file_name(files.w.dir,"cdd_dir3","simeval_dir1",quit_opt=FALSE))
  expect_message(get_more_csv_file_names(files.w.dir,c("cdd","cdd_dir2"),c("simeval","first_tool"),quit_opt=FALSE))
  expect_message(get_more_csv_file_names(files.w.dir,c("cdd","cdd_dir3"),c("simeval","simeval_dir1"),quit_opt=FALSE))
  expect_message(get_more_csv_file_names(files.w.dir,c("cdd","cdd_dir3"),c("sir","cir_dir1"),quit_opt=FALSE))
})

#...............................  (2) Test silent  .....................................
context("Compare, test silent")
test_that("silent, no returns, no messages",{
  expect_silent(check_tool_names("first_tool","cdd_dir3"))
  expect_silent(folder_existence(files.w.dir,"cdd_dir3"))
  expect_silent(folder_existence(files.w.dir,"first_tool"))
})

#...............................  (3) Test get_model_name  .....................................
model_name_1 <- get_model_name(files.w.dir,"cdd_dir3")
model_name_2 <- get_model_name(files.w.dir,"simeval_dir1")

context("Compare, test function get_model_name")
test_that("function get_model_name pass",{
  expect_equal("pheno.mod",model_name_1)
  expect_equal("run1.mod",model_name_2)
})

#...............................  (4) Test get_toolname_foldername  .....................................
toolname_foldername_1 <- get_toolname_foldername(files.w.dir,"cdd_dir3")
toolname_foldername_2 <- get_toolname_foldername(files.w.dir,"simeval_dir1")

context("Compare, test function get_toolname_foldername")
test_that("function get_toolname_foldername pass",{
  expect_equal(c("cdd","cdd_dir3"),toolname_foldername_1)
  expect_equal(c("simeval","simeval_dir1"),toolname_foldername_2)
})

#...............................  (5) Test create_folder_name  .....................................
new_folder_name_1 <- create_folder_name(folders.directory=files.w.dir,toolname_foldername_1=c("cdd","cdd_dir3"),toolname_foldername_2=c("simeval","first_tool"))
new_folder_name_2 <- create_folder_name(folders.directory=files.w.dir,toolname_foldername_1=c("simeval","simeval_dir1"),toolname_foldername_2=c("cdd","cdd_dir3"))

context("Compare, test function create_folder_name")
test_that("function create_folder_name pass",{
  expect_equal("cdd.simeval_dir4",new_folder_name_1)
  expect_equal("simeval.cdd_dir1",new_folder_name_2)
})

#...............................  (6) Test create_folder_directory  .....................................
new_folder_directory_1 <- create_folder_directory(files.w.dir,"cdd.simeval_dir3",create_dir=FALSE)
new_folder_directory_2 <- create_folder_directory(files.w.dir,"simeval.cdd_dir1",create_dir=FALSE)

context("Compare, test function create_folder_directory")
test_that("function create_folder_directory pass",{
  expect_equal(paste0(files.w.dir,"cdd.simeval_dir3"),new_folder_directory_1)
  expect_equal(paste0(files.w.dir,"simeval.cdd_dir1"),new_folder_directory_2)
})

#...............................  (7) Test get_raw_results_file_name  .....................................
raw_results_1 <- get_raw_results_file_name(files.w.dir,"cdd_dir3","first_tool")
raw_results_2 <- get_raw_results_file_name(files.w.dir,"first_tool","cdd_dir3")

context("Compare, test function get_raw_results_file_name")
test_that("function get_raw_results_file_name pass",{
  expect_equal("raw_results_pheno.csv",raw_results_1)
  expect_equal("raw_results_pheno.csv",raw_results_2)
})

#...............................  (8) Test get_more_csv_file_names  .....................................
other_files_1 <- get_more_csv_file_names(files.w.dir,c("cdd","cdd_dir3"),c("simeval","first_tool"))
other_files_2 <- get_more_csv_file_names(files.w.dir,c("simeval","first_tool"),c("cdd","cdd_dir3"))

exp_other_files_1 <- list(skipped_individuals="skipped_individuals14f.csv",
                          all.iofv.file="raw_all_iofv.csv",
                          residual.outliers.file="residual_outliers.csv",
                          ebe.npde.file="ebe_npde.csv")

context("Compare, test function get_more_csv_file_names")
test_that("function get_more_csv_file_names pass",{
  expect_equal(exp_other_files_1,other_files_1)
  expect_equal(exp_other_files_1,other_files_2)
})

#...............................  (9) Test get_list_of_files  .....................................
list_of_files_1 <- get_list_of_files(files.w.dir,c("cdd","cdd_dir3"),c("simeval","first_tool"),
                                     raw_results_1,other_files_1)
list_of_files_2 <- get_list_of_files(files.w.dir,c("simeval","first_tool"),c("cdd","cdd_dir3"),
                                     raw_results_1,other_files_1)

exp_list_of_files_1 <- c(paste0(files.w.dir,"first_tool/raw_all_iofv.csv"),
                         paste0(files.w.dir,"cdd_dir3/skipped_individuals14f.csv"),
                         paste0(files.w.dir,"cdd_dir3/raw_results_pheno.csv"),
                         paste0(files.w.dir,"first_tool/ebe_npde.csv"),
                         paste0(files.w.dir,"first_tool/residual_outliers.csv"))

context("Compare, test function get_list_of_files")
test_that("function get_list_of_files pass",{
  expect_equal(exp_list_of_files_1,list_of_files_1)
  expect_equal(exp_list_of_files_1,list_of_files_2)
})

#...............................  (10) Test get_input_values  .....................................
input_values_1 <- get_input_values(list_of_files_1,c("cdd","cdd_dir3"),c("simeval","first_tool"))
input_values_2 <- get_input_values(list_of_files_1,c("simeval","first_tool"),c("cdd","cdd_dir3"))

context("Compare, test function get_input_values")
test_that("function get_input_values pass",{
  expect_equal(list(successful.samples=4,n.subjects=5,eta.names=c("ETA.1.","ETA.2.","ETA.3.")),input_values_1)
  expect_equal(list(successful.samples=4,n.subjects=5,eta.names=c("ETA.1.","ETA.2.","ETA.3.")),input_values_2)
})

#...............................  (11) Test create_pdf.filename  .....................................
pdf.filename_1 <- create_pdf.filename(c("cdd","cdd_dir3"),c("simeval","first_tool"))
pdf.filename_2 <- create_pdf.filename(c("simeval","first_tool"),c("cdd","cdd_dir3"))

context("Compare, test function create_pdf.filename")
test_that("function create_pdf.filename pass",{
  expect_equal("cdd.simeval.pdf",pdf.filename_1)
  expect_equal("simeval.cdd.pdf",pdf.filename_2)
})

#...............................  (12) Test create_R_script  .....................................
R_input_1 <- create_R_script(rscript.w.dir,new_folder_directory_1,
                             c("cdd","cdd_dir3"),c("simeval","first_tool"),
                             raw_results_1,other_files_1,input_values_1,pdf.filename_1)
R_input_2 <- create_R_script(rscript.w.dir,new_folder_directory_2,
                             c("simeval","first_tool"),c("cdd","cdd_dir3"),
                             raw_results_2,other_files_2,input_values_2,pdf.filename_2)

exp_R_input_1 <- c(paste0("setwd('",new_folder_directory_1,"')"),
                   paste0("rscripts.directory <- '",rscript.w.dir,"'"),
                   "input_folder_1 <- 'cdd_dir3'","input_folder_2 <- 'first_tool'",
                   "tool_1 <- 'cdd'","tool_2 <- 'simeval'","pdf.filename <- 'cdd.simeval.pdf'",
                   "successful.samples <- 4","n.subjects <- 5",
                   "all.iofv.file <- 'raw_all_iofv.csv'","skipped.id.file <- 'skipped_individuals14f.csv'",
                   "raw.results.file <- 'raw_results_pheno.csv'","residual.outliers.file <- 'residual_outliers.csv'",
                   "ebe.npde.file <- 'ebe_npde.csv'","eta.names <- c('ETA.1.','ETA.2.','ETA.3.')","",
                   "source(paste0(rscripts.directory,'cdd.simeval_default.R'))",
                   "cdd.simeval(rscripts.directory,all.iofv.file,n.subjects,samples=successful.samples,\n              raw.results.file,skipped.id.file,residual.outliers.file,ebe.npde.file,eta.names,\n              pdf.filename,cutoff_cook=0.8,cutoff_delta.ofv=3.84)")

exp_R_input_2 <- c(paste0("setwd('",new_folder_directory_2,"')"),
                   paste0("rscripts.directory <- '",rscript.w.dir,"'"),
                   "input_folder_1 <- 'first_tool'","input_folder_2 <- 'cdd_dir3'",
                   "tool_1 <- 'simeval'","tool_2 <- 'cdd'","pdf.filename <- 'simeval.cdd.pdf'",
                   "successful.samples <- 4","n.subjects <- 5",
                   "all.iofv.file <- 'raw_all_iofv.csv'","skipped.id.file <- 'skipped_individuals14f.csv'",
                   "raw.results.file <- 'raw_results_pheno.csv'","residual.outliers.file <- 'residual_outliers.csv'",
                   "ebe.npde.file <- 'ebe_npde.csv'","eta.names <- c('ETA.1.','ETA.2.','ETA.3.')","",
                   "source(paste0(rscripts.directory,'cdd.simeval_default.R'))",
                   "cdd.simeval(rscripts.directory,all.iofv.file,n.subjects,samples=successful.samples,\n              raw.results.file,skipped.id.file,residual.outliers.file,ebe.npde.file,eta.names,\n              pdf.filename,cutoff_cook=0.8,cutoff_delta.ofv=3.84)")
context("Compare, test function create_R_script")
test_that("function create_R_script pass",{
  expect_equal(exp_R_input_1,R_input_1)
  expect_equal(exp_R_input_2,R_input_2)
})
