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

# all.iofv.file <- paste0(files.w.dir,'raw_all_iofv.csv')
# all.iofv.file_2 <- paste0(files.w.dir,'raw_all_iofv_2.csv')
# residual.outliers.file <- paste0(files.w.dir,'residual_outliers.csv')
# residual.outliers.file.1 <- paste0(files.w.dir,'residual_outliers_1.csv')
# residual.outliers.file.2 <- paste0(files.w.dir,'residual_outliers_2.csv')
# ebe.npde.file <- paste0(files.w.dir,'ebe_npde.csv')
###################################     3. Make a test    ###################################
#...........................  (1) Test if message shows up .....................................  
context("Test if messages shows up")
test_that("messages shows up",{
  expect_message(check_tool_names("first_tool","first_tool",quit_opt=FALSE))
  expect_message(folder_existence(files.w.dir,"sec_tool",quit_opt=FALSE))
  expect_message(folder_existence(files.w.dir,"cdd_dir1",quit_opt=FALSE))
})

#...............................  (2) Test silent  .....................................
context("Test silent")
test_that("silent",{
  expect_silent(check_tool_names("first_tool","cdd_dir3"))
  expect_silent(folder_existence(files.w.dir,"cdd_dir3"))
  expect_silent(folder_existence(files.w.dir,"first_tool"))
})

#...............................  (3) Test get_model_name  .....................................
model_name <- get_model_name(files.w.dir,"cdd_dir3")
model_name_1 <- get_model_name(files.w.dir,"simeval_dir1")

context("Test function get_model_name")
test_that("get_model_name",{
  expect_equal("pheno.mod",model_name)
  expect_equal("run1.mod",model_name_1)
})


