library(testthat)

tool = 'common'

#####################    1.Source functions which are going to be testet   ##################
# create a working directory for R scripts
source("../set.working.directory.R")
rscript.w.dir <- fun.rscript.w.dir()

directory_and_script <- find_r_files_in_subdir(toolname = tool,topdir = rscript.w.dir)

#source functions
for (i in 1:length(directory_and_script)) {
  source(directory_and_script[i])
}

###################################     3. Make a test    ###################################
#...........................  (1) Test function fix_column_names.R .....................................  
col_names <- c("model","col_one","_col_sec",".col.third","1TVCL","3 RV","4 CRCL on CL","OMEGA(1,1)","se1 TVCL","#shrinkage_eta1(%)")
new_names <- fix_column_names(col_names)

# Compare
context("Common, function fix_column_names")
test_that("If function fix_column_names works as expected",{
  expect_equal(c("model","col_one","X_col_sec",".col.third","X1TVCL","X3.RV","X4.CRCL.on.CL","OMEGA.1.1.","se1.TVCL","X.shrinkage_eta1..."),
               new_names)
})
