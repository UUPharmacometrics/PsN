library(testthat)

tool = 'common'

#####################    1.Source functions which are going to be testet   ##################
# create a working directory for R scripts
source("../set.working.directory.R")
rscript.w.dir <- fun.rscript.w.dir()
files.w.dir <- fun.files.w.dir(toolname = tool)

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

#...........................  (2) Test function get_initial_estimates_from_ext.R .....................................
context("common, get_initial_estimates_from_ext")
test_that("subset initial estimates from ext file",{
  expect_equal(get_initial_estimates_from_ext(filename=file.path(files.w.dir,"file_1.ext")),
               data.frame("THETA1"=2.1,"THETA2"=as.numeric(NA),"THETA3"=-1.1,"THETA4"=-0.819,"SIGMA.1.1."=0.982,
                          "OMEGA.1.1."=0.01,"OMEGA.2.1."=0.22,"OMEGA.2.2."=0.160003,"OMEGA.4.1."=0,
                          stringsAsFactors = F)) # selecting all initial estimates
  expect_equal(get_initial_estimates_from_ext(filename=file.path(files.w.dir,"file_1.ext"),select="omega"),
               data.frame("OMEGA.1.1."=0.01,"OMEGA.2.1."=0.22,"OMEGA.2.2."=0.160003,"OMEGA.4.1."=0,
                          stringsAsFactors = F)) # selecting just omegas
  expect_equal(get_initial_estimates_from_ext(filename=file.path(files.w.dir,"file_1.ext"),select="theta"),
               data.frame("THETA1"=2.1,"THETA2"=as.numeric(NA),"THETA3"=-1.1,"THETA4"=-0.819,
                          stringsAsFactors = F)) # selecting just thetas
  expect_equal(get_initial_estimates_from_ext(filename=file.path(files.w.dir,"file_1.ext"),select="sigma"),
               data.frame("SIGMA.1.1."=0.982,stringsAsFactors = F)) # selecting just  sigmas
  expect_equal(get_initial_estimates_from_ext(filename=file.path(files.w.dir,"file_2.ext"),select="omega"),
               data.frame()) # selecting just omegas, no omegas in the file
  expect_equal(get_initial_estimates_from_ext(filename=file.path(files.w.dir,"file_1.ext"),select="omega", iteration=19),
               data.frame("OMEGA.1.1."=0.01,"OMEGA.2.1."=0.2,"OMEGA.2.2."=0.107,"OMEGA.4.1."=0,
                          stringsAsFactors = F)) # selecting just omegas, with different iteration
  expect_message(get_initial_estimates_from_ext(filename=file.path(files.w.dir,"file_1.ext"),select="wrong", iteration=19,do.stop=F)) # gives a message
})
