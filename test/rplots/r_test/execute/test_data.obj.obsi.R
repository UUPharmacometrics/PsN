library(testthat)

tool = 'execute'

#####################    1.Source functions which are going to be testet   ##################
# create a working directory for R scripts
source("../set.working.directory.R")
rscript.w.dir <- fun.rscript.w.dir()

directory_and_script <- find_r_files_in_subdir(toolname = tool,topdir = rscript.w.dir)

#source functions
for (i in 1:length(directory_and_script)) {
  source(directory_and_script[i])
}

###################################    2.Input data    ######################################
source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

obj.data.dir <- paste0(files.w.dir,'run3.phi')
obj.data.dir_1 <- paste0(files.w.dir,'run3_1.phi')
obsi.data.dir <- paste0(files.w.dir,'cotab3')
obsi.data.dir_1 <- paste0(files.w.dir,'cotab3_1')
###################################     3. Make a test    ###################################
#...........................  (1) Test function data.obj.obsi.R .....................................  
out <- data.obj.obsi(obj.data.dir,obsi.data.dir)
out_1 <- data.obj.obsi(obj.data.dir_1,obsi.data.dir_1)

# unlist
OBJ_data_input <- out$OBJ_data_input
OBSi_data_input <- out$OBSi_data_input
OBJ_data <- out$OBJ_data
OBSi_data <- out$OBSi_data
OBJ_vector <- out$OBJ_vector
OBSi_vector <- out$OBSi_vector

OBJ_data_input_1 <- out_1$OBJ_data_input
OBSi_data_input_1 <- out_1$OBSi_data_input
OBJ_data_1 <- out_1$OBJ_data
OBSi_data_1 <- out_1$OBSi_data
OBJ_vector_1 <- out_1$OBJ_vector
OBSi_vector_1 <- out_1$OBSi_vector

# Create expected input data
exp_OBJ_data_input <- data.frame("SUBJECT_NO"=c(1,2,3),"ID"=c(1,2,3),
                                 "ETA(1)"=c(-6.53885E-02,-2.32906E-01,7.36665E-02),
                                 "ETA(2)"=c(6.45645E-03,-7.19004E-02,2.08840E-01),
                                 "OBJ"=c(7.538,13.835,11.229))
exp_OBSi_data_input <- data.frame("ID"=c(1.0000E+00,1.0000E+00,1.0000E+00,1.0000E+00,
                                         2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,
                                         3.0000E+00,3.0000E+00,3.0000E+00,3.0000E+00),
                                  "RES"=c(0.0000E+00,-9.7952E-01,0.0000E+00,2.4968E+00,
                                          0.0000E+00,-1.2563E+00,0.0000E+00,0.0000E+00,5.2902E+00,6.6742E+00,
                                          0.0000E+00,-3.9742E+00,0.0000E+00,-3.1333E+00))
exp_OBJ_data <- data.frame("SUBJECT_NO"=c(1,2,3),"ID"=c(1,2,3),
                           "ETA(1)"=c(-6.53885E-02,-2.32906E-01,7.36665E-02),
                           "ETA(2)"=c(6.45645E-03,-7.19004E-02,2.08840E-01),
                           "OBJ"=c(7.538,13.835,11.229))
exp_OBSi_data <- data.frame("ID"=c(1.0000E+00,1.0000E+00,1.0000E+00,1.0000E+00,
                                   2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,
                                   3.0000E+00,3.0000E+00,3.0000E+00,3.0000E+00),
                            "RES"=c(0.0000E+00,-9.7952E-01,0.0000E+00,2.4968E+00,
                                    0.0000E+00,-1.2563E+00,0.0000E+00,0.0000E+00,5.2902E+00,6.6742E+00,
                                    0.0000E+00,-3.9742E+00,0.0000E+00,-3.1333E+00))
exp_OBJ_vector <- c(7.538,13.835,11.229)
exp_OBSi_vector <- c(2,3,2)

exp_OBJ_data_input_1 <- data.frame("SUBJECT_NO"=c(1,2,3),"ID"=c(2,1,3),
                                 "ETA(1)"=c(-6.53885E-02,-2.32906E-01,7.36665E-02),
                                 "ETA(2)"=c(6.45645E-03,-7.19004E-02,2.08840E-01),
                                 "OBJ"=c(7.538,13.835,11.229))
exp_OBSi_data_input_1 <- data.frame("ID"=c(3.0000E+00,3.0000E+00,3.0000E+00,3.0000E+00,
                                         2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,
                                         1.0000E+00,1.0000E+00,1.0000E+00,1.0000E+00),
                                  "RES"=c(0.0000E+00,0.0000E+00,0.0000E+00,2.4968E+00,
                                          0.0000E+00,-1.2563E+00,0.0000E+00,0.0000E+00,5.2902E+00,6.6742E+00,
                                          0.0000E+00,-3.9742E+00,0.0000E+00,-3.1333E+00))
exp_OBJ_data_1 <- data.frame("SUBJECT_NO"=c(2,1,3),"ID"=c(1,2,3),
                           "ETA(1)"=c(-2.32906E-01,-6.53885E-02,7.36665E-02),
                           "ETA(2)"=c(-7.19004E-02,6.45645E-03,2.08840E-01),
                           "OBJ"=c(13.835,7.538,11.229))
exp_OBSi_data_1 <- data.frame("ID"=c(1.0000E+00,1.0000E+00,1.0000E+00,1.0000E+00,
                                   2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,2.0000E+00,
                                   3.0000E+00,3.0000E+00,3.0000E+00,3.0000E+00),
                            "RES"=c(0.0000E+00,-3.9742E+00,0.0000E+00,-3.1333E+00,
                                    0.0000E+00,-1.2563E+00,0.0000E+00,0.0000E+00,5.2902E+00,6.6742E+00,
                                    0.0000E+00,0.0000E+00,0.0000E+00,2.4968E+00))
exp_OBJ_vector_1<- c(13.835,7.538,11.229)
exp_OBSi_vector_1 <- c(2,3,1)

# Compare expected input data with real input data
context("Execute, function data.obj.obsi")
test_that("If function data.obj.obsi works as expected",{
  expect_equal(exp_OBJ_data_input,OBJ_data_input)
  expect_equal(exp_OBSi_data_input,OBSi_data_input)
  expect_equal(exp_OBJ_data,OBJ_data)
  expect_equal(exp_OBSi_data,OBSi_data)
  expect_equal(exp_OBJ_vector,OBJ_vector)
  expect_equal(exp_OBSi_vector,OBSi_vector)
  
  expect_equal(exp_OBJ_data_input_1,OBJ_data_input_1)
  expect_equal(exp_OBSi_data_input_1,OBSi_data_input_1)
  expect_equal(exp_OBJ_data_1,OBJ_data_1)
  expect_equal(exp_OBSi_data_1,OBSi_data_1)
  expect_equal(exp_OBJ_vector_1,OBJ_vector_1)
  expect_equal(exp_OBSi_vector_1,OBSi_vector_1)
})