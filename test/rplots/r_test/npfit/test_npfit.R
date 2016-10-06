################################    1. Library for testing     ##############################
library(testthat)
tool = 'npfit'
#####################    2.Source functions which are going to be testet   ##################
# create a working directory for R scripts
source("../set.working.directory.R")
rscript.w.dir <- fun.rscript.w.dir()

directory_and_script <- find_r_files_in_subdir(toolname = tool,topdir = rscript.w.dir)

#source functions
for (i in 1:length(directory_and_script)) {
  source(directory_and_script[i])
}

###################################    3.Input data    ######################################
# create a working directory for R scripts
source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

raw.nonparametric.file <- paste0(files.w.dir,'nonparam_test.csv')
raw.nonparametric.file_1 <- paste0(files.w.dir,'nonparam_test_1.csv')
raw.nonparametric.file_2 <- paste0(files.w.dir,'nonparam_test_2.csv')

###################################     3. Make a test    ###################################
#...........................  (1) Test function data.npfit.R .....................................  
raw_nonparametric <- data.npfit(raw.nonparametric.file)
raw_nonparametric_1 <- data.npfit(raw.nonparametric.file_1)
raw_nonparametric_2 <- data.npfit(raw.nonparametric.file_2)

# Create expected input data
exp_raw_nonparametric <- data.frame('model'=c(1,2,3),'problem'=c(1,1,1),'subproblem'=c(1,1,1),
                                    'minimization_successful'=c(1,1,1),'ofv'=c(-57.06,-57.06,-57.06),
                                    'npofv'=c(-60.46,80.23,100.2),'npsupp'=c(100,200,300),
                                    'EXP(ETA1)'=c(0.056,0.035,0.034),'OMEGA(1,1)'=c(0.98,0.99,0.97))

exp_raw_nonparametric_1 <- data.frame('model'=c(1,3),'problem'=c(1,1),'subproblem'=c(1,1),
                                      'minimization_successful'=factor(c(1,1),levels=c(1,"run failed: NONMEM run failed")),'ofv'=c(-57.06,-57.06),
                                      'npofv'=c(-60.46,100.2),'npsupp'=c(100,300),
                                      'EXP(ETA1)'=c(0.056,0.034),'OMEGA(1,1)'=c(0.98,0.97))

exp_raw_nonparametric_2 <- data.frame('model'=integer(0),'problem'=integer(0),'subproblem'=integer(0),
                                      'minimization_successful'=factor(c(),levels=c("run failed: NONMEM run failed")),
                                      'ofv'=logical(0),'npofv'=logical(0),'npsupp'=integer(0),
                                      'EXP(ETA1)'=logical(0),'OMEGA(1,1)'=logical(0))
colnames(exp_raw_nonparametric_2) <- c('model','problem','subproblem','minimization_successful',
                                       'ofv','npofv','npsupp','EXP.ETA1.','OMEGA.1.1.')

# Compare expected input data with real input data
context("Test function data.npfit")
test_that("If function data.npfit works as expected",{
  expect_equal(exp_raw_nonparametric,raw_nonparametric)
  expect_equal(exp_raw_nonparametric_1,raw_nonparametric_1)
  expect_equal(exp_raw_nonparametric_2,raw_nonparametric_2)
})