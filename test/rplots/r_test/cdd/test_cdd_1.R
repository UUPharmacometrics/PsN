################################    1. Library for testing     ##############################
library(testthat)
tool = 'cdd'
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

raw.results.file <- paste0(files.w.dir,'raw_results.csv')
skipped.id.file <- paste0(files.w.dir,'skipped.csv')
raw.results.file_1 <- paste0(files.w.dir,'raw_results_1.csv')
skipped.id.file_1 <- paste0(files.w.dir,'skipped_1.csv')

## read files
input_cdd.data <- read.csv(raw.results.file)
input_cdd.data_1 <- read.csv(raw.results.file_1)
cdd.inds <- read.csv(skipped.id.file, header=F)
cdd.inds_1 <- read.csv(skipped.id.file_1, header=F)

###################################     4. Make tests     ###################################
# 4.1. Test if input data tables are correct
# Create correct input data tables
# correct_cdd.data with column method
correct_cdd.data <- data.frame(method=factor(c(rep("cdd",5),"other"),levels=c("cdd","other")),model=as.integer(c(0,1,2,3,4,5)),
                               minimization_successful=as.integer(c(1,1,1,1,1,1)),covariance_step_successful=as.integer(c(1,1,1,0,1,1)),
                               covariance_step_warnings=as.integer(c(0,0,1,0,0,0)),estimate_near_boundary=as.integer(c(1,0,0,0,1,0)),
                               EI1=c(NA,NA,NA,NA,NA,NA),cook.scores=c(0,1.69474,0.65269,1.34665,2.21297,1.66382),
                               cov.ratios=c(0,1.06352,0,0,1.43112,2.32917),outside.n.sd=as.integer(c(0,1,0,0,0,0)))
correct_cdd.inds <- data.frame(V1=as.integer(c(1,2,3,4)))
# correct_cdd.data_1 without column method
correct_cdd.data_1 <- data.frame(model=as.integer(c(0,1,2)),minimization_successful=as.integer(c(1,1,1)),
                               covariance_step_successful=as.integer(c(1,1,1)),covariance_step_warnings=as.integer(c(0,0,0)),
                               estimate_near_boundary=as.integer(c(0,0,0)),EI1=c(NA,NA,NA),
                               cook.scores=c(0,1.69474,0.65269),
                               cov.ratios=c(0,1.06352,0),outside.n.sd=as.integer(c(0,0,0)))
correct_cdd.inds_1 <- data.frame(V1=as.integer(c(1,2)))
# Test input data files input_cdd.data and cdd.inds
context("CDD, function input data")
test_that("If input data files are correct",{
  expect_identical(correct_cdd.data,input_cdd.data)
  expect_identical(correct_cdd.inds,cdd.inds)
  expect_identical(correct_cdd.data_1,input_cdd.data_1)
  expect_identical(correct_cdd.inds_1,cdd.inds_1)
})

# 4.2.Testing function create.data
cdd.data <- create.data(input_cdd.data,cdd.inds)
cdd.data_1 <- create.data(input_cdd.data_1,cdd.inds_1)
# Create correct cdd.data table
correct_cdd.data <- data.frame(method=factor(rep("cdd",4),levels=c("cdd","other")),model=as.integer(c(1,2,3,4)),
                               minimization_successful=as.integer(c(1,1,1,1)),covariance_step_successful=as.integer(c(1,1,0,1)),
                               covariance_step_warnings=as.integer(c(0,1,0,0)),estimate_near_boundary=as.integer(c(0,0,0,1)),
                               EI1=c(NA,NA,NA,NA),cook.scores=c(1.69474,0.65269,1.34665,2.21297),cov.ratios=c(1.06352,0,0,1.43112),
                               outside.n.sd=as.integer(c(1,0,0,0)),ID=as.integer(c(1,2,3,4)))
correct_cdd.data_1 <- data.frame(model=as.integer(c(1,2)),minimization_successful=as.integer(c(1,1)),
                                 covariance_step_successful=as.integer(c(1,1)),covariance_step_warnings=as.integer(c(0,0)),
                                 estimate_near_boundary=as.integer(c(0,0)),EI1=c(NA,NA),
                                 cook.scores=c(1.69474,0.65269),cov.ratios=c(1.06352,0),
                                 outside.n.sd=as.integer(c(0,0)),ID=as.integer(c(1,2)))
# Test cdd.data_outtable function
context("CDD, function create.data")
test_that("Testing function create.data",{
  expect_identical(correct_cdd.data,cdd.data)
  expect_identical(correct_cdd.data_1,cdd.data_1)
})

# 4.3.Testing function failed.values
fail <- failed.values(cdd.data)
# Correct failed values
correct_fail <- as.integer(c(0,1,1,1))
# Test failed.values function
context("CDD, function failed.values")
test_that("Testing function failed.values",{
  expect_identical(correct_fail,fail)
})

# 4.4.Testing function warnings.data
p1 <- warnings.data(cdd.data)
p2 <- warnings.data(cdd.data,min.failed=TRUE,cov.failed=TRUE,cov.warnings=TRUE,boundary=TRUE)
p3 <- warnings.data(cdd.data,min.failed=TRUE,cov.failed=TRUE,cov.warnings=TRUE)
p4 <- warnings.data(cdd.data,cov.failed=TRUE,boundary=TRUE)
p5 <- warnings.data(cdd.data,min.failed=TRUE)
p6 <- warnings.data(cdd.data,cov.failed=TRUE)
p7 <- warnings.data(cdd.data,cov.warnings=TRUE)
p8 <- warnings.data(cdd.data,boundary=TRUE)
# Create correct p1 tables (and cdd.warn tables, if they exist)
correct_p1 <- data.frame(method=factor(rep("cdd",4),levels=c("cdd","other")),model=as.integer(c(1,2,3,4)),
                         minimization_successful=as.integer(c(1,1,1,1)),covariance_step_successful=as.integer(c(1,1,0,1)),
                         covariance_step_warnings=as.integer(c(0,1,0,0)),estimate_near_boundary=as.integer(c(0,0,0,1)),
                         EI1=as.logical(c(NA,NA,NA,NA)),cook.scores=as.numeric(c(1.69474,0.65269,1.34665,2.21297)),cov.ratios=as.numeric(c(1.06352,0,0,1.43112)),
                         outside.n.sd=as.integer(c(1,0,0,0)),ID=as.integer(c(1,2,3,4)))
correct_p2 <- list(cdd.warn = data.frame(method=factor(rep("cdd",3),levels=c("cdd","other")),model=as.integer(c(3,2,4)),
                                         minimization_successful=as.integer(c(1,1,1)),covariance_step_successful=as.integer(c(0,1,1)),
                                         covariance_step_warnings=as.integer(c(0,1,0)),estimate_near_boundary=as.integer(c(0,0,1)),
                                         EI1=as.logical(c(NA,NA,NA)),cook.scores=as.numeric(c(1.34665,0.65269,2.21297)),cov.ratios=as.numeric(c(0,0,1.43112)),
                                         outside.n.sd=as.integer(c(0,0,0)),ID=as.integer(c(3,2,4))),
                   p1 = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(1)),
                                    minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                    covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(0)),
                                    EI1=as.logical(c(NA)),cook.scores=as.numeric(c(1.69474)),cov.ratios=as.numeric(c(1.06352)),
                                    outside.n.sd=as.integer(c(1)),ID=as.integer(c(1))))
correct_p3 <- list(cdd.warn = data.frame(method=factor(rep("cdd",2),levels=c("cdd","other")),model=as.integer(c(3,2)),
                                         minimization_successful=as.integer(c(1,1)),covariance_step_successful=as.integer(c(0,1)),
                                         covariance_step_warnings=as.integer(c(0,1)),estimate_near_boundary=as.integer(c(0,0)),
                                         EI1=as.logical(c(NA,NA)),cook.scores=as.numeric(c(1.34665,0.65269)),cov.ratios=as.numeric(c(0,0)),
                                         outside.n.sd=as.integer(c(0,0)),ID=as.integer(c(3,2))),
                   p1 = data.frame(method=factor(rep("cdd",2),levels=c("cdd","other")),model=as.integer(c(1,4)),
                                   minimization_successful=as.integer(c(1,1)),covariance_step_successful=as.integer(c(1,1)),
                                   covariance_step_warnings=as.integer(c(0,0)),estimate_near_boundary=as.integer(c(0,1)),
                                   EI1=as.logical(c(NA,NA)),cook.scores=as.numeric(c(1.69474,2.21297)),cov.ratios=as.numeric(c(1.06352,1.43112)),
                                   outside.n.sd=as.integer(c(1,0)),ID=as.integer(c(1,4))))
correct_p4 <- list(cdd.warn = data.frame(method=factor(rep("cdd",2),levels=c("cdd","other")),model=as.integer(c(3,4)),
                                         minimization_successful=as.integer(c(1,1)),covariance_step_successful=as.integer(c(0,1)),
                                         covariance_step_warnings=as.integer(c(0,0)),estimate_near_boundary=as.integer(c(0,1)),
                                         EI1=as.logical(c(NA,NA)),cook.scores=as.numeric(c(1.34665,2.21297)),cov.ratios=as.numeric(c(0,1.43112)),
                                         outside.n.sd=as.integer(c(0,0)),ID=as.integer(c(3,4))),
                   p1 = data.frame(method=factor(rep("cdd",2),levels=c("cdd","other")),model=as.integer(c(1,2)),
                                   minimization_successful=as.integer(c(1,1)),covariance_step_successful=as.integer(c(1,1)),
                                   covariance_step_warnings=as.integer(c(0,1)),estimate_near_boundary=as.integer(c(0,0)),
                                   EI1=as.logical(c(NA,NA)),cook.scores=as.numeric(c(1.69474,0.65269)),cov.ratios=as.numeric(c(1.06352,0)),
                                   outside.n.sd=as.integer(c(1,0)),ID=as.integer(c(1,2))))
correct_p5 <- data.frame(method=factor(rep("cdd",4),levels=c("cdd","other")),model=as.integer(c(1,2,3,4)),
                         minimization_successful=as.integer(c(1,1,1,1)),covariance_step_successful=as.integer(c(1,1,0,1)),
                         covariance_step_warnings=as.integer(c(0,1,0,0)),estimate_near_boundary=as.integer(c(0,0,0,1)),
                         EI1=as.logical(c(NA,NA,NA,NA)),cook.scores=as.numeric(c(1.69474,0.65269,1.34665,2.21297)),cov.ratios=as.numeric(c(1.06352,0,0,1.43112)),
                         outside.n.sd=as.integer(c(1,0,0,0)),ID=as.integer(c(1,2,3,4)))
correct_p6 <- list(cdd.warn = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(3)),
                                         minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(0)),
                                         covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(0)),
                                         EI1=as.logical(c(NA)),cook.scores=as.numeric(c(1.34665)),cov.ratios=as.numeric(c(0)),
                                         outside.n.sd=as.integer(c(0)),ID=as.integer(c(3))),
                   p1 = data.frame(method=factor(rep("cdd",3),levels=c("cdd","other")),model=as.integer(c(1,2,4)),
                                   minimization_successful=as.integer(c(1,1,1)),covariance_step_successful=as.integer(c(1,1,1)),
                                   covariance_step_warnings=as.integer(c(0,1,0)),estimate_near_boundary=as.integer(c(0,0,1)),
                                   EI1=c(NA,NA,NA),cook.scores=c(1.69474,0.65269,2.21297),cov.ratios=c(1.06352,0,1.43112),
                                   outside.n.sd=as.integer(c(1,0,0)),ID=as.integer(c(1,2,4))))
correct_p7 <- list(cdd.warn = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(2)),
                                         minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                         covariance_step_warnings=as.integer(c(1)),estimate_near_boundary=as.integer(c(0)),
                                         EI1=as.logical(c(NA)),cook.scores=as.numeric(c(0.65269)),cov.ratios=as.numeric(c(0)),
                                         outside.n.sd=as.integer(c(0)),ID=as.integer(c(2))),
                   p1 = data.frame(method=factor(rep("cdd",3),levels=c("cdd","other")),model=as.integer(c(1,3,4)),
                                   minimization_successful=as.integer(c(1,1,1)),covariance_step_successful=as.integer(c(1,0,1)),
                                   covariance_step_warnings=as.integer(c(0,0,0)),estimate_near_boundary=as.integer(c(0,0,1)),
                                   EI1=as.logical(c(NA,NA,NA)),cook.scores=as.numeric(c(1.69474,1.34665,2.21297)),cov.ratios=as.numeric(c(1.06352,0,1.43112)),
                                   outside.n.sd=as.integer(c(1,0,0)),ID=as.integer(c(1,3,4))))
correct_p8 <- list(cdd.warn = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(4)),
                                         minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                         covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(1)),
                                         EI1=as.logical(c(NA)),cook.scores=as.numeric(c(2.21297)),cov.ratios=as.numeric(c(1.43112)),
                                         outside.n.sd=as.integer(c(0)),ID=as.integer(c(4))),
                   p1 = data.frame(method=factor(rep("cdd",3),levels=c("cdd","other")),model=as.integer(c(1,2,3)),
                                   minimization_successful=as.integer(c(1,1,1)),covariance_step_successful=as.integer(c(1,1,0)),
                                   covariance_step_warnings=as.integer(c(0,1,0)),estimate_near_boundary=as.integer(c(0,0,0)),
                                   EI1=as.logical(c(NA,NA,NA)),cook.scores=as.numeric(c(1.69474,0.65269,1.34665)),cov.ratios=as.numeric(c(1.06352,0,0)),
                                   outside.n.sd=as.integer(c(1,0,0)),ID=as.integer(c(1,2,3))))
# Test warnings.data function
context("CDD, function warnings.data")
test_that("Testing function warnings.data",{
  expect_identical(correct_p1,p1)
  expect_identical(correct_p2,p2)
  expect_identical(correct_p3,p3)
  expect_identical(correct_p4,p4)
  expect_identical(correct_p5,p5)
  expect_identical(correct_p6,p6)
  expect_identical(correct_p7,p7)
  expect_identical(correct_p8,p8)
})
# 4.5. Unlist p1 from different test lists (we need them to test mark.options.data function)
p2 <- p2$p1
p3 <- p3$p1
p4 <- p4$p1
p6 <- p6$p1
p7 <- p7$p1
p8 <- p8$p1
# 4.6.Testing function mark.options.data
# markeropt == 1
out_1 <- mark.options.data(p1,1)
out_1a <- mark.options.data(p2,1)
out_1b <- mark.options.data(p3,1)
out_1c <- mark.options.data(p4,1)
# markeropt == 2
out_2 <- mark.options.data(p1,2)
out_2a <- mark.options.data(p2,2)
out_2b <- mark.options.data(p3,2)
out_2c <- mark.options.data(p4,2)
# markeropt == 3
out_3 <- mark.options.data(p1,3)
out_3a <- mark.options.data(p2,3)
out_3b <- mark.options.data(p3,3)
out_3c <- mark.options.data(p4,3)
# Create correct cdd.pt and cdd.txt tables
# markeropt == 1
correct_out_1 <- list(cdd.pt = data.frame(method=factor(rep("cdd",4),levels=c("cdd","other")),model=as.integer(c(1,2,3,4)),
                                          minimization_successful=as.integer(c(1,1,1,1)),covariance_step_successful=as.integer(c(1,1,0,1)),
                                          covariance_step_warnings=as.integer(c(0,1,0,0)),estimate_near_boundary=as.integer(c(0,0,0,1)),
                                          EI1=as.logical(c(NA,NA,NA,NA)),cook.scores=as.numeric(c(1.69474,0.65269,1.34665,2.21297)),cov.ratios=as.numeric(c(1.06352,0,0,1.43112)),
                                          outside.n.sd=as.integer(c(1,0,0,0)),ID=as.integer(c(1,2,3,4))),
                      cdd.txt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                          minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                          covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                          EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                          outside.n.sd=as.integer(c()),ID=as.integer(c())))
correct_out_1a <- list(cdd.pt = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(1)),
                                           minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                           covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(0)),
                                           EI1=as.logical(c(NA)),cook.scores=as.numeric(c(1.69474)),cov.ratios=as.numeric(c(1.06352)),
                                           outside.n.sd=as.integer(c(1)),ID=as.integer(c(1))),
                      cdd.txt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                           minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                           covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                           EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                           outside.n.sd=as.integer(c()),ID=as.integer(c())))
correct_out_1b <- list(cdd.pt = data.frame(method=factor(rep("cdd",2),levels=c("cdd","other")),model=as.integer(c(1,4)),
                                           minimization_successful=as.integer(c(1,1)),covariance_step_successful=as.integer(c(1,1)),
                                           covariance_step_warnings=as.integer(c(0,0)),estimate_near_boundary=as.integer(c(0,1)),
                                           EI1=as.logical(c(NA,NA)),cook.scores=as.numeric(c(1.69474,2.21297)),cov.ratios=as.numeric(c(1.06352,1.43112)),
                                           outside.n.sd=as.integer(c(1,0)),ID=as.integer(c(1,4))),
                       cdd.txt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                            minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                            covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                            EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                            outside.n.sd=as.integer(c()),ID=as.integer(c())))
correct_out_1c <- list(cdd.pt = data.frame(method=factor(rep("cdd",2),levels=c("cdd","other")),model=as.integer(c(1,2)),
                                           minimization_successful=as.integer(c(1,1)),covariance_step_successful=as.integer(c(1,1)),
                                           covariance_step_warnings=as.integer(c(0,1)),estimate_near_boundary=as.integer(c(0,0)),
                                           EI1=as.logical(c(NA,NA)),cook.scores=as.numeric(c(1.69474,0.65269)),cov.ratios=as.numeric(c(1.06352,0)),
                                           outside.n.sd=as.integer(c(1,0)),ID=as.integer(c(1,2))),
                       cdd.txt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                            minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                            covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                            EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                            outside.n.sd=as.integer(c()),ID=as.integer(c())))
# markeropt == 2
correct_out_2 <- list(cdd.pt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                          minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                          covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                          EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                          outside.n.sd=as.integer(c()),ID=as.integer(c())),
                      cdd.txt = data.frame(method=factor(rep("cdd",4),levels=c("cdd","other")),model=as.integer(c(1,2,3,4)),
                                           minimization_successful=as.integer(c(1,1,1,1)),covariance_step_successful=as.integer(c(1,1,0,1)),
                                           covariance_step_warnings=as.integer(c(0,1,0,0)),estimate_near_boundary=as.integer(c(0,0,0,1)),
                                           EI1=as.logical(c(NA,NA,NA,NA)),cook.scores=as.numeric(c(1.69474,0.65269,1.34665,2.21297)),cov.ratios=as.numeric(c(1.06352,0,0,1.43112)),
                                           outside.n.sd=as.integer(c(1,0,0,0)),ID=as.integer(c(1,2,3,4))))
correct_out_2a <- list(cdd.pt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                           minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                           covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                           EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                           outside.n.sd=as.integer(c()),ID=as.integer(c())),
                       cdd.txt = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(1)),
                                            minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                            covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(0)),
                                            EI1=as.logical(c(NA)),cook.scores=as.numeric(c(1.69474)),cov.ratios=as.numeric(c(1.06352)),
                                            outside.n.sd=as.integer(c(1)),ID=as.integer(c(1))))
correct_out_2b <- list(cdd.pt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                           minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                           covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                           EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                           outside.n.sd=as.integer(c()),ID=as.integer(c())),
                       cdd.txt = data.frame(method=factor(rep("cdd",2),levels=c("cdd","other")),model=as.integer(c(1,4)),
                                            minimization_successful=as.integer(c(1,1)),covariance_step_successful=as.integer(c(1,1)),
                                            covariance_step_warnings=as.integer(c(0,0)),estimate_near_boundary=as.integer(c(0,1)),
                                            EI1=as.logical(c(NA,NA)),cook.scores=as.numeric(c(1.69474,2.21297)),cov.ratios=as.numeric(c(1.06352,1.43112)),
                                            outside.n.sd=as.integer(c(1,0)),ID=as.integer(c(1,4))))
correct_out_2c <- list(cdd.pt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                           minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                           covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                           EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                           outside.n.sd=as.integer(c()),ID=as.integer(c())),
                       cdd.txt = data.frame(method=factor(rep("cdd",2),levels=c("cdd","other")),model=as.integer(c(1,2)),
                                            minimization_successful=as.integer(c(1,1)),covariance_step_successful=as.integer(c(1,1)),
                                            covariance_step_warnings=as.integer(c(0,1)),estimate_near_boundary=as.integer(c(0,0)),
                                            EI1=as.logical(c(NA,NA)),cook.scores=as.numeric(c(1.69474,0.65269)),cov.ratios=as.numeric(c(1.06352,0)),
                                            outside.n.sd=as.integer(c(1,0)),ID=as.integer(c(1,2))))
# markeropt == 3
correct_out_3 <- list(cdd.pt = data.frame(method=factor(rep("cdd",3),levels=c("cdd","other")),model=as.integer(c(2,3,4)),
                                          minimization_successful=as.integer(c(1,1,1)),covariance_step_successful=as.integer(c(1,0,1)),
                                          covariance_step_warnings=as.integer(c(1,0,0)),estimate_near_boundary=as.integer(c(0,0,1)),
                                          EI1=as.logical(c(NA,NA,NA)),cook.scores=as.numeric(c(0.65269,1.34665,2.21297)),cov.ratios=as.numeric(c(0,0,1.43112)),
                                          outside.n.sd=as.integer(c(0,0,0)),ID=as.integer(c(2,3,4))),
                      cdd.txt = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(1)),
                                           minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                           covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(0)),
                                           EI1=c(NA),cook.scores=c(1.69474),cov.ratios=c(1.06352),
                                           outside.n.sd=as.integer(c(1)),ID=as.integer(c(1))))
correct_out_3a <- list(cdd.pt = data.frame(method=factor(c(),levels=c("cdd","other")),model=as.integer(c()),
                                           minimization_successful=as.integer(c()),covariance_step_successful=as.integer(c()),
                                           covariance_step_warnings=as.integer(c()),estimate_near_boundary=as.integer(c()),
                                           EI1=as.logical(c()),cook.scores=as.numeric(c()),cov.ratios=as.numeric(c()),
                                           outside.n.sd=as.integer(c()),ID=as.integer(c())),
                      cdd.txt = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(1)),
                                           minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                           covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(0)),
                                           EI1=c(NA),cook.scores=c(1.69474),cov.ratios=c(1.06352),
                                           outside.n.sd=as.integer(c(1)),ID=as.integer(c(1))))
correct_out_3b <- list(cdd.pt = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(4)),
                                           minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                           covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(1)),
                                           EI1=as.logical(c(NA)),cook.scores=as.numeric(c(2.21297)),cov.ratios=as.numeric(c(1.43112)),
                                           outside.n.sd=as.integer(c(0)),ID=as.integer(c(4))),
                      cdd.txt = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(1)),
                                           minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                           covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(0)),
                                           EI1=as.logical(c(NA)),cook.scores=as.numeric(c(1.69474)),cov.ratios=as.numeric(c(1.06352)),
                                           outside.n.sd=as.integer(c(1)),ID=as.integer(c(1))))
correct_out_3c <- list(cdd.pt = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(2)),
                                           minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                           covariance_step_warnings=as.integer(c(1)),estimate_near_boundary=as.integer(c(0)),
                                           EI1=as.logical(c(NA)),cook.scores=as.numeric(c(0.65269)),cov.ratios=as.numeric(c(0)),
                                           outside.n.sd=as.integer(c(0)),ID=as.integer(c(2))),
                       cdd.txt = data.frame(method=factor(rep("cdd",1),levels=c("cdd","other")),model=as.integer(c(1)),
                                            minimization_successful=as.integer(c(1)),covariance_step_successful=as.integer(c(1)),
                                            covariance_step_warnings=as.integer(c(0)),estimate_near_boundary=as.integer(c(0)),
                                            EI1=as.logical(c(NA)),cook.scores=as.numeric(c(1.69474)),cov.ratios=as.numeric(c(1.06352)),
                                            outside.n.sd=as.integer(c(1)),ID=as.integer(c(1))))
# Test mark.options.data function
context("CDD, function mark.options.data")
test_that("Testing function mark.options.data",{
  expect_identical(correct_out_1,out_1)
  expect_identical(correct_out_1a,out_1a)
  expect_identical(correct_out_1b,out_1b)
  expect_identical(correct_out_1c,out_1c)
  expect_identical(correct_out_2,out_2)
  expect_identical(correct_out_2a,out_2a)
  expect_identical(correct_out_2b,out_2b)
  expect_identical(correct_out_2c,out_2c)
  expect_identical(correct_out_3,out_3)
  expect_identical(correct_out_3a,out_3a)
  expect_identical(correct_out_3b,out_3b)
  expect_identical(correct_out_3c,out_3c)
})



