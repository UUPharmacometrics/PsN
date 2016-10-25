################################    1. Library for testing     ##############################
library(testthat)
tool <- 'cdd'
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

raw.results.file <- paste0(files.w.dir,'raw_results_2.csv')
skipped.id.file <- paste0(files.w.dir,'skipped_2.csv')
raw.results.file_3 <- paste0(files.w.dir,'raw_results_3.csv')
raw.results.file_4 <- paste0(files.w.dir,'raw_results_4.csv')
skipped.id.file_4 <- paste0(files.w.dir,'skipped_4.csv')
raw.results.file_5 <- paste0(files.w.dir,'raw_results_5.csv')

###################################     4. Make tests     ###################################
#.................................  (1) Test create.data.full  ......................................
out_data <- create.data.full(raw.results.file,skipped.id.file)
out_data_3 <- create.data.full(raw.results.file_3,skipped.id.file)
out_data_4 <- create.data.full(raw.results.file_4,skipped.id.file_4)
out_data_5 <- create.data.full(raw.results.file_5,skipped.id.file_4)

# unlist
input_cdd.data <- out_data$input_cdd.data
cdd.data.all <- out_data$cdd.data.all
input_cdd.data_3 <- out_data_3$input_cdd.data
cdd.data.all_3 <- out_data_3$cdd.data.all
input_cdd.data_4 <- out_data_4$input_cdd.data
cdd.data.all_4 <- out_data_4$cdd.data.all
input_cdd.data_5 <- out_data_5$input_cdd.data
cdd.data.all_5 <- out_data_5$cdd.data.all

# Create expected data
exp_input_cdd.data <- data.frame(method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                 model=as.integer(c(0,1,2,3,4)),
                                 THETA1=c(27.9,28.2,28.8,27.5,27.6),
                                 seTHETA1=c(2.2,2.4,2.3,NA,2.39),
                                 seTHETA5=c(0.05,0.06,0.047,NA,0.056),
                                 seOMEGA.3.3.=c(0.007,0.0078,0.0074,NA,0.012),
                                 seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                 cook.scores=c(0,1,1.5,4,6.23),
                                 cov.ratios=c(0,0.057,0,0.06,0.083),
                                 cook.par.THETA1=c(0,0.13,0.4,0.17,0.12),
                                 cook.par.THETA5=c(0,0.1,0.2,0.045,0.32),
                                 cook.par.OMEGA.3.3.=c(0,0.11,0.09,0.2,0.042),
                                 jack.cook.par.THETA1=c(0,0.12,0.41,0.16,0.12))
exp_cdd.data.all <- data.frame(ID=c(NA,11,13,16,19),
                               method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                               model=as.integer(c(0,1,2,3,4)),
                               THETA1=c(27.9,28.2,28.8,27.5,27.6),
                               seTHETA1=c(2.2,2.4,2.3,NA,2.39),
                               seTHETA5=c(0.05,0.06,0.047,NA,0.056),
                               seOMEGA.3.3.=c(0.007,0.0078,0.0074,NA,0.012),
                               seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                               cook.scores=c(0,1,1.5,4,6.23),
                               cov.ratios=c(0,0.057,0,0.06,0.083),
                               cook.par.THETA1=c(0,0.13,0.4,0.17,0.12),
                               cook.par.THETA5=c(0,0.1,0.2,0.045,0.32),
                               cook.par.OMEGA.3.3.=c(0,0.11,0.09,0.2,0.042),
                               jack.cook.par.THETA1=c(0,0.12,0.41,0.16,0.12))
exp_input_cdd.data_3 <- data.frame(method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                 model=as.integer(c(0,1,2,3,4)),
                                 ofv=c(-114,-109.2,-94.7,-102,-89),
                                 seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                 cdd.delta.ofv=c(0,0.54,0.6,0.5,2.3))
exp_cdd.data.all_3 <- data.frame(ID=c(NA,11,13,16,19),
                                 method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                 model=as.integer(c(0,1,2,3,4)),
                                 ofv=c(-114,-109.2,-94.7,-102,-89),
                                 seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                 cdd.delta.ofv=c(0,0.54,0.6,0.5,2.3))
  
# Compare expected data with real data
context("Test function create.data.full")
test_that("If function create.data.full works as expected",{
  expect_equal(exp_input_cdd.data,input_cdd.data)
  expect_equal(exp_cdd.data.all,cdd.data.all)
  expect_equal(exp_input_cdd.data_3,input_cdd.data_3)
  expect_equal(exp_cdd.data.all_3,cdd.data.all_3)
})

#.................................  (2) Test cov.cook.par.data  ...............................
out_cov.cook.par.data <- cov.cook.par.data(cdd.data.all)

# unlist
cook.par.data <- out_cov.cook.par.data$cook.par.data
cov.par.data <- round(out_cov.cook.par.data$cov.par.data,6)
parameters <- out_cov.cook.par.data$parameters
# Create expected data
exp_cook.par.data <- data.frame(cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                                 cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                                 cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042))
exp_cov.par.data <- data.frame(cov.par.THETA1=c(1.090909,1.045455,NA,1.086364),
                               cov.par.THETA5=c(1.2,0.94,NA,1.12),
                               cov.par.OMEGA.3.3.=c(1.114286,1.057143,NA,1.714286))
exp_parameters <- c("THETA1","THETA5","OMEGA.3.3.")
# Compare expected data with real data
context("Test function cov.cook.par.data")
test_that("If function cov.cook.par.data works as expected",{
  expect_equal(exp_cook.par.data,cook.par.data)
  expect_equal(exp_cov.par.data,cov.par.data)
  expect_equal(exp_parameters,parameters)
})

#.................................  (3) Test cutoff.cov.cook  ...............................
out_cutoff.cov.cook <- cutoff.cov.cook(raw.results.file,skipped.id.file,
                                           cutoff_cook=3,cutoff_cov=0.03)
out_cutoff.cov.cook_1 <- cutoff.cov.cook(raw.results.file,skipped.id.file,cutoff_cook=3)
out_cutoff.cov.cook_2 <- cutoff.cov.cook(raw.results.file,skipped.id.file,cutoff_cov=0.03)

# unlist
cook.data <- out_cutoff.cov.cook$cook.data
cov.data <- out_cutoff.cov.cook$cov.data
cook_outliers <- out_cutoff.cov.cook$cook_outliers
cov_outliers <- out_cutoff.cov.cook$cov_outliers
cook_outliers_data <- out_cutoff.cov.cook$cook_outliers_data
cov_outliers_data <- out_cutoff.cov.cook$cov_outliers_data

cook.data_1 <- out_cutoff.cov.cook_1$cook.data
cook_outliers_1 <- out_cutoff.cov.cook_1$cook_outliers
cook_outliers_data_1 <- out_cutoff.cov.cook_1$cook_outliers_data

cov.data_2 <- out_cutoff.cov.cook_2$cov.data
cov_outliers_2 <- out_cutoff.cov.cook_2$cov_outliers
cov_outliers_data_2 <- out_cutoff.cov.cook_2$cov_outliers_data
# Create expected data
exp_cook.data <- data.frame(ID=as.integer(c(11,13,16,19)),
                            cook.scores=c(1,1.5,4,6.23),
                            cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                            cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                            cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042))
exp_cov.data <- data.frame(ID=as.integer(c(11,13,16,19)),
                           cov.ratios=c(0.057,0,0.06,0.083),
                           cov.par.THETA1=c(1.09091,1.04545,NA,1.08636),
                           cov.par.THETA5=c(1.2,0.94,NA,1.12),
                           cov.par.OMEGA.3.3.=c(1.11429,1.05714,NA,1.71429))
exp_cook_outliers <- c(4,6.23)
exp_cov_outliers <- c(0.057,0.06,0.083)
exp_cook_outliers_data <- data.frame(ID=as.integer(c(16,19)),
                                     cook.scores=c(4,6.23),
                                     cook.par.THETA1=c(0.17,0.12),
                                     cook.par.THETA5=c(0.045,0.32),
                                     cook.par.OMEGA.3.3.=c(0.2,0.042))
exp_cov_outliers_data <- data.frame(ID=as.integer(c(11,16,19)),
                                    cov.ratios=c(0.057,0.06,0.083),
                                    cov.par.THETA1=c(1.09091,NA,1.08636),
                                    cov.par.THETA5=c(1.2,NA,1.12),
                                    cov.par.OMEGA.3.3.=c(1.11429,NA,1.71429))

exp_cook.data_1 <- data.frame(ID=as.integer(c(11,13,16,19)),
                            cook.scores=c(1,1.5,4,6.23),
                            cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                            cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                            cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042))
exp_cook_outliers_1 <- c(4,6.23)
exp_cook_outliers_data_1 <- data.frame(ID=as.integer(c(16,19)),
                                     cook.scores=c(4,6.23),
                                     cook.par.THETA1=c(0.17,0.12),
                                     cook.par.THETA5=c(0.045,0.32),
                                     cook.par.OMEGA.3.3.=c(0.2,0.042))

exp_cov.data_2 <- data.frame(ID=as.integer(c(11,13,16,19)),
                           cov.ratios=c(0.057,0,0.06,0.083),
                           cov.par.THETA1=c(1.09091,1.04545,NA,1.08636),
                           cov.par.THETA5=c(1.2,0.94,NA,1.12),
                           cov.par.OMEGA.3.3.=c(1.11429,1.05714,NA,1.71429))
exp_cov_outliers_2 <- c(0.057,0.06,0.083)
exp_cov_outliers_data_2 <- data.frame(ID=as.integer(c(11,16,19)),
                                    cov.ratios=c(0.057,0.06,0.083),
                                    cov.par.THETA1=c(1.09091,NA,1.08636),
                                    cov.par.THETA5=c(1.2,NA,1.12),
                                    cov.par.OMEGA.3.3.=c(1.11429,NA,1.71429))

# Compare expected data with real data
context("Test function cutoff.cov.cook")
test_that("If function cutoff.cov.cook works as expected",{
  expect_equal(exp_cook.data,cook.data)
  expect_equal(exp_cov.data,cov.data)
  expect_equal(exp_cook_outliers,cook_outliers)
  expect_equal(exp_cov_outliers,cov_outliers)
  expect_equal(exp_cook_outliers_data,cook_outliers_data)
  expect_equal(exp_cov_outliers_data,cov_outliers_data)
  
  expect_equal(exp_cook.data_1,cook.data_1)
  expect_equal(exp_cook_outliers_1,cook_outliers_1)
  expect_equal(exp_cook_outliers_data_1,cook_outliers_data_1)
  
  expect_equal(exp_cov.data_2,cov.data_2)
  expect_equal(exp_cov_outliers_2,cov_outliers_2)
  expect_equal(exp_cov_outliers_data_2,cov_outliers_data_2)
})

#.................................  (4) Test plot.cov.cook  ...............................
cdd.data <- plot.cov.cook(cdd.data.all)

# Create expected data
exp_cdd.data <- data.frame(ID=as.integer(c(11,13,16,19)),
                           method=factor(c(rep("cdd",4)),levels=c("cdd","other")),
                           model=as.integer(c(1,2,3,4)),
                           THETA1=c(28.2,28.8,27.5,27.6),
                           seTHETA1=c(2.4,2.3,NA,2.39),
                           seTHETA5=c(0.06,0.047,NA,0.056),
                           seOMEGA.3.3.=c(0.0078,0.0074,NA,0.012),
                           seSIGMA.1.1.=c(NA,NA,NA,NA),
                           cook.scores=c(1,1.5,4,6.23),
                           cov.ratios=c(0.057,0,0.06,0.083),
                           cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                           cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                           cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042),
                           jack.cook.par.THETA1=c(0.12,0.41,0.16,0.12))
# Compare expected data with real data
context("Test function plot.cov.cook")
test_that("If function plot.cov.cook works as expected",{
  expect_equal(exp_cdd.data,cdd.data)
})

#.................................  (5) Test plot.cov.cook.par  ...............................
nr.parameters <- plot.cov.cook.par(cook.par.data,cov.par.data,parameters)

# Create expected data
exp_nr.parameters <- 3
# Compare expected data with real data
context("Test function plot.cov.cook.par")
test_that("If function plot.cov.cook.par works as expected",{
  expect_equal(exp_nr.parameters,nr.parameters)
})

#.................................  (6) Test plot.ofv  ...............................
out_ofv.data <- plot.ofv(cdd.data.all_3)

# unlist
ofv.orig <- out_ofv.data$ofv.orig
ofv.est <- out_ofv.data$ofv.est

# Create expected data
exp_ofv.orig <- c(-108.66,-94.1,-101.5,-86.7)
exp_ofv.est <- c(-109.2,-94.7,-102,-89)
# Compare expected data with real data
context("Test function plot.ofv")
test_that("If function plot.ofv works as expected",{
  expect_equal(exp_ofv.orig,ofv.orig)
  expect_equal(exp_ofv.est,ofv.est)
})

#.................................  (7) Test delta.ofv.data  ...............................
out_ofv.delta <- delta.ofv.data(cdd.data.all_4)
out_ofv.delta_1 <- delta.ofv.data(cdd.data.all_4,outlier_ID=c(19,23,41))
out_ofv.delta_2 <- delta.ofv.data(cdd.data.all_4,outlier_ID=c(23,41))
out_ofv.delta_3 <- delta.ofv.data(cdd.data.all_4,outlier_ID=c(19,38))
out_ofv.delta_4 <- delta.ofv.data(cdd.data.all_4,outlier_ID=c())
out_ofv.delta_5 <- delta.ofv.data(cdd.data.all_5)

# unlist
delta.ofv <- out_ofv.delta$delta.ofv
delta.ofv_10_pr <- out_ofv.delta$delta.ofv_10_pr
row_infl <- out_ofv.delta$row_infl
ID_infl <- out_ofv.delta$ID_infl

delta.ofv_5 <- out_ofv.delta_5$delta.ofv
delta.ofv_10_pr_5 <- out_ofv.delta_5$delta.ofv_10_pr
row_infl_5 <- out_ofv.delta_5$row_infl
ID_infl_5 <- out_ofv.delta_5$ID_infl
model_5 <- out_ofv.delta_5$model
fail_ID_5 <- out_ofv.delta_5$fail_ID
  
delta.ofv_1 <- out_ofv.delta_1$delta.ofv
delta.ofv_10_pr_1 <- out_ofv.delta_1$delta.ofv_10_pr
row_infl_all_1 <- out_ofv.delta_1$row_infl_all
row_outl_all_1 <- out_ofv.delta_1$row_outl_all
row_outl_infl_1 <- out_ofv.delta_1$row_outl_infl
row_outl_1 <- out_ofv.delta_1$row_outl
row_infl_1 <- out_ofv.delta_1$row_infl
ID_infl_1 <- out_ofv.delta_1$ID_infl
ID_outl_1 <- out_ofv.delta_1$ID_outl
ID_outl_infl_1 <- out_ofv.delta_1$ID_outl_infl

delta.ofv_2 <- out_ofv.delta_2$delta.ofv
delta.ofv_10_pr_2 <- out_ofv.delta_2$delta.ofv_10_pr
row_infl_all_2 <- out_ofv.delta_2$row_infl_all
row_outl_all_2 <- out_ofv.delta_2$row_outl_all
row_outl_infl_2 <- out_ofv.delta_2$row_outl_infl
row_outl_2 <- out_ofv.delta_2$row_outl
row_infl_2 <- out_ofv.delta_2$row_infl
ID_infl_2 <- out_ofv.delta_2$ID_infl
ID_outl_2 <- out_ofv.delta_2$ID_outl
ID_outl_infl_2 <- out_ofv.delta_2$ID_outl_infl

delta.ofv_3 <- out_ofv.delta_3$delta.ofv
delta.ofv_10_pr_3 <- out_ofv.delta_3$delta.ofv_10_pr
row_infl_all_3 <- out_ofv.delta_3$row_infl_all
row_outl_all_3 <- out_ofv.delta_3$row_outl_all
row_outl_infl_3 <- out_ofv.delta_3$row_outl_infl
row_outl_3 <- out_ofv.delta_3$row_outl
row_infl_3 <- out_ofv.delta_3$row_infl
ID_infl_3 <- out_ofv.delta_3$ID_infl
ID_outl_3 <- out_ofv.delta_3$ID_outl
ID_outl_infl_3 <- out_ofv.delta_3$ID_outl_infl

delta.ofv_4 <- out_ofv.delta_4$delta.ofv
delta.ofv_10_pr_4 <- out_ofv.delta_4$delta.ofv_10_pr
row_infl_all_4 <- out_ofv.delta_4$row_infl_all
row_outl_all_4 <- out_ofv.delta_4$row_outl_all
row_outl_infl_4 <- out_ofv.delta_4$row_outl_infl
row_outl_4 <- out_ofv.delta_4$row_outl
row_infl_4 <- out_ofv.delta_4$row_infl
ID_infl_4 <- out_ofv.delta_4$ID_infl
ID_outl_4 <- out_ofv.delta_4$ID_outl
ID_outl_infl_4 <- out_ofv.delta_4$ID_outl_infl

# Create expected data
exp_delta.ofv <- c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3)
exp_delta.ofv_10_pr <- c(6.3,5.2)
exp_row_infl <- c(4,13)
exp_ID_infl <- c(19,38)

exp_delta.ofv_5 <- c(0.54,0.5,6.3,0.4,3.2,3.2,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3)
exp_delta.ofv_10_pr_5 <- c(6.3)
exp_row_infl_5 <- c(3)
exp_ID_infl_5 <- c(19)
exp_model_5 <- c(1,3,4,5,6,7,9,10,11,12,13,14,15,16)
exp_fail_ID_5 <- c(13,27)

exp_delta.ofv_1 <- c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3)
exp_delta.ofv_10_pr_1 <- c(6.3,5.2)
exp_row_infl_all_1 <- c(4,13)
exp_row_outl_all_1 <- c(4,6,15)
exp_row_outl_infl_1 <- c(4)
exp_row_outl_1 <- c(6,15)
exp_row_infl_1 <- c(13)
exp_ID_infl_1 <- c(38)
exp_ID_outl_1 <- c(23,41)
exp_ID_outl_infl_1 <- c(19)

exp_delta.ofv_2 <- c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3)
exp_delta.ofv_10_pr_2 <- c(6.3,5.2)
exp_row_infl_all_2 <- c(4,13)
exp_row_outl_all_2 <- c(6,15)
exp_row_outl_infl_2 <- as.integer(c())
exp_row_outl_2 <- c(6,15)
exp_row_infl_2 <- c(4,13)
exp_ID_infl_2 <- c(19,38)
exp_ID_outl_2 <- c(23,41)
exp_ID_outl_infl_2 <- as.integer(c())

exp_delta.ofv_3 <- c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3)
exp_delta.ofv_10_pr_3 <- c(6.3,5.2)
exp_row_infl_all_3 <- c(4,13)
exp_row_outl_all_3 <- c(4,13)
exp_row_outl_infl_3 <- c(4,13)
exp_row_outl_3 <- as.integer(c())
exp_row_infl_3 <- as.integer(c())
exp_ID_infl_3 <- as.integer(c())
exp_ID_outl_3 <- as.integer(c())
exp_ID_outl_infl_3 <- c(19,38)

exp_delta.ofv_4 <- c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3)
exp_delta.ofv_10_pr_4 <- c(6.3,5.2)
exp_row_infl_all_4 <- c(4,13)
exp_row_outl_all_4 <- as.integer(c())
exp_row_outl_infl_4 <- as.integer(c())
exp_row_outl_4 <- as.integer(c())
exp_row_infl_4 <- c(4,13)
exp_ID_infl_4 <- c(19,38)
exp_ID_outl_4 <- as.integer(c())
exp_ID_outl_infl_4 <- as.integer(c())

# Compare expected data with real data
context("Test function delta.ofv.data")
test_that("If function delta.ofv.data works as expected",{
  expect_equal(exp_delta.ofv,delta.ofv)
  expect_equal(exp_delta.ofv_10_pr,delta.ofv_10_pr)
  expect_equal(exp_row_infl,row_infl)
  expect_equal(exp_ID_infl,ID_infl)
  
  expect_equal(exp_delta.ofv_5,delta.ofv_5)
  expect_equal(exp_delta.ofv_10_pr_5,delta.ofv_10_pr_5)
  expect_equal(exp_row_infl_5,row_infl_5)
  expect_equal(exp_ID_infl_5,ID_infl_5)
  expect_equal(exp_model_5,model_5)
  expect_equal(exp_fail_ID_5,fail_ID_5)
  
  expect_equal(exp_delta.ofv_1,delta.ofv_1)
  expect_equal(exp_delta.ofv_10_pr_1,delta.ofv_10_pr_1)
  expect_equal(exp_row_infl_all_1,row_infl_all_1)
  expect_equal(exp_row_outl_all_1,row_outl_all_1)
  expect_equal(exp_row_outl_infl_1,row_outl_infl_1)
  expect_equal(exp_row_outl_1,row_outl_1)
  expect_equal(exp_row_infl_1,row_infl_1)
  expect_equal(exp_ID_infl_1,ID_infl_1)
  expect_equal(exp_ID_outl_1,ID_outl_1)
  expect_equal(exp_ID_outl_infl_1,ID_outl_infl_1)
  
  expect_equal(exp_delta.ofv_2,delta.ofv_2)
  expect_equal(exp_delta.ofv_10_pr_2,delta.ofv_10_pr_2)
  expect_equal(exp_row_infl_all_2,row_infl_all_2)
  expect_equal(exp_row_outl_all_2,row_outl_all_2)
  expect_equal(exp_row_outl_infl_2,row_outl_infl_2)
  expect_equal(exp_row_outl_2,row_outl_2)
  expect_equal(exp_row_infl_2,row_infl_2)
  expect_equal(exp_ID_infl_2,ID_infl_2)
  expect_equal(exp_ID_outl_2,ID_outl_2)
  expect_equal(exp_ID_outl_infl_2,ID_outl_infl_2)
  
  expect_equal(exp_delta.ofv_3,delta.ofv_3)
  expect_equal(exp_delta.ofv_10_pr_3,delta.ofv_10_pr_3)
  expect_equal(exp_row_infl_all_3,row_infl_all_3)
  expect_equal(exp_row_outl_all_3,row_outl_all_3)
  expect_equal(exp_row_outl_infl_3,row_outl_infl_3)
  expect_equal(exp_row_outl_3,row_outl_3)
  expect_equal(exp_row_infl_3,row_infl_3)
  expect_equal(exp_ID_infl_3,ID_infl_3)
  expect_equal(exp_ID_outl_3,ID_outl_3)
  expect_equal(exp_ID_outl_infl_3,ID_outl_infl_3)
  
  expect_equal(exp_delta.ofv_4,delta.ofv_4)
  expect_equal(exp_delta.ofv_10_pr_4,delta.ofv_10_pr_4)
  expect_equal(exp_row_infl_all_4,row_infl_all_4)
  expect_equal(exp_row_outl_all_4,row_outl_all_4)
  expect_equal(exp_row_outl_infl_4,row_outl_infl_4)
  expect_equal(exp_row_outl_4,row_outl_4)
  expect_equal(exp_row_infl_4,row_infl_4)
  expect_equal(exp_ID_infl_4,ID_infl_4)
  expect_equal(exp_ID_outl_4,ID_outl_4)
  expect_equal(exp_ID_outl_infl_4,ID_outl_infl_4)
})
