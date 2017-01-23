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
raw.results.file_6 <- paste0(files.w.dir,'raw_results_6.csv')
raw.results.file_7 <- paste0(files.w.dir,'raw_results_7.csv')

###################################     4. Make tests     ###################################
#.................................  (1) Test create.data.full  ......................................
out_data <- create.data.full(raw.results.file,skipped.id.file)
out_data_3 <- create.data.full(raw.results.file_3,skipped.id.file)
out_data_4 <- create.data.full(raw.results.file_4,skipped.id.file_4)
out_data_5 <- create.data.full(raw.results.file_5,skipped.id.file_4)
out_data_6 <- create.data.full(raw.results.file_6,skipped.id.file)
out_data_7 <- create.data.full(raw.results.file_7,skipped.id.file)

# unlist
input_cdd.data <- out_data$input_cdd.data
cdd.data.all <- out_data$cdd.data.all
no.cook.cov <- out_data$no.cook.cov
input_cdd.data_3 <- out_data_3$input_cdd.data
cdd.data.all_3 <- out_data_3$cdd.data.all
no.cook.cov_3 <- out_data_3$no.cook.cov
input_cdd.data_4 <- out_data_4$input_cdd.data
cdd.data.all_4 <- out_data_4$cdd.data.all
no.cook.cov_4 <- out_data_4$no.cook.cov
input_cdd.data_5 <- out_data_5$input_cdd.data
cdd.data.all_5 <- out_data_5$cdd.data.all
no.cook.cov_5 <- out_data_5$no.cook.cov
input_cdd.data_6 <- out_data_6$input_cdd.data
cdd.data.all_6 <- out_data_6$cdd.data.all
no.cook.cov_6 <- out_data_6$no.cook.cov
input_cdd.data_7 <- out_data_7$input_cdd.data
cdd.data.all_7 <- out_data_7$cdd.data.all
no.cook.cov_7 <- out_data_7$no.cook.cov

# Create expected data
exp_input_cdd.data <- data.frame(method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                 model=as.integer(c(0,1,2,3,4)),
                                 THETA1=c(27.9,28.2,28.8,27.5,27.6),
                                 seTHETA1=c(2.2,2.4,2.3,NA,2.39),
                                 seTHETA5=c(0.05,0.06,0.047,NA,0.056),
                                 seOMEGA.3.3.=c(0.007,0.0078,0.0074,NA,0.012),
                                 seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                 cook.scores=c(0,1,1.5,4,6.23),
                                 cov.ratios=c(0,0.057,6.4,7.06,0.083),
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
                               cov.ratios=c(0,0.057,6.4,7.06,0.083),
                               cook.par.THETA1=c(0,0.13,0.4,0.17,0.12),
                               cook.par.THETA5=c(0,0.1,0.2,0.045,0.32),
                               cook.par.OMEGA.3.3.=c(0,0.11,0.09,0.2,0.042),
                               jack.cook.par.THETA1=c(0,0.12,0.41,0.16,0.12))

exp_input_cdd.data_6 <- data.frame(method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                 model=as.integer(c(0,1,2,3,4)),
                                 THETA1=c(27.9,28.2,28.8,27.5,27.6),
                                 seTHETA1=c(2.2,2.4,2.3,NA,2.39),
                                 seTHETA5=c(0.05,0.06,0.047,NA,0.056),
                                 seOMEGA.3.3.=c(0.007,0.0078,0.0074,NA,0.012),
                                 seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                 cook.scores=c(0,1,1.5,4,6.23),
                                 cov.ratios=c(0,0.057,0.4,0.06,0.083),
                                 cook.par.THETA1=c(0,0.13,0.4,0.17,0.12),
                                 cook.par.THETA5=c(0,0.1,0.2,0.045,0.32),
                                 cook.par.OMEGA.3.3.=c(0,0.11,0.09,0.2,0.042),
                                 jack.cook.par.THETA1=c(0,0.12,0.41,0.16,0.12))
exp_cdd.data.all_6 <- data.frame(ID=c(NA,11,13,16,19),
                               method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                               model=as.integer(c(0,1,2,3,4)),
                               THETA1=c(27.9,28.2,28.8,27.5,27.6),
                               seTHETA1=c(2.2,2.4,2.3,NA,2.39),
                               seTHETA5=c(0.05,0.06,0.047,NA,0.056),
                               seOMEGA.3.3.=c(0.007,0.0078,0.0074,NA,0.012),
                               seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                               cook.scores=c(0,1,1.5,4,6.23),
                               cov.ratios=c(0,0.057,0.4,0.06,0.083),
                               cook.par.THETA1=c(0,0.13,0.4,0.17,0.12),
                               cook.par.THETA5=c(0,0.1,0.2,0.045,0.32),
                               cook.par.OMEGA.3.3.=c(0,0.11,0.09,0.2,0.042),
                               jack.cook.par.THETA1=c(0,0.12,0.41,0.16,0.12))
exp_input_cdd.data_7 <- data.frame(method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                   model=as.integer(c(0,1,2,3,4)),
                                   THETA1=c(27.9,28.2,28.8,27.5,27.6),
                                   seTHETA1=c(2.2,2.4,2.3,NA,2.39),
                                   seTHETA5=c(0.05,0.06,0.047,NA,0.056),
                                   seOMEGA.3.3.=c(0.007,0.0078,0.0074,NA,0.012),
                                   seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                   cook.scores=c(0,1,1.5,2,1.23),
                                   cov.ratios=c(0,0.057,0.4,0.06,0.083),
                                   cook.par.THETA1=c(0,0.13,0.4,0.17,0.12),
                                   cook.par.THETA5=c(0,0.1,0.2,0.045,0.32),
                                   cook.par.OMEGA.3.3.=c(0,0.11,0.09,0.2,0.042),
                                   jack.cook.par.THETA1=c(0,0.12,0.41,0.16,0.12))
exp_cdd.data.all_7 <- data.frame(ID=c(NA,11,13,16,19),
                                 method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                 model=as.integer(c(0,1,2,3,4)),
                                 THETA1=c(27.9,28.2,28.8,27.5,27.6),
                                 seTHETA1=c(2.2,2.4,2.3,NA,2.39),
                                 seTHETA5=c(0.05,0.06,0.047,NA,0.056),
                                 seOMEGA.3.3.=c(0.007,0.0078,0.0074,NA,0.012),
                                 seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                 cook.scores=c(0,1,1.5,2,1.23),
                                 cov.ratios=c(0,0.057,0.4,0.06,0.083),
                                 cook.par.THETA1=c(0,0.13,0.4,0.17,0.12),
                                 cook.par.THETA5=c(0,0.1,0.2,0.045,0.32),
                                 cook.par.OMEGA.3.3.=c(0,0.11,0.09,0.2,0.042),
                                 jack.cook.par.THETA1=c(0,0.12,0.41,0.16,0.12))
exp_input_cdd.data_3 <- data.frame(method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                 model=as.integer(c(0,1,2,3,4)),
                                 ofv=c(-114,-109.2,-94.7,-102,-89),
                                 seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                 cdd.delta.ofv=c(0,0.54,0.6,0.5,2.3),
                                 cook.scores=c(0,NA,NA,NA,NA),
                                 cov.ratios=c(1,0,0,0,0))
exp_cdd.data.all_3 <- data.frame(ID=c(NA,11,13,16,19),
                                 method=factor(c(rep("cdd",5)),levels=c("cdd","other")),
                                 model=as.integer(c(0,1,2,3,4)),
                                 ofv=c(-114,-109.2,-94.7,-102,-89),
                                 seSIGMA.1.1.=c(NA,NA,NA,NA,NA),
                                 cdd.delta.ofv=c(0,0.54,0.6,0.5,2.3),
                                 cook.scores=c(0,NA,NA,NA,NA),
                                 cov.ratios=c(1,0,0,0,0))
  
# Compare expected data with real data
context("CDD, function create.data.full")
test_that("If function create.data.full works as expected",{
  expect_equal(exp_input_cdd.data,input_cdd.data)
  expect_equal(exp_cdd.data.all,cdd.data.all)
  expect_equal(FALSE,no.cook.cov)
  expect_equal(exp_input_cdd.data_3,input_cdd.data_3)
  expect_equal(exp_cdd.data.all_3,cdd.data.all_3)
  expect_equal(TRUE,no.cook.cov_3)
  expect_equal(exp_input_cdd.data_6,input_cdd.data_6)
  expect_equal(exp_cdd.data.all_6,cdd.data.all_6)
  expect_equal(FALSE,no.cook.cov_6)
  expect_equal(exp_input_cdd.data_7,input_cdd.data_7)
  expect_equal(exp_cdd.data.all_7,cdd.data.all_7)
  expect_equal(FALSE,no.cook.cov_7)
})

#.................................  (2) Test cov.cook.par.data  ...............................
out_cov.cook.par.data <- cov.cook.par.data(cdd.data.all)
out_cov.cook.par.data_6 <- cov.cook.par.data(cdd.data.all_6)
out_cov.cook.par.data_7 <- cov.cook.par.data(cdd.data.all_7)

# unlist
cook.par.data <- out_cov.cook.par.data$cook.par.data
cov.par.data <- round(out_cov.cook.par.data$cov.par.data,6)
parameters <- out_cov.cook.par.data$parameters
cook.par.data_6 <- out_cov.cook.par.data_6$cook.par.data
cov.par.data_6 <- round(out_cov.cook.par.data_6$cov.par.data,6)
parameters_6 <- out_cov.cook.par.data_6$parameters

# Create expected data
exp_cook.par.data <- data.frame(cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                                 cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                                 cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042))
exp_cov.par.data <- data.frame(cov.par.THETA1=c(1.090909,1.045455,NA,1.086364),
                               cov.par.THETA5=c(1.2,0.94,NA,1.12),
                               cov.par.OMEGA.3.3.=c(1.114286,1.057143,NA,1.714286))
exp_parameters <- c("THETA1","THETA5","OMEGA.3.3.")
# Compare expected data with real data
context("CDD, function cov.cook.par.data")
test_that("If function cov.cook.par.data works as expected",{
  expect_equal(exp_cook.par.data,cook.par.data)
  expect_equal(exp_cov.par.data,cov.par.data)
  expect_equal(exp_parameters,parameters)
  expect_equal(exp_cook.par.data,cook.par.data_6)
  expect_equal(exp_cov.par.data,cov.par.data_6)
  expect_equal(exp_parameters,parameters_6)
})

#.................................  (3) Test cutoff.cov.cook  ...............................
out_cutoff.cov.cook <- cutoff.cov.cook(raw.results.file,skipped.id.file,cutoff_cook=3)
out_cutoff.cov.cook_6 <- cutoff.cov.cook(raw.results.file_6,skipped.id.file,cutoff_cook=3)
out_cutoff.cov.cook_7 <- cutoff.cov.cook(raw.results.file_7,skipped.id.file,cutoff_cook=2.1)

# unlist
cook.data <- out_cutoff.cov.cook$cook.data
cov.data <- out_cutoff.cov.cook$cov.data
cook_outliers <- out_cutoff.cov.cook$cook_outliers
cov_outliers <- out_cutoff.cov.cook$cov_outliers
cook_outliers_data <- out_cutoff.cov.cook$cook_outliers_data
cov_outliers_data <- out_cutoff.cov.cook$cov_outliers_data
cutoff_cov <- out_cutoff.cov.cook$cutoff_cov
infl_cov_data <- out_cutoff.cov.cook$infl_cov_data
infl_cook_data <- out_cutoff.cov.cook$infl_cook_data

cook.data_6 <- out_cutoff.cov.cook_6$cook.data
cov.data_6 <- out_cutoff.cov.cook_6$cov.data
cook_outliers_6 <- out_cutoff.cov.cook_6$cook_outliers
cov_outliers_6 <- out_cutoff.cov.cook_6$cov_outliers
cook_outliers_data_6 <- out_cutoff.cov.cook_6$cook_outliers_data
cov_outliers_data_6 <- out_cutoff.cov.cook_6$cov_outliers_data
cutoff_cov_6 <- out_cutoff.cov.cook_6$cutoff_cov
infl_cov_data_6 <- out_cutoff.cov.cook_6$infl_cov_data
infl_cook_data_6 <- out_cutoff.cov.cook_6$infl_cook_data

cook.data_7 <- out_cutoff.cov.cook_7$cook.data
cov.data_7 <- out_cutoff.cov.cook_7$cov.data
cook_outliers_7 <- out_cutoff.cov.cook_7$cook_outliers
cov_outliers_7 <- out_cutoff.cov.cook_7$cov_outliers
cook_outliers_data_7 <- out_cutoff.cov.cook_7$cook_outliers_data
cov_outliers_data_7 <- out_cutoff.cov.cook_7$cov_outliers_data
cutoff_cov_7 <- out_cutoff.cov.cook_7$cutoff_cov
infl_cov_data_7 <- out_cutoff.cov.cook_7$infl_cov_data
infl_cook_data_7 <- out_cutoff.cov.cook_7$infl_cook_data

# Create expected data
exp_cutoff_cov <- c(-1.25,3.25)
exp_cook.data <- data.frame(ID=as.integer(c(11,13,16,19)),
                            cook.scores=c(1,1.5,4,6.23),
                            cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                            cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                            cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042))
exp_cov.data <- data.frame(ID=as.integer(c(11,13,16,19)),
                           cov.ratios=c(0.057,6.4,7.06,0.083),
                           cov.par.THETA1=c(1.09091,1.04545,NA,1.08636),
                           cov.par.THETA5=c(1.2,0.94,NA,1.12),
                           cov.par.OMEGA.3.3.=c(1.11429,1.05714,NA,1.71429))
exp_cook_outliers <- c(4,6.23)
exp_cov_outliers <- c(6.4,7.06)
exp_cook_outliers_data <- data.frame(ID=as.integer(c(16,19)),
                                     cook.scores=c(4,6.23),
                                     cook.par.THETA1=c(0.17,0.12),
                                     cook.par.THETA5=c(0.045,0.32),
                                     cook.par.OMEGA.3.3.=c(0.2,0.042))
exp_cov_outliers_data <- data.frame(ID=as.integer(c(13,16)),
                                    cov.ratios=c(6.4,7.06),
                                    cov.par.THETA1=c(1.04545,NA),
                                    cov.par.THETA5=c(0.94,NA),
                                    cov.par.OMEGA.3.3.=c(1.05714,NA))
exp_infl_cook_data <- data.frame(ID=as.integer(c(16,19)),
                                   cook.scores=c(4,6.23))
exp_infl_cov_data <- data.frame(ID=as.integer(c(13,16)),
                                cov.ratios=c(6.4,7.06))
exp_cutoff_cov_6 <- c(-1.25,3.25)
exp_cook.data_6 <- data.frame(ID=as.integer(c(11,13,16,19)),
                            cook.scores=c(1,1.5,4,6.23),
                            cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                            cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                            cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042))
exp_cov.data_6 <- data.frame(ID=as.integer(c(11,13,16,19)),
                           cov.ratios=c(0.057,0.4,0.06,0.083),
                           cov.par.THETA1=c(1.09091,1.04545,NA,1.08636),
                           cov.par.THETA5=c(1.2,0.94,NA,1.12),
                           cov.par.OMEGA.3.3.=c(1.11429,1.05714,NA,1.71429))
exp_cook_outliers_6 <- c(4,6.23)
exp_cov_outliers_6 <- NULL
exp_cook_outliers_data_6 <- data.frame(ID=as.integer(c(16,19)),
                                     cook.scores=c(4,6.23),
                                     cook.par.THETA1=c(0.17,0.12),
                                     cook.par.THETA5=c(0.045,0.32),
                                     cook.par.OMEGA.3.3.=c(0.2,0.042))
exp_cov_outliers_data_6 <- data.frame(C = c("Don't have cov ratios bigger than cutoff."))
names(exp_cov_outliers_data_6) <- NULL
exp_infl_cook_data_6 <- data.frame(ID=as.integer(c(16,19)),
                                 cook.scores=c(4,6.23))

exp_cutoff_cov_7 <- c(-1.25,3.25)
exp_cook.data_7 <- data.frame(ID=as.integer(c(11,13,16,19)),
                              cook.scores=c(1,1.5,2,1.23),
                              cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                              cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                              cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042))
exp_cov.data_7 <- data.frame(ID=as.integer(c(11,13,16,19)),
                             cov.ratios=c(0.057,0.4,0.06,0.083),
                             cov.par.THETA1=c(1.09091,1.04545,NA,1.08636),
                             cov.par.THETA5=c(1.2,0.94,NA,1.12),
                             cov.par.OMEGA.3.3.=c(1.11429,1.05714,NA,1.71429))
exp_cook_outliers_7 <- NULL
exp_cov_outliers_7 <- NULL
exp_cook_outliers_data_7 <- data.frame(C = c("Don't have cook scores bigger than cutoff."))
names(exp_cook_outliers_data_7) <- NULL
exp_cov_outliers_data_7 <- data.frame(C = c("Don't have cov ratios bigger than cutoff."))
names(exp_cov_outliers_data_7) <- NULL

# Compare expected data with real data
context("CDD, function cutoff.cov.cook")
test_that("If function cutoff.cov.cook works as expected",{
  expect_equal(exp_cutoff_cov,cutoff_cov)
  expect_equal(exp_cook.data,cook.data)
  expect_equal(exp_cov.data,cov.data)
  expect_equal(exp_cook_outliers,cook_outliers)
  expect_equal(exp_cov_outliers,cov_outliers)
  expect_equal(exp_cook_outliers_data,cook_outliers_data)
  expect_equal(exp_cov_outliers_data,cov_outliers_data)
  expect_equal(exp_infl_cook_data,infl_cook_data)
  expect_equal(exp_infl_cov_data,infl_cov_data)
  
  expect_equal(exp_cutoff_cov_6,cutoff_cov_6)
  expect_equal(exp_cook.data_6,cook.data_6)
  expect_equal(exp_cov.data_6,cov.data_6)
  expect_equal(exp_cook_outliers_6,cook_outliers_6)
  expect_equal(exp_cov_outliers_6,cov_outliers_6)
  expect_equal(exp_cook_outliers_data_6,cook_outliers_data_6)
  expect_equal(exp_cov_outliers_data_6,cov_outliers_data_6)
  expect_equal(exp_infl_cook_data_6,infl_cook_data_6)
  expect_equal(exp_cov_outliers_data_6,infl_cov_data_6)
  
  expect_equal(exp_cutoff_cov_7,cutoff_cov_7)
  expect_equal(exp_cook.data_7,cook.data_7)
  expect_equal(exp_cov.data_7,cov.data_7)
  expect_equal(exp_cook_outliers_7,cook_outliers_7)
  expect_equal(exp_cov_outliers_7,cov_outliers_7)
  expect_equal(exp_cook_outliers_data_7,cook_outliers_data_7)
  expect_equal(exp_cov_outliers_data_7,cov_outliers_data_7)
  expect_equal(exp_cook_outliers_data_7,infl_cook_data_7)
  expect_equal(exp_cov_outliers_data_7,infl_cov_data_7)
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
                           cov.ratios=c(0.057,6.4,7.06,0.083),
                           cook.par.THETA1=c(0.13,0.4,0.17,0.12),
                           cook.par.THETA5=c(0.1,0.2,0.045,0.32),
                           cook.par.OMEGA.3.3.=c(0.11,0.09,0.2,0.042),
                           jack.cook.par.THETA1=c(0.12,0.41,0.16,0.12))
# Compare expected data with real data
context("CDD, function plot.cov.cook")
test_that("If function plot.cov.cook works as expected",{
  expect_equal(exp_cdd.data,cdd.data)
})

#.................................  (5) Test plot.cov.cook.par  ...............................
nr.parameters <- plot.cov.cook.par(cook.par.data,cov.par.data,parameters)

# Create expected data
exp_nr.parameters <- 3
# Compare expected data with real data
context("CDD, function plot.cov.cook.par")
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
context("CDD, function plot.ofv")
test_that("If function plot.ofv works as expected",{
  expect_equal(exp_ofv.orig,ofv.orig)
  expect_equal(exp_ofv.est,ofv.est)
})

#.................................  (7) Test delta.ofv.data  ...............................
out_ofv.delta <- delta.ofv.data(cdd.data.all_4,cutoff_delta.ofv=5.1)
out_ofv.delta_1 <- delta.ofv.data(cdd.data.all_4,cutoff_delta.ofv=5.1,outlier_ID=c(19,23,41))
out_ofv.delta_2 <- delta.ofv.data(cdd.data.all_4,cutoff_delta.ofv=5.1,outlier_ID=c(23,41))
out_ofv.delta_3 <- delta.ofv.data(cdd.data.all_4,cutoff_delta.ofv=5.1,outlier_ID=c(19,38))
out_ofv.delta_4 <- delta.ofv.data(cdd.data.all_4,cutoff_delta.ofv=5.1,outlier_ID=c())
out_ofv.delta_5 <- delta.ofv.data(cdd.data.all_5,cutoff_delta.ofv=6.1)
out_ofv.delta_6 <- delta.ofv.data(cdd.data.all_5,cutoff_delta.ofv=9.1)

# unlist
data_plot <- out_ofv.delta$data_plot
delta.ofv_infl <- out_ofv.delta$delta.ofv_infl
row_infl <- out_ofv.delta$row_infl
ID_infl <- out_ofv.delta$ID_infl
fail_ID <- out_ofv.delta$fail_ID
infl_ofv <- out_ofv.delta$infl_ofv

data_plot_5 <- out_ofv.delta_5$data_plot
delta.ofv_infl_5 <- out_ofv.delta_5$delta.ofv_infl
row_infl_5 <- out_ofv.delta_5$row_infl
ID_infl_5 <- out_ofv.delta_5$ID_infl
fail_ID_5 <- out_ofv.delta_5$fail_ID
infl_ofv_5 <- out_ofv.delta_5$infl_ofv
  
data_plot_1 <- out_ofv.delta_1$data_plot
delta.ofv_infl_1 <- out_ofv.delta_1$delta.ofv_infl
row_infl_all_1 <- out_ofv.delta_1$row_infl_all
row_outl_all_1 <- out_ofv.delta_1$row_outl_all
row_outl_infl_1 <- out_ofv.delta_1$row_outl_infl
row_outl_1 <- out_ofv.delta_1$row_outl
row_infl_1 <- out_ofv.delta_1$row_infl
ID_infl_1 <- out_ofv.delta_1$ID_infl
ID_outl_1 <- out_ofv.delta_1$ID_outl
ID_outl_infl_1 <- out_ofv.delta_1$ID_outl_infl
infl_ofv_1 <- out_ofv.delta_1$infl_ofv

data_plot_2 <- out_ofv.delta_2$data_plot
delta.ofv_infl_2 <- out_ofv.delta_2$delta.ofv_infl
row_infl_all_2 <- out_ofv.delta_2$row_infl_all
row_outl_all_2 <- out_ofv.delta_2$row_outl_all
row_outl_infl_2 <- out_ofv.delta_2$row_outl_infl
row_outl_2 <- out_ofv.delta_2$row_outl
row_infl_2 <- out_ofv.delta_2$row_infl
ID_infl_2 <- out_ofv.delta_2$ID_infl
ID_outl_2 <- out_ofv.delta_2$ID_outl
ID_outl_infl_2 <- out_ofv.delta_2$ID_outl_infl
infl_ofv_2 <- out_ofv.delta_2$infl_ofv

data_plot_3 <- out_ofv.delta_3$data_plot
delta.ofv_infl_3 <- out_ofv.delta_3$delta.ofv_infl
row_infl_all_3 <- out_ofv.delta_3$row_infl_all
row_outl_all_3 <- out_ofv.delta_3$row_outl_all
row_outl_infl_3 <- out_ofv.delta_3$row_outl_infl
row_outl_3 <- out_ofv.delta_3$row_outl
row_infl_3 <- out_ofv.delta_3$row_infl
ID_infl_3 <- out_ofv.delta_3$ID_infl
ID_outl_3 <- out_ofv.delta_3$ID_outl
ID_outl_infl_3 <- out_ofv.delta_3$ID_outl_infl
infl_ofv_3 <- out_ofv.delta_3$infl_ofv

data_plot_4 <- out_ofv.delta_4$data_plot
delta.ofv_infl_4 <- out_ofv.delta_4$delta.ofv_infl
row_infl_all_4 <- out_ofv.delta_4$row_infl_all
row_outl_all_4 <- out_ofv.delta_4$row_outl_all
row_outl_infl_4 <- out_ofv.delta_4$row_outl_infl
row_outl_4 <- out_ofv.delta_4$row_outl
row_infl_4 <- out_ofv.delta_4$row_infl
ID_infl_4 <- out_ofv.delta_4$ID_infl
ID_outl_4 <- out_ofv.delta_4$ID_outl
ID_outl_infl_4 <- out_ofv.delta_4$ID_outl_infl
infl_ofv_4 <- out_ofv.delta_4$infl_ofv

data_plot_6 <- out_ofv.delta_6$data_plot
delta.ofv_infl_6 <- out_ofv.delta_6$delta.ofv_infl
row_infl_6 <- out_ofv.delta_6$row_infl
ID_infl_6 <- out_ofv.delta_6$ID_infl
fail_ID_6 <- out_ofv.delta_6$fail_ID
infl_ofv_6 <- out_ofv.delta_6$infl_ofv

# Create expected data
exp_data_plot <- data.frame("ID"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                            "cdd.delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3),
                            "cook.scores"=c(0.3,0.7,0.7,0.9,0.21,0.4,0.03,0.07,1.1,0.12,0.16,0.15,0.38,0.6,0.2,0.4))
exp_delta.ofv_infl <- c(6.3,5.2)
exp_row_infl <- c(4,13)
exp_ID_infl <- c(19,38)
exp_fail_ID <- NULL
exp_infl_ofv <- data.frame("ID"=c(19,38),"cdd.delta.ofv"=c(6.3,5.2))

exp_data_plot_5 <- data.frame("ID"=c(11,16,19,21,23,24,31,34,35,37,38,39,41,42),
                              "cdd.delta.ofv"=c(0.54,0.5,6.3,0.4,3.2,3.2,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3),
                              "cook.scores"=c(0.3,0.7,0.9,0.21,0.4,0.03,1.1,0.12,0.16,0.15,0.38,0.6,0.2,0.4))
exp_delta.ofv_infl_5 <- c(6.3)
exp_row_infl_5 <- c(3)
exp_ID_infl_5 <- c(19)
exp_fail_ID_5 <- c(13,27)
exp_infl_ofv_5 <- data.frame("ID"=c(19),"cdd.delta.ofv"=c(6.3))


exp_delta.ofv_infl_1 <- c(6.3,5.2)
exp_row_infl_all_1 <- c(4,13)
exp_row_outl_all_1 <- c(4,6,15)
exp_row_outl_infl_1 <- c(4)
exp_row_outl_1 <- c(6,15)
exp_row_infl_1 <- c(13)
exp_ID_infl_1 <- c(38)
exp_ID_outl_1 <- c(23,41)
exp_ID_outl_infl_1 <- c(19)

exp_delta.ofv_infl_2 <- c(6.3,5.2)
exp_row_infl_all_2 <- c(4,13)
exp_row_outl_all_2 <- c(6,15)
exp_row_outl_infl_2 <- as.integer(c())
exp_row_outl_2 <- c(6,15)
exp_row_infl_2 <- c(4,13)
exp_ID_infl_2 <- c(19,38)
exp_ID_outl_2 <- c(23,41)
exp_ID_outl_infl_2 <- as.integer(c())

exp_delta.ofv_infl_3 <- c(6.3,5.2)
exp_row_infl_all_3 <- c(4,13)
exp_row_outl_all_3 <- c(4,13)
exp_row_outl_infl_3 <- c(4,13)
exp_row_outl_3 <- as.integer(c())
exp_row_infl_3 <- as.integer(c())
exp_ID_infl_3 <- as.integer(c())
exp_ID_outl_3 <- as.integer(c())
exp_ID_outl_infl_3 <- c(19,38)

exp_delta.ofv_infl_4 <- c(6.3,5.2)
exp_row_infl_all_4 <- c(4,13)
exp_row_outl_all_4 <- as.integer(c())
exp_row_outl_infl_4 <- as.integer(c())
exp_row_outl_4 <- as.integer(c())
exp_row_infl_4 <- c(4,13)
exp_ID_infl_4 <- c(19,38)
exp_ID_outl_4 <- as.integer(c())
exp_ID_outl_infl_4 <- as.integer(c())

exp_data_plot_6 <- data.frame("ID"=c(11,16,19,21,23,24,31,34,35,37,38,39,41,42),
                              "cdd.delta.ofv"=c(0.54,0.5,6.3,0.4,3.2,3.2,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3),
                              "cook.scores"=c(0.3,0.7,0.9,0.21,0.4,0.03,1.1,0.12,0.16,0.15,0.38,0.6,0.2,0.4))
exp_delta.ofv_infl_6 <- numeric(0)
exp_row_infl_6 <- c()
exp_ID_infl_6 <- as.integer(c())
exp_fail_ID_6 <- c(13,27)
exp_infl_ofv_6 <- data.frame(C = c("No influential individuals detected"))
names(exp_infl_ofv_6) <- NULL


# Compare expected data with real data
context("CDD, function delta.ofv.data")
test_that("If function delta.ofv.data works as expected",{
  expect_equal(exp_data_plot,data_plot)
  expect_equal(exp_delta.ofv_infl,delta.ofv_infl)
  expect_equal(exp_row_infl,row_infl)
  expect_equal(exp_ID_infl,ID_infl)
  expect_equal(exp_fail_ID,fail_ID)
  expect_equal(exp_infl_ofv,infl_ofv)
  
  expect_equal(exp_data_plot_5,data_plot_5)
  expect_equal(exp_delta.ofv_infl_5,delta.ofv_infl_5)
  expect_equal(exp_row_infl_5,row_infl_5)
  expect_equal(exp_ID_infl_5,ID_infl_5)
  expect_equal(exp_fail_ID_5,fail_ID_5)
  expect_equal(exp_infl_ofv_5,infl_ofv_5)
  
  expect_equal(exp_data_plot,data_plot_1)
  expect_equal(exp_delta.ofv_infl_1,delta.ofv_infl_1)
  expect_equal(exp_row_infl_all_1,row_infl_all_1)
  expect_equal(exp_row_outl_all_1,row_outl_all_1)
  expect_equal(exp_row_outl_infl_1,row_outl_infl_1)
  expect_equal(exp_row_outl_1,row_outl_1)
  expect_equal(exp_row_infl_1,row_infl_1)
  expect_equal(exp_ID_infl_1,ID_infl_1)
  expect_equal(exp_ID_outl_1,ID_outl_1)
  expect_equal(exp_ID_outl_infl_1,ID_outl_infl_1)
  expect_equal(exp_infl_ofv,infl_ofv_1)
  
  expect_equal(exp_data_plot,data_plot_2)
  expect_equal(exp_delta.ofv_infl_2,delta.ofv_infl_2)
  expect_equal(exp_row_infl_all_2,row_infl_all_2)
  expect_equal(exp_row_outl_all_2,row_outl_all_2)
  expect_equal(exp_row_outl_infl_2,row_outl_infl_2)
  expect_equal(exp_row_outl_2,row_outl_2)
  expect_equal(exp_row_infl_2,row_infl_2)
  expect_equal(exp_ID_infl_2,ID_infl_2)
  expect_equal(exp_ID_outl_2,ID_outl_2)
  expect_equal(exp_ID_outl_infl_2,ID_outl_infl_2)
  expect_equal(exp_infl_ofv,infl_ofv_2)
  
  expect_equal(exp_data_plot,data_plot_3)
  expect_equal(exp_delta.ofv_infl_3,delta.ofv_infl_3)
  expect_equal(exp_row_infl_all_3,row_infl_all_3)
  expect_equal(exp_row_outl_all_3,row_outl_all_3)
  expect_equal(exp_row_outl_infl_3,row_outl_infl_3)
  expect_equal(exp_row_outl_3,row_outl_3)
  expect_equal(exp_row_infl_3,row_infl_3)
  expect_equal(exp_ID_infl_3,ID_infl_3)
  expect_equal(exp_ID_outl_3,ID_outl_3)
  expect_equal(exp_ID_outl_infl_3,ID_outl_infl_3)
  expect_equal(exp_infl_ofv,infl_ofv_3)
  
  expect_equal(exp_data_plot,data_plot_4)
  expect_equal(exp_delta.ofv_infl_4,delta.ofv_infl_4)
  expect_equal(exp_row_infl_all_4,row_infl_all_4)
  expect_equal(exp_row_outl_all_4,row_outl_all_4)
  expect_equal(exp_row_outl_infl_4,row_outl_infl_4)
  expect_equal(exp_row_outl_4,row_outl_4)
  expect_equal(exp_row_infl_4,row_infl_4)
  expect_equal(exp_ID_infl_4,ID_infl_4)
  expect_equal(exp_ID_outl_4,ID_outl_4)
  expect_equal(exp_ID_outl_infl_4,ID_outl_infl_4)
  expect_equal(exp_infl_ofv,infl_ofv_4)
  
  expect_equal(exp_data_plot_6,data_plot_6)
  expect_equal(exp_delta.ofv_infl_6,delta.ofv_infl_6)
  expect_equal(exp_row_infl_6,row_infl_6)
  expect_equal(exp_ID_infl_6,ID_infl_6)
  expect_equal(exp_fail_ID_6,fail_ID_6)
  expect_equal(exp_infl_ofv_6,infl_ofv_6)
})

#.................................  (8) Test all.infl.indiv.table.R  ...............................
#input
# ofv
infl_ofv_1 <- data.frame("ID"=c(3,19,7),"cdd.delta.ofv"=c(4,6.3,4.1))
infl_ofv_2 <- data.frame(C = c("No influential individuals detected"))
names(infl_ofv_2) <- NULL

# cook
infl_cook_data_1 <- data.frame(ID=as.integer(c(11,19)),cook.scores=c(1,6.23))
infl_cook_data_2 <- data.frame(ID=as.integer(c(12,1)),cook.scores=c(1,6.23))
infl_cook_data_3 <- data.frame(C = c("Don't have cov ratios bigger than cutoff."))
names(infl_cook_data_3) <- NULL

# cov
infl_cov_data_1 <- data.frame(ID=as.integer(c(11,13,19)),cov.ratios=c(0.7,0.9,1.2))
infl_cov_data_2 <- data.frame(ID=as.integer(c(11)),cov.ratios=c(0.7))
infl_cov_data_3 <- data.frame(C = c("Don't have cook scores bigger than cutoff."))
names(infl_cov_data_3) <- NULL


#use function
all_infl_indiv_table_1 <- all.infl.indiv.table(infl_ofv_1,infl_cook_data_1,infl_cov_data_1,fail_ID=c())
all_infl_indiv_table_2 <- all.infl.indiv.table(infl_ofv_1,infl_cook_data_2,infl_cov_data_2,fail_ID=c(12,11))
all_infl_indiv_table_3 <- all.infl.indiv.table(infl_ofv_1,infl_cook_data_3,infl_cov_data_1,fail_ID=c())
all_infl_indiv_table_4 <- all.infl.indiv.table(infl_ofv_2,infl_cook_data_3,infl_cov_data_2,fail_ID=c())
all_infl_indiv_table_5 <- all.infl.indiv.table(infl_ofv_2,infl_cook_data_3,infl_cov_data_3,fail_ID=c())
all_infl_indiv_table_6 <- all.infl.indiv.table(infl_ofv,infl_cook_data,infl_cov_data,fail_ID=c())

# Create expected data
exp_all_infl_indiv_table_1 <- data.frame("ID"=as.character(c(3,7,11,13,19)),"Delta OFV influentials"=as.character(c(4,4.1,"","",6.3)),
                                         "Cook score influentials"=as.character(c("","",1,"",6.23)),
                                         "Cov ratio influentials"=as.character(c("","",0.7,0.9,1.2)),check.names = F, stringsAsFactors=FALSE)
exp_all_infl_indiv_table_2 <- data.frame("ID"=as.character(c(1,3,7,11,12,19)),"Delta OFV influentials"=as.character(c("",4,4.1,"NA","NA",6.3)),
                                         "Cook score influentials"=as.character(c(6.23,"","","",1,"")),
                                         "Cov ratio influentials"=as.character(c("","","",0.7,"","")),check.names = F, stringsAsFactors=FALSE)
exp_all_infl_indiv_table_3 <- data.frame("ID"=as.character(c(3,7,11,13,19)),"Delta OFV influentials"=as.character(c(4,4.1,"","",6.3)),
                                         "Cook score influentials"=as.character(c("","","","","")),
                                         "Cov ratio influentials"=as.character(c("","",0.7,0.9,1.2)),check.names = F, stringsAsFactors=FALSE)
exp_all_infl_indiv_table_4 <- data.frame("ID"=as.character(c(11)),"Delta OFV influentials"=as.character(c("")),
                                         "Cook score influentials"=as.character(c("")),
                                         "Cov ratio influentials"=as.character(c(0.7)),check.names = F, stringsAsFactors=FALSE)
exp_all_infl_indiv_table_5 <- data.frame(C = c("No influential individuals detected"))
names(exp_all_infl_indiv_table_5) <- NULL

exp_all_infl_indiv_table_6 <- data.frame("ID"=as.character(c(13,16,19,38)),"Delta OFV influentials"=as.character(c("","",6.3,5.2)),
                                         "Cook score influentials"=as.character(c("",4,6.23,"")),
                                         "Cov ratio influentials"=as.character(c(6.4,7.06,"","")),check.names = F, stringsAsFactors=FALSE)

# Compare expected data with real data
context("CDD, function all.infl.indiv.table")
test_that("If function all.infl.indiv.table works as expected",{
  expect_equal(exp_all_infl_indiv_table_1,all_infl_indiv_table_1)
  expect_equal(exp_all_infl_indiv_table_2,all_infl_indiv_table_2)
  expect_equal(exp_all_infl_indiv_table_3,all_infl_indiv_table_3)
  expect_equal(exp_all_infl_indiv_table_4,all_infl_indiv_table_4)
  expect_equal(exp_all_infl_indiv_table_5,all_infl_indiv_table_5)
  expect_equal(exp_all_infl_indiv_table_6,all_infl_indiv_table_6)
})
