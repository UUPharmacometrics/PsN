################################    1. Library for testing     ##############################
library(testthat)
tool = 'bootstrap'
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
raw.results.file_1 <- paste0(files.w.dir,'raw_results_NA.csv')
included.ids.file_1 <- paste0(files.w.dir,'included_individuals1.csv')
included.ids.file_2 <- paste0(files.w.dir,'included_individuals2.csv')

###################################     4. Make tests     ###################################
#.................................................   (1) Test function cook_cov_calcul   ..................................................................
list_cook.cov <- cook_cov_calcul(raw.results.file,included.ids.file=included.ids.file_1,est.param.names=c("THETA1","THETA2","OMEGA.1.1.","SIGMA.1.1."))
list_cook.cov_1 <- cook_cov_calcul(raw.results.file,included.ids.file=included.ids.file_2,est.param.names=c("THETA1","THETA2","OMEGA.1.1.","SIGMA.1.1."))
list_cook.cov_2 <- cook_cov_calcul(raw.results.file_1,included.ids.file=included.ids.file_1,est.param.names=c("THETA1","THETA2","OMEGA.1.1.","SIGMA.1.1."),show.warning=F)

#unlist
raw.results.data <- list_cook.cov$raw.results.data
included.ids.data_input <- list_cook.cov$included.ids.data_input
included.ids.data <- list_cook.cov$included.ids.data
ID <- list_cook.cov$ID
parameter_data <- list_cook.cov$parameter_data
P_orig <- round(list_cook.cov$P_orig,6)
var_param_no_ID <- round(list_cook.cov$var_param_no_ID,6)
list_parameter_data_per_no_ID <- list_cook.cov$list_parameter_data_per_no_ID
data_plots <- round(list_cook.cov$data_plots,6)
failed_cov_ID <- list_cook.cov$failed_cov_ID
samples <- list_cook.cov$samples
estimation_failures <- list_cook.cov$estimation_failures

raw.results.data_1 <- list_cook.cov_1$raw.results.data
included.ids.data_input_1 <- list_cook.cov_1$included.ids.data_input
included.ids.data_1 <- list_cook.cov_1$included.ids.data
ID_1 <- list_cook.cov_1$ID
parameter_data_1 <- list_cook.cov_1$parameter_data
P_orig_1 <- round(list_cook.cov_1$P_orig,6)
var_param_no_ID_1 <- round(list_cook.cov_1$var_param_no_ID,6)
list_parameter_data_per_no_ID_1 <- list_cook.cov_1$list_parameter_data_per_no_ID
data_plots_1 <- round(list_cook.cov_1$data_plots,6)
failed_cov_ID_1 <- as.integer(list_cook.cov_1$failed_cov_ID)
samples_1 <- list_cook.cov_1$samples
estimation_failures_1 <- list_cook.cov_1$estimation_failures

raw.results.data_2 <- list_cook.cov_2$raw.results.data
included.ids.data_input_2 <- list_cook.cov_2$included.ids.data_input
included.ids.data_2 <- list_cook.cov_2$included.ids.data
ID_2 <- list_cook.cov_2$ID
parameter_data_2 <- list_cook.cov_2$parameter_data
P_orig_2 <- round(list_cook.cov_2$P_orig,6)
var_param_no_ID_2 <- round(list_cook.cov_2$var_param_no_ID,6)
list_parameter_data_per_no_ID_2 <- list_cook.cov_2$list_parameter_data_per_no_ID
data_plots_2 <- round(list_cook.cov_2$data_plots,6)
failed_cov_ID_2 <- list_cook.cov_2$failed_cov_ID
samples_2 <- list_cook.cov_2$samples
estimation_failures_2 <- list_cook.cov_2$estimation_failures

# Create expected data
exp_list_parameter_data_per_no_ID <- list(data.frame(THETA1=c(0.6,0.5,0.6),
                                           THETA2=c(1.47,1.5,1.68),
                                           OMEGA.1.1.=c(0.23,0.09,0.103),
                                           SIGMA.1.1.=c(0.011,0.016,0.019)),
                                data.frame(THETA1=c(0.62,0.5,0.56),
                                           THETA2=c(1.43,1.6,1.56),
                                           OMEGA.1.1.=c(0.01,0.009,0.077),
                                           SIGMA.1.1.=c(0.013,0.02,0.01)),
                                data.frame(THETA1=c(0.5,0.5,0.56),
                                           THETA2=c(1.5,1.6,1.56),
                                           OMEGA.1.1.=c(0.09,0.009,0.077),
                                           SIGMA.1.1.=c(0.016,0.02,0.01)),
                                data.frame(THETA1=c(0.6,0.56),
                                           THETA2=c(1.68,1.56),
                                           OMEGA.1.1.=c(0.103,0.077),
                                           SIGMA.1.1.=c(0.019,0.01)),
                                data.frame(THETA1=c(0.57,0.5,0.62,0.56),
                                           THETA2=c(1.28,1.5,1.43,1.56),
                                           OMEGA.1.1.=c(0.14,0.09,0.01,0.077),
                                           SIGMA.1.1.=c(0.011,0.016,0.013,0.01))
                                )



exp_list_parameter_data_per_no_ID_1 <- list(data.frame(THETA1=c(0.6,0.5,0.6),
                                                     THETA2=c(1.47,1.5,1.68),
                                                     OMEGA.1.1.=c(0.23,0.09,0.103),
                                                     SIGMA.1.1.=c(0.011,0.016,0.019)),
                                          data.frame(THETA1=c(0.62,0.5,0.56),
                                                     THETA2=c(1.43,1.6,1.56),
                                                     OMEGA.1.1.=c(0.01,0.009,0.077),
                                                     SIGMA.1.1.=c(0.013,0.02,0.01)),
                                          NULL,
                                          data.frame(THETA1=c(0.56),
                                                     THETA2=c(1.56),
                                                     OMEGA.1.1.=c(0.077),
                                                     SIGMA.1.1.=c(0.01)),
                                          data.frame(THETA1=c(0.57,0.5,0.62,0.56),
                                                     THETA2=c(1.28,1.5,1.43,1.56),
                                                     OMEGA.1.1.=c(0.14,0.09,0.01,0.077),
                                                     SIGMA.1.1.=c(0.011,0.016,0.013,0.01))
)

exp_list_parameter_data_per_no_ID_2 <- list(data.frame(THETA1=c(0.5,0.6),
                                                       THETA2=c(1.5,1.68),
                                                       OMEGA.1.1.=c(0.09,0.103),
                                                       SIGMA.1.1.=c(0.016,0.019)),
                                          data.frame(THETA1=c(0.62,0.56),
                                                     THETA2=c(1.43,1.56),
                                                     OMEGA.1.1.=c(0.01,0.077),
                                                     SIGMA.1.1.=c(0.013,0.01)),
                                          data.frame(THETA1=c(0.5,0.56),
                                                     THETA2=c(1.5,1.56),
                                                     OMEGA.1.1.=c(0.09,0.077),
                                                     SIGMA.1.1.=c(0.016,0.01)),
                                          data.frame(THETA1=c(0.6,0.56),
                                                     THETA2=c(1.68,1.56),
                                                     OMEGA.1.1.=c(0.103,0.077),
                                                     SIGMA.1.1.=c(0.019,0.01)),
                                          data.frame(THETA1=c(0.57,0.5,0.62,0.56),
                                                     THETA2=c(1.28,1.5,1.43,1.56),
                                                     OMEGA.1.1.=c(0.14,0.09,0.01,0.077),
                                                     SIGMA.1.1.=c(0.011,0.016,0.013,0.01))
)

# Compare expected data with real data
context("Bootstrap, function cook_cov_calcul")
test_that("If function cook_cov_calcul works as expected",{
  expect_equal(data.frame(model=as.integer(c(0,1,2,3,4,5,6,7)),ofv=c(730.1,686,740,733,633,716,742,760),
                          dofv=c(4,3,2,5,6,4,3,2),
                          THETA1=c(0.58,0.57,0.6,0.5,0.62,0.5,0.6,0.56),
                          THETA2=c(1.4,1.28,1.47,1.5,1.43,1.6,1.68,1.56),
                          THETA3=c(0,0,0,0,0,0,0,0),
                          OMEGA.1.1.=c(0.11,0.14,0.23,0.09,0.01,0.009,0.103,0.077),
                          OMEGA.2.2.=c(0.11,0.14,0.23,0.09,0.01,0.009,0.103,0.077),
                          SIGMA.1.1.=c(0.01,0.011,0.011,0.016,0.013,0.02,0.019,0.01),
                          seTHETA1=c(0.04,NA,NA,NA,NA,NA,NA,NA)),raw.results.data)
  expect_identical(data.frame(V1=as.integer(c(2,7,10,2,11,7,2)),
                              V2=as.integer(c(7,10,10,10,10,11,2)),
                              V3=as.integer(c(2,11,10,9,11,7,2)),
                              V4=as.integer(c(9,7,7,2,2,7,2)),
                              V5=as.integer(c(10,9,7,2,11,9,2))),included.ids.data_input)
  expect_identical(data.frame(V1=as.integer(c(2,7,10,2,11,7,2)),
                              V2=as.integer(c(7,10,10,10,10,11,2)),
                              V3=as.integer(c(2,11,10,9,11,7,2)),
                              V4=as.integer(c(9,7,7,2,2,7,2)),
                              V5=as.integer(c(10,9,7,2,11,9,2))),included.ids.data)
  expect_identical(as.integer(c(2,7,9,10,11)),ID)
  expect_identical(data.frame(THETA1=c(0.57,0.6,0.5,0.62,0.5,0.6,0.56),
                              THETA2=c(1.28,1.47,1.5,1.43,1.6,1.68,1.56),
                              OMEGA.1.1.=c(0.14,0.23,0.09,0.01,0.009,0.103,0.077),
                              SIGMA.1.1.=c(0.011,0.011,0.016,0.013,0.02,0.019,0.01)),parameter_data)
  expect_identical(c(0.564286,1.502857,0.094143,0.014286),P_orig)
  expect_identical(exp_list_parameter_data_per_no_ID,list_parameter_data_per_no_ID)
  expect_identical(data.frame(THETA1=c(0.003333,0.0036,0.0012,0.0008,0.002425),
                              THETA2=c(0.0129,0.0079,0.002533,0.0072,0.014558),
                              OMEGA.1.1.=c(0.005983,0.001519,0.001892,0.000338,0.002869),
                              SIGMA.1.1.=c(0.000016,0.000026,0.000025,0.00004,0.000007)),var_param_no_ID)
  expect_identical(data.frame(ID=c(2,7,9,10,11),
                              cook.scores=c(0.758441,0.841988,1.129236,0.96639,0.67066),
                              cov.ratios=c(1.053393,0.548095,0.196174,0.144297,0.432692)),data_plots)
  expect_identical(NULL,failed_cov_ID)
  expect_identical(as.integer(7),samples)
  expect_identical(as.integer(0),estimation_failures)

  expect_equal(data.frame(model=as.integer(c(0,1,2,3,4,5,6,7)),ofv=c(730.1,686,740,733,633,716,742,760),
                          dofv=c(4,3,2,5,6,4,3,2),
                          THETA1=c(0.58,0.57,0.6,0.5,0.62,0.5,0.6,0.56),
                          THETA2=c(1.4,1.28,1.47,1.5,1.43,1.6,1.68,1.56),
                          THETA3=c(0,0,0,0,0,0,0,0),
                          OMEGA.1.1.=c(0.11,0.14,0.23,0.09,0.01,0.009,0.103,0.077),
                          OMEGA.2.2.=c(0.11,0.14,0.23,0.09,0.01,0.009,0.103,0.077),
                          SIGMA.1.1.=c(0.01,0.011,0.011,0.016,0.013,0.02,0.019,0.01),
                          seTHETA1=c(0.04,NA,NA,NA,NA,NA,NA,NA)),raw.results.data_1)
  expect_identical(data.frame(V1=as.integer(c(2,7,10,2,11,7,9)),
                              V2=as.integer(c(7,10,10,10,10,11,2)),
                              V3=as.integer(c(2,11,9,9,9,10,2)),
                              V4=as.integer(c(9,7,7,2,2,7,2)),
                              V5=as.integer(c(10,9,7,2,11,9,2))),included.ids.data_input_1)
  expect_identical(data.frame(V1=as.integer(c(2,7,10,2,11,7,9)),
                              V2=as.integer(c(7,10,10,10,10,11,2)),
                              V3=as.integer(c(2,11,9,9,9,10,2)),
                              V4=as.integer(c(9,7,7,2,2,7,2)),
                              V5=as.integer(c(10,9,7,2,11,9,2))),included.ids.data_1)
  expect_identical(as.integer(c(2,7,9,10,11)),ID_1)
  expect_identical(data.frame(THETA1=c(0.57,0.6,0.5,0.62,0.5,0.6,0.56),
                              THETA2=c(1.28,1.47,1.5,1.43,1.6,1.68,1.56),
                              OMEGA.1.1.=c(0.14,0.23,0.09,0.01,0.009,0.103,0.077),
                              SIGMA.1.1.=c(0.011,0.011,0.016,0.013,0.02,0.019,0.01)),parameter_data_1)
  expect_identical(c(0.564286,1.502857,0.094143,0.014286),P_orig_1)
  expect_identical(exp_list_parameter_data_per_no_ID_1,list_parameter_data_per_no_ID_1)
  expect_identical(data.frame(THETA1=c(0.003333,0.0036,NA,NA,0.002425),
                              THETA2=c(0.0129,0.0079,NA,NA,0.014558),
                              OMEGA.1.1.=c(0.005983,0.001519,NA,NA,0.002869),
                              SIGMA.1.1.=c(0.000016,0.000026,NA,NA,0.000007)),var_param_no_ID_1)
  expect_identical(data.frame(ID=c(2,7,11),
                              cook.scores=c(0.758441,0.841988,0.67066),
                              cov.ratios=c(1.053393,0.548095,0.432692)),data_plots_1)
  expect_identical(as.integer(c(9,10)),failed_cov_ID_1)
  expect_identical(as.integer(7),samples_1)
  expect_identical(as.integer(0),estimation_failures_1)
  
  expect_equal(data.frame(model=as.integer(c(0,1,2,3,4,5,6,7)),ofv=c(730.1,686,NA,733,633,NA,742,760),
                          dofv=c(4,3,NA,5,6,NA,3,2),
                          THETA1=c(0.58,0.57,NA,0.5,0.62,NA,0.6,0.56),
                          THETA2=c(1.4,1.28,NA,1.5,1.43,NA,1.68,1.56),
                          THETA3=c(0,0,NA,0,0,NA,0,0),
                          OMEGA.1.1.=c(0.11,0.14,NA,0.09,0.01,NA,0.103,0.077),
                          OMEGA.2.2.=c(0.11,0.14,NA,0.09,0.01,NA,0.103,0.077),
                          SIGMA.1.1.=c(0.01,0.011,NA,0.016,0.013,NA,0.019,0.01),
                          seTHETA1=c(0.04,NA,NA,NA,NA,NA,NA,NA)),raw.results.data_2)
  expect_identical(data.frame(V1=as.integer(c(2,7,10,2,11,7,2)),
                              V2=as.integer(c(7,10,10,10,10,11,2)),
                              V3=as.integer(c(2,11,10,9,11,7,2)),
                              V4=as.integer(c(9,7,7,2,2,7,2)),
                              V5=as.integer(c(10,9,7,2,11,9,2))),included.ids.data_input_2)
  expect_identical(data.frame(V1=as.integer(c(2,10,2,7,2)),
                              V2=as.integer(c(7,10,10,11,2)),
                              V3=as.integer(c(2,10,9,7,2)),
                              V4=as.integer(c(9,7,2,7,2)),
                              V5=as.integer(c(10,7,2,9,2))),included.ids.data_2)
  expect_identical(as.integer(c(2,7,9,10,11)),ID_2)
  expect_identical(data.frame(THETA1=c(0.57,0.5,0.62,0.6,0.56),
                              THETA2=c(1.28,1.5,1.43,1.68,1.56),
                              OMEGA.1.1.=c(0.14,0.09,0.01,0.103,0.077),
                              SIGMA.1.1.=c(0.011,0.016,0.013,0.019,0.01)),parameter_data_2)
  expect_identical(c(0.57,1.49,0.084,0.0138),P_orig_2)
  expect_identical(exp_list_parameter_data_per_no_ID_2,list_parameter_data_per_no_ID_2)
  expect_identical(data.frame(THETA1=c(0.005,0.0018,0.0018,0.0008,0.002425),
                              THETA2=c(0.0162,0.00845,0.0018,0.0072,0.014558),
                              OMEGA.1.1.=c(0.000084,0.002244,0.000084,0.000338,0.002869),
                              SIGMA.1.1.=c(0.000004,0.000004,0.000018,0.00004,0.000007)),var_param_no_ID_2)
  expect_identical(data.frame(ID=c(2,7,9,10,11),
                              cook.scores=c(1.307362,1.141081,0.938511,0.927655,0.511605),
                              cov.ratios=c(0.14593,0.32591,0.058372,0.233488,0.700144)),data_plots_2)
  expect_identical(NULL,failed_cov_ID_2)
  expect_identical(as.integer(7),samples_2)
  expect_identical(as.integer(2),estimation_failures_2)
  })

context("Bootstrap, messages from function cook_cov_calcul")
test_that("messages shows up",{
  expect_message(cook_cov_calcul(raw.results.file_1,included.ids.file=included.ids.file_1,est.param.names=c("THETA1","THETA2","OMEGA.1.1.","SIGMA.1.1.")))  
})
