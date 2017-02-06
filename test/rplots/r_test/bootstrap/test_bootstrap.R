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
included.ids.file_1 <- paste0(files.w.dir,'included_individuals1.csv')
included.ids.file_2 <- paste0(files.w.dir,'included_individuals2.csv')

###################################     4. Make tests     ###################################
#.................................................   (1) Test function cook_cov_calcul   ..................................................................
list_cook.cov <- cook_cov_calcul(raw.results.file,included.ids.file=included.ids.file_1,N.ESTIMATED.PARAMS=4)
list_cook.cov_1 <- cook_cov_calcul(raw.results.file,included.ids.file=included.ids.file_2,N.ESTIMATED.PARAMS=4)

#unlist
raw.results.data <- list_cook.cov$raw.results.data
included.ids.data <- list_cook.cov$included.ids.data
ID <- list_cook.cov$ID
parameter_names <- list_cook.cov$parameter_names
parameter_data <- list_cook.cov$parameter_data
P_orig <- round(list_cook.cov$P_orig,6)
var_param_no_ID <- round(list_cook.cov$var_param_no_ID,6)
list_parameter_data_per_no_ID <- list_cook.cov$list_parameter_data_per_no_ID
data_plots <- round(list_cook.cov$data_plots,6)
failed_cov_ID <- list_cook.cov$failed_cov_ID

raw.results.data_1 <- list_cook.cov_1$raw.results.data
included.ids.data_1 <- list_cook.cov_1$included.ids.data
ID_1 <- list_cook.cov_1$ID
parameter_names_1 <- list_cook.cov_1$parameter_names
parameter_data_1 <- list_cook.cov_1$parameter_data
P_orig_1 <- round(list_cook.cov_1$P_orig,6)
var_param_no_ID_1 <- round(list_cook.cov_1$var_param_no_ID,6)
list_parameter_data_per_no_ID_1 <- list_cook.cov_1$list_parameter_data_per_no_ID
data_plots_1 <- round(list_cook.cov_1$data_plots,6)
failed_cov_ID_1 <- as.integer(list_cook.cov_1$failed_cov_ID)

# Create expected data
exp_raw.results.data <- data.frame(model=as.integer(c(0,1,2,3,4,5,6,7)),ofv=c(730.1,686,740,733,633,716,742,760),
                                   THETA1=c(0.58,0.57,0.6,0.5,0.62,0.5,0.6,0.56),
                                   THETA2=c(1.4,1.28,1.47,1.5,1.43,1.6,1.68,1.56),
                                   OMEGA.1.1.=c(0.11,0.14,0.23,0.09,0.01,0.009,0.103,0.077),
                                   SIGMA.1.1.=c(0.01,0.011,0.011,0.016,0.013,0.02,0.019,0.01),
                                   seTHETA1=c(0.0004,NA,NA,NA,NA,NA,NA,NA))
exp_included.ids.data <- data.frame(V1=as.integer(c(2,7,10,2,11,7,2)),V2=as.integer(c(7,10,10,10,10,11,2)),
                                    V3=as.integer(c(2,11,10,9,11,7,2)),V4=as.integer(c(9,7,7,2,2,7,2)),V5=as.integer(c(10,9,7,2,11,9,2)))
exp_ID <- as.integer(c(2,7,9,10,11))
exp_parameter_names <- c("THETA1","THETA2","OMEGA.1.1.","SIGMA.1.1.")
exp_parameter_data <- data.frame(THETA1=c(0.57,0.6,0.5,0.62,0.5,0.6,0.56),
                                 THETA2=c(1.28,1.47,1.5,1.43,1.6,1.68,1.56),
                                 OMEGA.1.1.=c(0.14,0.23,0.09,0.01,0.009,0.103,0.077),
                                 SIGMA.1.1.=c(0.011,0.011,0.016,0.013,0.02,0.019,0.01))
exp_P_orig <- c(0.002329,0.01669,0.00588,0.000017)
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
exp_var_param_no_ID <- data.frame(THETA1=c(0.003333,0.0036,0.0012,0.0008,0.002425),
                                  THETA2=c(0.0129,0.0079,0.002533,0.0072,0.014558),
                                  OMEGA.1.1.=c(0.005983,0.001519,0.001892,0.000338,0.002869),
                                  SIGMA.1.1.=c(0.000016,0.000026,0.000025,0.00004,0.000007))
exp_data_plots <- data.frame(ID=c(2,7,9,10,11),
                             cook.scores=c(0.036003,0.092542,0.123548,0.107971,0.042705),
                             cov.ratios=c(1.053393,0.548095,0.196174,0.144297,0.432692))
exp_failed_cov_ID <- NULL


exp_included.ids.data_1 <- data.frame(V1=as.integer(c(2,7,10,2,11,7,9)),V2=as.integer(c(7,10,10,10,10,11,2)),
                                    V3=as.integer(c(2,11,9,9,9,10,2)),V4=as.integer(c(9,7,7,2,2,7,2)),V5=as.integer(c(10,9,7,2,11,9,2)))
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
exp_var_param_no_ID_1 <- data.frame(THETA1=c(0.003333,0.0036,NA,NA,0.002425),
                                  THETA2=c(0.0129,0.0079,NA,NA,0.014558),
                                  OMEGA.1.1.=c(0.005983,0.001519,NA,NA,0.002869),
                                  SIGMA.1.1.=c(0.000016,0.000026,NA,NA,0.000007))
exp_data_plots_1 <- data.frame(ID=c(2,7,11),
                             cook.scores=c(0.036003,0.092542,0.042705),
                             cov.ratios=c(1.053393,0.548095,0.432692))
exp_failed_cov_ID_1 <- as.integer(c(9,10))

# Compare expected data with real data
context("Bootstrap, function cook_cov_calcul")
test_that("If function cook_cov_calcul works as expected",{
  expect_identical(exp_raw.results.data,raw.results.data)
  expect_identical(exp_included.ids.data,included.ids.data)
  expect_identical(exp_ID,ID)
  expect_identical(exp_parameter_names,parameter_names)
  expect_identical(exp_parameter_data,parameter_data)
  expect_identical(exp_P_orig,P_orig)
  expect_identical(exp_list_parameter_data_per_no_ID,list_parameter_data_per_no_ID)
  expect_identical(exp_var_param_no_ID,var_param_no_ID)
  expect_identical(exp_data_plots,data_plots)
  expect_identical(exp_failed_cov_ID,failed_cov_ID)
  
  expect_identical(exp_raw.results.data,raw.results.data_1)
  expect_identical(exp_included.ids.data_1,included.ids.data_1)
  expect_identical(exp_ID,ID_1)
  expect_identical(exp_parameter_names,parameter_names_1)
  expect_identical(exp_parameter_data,parameter_data_1)
  expect_identical(exp_P_orig,P_orig_1)
  expect_identical(exp_list_parameter_data_per_no_ID_1,list_parameter_data_per_no_ID_1)
  expect_identical(exp_var_param_no_ID_1,var_param_no_ID_1)
  expect_identical(exp_data_plots_1,data_plots_1)
  expect_identical(exp_failed_cov_ID_1,failed_cov_ID_1)
  })
