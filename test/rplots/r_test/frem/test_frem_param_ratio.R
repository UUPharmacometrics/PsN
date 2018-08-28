################################    1. Library for testing     ##############################
library(testthat)
library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)
tool = 'frem'
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

covdata_file_1 <- paste0(files.w.dir,'covdata_1.csv')
covdata_file_2 <- paste0(files.w.dir,'covdata_2.csv')
pardata_file_1 <- paste0(files.w.dir,'pardata_1.csv')
frem_ratio_1 <- paste0(files.w.dir,'frem_ratio_1.csv')
frem_ratio_2 <- paste0(files.w.dir,'frem_ratio_2.csv')

###################################     3. Make a test    ###################################
#...........................  (1) Test function delete_prefix.R .....................................  
list_covdata_1 <- delete_prefix(covdata=covdata_file_1)
list_covdata_2 <- delete_prefix(covdata=covdata_file_2)

#unlist
covdata_1 <- list_covdata_1$covdata
covariate_1 <- list_covdata_1$covariate
covdata_2 <- list_covdata_2$covdata
covariate_2 <- list_covdata_2$covariate

# Create expected input data
exp_covdata_1 <- data.frame('covname'=c('WT','AGE','SEX','ACE'),
                            'perc5th'=c(3.97029191355,55,1,0),
                            'mean'=c(4.34273242425,65.1756756757,1.2027027027,0.635135135135),
                            'perc95th'=c(4.65396035016,77,2,1),
                            'reference'=c(4.34273242425,65.1756756757,1,1),
                            'is.categorical'=c(0,0,1,1),
                            'unit'=c('','years','',''),
                            'category.reference'=c('','','male',1),
                            'category.other'=c('','','female',0),stringsAsFactors = F)
exp_covariate_1 <- c('WT','AGE','SEX','ACE')
exp_covdata_2 <- data.frame('covname'=c("LNAGE","WGT","LBW","LNCRCO","SEX"),
                            'unit'=c("years","kg","kg",'',''),stringsAsFactors = F)
exp_covariate_2 <- c("AGE","WGT","LBW","CRCO","SEX")

# Compare expected input data with real input data
context("Test function delete_prefix")
test_that("If function delete_prefix works as expected",{
  expect_equal(exp_covdata_1,covdata_1)
  expect_equal(exp_covariate_1,covariate_1)
  expect_equal(exp_covdata_2,covdata_2)
  expect_equal(exp_covariate_2,covariate_2)
})
#...........................  (2) Test function param_round_values.R .....................................  
covdata_3 <- param_round_values(covdata=covdata_1)

# Create expected input data
exp_covdata_3 <- data.frame('covname'=c('WT','AGE','SEX','ACE'),
                            'perc5th'=c(3.97,55,1,0),
                            'mean'=c(4.343,65.18,1.2027027027,0.635135135135),
                            'perc95th'=c(4.654,77,2,1),
                            'reference'=c(4.343,65.18,1,1),
                            'is.categorical'=c(0,0,1,1),
                            'unit'=c('','years','',''),
                            'category.reference'=c('','','male',1),
                            'category.other'=c('','','female',0),stringsAsFactors = F)

# Compare expected input data with real input data
context("Test function param_round_values")
test_that("If function param_round_values works as expected",{
  expect_equal(exp_covdata_3,covdata_3)
})

# #...........................  (3) Test function param_values_for_plots.R .....................................  
list_values_for_plots <- param_values_for_plots(covdata=covdata_3,covariate=covariate_1)

# unlist
point_names <- list_values_for_plots$point_names
point_color <- list_values_for_plots$point_color
MEAN <- list_values_for_plots$MEAN
COVARIATE <- list_values_for_plots$COVARIATE

# Create expected input data
exp_point_names <- c("3.97","4.654","55","77","female","0")
exp_point_color <- c("cov5th","cov95th","cov5th","cov95th","other","other")
exp_MEAN <- c("4.3 ","","65 years","","male","1")
exp_COVARIATE <- c("WT","","AGE","","SEX","ACE")

# Compare expected input data with real input data
context("Test function param_values_for_plots")
test_that("If function param_values_for_plots works as expected",{
  expect_equal(exp_point_names,point_names)
  expect_equal(exp_point_color,point_color)
  expect_equal(exp_MEAN,MEAN)
  expect_equal(exp_COVARIATE,COVARIATE)
})

#...........................  (4) Test function parameter_datas.R .....................................  
list_DF_1 <- parameter_datas(inTable_frem=frem_ratio_1,pardata=pardata_file_1,covdata=covdata_3,covariate=covariate_1)
list_DF_2 <- parameter_datas(inTable_frem=frem_ratio_2,pardata=pardata_file_1,covdata=covdata_3,covariate=covariate_1)

# unlist
DF_1 <- list_DF_1[[1]]
DF_2 <- list_DF_1[[2]]

DF_3 <- list_DF_2[[1]]
DF_4 <- list_DF_2[[2]]

# Create expected input data
exp_DF_1 <- data.frame('WT.cov5th'=c(0.79,0.81,0.8,0.84),'WT.cov95th'=c(1.209,1.19,1.2,1.15),
                       'AGE.cov5th'=c(1.16,1.17,1.18,1.22),'AGE.cov95th'=c(0.83,0.82,0.81,0.79),
                       'SEX.other'=c(0.94,0.96,1.005,1.01),'ACE.other'=c(0.85,0.91,0.77,0.806))
exp_DF_2 <- data.frame('WT.cov5th'=c(0.74,0.71,0.805,0.77),'WT.cov95th'=c(1.27,1.31,1.19,1.23),
                       'AGE.cov5th'=c(1.07,1.108,1.06,1.13),'AGE.cov95th'=c(0.92,0.88,0.92,0.86),
                       'SEX.other'=c(0.809,0.8,0.88,0.84),'ACE.other'=c(0.94,1.002,0.88,0.92))

# Compare expected input data with real input data
context("Test function parameter_data")
test_that("If function parameter_data works as expected",{
  expect_equal(exp_DF_1,DF_1)
  expect_equal(exp_DF_2,DF_2)
  expect_equal(exp_DF_1,DF_3)
  expect_equal(exp_DF_2,DF_4)
})

#...........................  (6) Test function param_plot_text.R .....................................  
list_outTable_text <- param_plot_text(list_outTable)

# unlist
outTable_text_1 <- list_outTable_text[[1]]
outTable_text_2 <- list_outTable_text[[2]]

# Create expected input data
exp_outTable_text_1 <- data.frame('V1'=c('COVARIATE','WT','','AGE','','SEX','ACE','MEAN','4.3 ',
                                         '','65 years','','male','1','EXPECTED',
                                         '-21 %  [-20, -16]','+20.9 %  [+15, +20]',
                                         '+16 %  [+17, +22]','-17 %  [-21, -18]',
                                         '-6 %  [-4, +1]','-15 %  [-23, -9]'),
                                  'V05'=c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3),
                                  'V0'=factor(rep(c(1,2,3,4,5,6,7),3),levels=c(7,6,5,4,3,2,1)))
exp_outTable_text_2 <- data.frame('V1'=c('COVARIATE','WT','','AGE','','SEX','ACE','MEAN','4.3 ',
                                         '','65 years','','male','1','EXPECTED',
                                         '-26 %  [-29, -19.5]','+27 %  [+19, +31]',
                                         '+7 %  [+6, +13]','-8 %  [-14, -8]',
                                         '-19.1 %  [-20, -12]','-6 %  [-12, +0.2]'),
                                  'V05'=c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3),
                                  'V0'=factor(rep(c(1,2,3,4,5,6,7),3),levels=c(7,6,5,4,3,2,1)))

# Compare expected input data with real input data
context("Test function param_plot_text")
test_that("If function param_plot_text works as expected",{
  expect_equal(exp_outTable_text_1,outTable_text_1)
  expect_equal(exp_outTable_text_2,outTable_text_2)
})
