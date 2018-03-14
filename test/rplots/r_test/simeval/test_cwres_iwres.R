library(testthat)
suppressMessages(library(PerformanceAnalytics))
suppressMessages(library(dplyr))
tool = 'simeval'

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

residual.files <- c(paste0(files.w.dir,'summary_cwres.csv'),paste0(files.w.dir,'summary_iwres.csv'))
residual.files_1 <- c(paste0(files.w.dir,'summary_cwres_1.csv'),paste0(files.w.dir,'summary_iwres.csv'))
residual.outliers.file <- paste0(files.w.dir,'residual_outliers.csv')
residual.outliers.file.1 <- paste0(files.w.dir,'residual_outliers_1.csv')
residual.outliers.file.3 <- paste0(files.w.dir,'residual_outliers_3.csv')
residual.outliers.file.4 <- paste0(files.w.dir,'residual_outliers_4.csv')
residual.names <- c('CWRES','IWRES')

###################################     3. Make tests     ###################################
#...............................  (1) Test function histograms.cwres.iwres  ...............................
inf_list <-histograms.cwres.iwres(residual.files,residual.names)

#unlist information
RESIDUAL_1 <- inf_list[[1]]$RESIDUAL
residual_npde_1 <- inf_list[[1]]$residual_npde
ylimit_1 <- round(inf_list[[1]]$ylimit,digits = 6)
xlimit_min_1 <- inf_list[[1]]$xlimit_min
xlimit_max_1 <- inf_list[[1]]$xlimit_max

RESIDUAL_2 <- inf_list[[2]]$RESIDUAL
residual_npde_2 <- inf_list[[2]]$residual_npde
ylimit_2 <- round(inf_list[[2]]$ylimit,digits = 6)
xlimit_min_2 <- inf_list[[2]]$xlimit_min
xlimit_max_2 <- inf_list[[2]]$xlimit_max

# Create expected data set
#from list nr.1
exp_RESIDUAL_1 <- data.frame(ID=as.integer(c(2,2,4,4,4)),MDV=as.integer(c(1,0,0,1,0)),
                             ORIGINAL=c(NA,-1.1762,0.64089,NA,1.2165),
                             NPDE=c(NA,-1.5548,0.7063,NA,2.0537),OUTLIER=c(NA,0,0,NA,0))
exp_residual_npde_1 <- c(-1.5548,0.7063,2.0537)
exp_ylimit_1 <- 1.196277
exp_xlimit_min_1 <- -3
exp_xlimit_max_1 <- 3
#from list nr.2
exp_RESIDUAL_2 <- data.frame(ID=as.integer(c(2,2,4,4,4)),MDV=as.integer(c(1,0,0,1,0)),
                             ORIGINAL=c(NA,-0.12918,0.012028,NA,0.052893),
                             NPDE=c(NA,-1.2816,0.41246,NA,0.77219),OUTLIER=c(NA,0,0,NA,0))
exp_residual_npde_2 <- c(-1.2816,0.41246,0.77219)
exp_ylimit_2 <- 1
exp_xlimit_min_2 <- -3
exp_xlimit_max_2 <- 3

# Compare expected data with real data
context("Simeval, residuals, function histograms.cwres.iwres")
test_that("If function histograms.cwres.iwres works as expected",{
  expect_equal(exp_RESIDUAL_1,RESIDUAL_1)
  expect_equal(exp_residual_npde_1,residual_npde_1)
  expect_equal(exp_ylimit_1,ylimit_1)
  expect_equal(exp_xlimit_min_1,xlimit_min_1)
  expect_equal(exp_xlimit_max_1,xlimit_max_1)
  expect_equal(exp_RESIDUAL_2,RESIDUAL_2)
  expect_equal(exp_residual_npde_2,residual_npde_2)
  expect_equal(exp_ylimit_2,ylimit_2)
  expect_equal(exp_xlimit_min_2,xlimit_min_2)
  expect_equal(exp_xlimit_max_2,xlimit_max_2)
})

#...............................  (2) Test function summary.table  ...............................
mydataframe <- summary.table(residual.files,residual.names)
mydataframe_1 <- summary.table(residual.files_1,residual.names)

# Create expected data set
exp_mydataframe <- data.frame(C1 = c('CWRES','IWRES'), C2 = c(0.402,-0.032),C3=c(0.75,1.00),
                              C4=c(3.325,1.203),C5=c(0.864,0.764),c(-0.298,-0.622),c(-1.5,-1.5),c(0.723,0.315))
exp_mydataframe_1 <- data.frame(C1 = c('CWRES','IWRES'), C2 = c(-0.424,-0.032),C3=c(1,1.00),
                              C4=c(2.556,1.203),C5=c(0.883,0.764),c(0,-0.622),c(-2,-1.5),c(NA,0.315))
names(exp_mydataframe) <- c("NPDE","mean","p-value\n(H_0: mean==0)","variance","p-value\n(H_0: var==1)","skewness","kurtosis","p-value\n(normality)")
names(exp_mydataframe_1) <- c("NPDE","mean","p-value\n(H_0: mean==0)","variance","p-value\n(H_0: var==1)","skewness","kurtosis","p-value\n(normality)")
# Compare expected data with real data
context("Simeval, residuals, function summary.table")
test_that("If function summary.table works as expected",{
  expect_equal(exp_mydataframe,mydataframe)
  expect_equal(exp_mydataframe_1,mydataframe_1)
})

#...............................  (3) Test function outlier.tablee  ...............................
out_list <- outlier.table(residual.outliers.file)
out_list_1 <- outlier.table(residual.outliers.file.1)
out_list_3 <- outlier.table(residual.outliers.file.3)
out_list_4 <- outlier.table(residual.outliers.file.4)
out_list_5 <- outlier.table(paste0(files.w.dir,'residual_outliers_not_exist.csv'))

# Create expected data set
exp_outlierframe <- data.frame(C = c("No residual outliers detected"))
names(exp_outlierframe) <- NULL
exp_outlierframe_1 <- data.frame(ID=c(24,24,42),TIME=c(176,61.5,95.5),DV=c("38.1","16.7","13.9"),
                                 PRED=c("116.400","17.707","52.912"),IWRES=c("-0.3635","-0.2838","-0.4272"),
                                 CWRES=c("-2.1581","-2.4754","-2.3480"),OUTLIER.IWRES=c(1,0,1),OUTLIER.CWRES=c(1,1,0),stringsAsFactors = F)
exp_outliers_count_1 <- data.frame(ID=c(24,42),OUTLIER.CWRES=c(2,""),OUTLIER.IWRES=c(1,1),stringsAsFactors = F)
exp_outlierframe_4 <- data.frame(ID=c(1,56,56),TIME=c(176,61.5,95.5),DV=c("38.1","16.7","13.9"),
                                 PRED=c("116.400","17.707","52.912"),CWRES=c("-2.1581","-2.4754","-2.3480"),
                                 OUTLIER.CWRES=c(1,1,1),stringsAsFactors = F)
exp_outliers_count_4 <- data.frame(ID=c(1,56),OUTLIER.CWRES=c(1,2))
# Compare expected data with real data
context("Simeval, residuals, function outlier.table")
test_that("If function outlier.table works as expected",{
  expect_equal(exp_outlierframe,out_list$outlier_table)
  expect_equal(data.frame(),out_list$outliers_count)
  expect_equal(c("CWRES","IWRES"),out_list$residual_names)
  expect_equal(exp_outlierframe_1,out_list_1$outlier_table)
  expect_equal(exp_outliers_count_1,out_list_1$outliers_count)
  expect_equal(c("CWRES","IWRES"),out_list_1$residual_names)
  expect_equal(exp_outlierframe,out_list_3$outlier_table)
  expect_equal(data.frame(),out_list_3$outliers_count)
  expect_equal(c("CWRES"),out_list_3$residual_names)
  expect_equal(exp_outlierframe_4,out_list_4$outlier_table)
  expect_equal(exp_outliers_count_4,out_list_4$outliers_count)
  expect_equal(c("CWRES"),out_list_4$residual_names)
  expect_equal(data.frame(),out_list_5$outlier_table)
  expect_equal(data.frame(),out_list_5$outliers_count)
  expect_equal(NULL,out_list_5$residual_names)
})