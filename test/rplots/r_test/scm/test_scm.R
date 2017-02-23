library(testthat)

tool = 'scm'

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

scmlog_full <- paste0(files.w.dir,'scmlog_full.txt')
scmlog_full_1 <- paste0(files.w.dir,'scmlog_full_1.txt')
scmlog_no_rel_added <- paste0(files.w.dir,'scmlog_no_rel_added.txt')
short_scmlog_full <- paste0(files.w.dir,'short_scmlog_full.txt')
short_scmlog_full_1 <- paste0(files.w.dir,'short_scmlog_full_1.txt')
short_scmlog_no_rel_added <- paste0(files.w.dir,'short_scmlog_no_rel_added.txt')
###################################     3. Make a test    ###################################
#...........................  (1) Test function included_covariates.R .....................................  
list_1 <- included_covariates(scm.short.log=short_scmlog_full)
list_2 <- included_covariates(scm.short.log=short_scmlog_full_1)
list_3 <- included_covariates(scm.short.log=short_scmlog_no_rel_added)

# unlist
log_input_1 <- list_1$log_input
n1_1 <- list_1$n1
log_1 <- list_1$log
log1_1 <- list_1$log1
out_1 <- list_1$out
alphaf_1 <- list_1$alphaf
sign_1 <- list_1$sign
data_1 <- list_1$data
name_1 <- list_1$name

log_input_2 <- list_2$log_input
n1_2 <- list_2$n1
log_2 <- list_2$log
log1_2 <- list_2$log1
out_2 <- list_2$out
alphaf_2 <- list_2$alphaf
sign_2 <- list_2$sign
data_2 <- list_2$data
name_2 <- list_2$name

log_input_3 <- list_3$log_input
n1_3 <- list_3$n1
log_3 <- list_3$log
log1_3 <- list_3$log1
out_3 <- list_3$out
alphaf_3 <- list_3$alphaf
sign_3 <- list_3$sign
data_3 <- list_3$data
name_3 <- list_3$name

# Compare expected input data with real input data
context("SCM, function included_covariates")
test_that("If function included_covariates works as expected",{
  expect_equal(c(10,1),dim(log_input_1))
  expect_equal(2,n1_1)
  expect_equal(2,length(log_1))
  expect_equal(TRUE,(grepl("^VWGT-5",log_1[1])&&(grepl("0.00$",log_1[1]))))
  expect_equal(TRUE,(grepl("^CLWGT-5",log_1[2])&&(grepl("1.38$",log_1[2]))))
  expect_equal(2,length(log1_1))
  expect_equal("list",class(log1_1))
  expect_equal(as.data.frame(matrix(c("VWGT-5","CLWGT-5","PVAL","PVAL","629.2","622.7","622.7","586.5","6.5","36.2",">",">","0.1","5.9",
                        "0","2","YES!","YES!","0.00","1.38"),2,10)),as.data.frame(out_1))
  expect_equal(0.1,alphaf_1)
  expect_equal(c(629.1,622.6),sign_1)
  expect_equal(data.frame(x=c(0,1,1,2,2,3),y=c(629.2,629.2,622.7,622.7,586.5,586.5)),data_1)
  expect_equal(c("none","VWGT-5","CLWGT-5"),name_1)
  
  expect_equal(c(5,1),dim(log_input_2))
  expect_equal(1,n1_2)
  expect_equal(1,length(log_2))
  expect_equal(TRUE,(grepl("^CLWGT-2",log_2[1])&&(grepl("3.5$",log_2[1]))))
  expect_equal(1,length(log1_2))
  expect_equal("list",class(log1_2))
  expect_equal(as.data.frame(matrix(c("CLWGT-2","PVAL","725.6","672.6","52.9",">","3.8","1",
                                      "YES!","3.5"),1,10)),as.data.frame(out_2))
  expect_equal(3.8,alphaf_2)
  expect_equal(721.8,sign_2)
  expect_equal(data.frame(x=c(0,1,1,2),y=c(725.6,725.6,672.6,672.6)),data_2)
  expect_equal(c("none","CLWGT-2"),name_2)
  
  expect_equal(c(3,1),dim(log_input_3))
  expect_equal(3,n1_3)
  expect_equal(3,length(log_3))
  expect_equal(TRUE,(grepl("^CLWGT-2",log_3[1])&&(grepl("0.4$",log_3[1]))))
  expect_equal(TRUE,(grepl("^CLWGT-5",log_3[2])&&(grepl("0.0$",log_3[2]))))
  expect_equal(TRUE,(grepl("^VCVD1-2",log_3[3])&&(grepl("0.006$",log_3[3]))))
  expect_equal(3,length(log1_3))
  expect_equal("list",class(log1_3))
  expect_equal(as.data.frame(matrix(c("CLWGT-2","CLWGT-5","VCVD1-2","PVAL","PVAL","PVAL","725.6","672.9","672.5",
                                      "672.9","672.5","665.0","52.6","0.4","7.4",">",">",">","3.8","0.0","3.8","1","0","1",
                                      "YES!","YES!","YES!","0.4","0.0","0.006"),3,10)),as.data.frame(out_3))
  expect_equal(3.8,alphaf_3)
  expect_equal(c(721.8,669.1,668.7),sign_3)
  expect_equal(data.frame(x=c(0,1,1,2,2,3,3,4),y=c(725.6,725.6,672.9,672.9,672.5,672.5,665.0,665.0)),data_3)
  expect_equal(c("none","CLWGT-2","CLWGT-5","VCVD1-2"),name_3)
})

#...........................  (2) Test function first_inclusion_step.R .....................................  
list_1 <- first_inclusion_step(scm.log.file=scmlog_full)
list_2 <- first_inclusion_step(scm.log.file=scmlog_full_1)
list_3 <- first_inclusion_step(scm.log.file=scmlog_no_rel_added)

# unlist
log_input_1 <- list_1$log_input
n1_1 <- list_1$n1
log_1 <- list_1$log
log1_1 <- list_1$log1
out_1 <- list_1$out
alphaf_1 <- list_1$alphaf
base_1 <- list_1$base
sign_1 <- list_1$sign
data_1 <- list_1$data
name_1 <- list_1$name

log_input_2 <- list_2$log_input
n1_2 <- list_2$n1
log_2 <- list_2$log
log1_2 <- list_2$log1
out_2 <- list_2$out
alphaf_2 <- list_2$alphaf
base_2 <- list_2$base
sign_2 <- list_2$sign
data_2 <- list_2$data
name_2 <- list_2$name

log_input_3 <- list_3$log_input
n1_3 <- list_3$n1
log_3 <- list_3$log
log1_3 <- list_3$log1
out_3 <- list_3$out
alphaf_3 <- list_3$alphaf
base_3 <- list_3$base
sign_3 <- list_3$sign
data_3 <- list_3$data
name_3 <- list_3$name

# Compare expected input data with real input data
context("SCM, function first_inclusion_step")
test_that("If function first_inclusion_step works as expected",{
  expect_equal(c(6,1),dim(log_input_1))
  expect_equal(2,n1_1)
  expect_equal(2,length(log_1))
  expect_equal(TRUE,(grepl("^CLWGT-2",log_1[1])&&(grepl("0.6$",log_1[1]))))
  expect_equal(TRUE,(grepl("^VCVD1-2",log_1[2])&&(grepl("0.005$",log_1[2]))))
  expect_equal(2,length(log1_1))
  expect_equal("list",class(log1_1))
  expect_equal(as.data.frame(matrix(c("CLWGT-2","VCVD1-2","PVAL","PVAL","725.6","725.6","672.9","717.7","52.6","7.8",">",">","3.8","3.8",
                                      "1","1","YES!","YES!","0.6","0.005"),2,10)),as.data.frame(out_1))
  expect_equal(3.8,alphaf_1)
  expect_equal(725.6,base_1)
  expect_equal(c(721.8),sign_1)
  expect_equal(data.frame(x=c(1,2),y=c(672.9,717.7)),data_1)
  expect_equal(c("CLWGT-2","VCVD1-2"),name_1)
  
  expect_equal(c(15,1),dim(log_input_2))
  expect_equal(1,n1_2)
  expect_equal(1,length(log_2))
  expect_equal(TRUE,(grepl("^CLCV1-4",log_2[1])&&(grepl("0.28$",log_2[1]))))
  expect_equal(1,length(log1_2))
  expect_equal("list",class(log1_2))
  expect_equal(as.data.frame(matrix(c("CLCV1-4","PVAL","661.3","662.5","-1.1",">","-3.8","-1","0.28"),1,9)),as.data.frame(out_2))
  expect_equal(-3.8,alphaf_2)
  expect_equal(661.3,base_2)
  expect_equal(665.1,sign_2)
  expect_equal(data.frame(x=c(1),y=c(662.5)),data_2)
  expect_equal("CLCV1-4",name_2)

  expect_equal(c(6,1),dim(log_input_3))
  expect_equal(3,n1_3)
  expect_equal(3,length(log_3))
  expect_equal(TRUE,(grepl("^CLAPGR-3",log_3[1])&&(grepl("0.8$",log_3[1]))))
  expect_equal(TRUE,(grepl("^CLWGT-3",log_3[2])&&(grepl("0.3$",log_3[2]))))
  expect_equal(TRUE,(grepl("^VAPGR-3",log_3[3])&&(grepl("0.2$",log_3[3]))))
  expect_equal(3,length(log1_3))
  expect_equal("list",class(log1_3))
  expect_equal(as.data.frame(matrix(c("CLAPGR-3","CLWGT-3","VAPGR-3","PVAL","PVAL","PVAL","584.2","584.2","584.2",
                                      "584.2","583.4","582.7","0.03","0.7","1.4",">",">",">","3.8","3.8","3.8","1","1","1",
                                      "0.8","0.3","0.2"),3,9)),as.data.frame(out_3))
  expect_equal(3.8,alphaf_3)
  expect_equal(584.2,base_3)
  expect_equal(580.4,sign_3)
  expect_equal(data.frame(x=c(1,2,3),y=c(584.2,583.4,582.7)),data_3)
  expect_equal(c("CLAPGR-3","CLWGT-3","VAPGR-3"),name_3)
})
