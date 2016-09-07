library(testthat)

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

all.iofv.file <- paste0(files.w.dir,'raw_all_iofv.csv')
residual.outliers.file <- paste0(files.w.dir,'residual_outliers.csv')
residual.outliers.file.1 <- paste0(files.w.dir,'residual_outliers_1.csv')
residual.outliers.file.2 <- paste0(files.w.dir,'residual_outliers_2.csv')
ebe.npde.file <- paste0(files.w.dir,'ebe_npde.csv')
###################################     3. Make a test    ###################################
#...........................  (1) Test function all.outlier.report.table.R .....................................  
all_outlier_table <- all.outlier.report.table(all.iofv.file=all.iofv.file,
                                              n.subjects=4,samples=3,ofv_outlier_criteria=1,
                                              ebe.npde.file=ebe.npde.file,n.eta=2,outlying_criteria=1,
                                              residual.outliers.file.1)
all_outlier_table_1 <- all.outlier.report.table(all.iofv.file=all.iofv.file,
                                              n.subjects=4,samples=3,ofv_outlier_criteria=2, # ofv no outliers
                                              ebe.npde.file=ebe.npde.file,n.eta=2,outlying_criteria=1,
                                              residual.outliers.file.1)
all_outlier_table_2 <- all.outlier.report.table(all.iofv.file=all.iofv.file,
                                              n.subjects=4,samples=3,ofv_outlier_criteria=1,
                                              ebe.npde.file=ebe.npde.file,n.eta=2,outlying_criteria=1,
                                              residual.outliers.file) # no residuals
all_outlier_table_3 <- all.outlier.report.table(all.iofv.file=all.iofv.file,
                                              n.subjects=4,samples=3,ofv_outlier_criteria=1,
                                              ebe.npde.file=ebe.npde.file,n.eta=2,outlying_criteria=-2, # no ebe.npde outliers
                                              residual.outliers.file.1)
all_outlier_table_4 <- all.outlier.report.table(all.iofv.file=all.iofv.file, # no outliers
                                              n.subjects=4,samples=3,ofv_outlier_criteria=2,# no outliers
                                              ebe.npde.file=ebe.npde.file,n.eta=2,outlying_criteria=-2,# no outliers
                                              residual.outliers.file)# no outliers
all_outlier_table_5 <- all.outlier.report.table(all.iofv.file=all.iofv.file,
                                              n.subjects=4,samples=3,ofv_outlier_criteria=1,
                                              ebe.npde.file=ebe.npde.file,n.eta=2,outlying_criteria=1,
                                              residual.outliers.file.2)
# Create expected input data
exp_all_outlier_table <- data.frame(id=as.factor(c(1,13,24,42,56)),ofv=as.factor(c(1.913,-1.236," "," "," ")),
                                    ebe=as.factor(c(" "," "," "," ",0.302)),iwres=as.factor(c(" "," ",1,1," ")),
                                    cwres=as.factor(c(" "," ",2," "," ")))
colnames(exp_all_outlier_table) <- c("ID","OFV outliers","EBE NPDE outliers","IWRES outliers","CWRES outliers")

exp_all_outlier_table_1 <- data.frame(id=as.factor(c(24,42,56)),ofv=as.factor(c(" "," "," ")),
                                    ebe=as.factor(c(" "," ",0.302)),iwres=as.factor(c(1,1," ")),
                                    cwres=as.factor(c(2," "," ")))
colnames(exp_all_outlier_table_1) <- c("ID","OFV outliers","EBE NPDE outliers","IWRES outliers","CWRES outliers")

exp_all_outlier_table_2 <- data.frame(id=as.factor(c(1,13,56)),ofv=as.factor(c(1.913,-1.236," ")),
                                    ebe=as.factor(c(" "," ",0.302)),iwres=as.factor(c(" "," "," ")),
                                    cwres=as.factor(c(" "," "," ")))
colnames(exp_all_outlier_table_2) <- c("ID","OFV outliers","EBE NPDE outliers","IWRES outliers","CWRES outliers")

exp_all_outlier_table_3 <- data.frame(id=as.factor(c(1,13,24,42)),ofv=as.factor(c(1.913,-1.236," "," ")),
                                    ebe=as.factor(c(" "," "," "," ")),iwres=as.factor(c(" "," ",1,1)),
                                    cwres=as.factor(c(" "," ",2," ")))
colnames(exp_all_outlier_table_3) <- c("ID","OFV outliers","EBE NPDE outliers","IWRES outliers","CWRES outliers")

exp_all_outlier_table_4 <- data.frame(C = c("No outliers detected"))
colnames(exp_all_outlier_table_4) <- NULL

exp_all_outlier_table_5 <- data.frame(id=as.factor(c(1,13,56)),ofv=as.factor(c(1.913,-1.236," ")),
                                    ebe=as.factor(c(" "," ",0.302)),iwres=as.factor(c(1," ",3)),
                                    cwres=as.factor(c(" "," ",2)))
colnames(exp_all_outlier_table_5) <- c("ID","OFV outliers","EBE NPDE outliers","IWRES outliers","CWRES outliers")
# Compare expected input data with real input data
context("Test function all.outlier.report.table")
test_that("If function all.outlier.report.table works as expected",{
  expect_equal(exp_all_outlier_table,all_outlier_table)
  expect_equal(exp_all_outlier_table_1,all_outlier_table_1)
  expect_equal(exp_all_outlier_table_2,all_outlier_table_2)
  expect_equal(exp_all_outlier_table_3,all_outlier_table_3)
  expect_equal(exp_all_outlier_table_4,all_outlier_table_4)
  expect_equal(exp_all_outlier_table_5,all_outlier_table_5)
})