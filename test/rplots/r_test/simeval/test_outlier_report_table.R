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

###################################     3. Make a test    ###################################
#input data
# ebe
ebe.npde_outliers_1 <- data.frame(c(14,26,9),c(-89.624,-51.661,-39.813))
colnames(ebe.npde_outliers_1) <- c("ID", "outlying criteria")

ebe.npde_outliers_2 <- data.frame(C = c("No outliers detected"))
names(ebe.npde_outliers_2) <- NULL

# ofv
ofv_outliers_1 <- data.frame(ID=c(13,9,14),MEDIAN=c(-4.758828,-4.4,-3.213746))
ofv_outliers_2 <- data.frame(ID=c(13,2),MEDIAN=c(-4.758828,-3.54))
ofv_outliers_3 <- data.frame()

# residuals
cwres.iwres_outliers_1 <- data.frame(ID=as.factor(c(14,42)),OUTLIERS.IWRES=as.factor(c(1,1)),OUTLIERS.CWRES= as.factor(c(2,"")))
cwres.iwres_outliers_2 <- data.frame(ID=as.factor(c(1,42)),OUTLIERS.IWRES=as.factor(c("",3)),OUTLIERS.CWRES= as.factor(c(2,"")))
cwres.iwres_outliers_3 <- data.frame()

# use function
all_outlier_table_1 <- all.outlier.report.table(ofv_outliers_1,ebe.npde_outliers_1,cwres.iwres_outliers_1,ID_deleted=c(),ID_deleted_ofv=c())
all_outlier_table_2 <- all.outlier.report.table(ofv_outliers_1,ebe.npde_outliers_1,cwres.iwres_outliers_2,ID_deleted=c(1),ID_deleted_ofv=c(42))
all_outlier_table_3 <- all.outlier.report.table(ofv_outliers_2,ebe.npde_outliers_1,cwres.iwres_outliers_2,ID_deleted=c(),ID_deleted_ofv=c())
all_outlier_table_4 <- all.outlier.report.table(ofv_outliers_3,ebe.npde_outliers_1,cwres.iwres_outliers_3,ID_deleted=c(4),ID_deleted_ofv=c())
all_outlier_table_5 <- all.outlier.report.table(ofv_outliers_3,ebe.npde_outliers_2,cwres.iwres_outliers_3,ID_deleted=c(),ID_deleted_ofv=c())
all_outlier_table_6 <- all.outlier.report.table(ofv_outliers_3,ebe.npde_outliers_1,ID_deleted=c(4),ID_deleted_ofv=c())

# expected data
exp_all_outlier_table_1 <- data.frame(id=as.character(c(9,13,14,26,42)),ofv=as.character(c(-4.4,-4.759,-3.214,"","")),
                                    ebe=as.character(c(-39.813,"",-89.624,-51.661,"")),iwres=as.character(c("","",1,"",1)),
                                    cwres=as.character(c("","",2,"","")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_1) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","IWRES outliers","CWRES outliers")

exp_all_outlier_table_2 <- data.frame(id=as.character(c(1,9,13,14,26,42)),ofv=as.character(c("",-4.4,-4.759,-3.214,"","NA")),
                                      ebe=as.character(c("NA",-39.813,"",-89.624,-51.661,"")),iwres=as.character(c("","","","","",3)),
                                      cwres=as.character(c(2,"","","","","")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_2) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","IWRES outliers","CWRES outliers")

exp_all_outlier_table_3 <- data.frame(id=as.character(c(1,2,9,13,14,26,42)),ofv=as.character(c("",-3.54,"",-4.759,"","","")),
                                      ebe=as.character(c("","",-39.813,"",-89.624,-51.661,"")),iwres=as.character(c("","","","","","",3)),
                                      cwres=as.character(c(2,"","","","","","")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_3) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","IWRES outliers","CWRES outliers")

exp_all_outlier_table_4 <- data.frame(id=as.character(c(9,14,26)),ofv=as.character(c("","","")),
                                      ebe=as.character(c(-39.813,-89.624,-51.661)),iwres=as.character(c("","","")),
                                      cwres=as.character(c("","","")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_4) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","IWRES outliers","CWRES outliers")

exp_all_outlier_table_5 <- data.frame(C = c("No outliers detected"))
names(exp_all_outlier_table_5) <- NULL

exp_all_outlier_table_6 <- data.frame(id=as.character(c(9,14,26)),ofv=as.character(c("","","")),
                                      ebe=as.character(c(-39.813,-89.624,-51.661)),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_6) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)")

# Compare expected data with real data
context("simeval, summary, function all.outlier.report.table")
test_that("If function all.outlier.report.table works as expected",{
  expect_equal(exp_all_outlier_table_1,all_outlier_table_1)
  expect_equal(exp_all_outlier_table_2,all_outlier_table_2)
  expect_equal(exp_all_outlier_table_3,all_outlier_table_3)
  expect_equal(exp_all_outlier_table_4,all_outlier_table_4)
  expect_equal(exp_all_outlier_table_5,all_outlier_table_5)
  expect_equal(exp_all_outlier_table_6,all_outlier_table_6)
})