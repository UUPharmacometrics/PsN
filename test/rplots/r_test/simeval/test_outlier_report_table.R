library(testthat)
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

###################################     3. Make a test    ###################################
#input data
# ebe
ebe.npde_outliers_1 <- data.frame(c(14,26,9),c("1,2","1,2,3,4","1"),stringsAsFactors = F)
colnames(ebe.npde_outliers_1) <- c("ID", "Outlying EBE (ETA numbers)")
ebe.npde_outliers_2 <- data.frame()

# ofv
ofv_outliers_1 <- data.frame(ID=c(13,9,14),MEDIAN=c(-4.758828,-4.4,-3.213746))
ofv_outliers_2 <- data.frame(ID=c(13,2),MEDIAN=c(-4.758828,-3.54))
ofv_outliers_3 <- data.frame()
ofv_outliers_4 <- data.frame(ID=c(13,2,14),MEDIAN=c(-4.758828,-4.4,-3.213746))

# residuals
cwres.iwres_outliers_1 <- data.frame(ID=c(14,42),OUTLIERS.CWRES=c(1,1),OUTLIERS.IWRES=c(2,""),stringsAsFactors = F)
cwres.iwres_outliers_2 <- data.frame(ID=c(1,42),OUTLIERS.CWRES=c("",3),OUTLIERS.IWRES=c(2,""),stringsAsFactors = F)
cwres.iwres_outliers_3 <- data.frame()
cwres.iwres_outliers_4 <- data.frame(ID=c(14,2,21),OUTLIERS.IWRES=c(2,"",1),OUTLIERS.CWRES=c("",3,2),stringsAsFactors = F)
cwres.iwres_outliers_5 <- data.frame(ID=c(14,2,21),OUTLIERS.CWRES=c(2,4,1),stringsAsFactors = F)

# use function
all_outlier_table_1 <- all.outlier.report.table(ofv_outliers=ofv_outliers_1,ebe.npde_outliers=ebe.npde_outliers_1,
                                                cwres.iwres_outliers=cwres.iwres_outliers_1,ID_deleted_ebe=c(),
                                                residual_names=c("CWRES","IWRES"),ID_deleted_ofv=c())
all_outlier_table_2 <- all.outlier.report.table(ofv_outliers=ofv_outliers_1,ebe.npde_outliers=ebe.npde_outliers_1,
                                                cwres.iwres_outliers=cwres.iwres_outliers_2,ID_deleted_ebe=c(1),
                                                residual_names=c("CWRES","IWRES"),ID_deleted_ofv=c(42))
all_outlier_table_3 <- all.outlier.report.table(ofv_outliers=ofv_outliers_2,ebe.npde_outliers=ebe.npde_outliers_1,
                                                cwres.iwres_outliers=cwres.iwres_outliers_2,ID_deleted_ebe=c(),
                                                residual_names=c("CWRES","IWRES"),ID_deleted_ofv=c())
all_outlier_table_4 <- all.outlier.report.table(ofv_outliers=ofv_outliers_3,ebe.npde_outliers=ebe.npde_outliers_1,
                                                cwres.iwres_outliers=cwres.iwres_outliers_3,ID_deleted_ebe=c(4),
                                                residual_names=c("CWRES","IWRES"),ID_deleted_ofv=c())
all_outlier_table_5 <- all.outlier.report.table(ofv_outliers=ofv_outliers_3,ebe.npde_outliers=ebe.npde_outliers_2,
                                                cwres.iwres_outliers=cwres.iwres_outliers_3,ID_deleted_ebe=c(),
                                                residual_names=c("CWRES","IWRES"),ID_deleted_ofv=c())
all_outlier_table_6 <- all.outlier.report.table(ofv_outliers=ofv_outliers_3,ebe.npde_outliers=ebe.npde_outliers_1,
                                                cwres.iwres_outliers=cwres.iwres_outliers_3,residual_names=c(),
                                                ID_deleted_ebe=c(4),ID_deleted_ofv=c())
all_outlier_table_7 <- all.outlier.report.table(ofv_outliers=ofv_outliers_4,ebe.npde_outliers=ebe.npde_outliers_1,
                                                cwres.iwres_outliers=cwres.iwres_outliers_4,residual_names=c("CWRES","IWRES"),
                                                ID_deleted_ebe=c(2,13),ID_deleted_ofv=c(9))
all_outlier_table_8 <- all.outlier.report.table(ofv_outliers=ofv_outliers_4,ebe.npde_outliers=ebe.npde_outliers_1,
                                                cwres.iwres_outliers=cwres.iwres_outliers_5,residual_names=c("CWRES"),ID_deleted_ebe=c(2,13),
                                                ID_deleted_ofv=c(9))
all_outlier_table_9 <- all.outlier.report.table(ofv_outliers=ofv_outliers_4,ebe.npde_outliers=ebe.npde_outliers_1,
                                                cwres.iwres_outliers=cwres.iwres_outliers_3,residual_names=c("CWRES"))
all_outlier_table_10 <- all.outlier.report.table(ofv_outliers=ofv_outliers_4,ebe.npde_outliers=ebe.npde_outliers_2,
                                                 cwres.iwres_outliers=cwres.iwres_outliers_3,residual_names=c(),
                                                 ID_deleted_ebe=c(12),ID_deleted_ofv=c(9))
all_outlier_table_11 <- all.outlier.report.table(ofv_outliers=ofv_outliers_3,ebe.npde_outliers=ebe.npde_outliers_2,
                                                 cwres.iwres_outliers=cwres.iwres_outliers_5,residual_names=c("CWRES"),ID_deleted_ebe=c(12),
                                                 ID_deleted_ofv=c(9))
all_outlier_table_12 <- all.outlier.report.table(ofv_outliers=ofv_outliers_3,ebe.npde_outliers=ebe.npde_outliers_2,
                                                 cwres.iwres_outliers=cwres.iwres_outliers_4,residual_names=c("CWRES","IWRES"),ID_deleted_ebe=c(21),
                                                 ID_deleted_ofv=c(2))
# expected data
exp_all_outlier_table_1 <- data.frame(id=as.character(c(9,13,14,26,42)),ofv=c(-4.4,-4.759,-3.214,"",""),
                                    ebe=c("1","","1,2","1,2,3,4",""),iwres=c("","",1,"",1),
                                    cwres=c("","",2,"",""),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_1) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers","IWRES outliers")

exp_all_outlier_table_2 <- data.frame(id=as.character(c(1,9,13,14,26,42)),ofv=c("",-4.4,-4.759,-3.214,"","NA"),
                                      ebe=c("NA","1","","1,2","1,2,3,4",""),iwres=c("","","","","",3),
                                      cwres=c(2,"","","","",""),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_2) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers","IWRES outliers")

exp_all_outlier_table_3 <- data.frame(id=as.character(c(1,2,9,13,14,26,42)),ofv=c("",-3.54,"",-4.759,"","",""),
                                      ebe=c("","","1","","1,2","1,2,3,4",""),iwres=c("","","","","","",3),
                                      cwres=c(2,"","","","","",""),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_3) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers","IWRES outliers")

exp_all_outlier_table_4 <- data.frame(id=as.character(c(9,14,26)),ofv=c("","",""),
                                      ebe=c("1","1,2","1,2,3,4"),iwres=c("","",""),
                                      cwres=c("","",""),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_4) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers","IWRES outliers")

exp_all_outlier_table_5 <- data.frame(C = c("No outliers detected"))
names(exp_all_outlier_table_5) <- NULL

exp_all_outlier_table_6 <- data.frame(id=as.character(c(9,14,26)),ofv=as.character(c("","","")),
                                      ebe=as.character(c("1","1,2","1,2,3,4")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_6) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)")

exp_all_outlier_table_7 <- data.frame(id=as.character(c(2,9,13,14,21,26)),ofv=c("-4.4","NA","-4.759","-3.214","",""),
                                      ebe=c("NA","1","NA","1,2","","1,2,3,4"),cwres=c(3,"","","",2,""),iwres=c("","","",2,1,""),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_7) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers","IWRES outliers")

exp_all_outlier_table_8 <- data.frame(id=as.character(c(2,9,13,14,21,26)),ofv=c("-4.4","NA","-4.759","-3.214","",""),
                                      ebe=c("NA","1","NA","1,2","","1,2,3,4"),cwres=c(4,"","",2,1,""),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_8) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers")

exp_all_outlier_table_9 <- data.frame(id=as.character(c(2,9,13,14,26)),ofv=c("-4.4","","-4.759","-3.214",""),
                                      ebe=c("","1","","1,2","1,2,3,4"),cwres=c(rep("",5)),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_9) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers")

exp_all_outlier_table_10 <- data.frame(id=as.character(c(2,13,14)),ofv=c("-4.4","-4.759","-3.214"),
                                      ebe=c("","",""),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_10) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)")

exp_all_outlier_table_11 <- data.frame(id=as.character(c(2,14,21)),ofv=c("","",""),
                                      ebe=c("","",""),cwres=c("4","2","1"),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_11) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers")

exp_all_outlier_table_12 <- data.frame(id=as.character(c(2,14,21)),ofv=c("NA","",""),
                                       ebe=c("","","NA"),cwres=c(3,"",2),iwres=c("",2,1),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_12) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","CWRES outliers","IWRES outliers")

# Compare expected data with real data
context("simeval, summary, function all.outlier.report.table")
test_that("If function all.outlier.report.table works as expected",{
  expect_equal(exp_all_outlier_table_1,all_outlier_table_1) # all exist and not epmty, no deleted ids (no NA created,equal IDs)
  expect_equal(exp_all_outlier_table_2,all_outlier_table_2) # all exist and not empty, one deleted ofv, one deleted ebenpde (will create NA)
  expect_equal(exp_all_outlier_table_3,all_outlier_table_3) # all exist and not epmty, no deleted ids (no NA created, no equal IDs)
  expect_equal(exp_all_outlier_table_4,all_outlier_table_4) # all exist, empty ofv and residuals (no NA, 2 residual names)
  expect_equal(exp_all_outlier_table_5,all_outlier_table_5) # all exist, all empty (no outliers detected)
  expect_equal(exp_all_outlier_table_6,all_outlier_table_6) # no residuals, ofv empty
  expect_equal(exp_all_outlier_table_7,all_outlier_table_7) # all exist and no empty, some deleted ids (will create NA)
  expect_equal(exp_all_outlier_table_8,all_outlier_table_8) # all exist, but only one residual, some deleted ids (will create NA)
  expect_equal(exp_all_outlier_table_9,all_outlier_table_9) # all exist, but only one residual and empty (no NA)
  expect_equal(exp_all_outlier_table_10,all_outlier_table_10) # no residuals, empty ebenpde
  expect_equal(exp_all_outlier_table_11,all_outlier_table_11) # empty ofv, ebe and only on residual
  expect_equal(exp_all_outlier_table_12,all_outlier_table_12) # empty ofv, ebe and but two residuals (some NA created)
})