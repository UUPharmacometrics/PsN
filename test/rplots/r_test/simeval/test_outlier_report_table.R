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
all_outlier_table_1 <- all.outlier.report.table(ofv_outliers_1,ebe.npde_outliers_1,cwres.iwres_outliers_1,ID_deleted=c())
all_outlier_table_2 <- all.outlier.report.table(ofv_outliers_1,ebe.npde_outliers_1,cwres.iwres_outliers_2,ID_deleted=c(1))
all_outlier_table_3 <- all.outlier.report.table(ofv_outliers_2,ebe.npde_outliers_1,cwres.iwres_outliers_2,ID_deleted=c())
all_outlier_table_4 <- all.outlier.report.table(ofv_outliers_3,ebe.npde_outliers_1,cwres.iwres_outliers_3,ID_deleted=c(4))
all_outlier_table_5 <- all.outlier.report.table(ofv_outliers_3,ebe.npde_outliers_2,cwres.iwres_outliers_3,ID_deleted=c())

# expected data
exp_all_outlier_table_1 <- data.frame(id=as.character(c(9,13,14,26,42)),ofv=as.character(c(-4.4,-4.759,-3.214,"","")),
                                    ebe=as.character(c(-39.813,"",-89.624,-51.661,"")),iwres=as.character(c("","",1,"",1)),
                                    cwres=as.character(c("","",2,"","")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_1) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")

exp_all_outlier_table_2 <- data.frame(id=as.character(c(1,9,13,14,26,42)),ofv=as.character(c("",-4.4,-4.759,-3.214,"","")),
                                      ebe=as.character(c("NA",-39.813,"",-89.624,-51.661,"")),iwres=as.character(c("","","","","",3)),
                                      cwres=as.character(c(2,"","","","","")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_2) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")

exp_all_outlier_table_3 <- data.frame(id=as.character(c(1,2,9,13,14,26,42)),ofv=as.character(c("",-3.54,"",-4.759,"","","")),
                                      ebe=as.character(c("","",-39.813,"",-89.624,-51.661,"")),iwres=as.character(c("","","","","","",3)),
                                      cwres=as.character(c(2,"","","","","","")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_3) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")

exp_all_outlier_table_4 <- data.frame(id=as.character(c(9,14,26)),ofv=as.character(c("","","")),
                                      ebe=as.character(c(-39.813,-89.624,-51.661)),iwres=as.character(c("","","")),
                                      cwres=as.character(c("","","")),stringsAsFactors=FALSE)
colnames(exp_all_outlier_table_4) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")

exp_all_outlier_table_5 <- data.frame(C = c("No outliers detected"))
names(exp_all_outlier_table_5) <- NULL

# Compare expected data with real data
context("simeval, summary, function all.outlier.report.table")
test_that("If function all.outlier.report.table works as expected",{
  expect_equal(exp_all_outlier_table_1,all_outlier_table_1)
  expect_equal(exp_all_outlier_table_2,all_outlier_table_2)
  expect_equal(exp_all_outlier_table_3,all_outlier_table_3)
  expect_equal(exp_all_outlier_table_4,all_outlier_table_4)
  expect_equal(exp_all_outlier_table_5,all_outlier_table_5)
})

















# #...........................  (1) Test function all.outlier.report.table.R .....................................  
# all_outlier_table <- all.outlier.report.table(all.iofv.file=all.iofv.file_2,
#                                               n.subjects=4,samples=3,
#                                               ebe.npde.file=ebe.npde.file,iiv.eta.names=iiv.eta.names,outlying_criteria=1,
#                                               residual.outliers.file.1)
# all_outlier_table_1 <- all.outlier.report.table(all.iofv.file=all.iofv.file,
#                                               n.subjects=4,samples=3, # ofv no outliers
#                                               ebe.npde.file=ebe.npde.file,iiv.eta.names=iiv.eta.names,outlying_criteria=1,
#                                               residual.outliers.file.1)
# all_outlier_table_2 <- all.outlier.report.table(all.iofv.file=all.iofv.file_2,
#                                               n.subjects=4,samples=3,
#                                               ebe.npde.file=ebe.npde.file,iiv.eta.names=iiv.eta.names,outlying_criteria=1,
#                                               residual.outliers.file) # no residuals
# all_outlier_table_3 <- all.outlier.report.table(all.iofv.file=all.iofv.file_2,
#                                               n.subjects=4,samples=3,
#                                               ebe.npde.file=ebe.npde.file,iiv.eta.names=iiv.eta.names,outlying_criteria=-2, # no ebe.npde outliers
#                                               residual.outliers.file.1)
# all_outlier_table_4 <- all.outlier.report.table(all.iofv.file=all.iofv.file, # no outliers
#                                               n.subjects=4,samples=3,
#                                               ebe.npde.file=ebe.npde.file,iiv.eta.names=iiv.eta.names,outlying_criteria=-2,# no outliers
#                                               residual.outliers.file)# no outliers
# all_outlier_table_5 <- all.outlier.report.table(all.iofv.file=all.iofv.file_2,
#                                               n.subjects=4,samples=3,
#                                               ebe.npde.file=ebe.npde.file,iiv.eta.names=iiv.eta.names,outlying_criteria=1,
#                                               residual.outliers.file.2)
# all_outlier_table_6 <- all.outlier.report.table(all.iofv.file=all.iofv.file_2,
#                                               n.subjects=4,samples=3,
#                                               ebe.npde.file=ebe.npde.file_1,iiv.eta.names=iiv.eta.names_1,outlying_criteria=1.1,
#                                               residual.outliers.file=residual.outliers.file.1)
# all_outlier_table_7 <- all.outlier.report.table(all.iofv.file=all.iofv.file,
#                                                 n.subjects=4,samples=3,
#                                                 ebe.npde.file=ebe.npde.file_1,iiv.eta.names=iiv.eta.names_1,outlying_criteria=1.1,
#                                                 residual.outliers.file)
# all_outlier_table_8 <- all.outlier.report.table(all.iofv.file=all.iofv.file,
#                                                 n.subjects=4,samples=3, # ofv no outliers, #no ebe
#                                                 ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_2,outlying_criteria=1.1,
#                                                 residual.outliers.file.1)
# all_outlier_table_9 <- all.outlier.report.table(all.iofv.file=all.iofv.file_2,
#                                                 n.subjects=4,samples=3,
#                                                 ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_2,outlying_criteria=1.1,
#                                                 residual.outliers.file=residual.outliers.file.1)
# 
# # Create expected input data
# exp_all_outlier_table <- data.frame(id=as.factor(c(1,13,24,42,56)),ofv=as.factor(c(9.806,-4.759,"","","")),
#                                     ebe=as.factor(c("","","","",0.302)),iwres=as.factor(c("","",1,1,"")),
#                                     cwres=as.factor(c("","",2,"","")))
# colnames(exp_all_outlier_table) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")
# 
# exp_all_outlier_table_1 <- data.frame(id=as.factor(c(24,42,56)),ofv=as.factor(c("","","")),
#                                     ebe=as.factor(c("","",0.302)),iwres=as.factor(c(1,1,"")),
#                                     cwres=as.factor(c(2,"","")))
# colnames(exp_all_outlier_table_1) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")
# 
# exp_all_outlier_table_2 <- data.frame(id=as.factor(c(1,13,56)),ofv=as.factor(c(9.806,-4.759,"")),
#                                     ebe=as.factor(c("","",0.302)),iwres=as.factor(c("","","")),
#                                     cwres=as.factor(c("","","")))
# colnames(exp_all_outlier_table_2) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")
# 
# exp_all_outlier_table_3 <- data.frame(id=as.factor(c(1,13,24,42)),ofv=as.factor(c(9.806,-4.759,"","")),
#                                     ebe=as.factor(c("","","","")),iwres=as.factor(c("","",1,1)),
#                                     cwres=as.factor(c("","",2,"")))
# colnames(exp_all_outlier_table_3) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")
# 
# exp_all_outlier_table_4 <- data.frame(C = c("No outliers detected"))
# colnames(exp_all_outlier_table_4) <- NULL
# 
# exp_all_outlier_table_5 <- data.frame(id=as.factor(c(1,13,56)),ofv=as.factor(c(9.806,-4.759,"")),
#                                     ebe=as.factor(c("","",0.302)),iwres=as.factor(c(1,"",3)),
#                                     cwres=as.factor(c("","",2)))
# colnames(exp_all_outlier_table_5) <- c("ID","OFV outliers (SD)","EBE NPDE outliers","IWRES outliers","CWRES outliers")
# 
# exp_all_outlier_table_6 <- data.frame(id=as.factor(c(1,11,13,24,42)),ofv=as.factor(c(9.806,"",-4.759,"","")),
#                                     ebe1=as.factor(c("","NA","NA","","")),ebe2=as.factor(c("",0.548,"","","")),
#                                     iwres=as.factor(c("","","",1,1)),cwres=as.factor(c("","","",2,"")))
# colnames(exp_all_outlier_table_6) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (1)","EBE NPDE outliers (2)","IWRES outliers","CWRES outliers")
# 
# exp_all_outlier_table_7 <- data.frame(id=as.factor(c(11)),ofv=as.factor(c("")),
#                                       ebe1=as.factor(c("NA")),ebe2=as.factor(c(0.548)),
#                                       iwres=as.factor(c("")),cwres=as.factor(c("")))
# colnames(exp_all_outlier_table_7) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (1)","EBE NPDE outliers (2)","IWRES outliers","CWRES outliers")
# 
# exp_all_outlier_table_8 <- data.frame(id=as.factor(c(24,42)),ofv=as.factor(c("","")),
#                                       ebe1=as.factor(c("","")),
#                                       iwres=as.factor(c(1,1)),
#                                       cwres=as.factor(c(2,"")))
# colnames(exp_all_outlier_table_8) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (1)","IWRES outliers","CWRES outliers")
# 
# exp_all_outlier_table_9 <- data.frame(id=as.factor(c(1,13,24,42)),ofv=as.factor(c(9.806,-4.759,"","")),
#                                       ebe1=as.factor(c("","NA","","")),
#                                       iwres=as.factor(c("","",1,1)),cwres=as.factor(c("","",2,"")))
# colnames(exp_all_outlier_table_9) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (1)","IWRES outliers","CWRES outliers")
# 
# 
# # Compare expected input data with real input data
# context("simeval, summary, function all.outlier.report.table")
# test_that("If function all.outlier.report.table works as expected",{
#   expect_equal(exp_all_outlier_table,all_outlier_table)
#   expect_equal(exp_all_outlier_table_1,all_outlier_table_1)
#   expect_equal(exp_all_outlier_table_3,all_outlier_table_3)
#   expect_equal(exp_all_outlier_table_4,all_outlier_table_4)
#   expect_equal(exp_all_outlier_table_5,all_outlier_table_5)
#   expect_equal(exp_all_outlier_table_6,all_outlier_table_6)
#   expect_equal(exp_all_outlier_table_7,all_outlier_table_7)
#   expect_equal(exp_all_outlier_table_8,all_outlier_table_8)
#   expect_equal(exp_all_outlier_table_9,all_outlier_table_9)
# })