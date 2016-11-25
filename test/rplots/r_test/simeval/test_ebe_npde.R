library(testthat)
suppressMessages(library(PerformanceAnalytics))
suppressMessages(require("PEIP"))

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
pdf.file.name <- 'PsN_ebe_npde_plots.pdf'
model.filename <- "run1.mod"
in_iiv.eta.names_1 <- c('ETA(1)','ETA(2)','ETA(3)')
in_iiv.eta.names_2 <- c('ETA(2)','ETA(3)')
in_iiv.eta.names_3 <- c('ETA(1)','ETA(3)')
in_iiv.eta.names_4 <- c('ETA(2)')
in_iov.eta.names_1 <- list(occ1=c('ETA(5)','ETA(6)'),occ2=c('ETA(7)','ETA(8)'),occ3=c('ETA(10)','ETA(11)'))
in_iov.eta.names_2 <- list(occ1=c('ETA(5)'),occ2=c('ETA(4)'))
in_iov.eta.names_3 <- list()
in_iov.eta.names_4 <- list(occ1=c('ETA(1)','ETA(3)'),occ2=c('ETA(4)','ETA(5)'))

source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

ebe.npde.file_1 <- paste0(files.w.dir,'ebe_npde_1.csv')
ebe.npde.file_2 <- paste0(files.w.dir,'ebe_npde_NA.csv')
ebe.npde.file_3 <- paste0(files.w.dir,'ebe_npde_NA_NA.csv')
ebe.npde.file_4 <- paste0(files.w.dir,'ebe_npde_iiv_iov.csv')

###################################     3. Make tests     ###################################
#.................................... (0) Test all_eta_names..................................
etas_list_1 <- eta_iiv_iov(iiv.eta.names=in_iiv.eta.names_1,iov.eta.names=in_iov.eta.names_3)
etas_list_2 <- eta_iiv_iov(iiv.eta.names=in_iiv.eta.names_2,iov.eta.names=in_iov.eta.names_3)
etas_list_3 <- eta_iiv_iov(iiv.eta.names=in_iiv.eta.names_3,iov.eta.names=in_iov.eta.names_3)
etas_list_4 <- eta_iiv_iov(iiv.eta.names=in_iiv.eta.names_1,iov.eta.names=in_iov.eta.names_1)
etas_list_5 <- eta_iiv_iov(iiv.eta.names=in_iiv.eta.names_1,iov.eta.names=in_iov.eta.names_2)
etas_list_6 <- eta_iiv_iov(iiv.eta.names=in_iiv.eta.names_4,iov.eta.names=in_iov.eta.names_4)

#unlist
eta.names_1 <- etas_list_1$eta.names
iiv.eta.names_1 <- etas_list_1$iiv.eta.names
iov.eta.names_1 <- etas_list_1$iov.eta.names
eta.names_text_1 <- etas_list_1$eta.names_text
iiv.eta.names_text_1 <- etas_list_1$iiv.eta.names_text
iov.eta.names_text_1 <- etas_list_1$iov.eta.names_text

eta.names_2 <- etas_list_2$eta.names
iiv.eta.names_2 <- etas_list_2$iiv.eta.names
iov.eta.names_2 <- etas_list_2$iov.eta.names
eta.names_text_2 <- etas_list_2$eta.names_text
iiv.eta.names_text_2 <- etas_list_2$iiv.eta.names_text
iov.eta.names_text_2 <- etas_list_2$iov.eta.names_text

eta.names_3 <- etas_list_3$eta.names
iiv.eta.names_3 <- etas_list_3$iiv.eta.names
iov.eta.names_3 <- etas_list_3$iov.eta.names
eta.names_text_3 <- etas_list_3$eta.names_text
iiv.eta.names_text_3 <- etas_list_3$iiv.eta.names_text
iov.eta.names_text_3 <- etas_list_3$iov.eta.names_text

eta.names_4 <- etas_list_4$eta.names
iiv.eta.names_4 <- etas_list_4$iiv.eta.names
iov.eta.names_4 <- etas_list_4$iov.eta.names
eta.names_text_4 <- etas_list_4$eta.names_text
iiv.eta.names_text_4 <- etas_list_4$iiv.eta.names_text
iov.eta.names_text_4 <- etas_list_4$iov.eta.names_text

eta.names_5 <- etas_list_5$eta.names
iiv.eta.names_5 <- etas_list_5$iiv.eta.names
iov.eta.names_5 <- etas_list_5$iov.eta.names
eta.names_text_5 <- etas_list_5$eta.names_text
iiv.eta.names_text_5 <- etas_list_5$iiv.eta.names_text
iov.eta.names_text_5 <- etas_list_5$iov.eta.names_text

eta.names_6 <- etas_list_6$eta.names
iiv.eta.names_6 <- etas_list_6$iiv.eta.names
iov.eta.names_6 <- etas_list_6$iov.eta.names
eta.names_text_6 <- etas_list_6$eta.names_text
iiv.eta.names_text_6 <- etas_list_6$iiv.eta.names_text
iov.eta.names_text_6 <- etas_list_6$iov.eta.names_text

# Compare expected input data with real input data
context("Simeval, ebe npde, function all.eta.names")
test_that("If function all.eta.names works as expected",{
  expect_equal(c('ETA.1.','ETA.2.','ETA.3.'),eta.names_1)
  expect_equal(c('ETA.1.','ETA.2.','ETA.3.'),iiv.eta.names_1)
  expect_equal(list(),iov.eta.names_1)
  expect_equal(c('ETA.1. IIV','ETA.2. IIV','ETA.3. IIV'),eta.names_text_1)
  expect_equal("Correlation graph for EBE NPDE IIV:\n(ETA.1.,ETA.2.,ETA.3.)",iiv.eta.names_text_1)
  expect_equal(c(),iov.eta.names_text_1)
  
  expect_equal(c('ETA.2.','ETA.3.'),eta.names_2)
  expect_equal(c('ETA.2.','ETA.3.'),iiv.eta.names_2)
  expect_equal(list(),iov.eta.names_2)
  expect_equal(c('ETA.2. IIV','ETA.3. IIV'),eta.names_text_2)
  expect_equal("Correlation graph for EBE NPDE IIV:\n(ETA.2.,ETA.3.)",iiv.eta.names_text_2)
  expect_equal(c(),iov.eta.names_text_2)
  
  expect_equal(c('ETA.1.','ETA.3.'),eta.names_3)
  expect_equal(c('ETA.1.','ETA.3.'),iiv.eta.names_3)
  expect_equal(list(),iov.eta.names_3)
  expect_equal(c('ETA.1. IIV','ETA.3. IIV'),eta.names_text_3)
  expect_equal(c(),iov.eta.names_text_3)
  expect_equal("Correlation graph for EBE NPDE IIV:\n(ETA.1.,ETA.3.)",iiv.eta.names_text_3)
  
  expect_equal(c('ETA.1.','ETA.2.','ETA.3.','ETA.5.','ETA.6.','ETA.7.','ETA.8.','ETA.10.','ETA.11.'),eta.names_4)
  expect_equal(c('ETA.1.','ETA.2.','ETA.3.'),iiv.eta.names_4)
  expect_equal(list(occ1=c('ETA.5.','ETA.6.'),occ2=c('ETA.7.','ETA.8.'),occ3=c('ETA.10.','ETA.11.')),iov.eta.names_4)
  expect_equal(c('ETA.1. IIV','ETA.2. IIV','ETA.3. IIV',
                 'ETA.5. IOV occ.1','ETA.6. IOV occ.1','ETA.7. IOV occ.2','ETA.8. IOV occ.2','ETA.10. IOV occ.3','ETA.11. IOV occ.3'),eta.names_text_4)
  expect_equal("Correlation graph for EBE NPDE IIV:\n(ETA.1.,ETA.2.,ETA.3.)",iiv.eta.names_text_4)
  expect_equal("Correlation graph for EBE NPDE IOV:\n(ETA.5.,ETA.6.)\n(ETA.7.,ETA.8.)\n(ETA.10.,ETA.11.)",iov.eta.names_text_4)
  
  expect_equal(c('ETA.1.','ETA.2.','ETA.3.','ETA.5.','ETA.4.'),eta.names_5)
  expect_equal(c('ETA.1.','ETA.2.','ETA.3.'),iiv.eta.names_5)
  expect_equal(list(occ1=c('ETA.5.'),occ2=c('ETA.4.')),iov.eta.names_5)
  expect_equal(c('ETA.1. IIV','ETA.2. IIV','ETA.3. IIV','ETA.5. IOV occ.1','ETA.4. IOV occ.2'),eta.names_text_5)
  expect_equal("Correlation graph for EBE NPDE IIV:\n(ETA.1.,ETA.2.,ETA.3.)",iiv.eta.names_text_5)
  expect_equal("Correlation graph for EBE NPDE IOV:\n(ETA.5.)\n(ETA.4.)",iov.eta.names_text_5)
  
  expect_equal(c('ETA.2.','ETA.1.','ETA.3.','ETA.4.','ETA.5.'),eta.names_6)
  expect_equal(c('ETA.2.'),iiv.eta.names_6)
  expect_equal(list(occ1=c('ETA.1.','ETA.3.'),occ2=c('ETA.4.','ETA.5.')),iov.eta.names_6)
  expect_equal(c('ETA.2. IIV','ETA.1. IOV occ.1','ETA.3. IOV occ.1','ETA.4. IOV occ.2','ETA.5. IOV occ.2'),eta.names_text_6)
  expect_equal("Correlation graph for EBE NPDE IIV:\n(ETA.2.)",iiv.eta.names_text_6)
  expect_equal("Correlation graph for EBE NPDE IOV:\n(ETA.1.,ETA.3.)\n(ETA.4.,ETA.5.)",iov.eta.names_text_6)
})


#..................................  (1) Test input.data .....................................  
input_data_1 <- input.data(ebe.npde.file=ebe.npde.file_1,eta.names=iiv.eta.names_1,show.warning=F)
input_data_2 <- input.data(ebe.npde.file=ebe.npde.file_2,eta.names=iiv.eta.names_1,show.warning=F)
input_data_3 <- input.data(ebe.npde.file=ebe.npde.file_3,eta.names=iiv.eta.names_1,show.warning=F)
input_data_4 <- input.data(ebe.npde.file=ebe.npde.file_3,eta.names=iiv.eta.names_2,show.warning=F)
input_data_5 <- input.data(ebe.npde.file=ebe.npde.file_3,eta.names=iiv.eta.names_3,show.warning=F)
input_data_6 <- input.data(ebe.npde.file=ebe.npde.file_4,eta.names=eta.names_6,show.warning=F)

#unlist
ebenpde_tmp_input_1 <- input_data_1$ebenpde_tmp_input
ebenpde_tmp_1 <- input_data_1$ebenpde_tmp
n.subjects_1 <- input_data_1$n.subjects
ebenpde_obs_1 <- input_data_1$ebenpde_obs
ID_deleted_1 <- input_data_1$ID_deleted

ebenpde_tmp_input_2 <- input_data_2$ebenpde_tmp_input
ebenpde_tmp_2 <- input_data_2$ebenpde_tmp
n.subjects_2 <- input_data_2$n.subjects
ebenpde_obs_2 <- input_data_2$ebenpde_obs
ID_deleted_2 <- input_data_2$ID_deleted

ebenpde_tmp_input_3 <- input_data_3$ebenpde_tmp_input
ebenpde_tmp_3 <- input_data_3$ebenpde_tmp
n.subjects_3 <- input_data_3$n.subjects
ebenpde_obs_3 <- input_data_3$ebenpde_obs
ID_deleted_3 <- input_data_3$ID_deleted

ebenpde_tmp_input_4 <- input_data_4$ebenpde_tmp_input
ebenpde_tmp_4 <- input_data_4$ebenpde_tmp
n.subjects_4 <- input_data_4$n.subjects
ebenpde_obs_4 <- input_data_4$ebenpde_obs
ID_deleted_4 <- input_data_4$ID_deleted

ebenpde_tmp_input_5 <- input_data_5$ebenpde_tmp_input
ebenpde_tmp_5 <- input_data_5$ebenpde_tmp
n.subjects_5 <- input_data_5$n.subjects
ebenpde_obs_5 <- input_data_5$ebenpde_obs
ID_deleted_5 <- input_data_5$ID_deleted

ebenpde_tmp_input_6 <- input_data_6$ebenpde_tmp_input
ebenpde_tmp_6 <- input_data_6$ebenpde_tmp
n.subjects_6 <- input_data_6$n.subjects
ebenpde_obs_6 <- input_data_6$ebenpde_obs
ID_deleted_6 <- input_data_6$ID_deleted

# Create expected input data
exp_ebenpde_tmp_input_1 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
exp_ebenpde_tmp_1 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
exp_n.subjects_1 <- 6
exp_ebenpde_obs_1 <- data.frame(ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
exp_ID_deleted_1 <- c()

exp_ebenpde_tmp_input_2 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA))
exp_ebenpde_tmp_2 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA))
exp_n.subjects_2 <- 6
exp_ebenpde_obs_2 <- data.frame(ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA))
exp_ID_deleted_2 <- c()

exp_ebenpde_tmp_input_3 <-data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,NA,-0.58,NA),ETA.2.=c(-0.25,0.34,0.15,NA,0.03,NA),ETA.3.=c(0.74,-0.201,NA,NA,NA,NA))
exp_ebenpde_tmp_3 <- data.frame(ID=as.integer(c(3,7,9,23)),STAND_EBE=as.integer(c(0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,-0.58),ETA.2.=c(-0.25,0.34,0.15,0.03),ETA.3.=c(0.74,-0.201,NA,NA))
exp_n.subjects_3 <- 4
exp_ebenpde_obs_3 <- data.frame(ETA.1.=c(1.2,-0.98,NA,-0.58),ETA.2.=c(-0.25,0.34,0.15,0.03),ETA.3.=c(0.74,-0.201,NA,NA))
exp_ID_deleted_3 <- c(14,26)

exp_ebenpde_tmp_input_4 <-data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.2.=c(-0.25,0.34,0.15,NA,0.03,NA),ETA.3.=c(0.74,-0.201,NA,NA,NA,NA))
exp_ebenpde_tmp_4 <- data.frame(ID=as.integer(c(3,7,9,23)),STAND_EBE=as.integer(c(0,0,0,0)),ETA.2.=c(-0.25,0.34,0.15,0.03),ETA.3.=c(0.74,-0.201,NA,NA))
exp_n.subjects_4 <- 4
exp_ebenpde_obs_4 <- data.frame(ETA.2.=c(-0.25,0.34,0.15,0.03),ETA.3.=c(0.74,-0.201,NA,NA))
exp_ID_deleted_4 <- c(14,26)

exp_ebenpde_tmp_input_5 <-data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,NA,-0.58,NA),ETA.3.=c(0.74,-0.201,NA,NA,NA,NA))
exp_ebenpde_tmp_5 <- data.frame(ID=as.integer(c(3,7,23)),STAND_EBE=as.integer(c(0,0,0)),ETA.1.=c(1.2,-0.98,-0.58),ETA.3.=c(0.74,-0.201,NA))
exp_n.subjects_5 <- 3
exp_ebenpde_obs_5 <- data.frame(ETA.1.=c(1.2,-0.98,-0.58),ETA.3.=c(0.74,-0.201,NA))
exp_ID_deleted_5 <- c(9,14,26)

exp_ebenpde_tmp_input_6 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA),ETA.4.=c(NA,NA,NA,NA,NA,NA),ETA.5.=c(0.7,0.75,-0.42,0.75,0.3,-0.54))
exp_ebenpde_tmp_6 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA),ETA.4.=c(NA,NA,NA,NA,NA,NA),ETA.5.=c(0.7,0.75,-0.42,0.75,0.3,-0.54))
exp_n.subjects_6 <- 6
exp_ebenpde_obs_6 <- data.frame(ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA),ETA.4.=c(NA,NA,NA,NA,NA,NA),ETA.5.=c(0.7,0.75,-0.42,0.75,0.3,-0.54))
exp_ID_deleted_6 <- c()

# Compare expected input data with real input data
context("Simeval, ebe npde, function input.data")
test_that("If function input.data works as expected",{
    expect_equal(exp_ebenpde_tmp_input_1,ebenpde_tmp_input_1)
    expect_equal(exp_ebenpde_tmp_1,ebenpde_tmp_1)
    expect_equal(exp_n.subjects_1,n.subjects_1)
    expect_equal(exp_ebenpde_obs_1,ebenpde_obs_1)
    
    expect_equal(exp_ebenpde_tmp_input_2,ebenpde_tmp_input_2)
    expect_equal(exp_ebenpde_tmp_2,ebenpde_tmp_2)
    expect_equal(exp_n.subjects_2,n.subjects_2)
    expect_equal(exp_ebenpde_obs_2,ebenpde_obs_2)
    
    expect_equal(exp_ebenpde_tmp_input_3,ebenpde_tmp_input_3)
    expect_equal(exp_ebenpde_tmp_3,ebenpde_tmp_3)
    expect_equal(exp_n.subjects_3,n.subjects_3)
    expect_equal(exp_ebenpde_obs_3,ebenpde_obs_3)
    
    expect_equal(exp_ebenpde_tmp_input_4,ebenpde_tmp_input_4)
    expect_equal(exp_ebenpde_tmp_4,ebenpde_tmp_4)
    expect_equal(exp_n.subjects_4,n.subjects_4)
    expect_equal(exp_ebenpde_obs_4,ebenpde_obs_4)
  
    expect_equal(exp_ebenpde_tmp_input_5,ebenpde_tmp_input_5)
    expect_equal(exp_ebenpde_tmp_5,ebenpde_tmp_5)
    expect_equal(exp_n.subjects_5,n.subjects_5)
    expect_equal(exp_ebenpde_obs_5,ebenpde_obs_5)
})
context("Simeval, ebe npde, expect warning messages from function input.data")
test_that("Expect warnings from function input.data",{
  expect_message(input.data(ebe.npde.file=ebe.npde.file_3,eta.names=iiv.eta.names_1))
  expect_message(input.data(ebe.npde.file=ebe.npde.file_3,eta.names=iiv.eta.names_2))
  expect_message(input.data(ebe.npde.file=ebe.npde.file_3,eta.names=iiv.eta.names_3))
})

# .................................  (2) Test summary.table.ebe.npde ...............................   
mydataframe_1 <- summary.table.ebe.npde(ebenpde_obs_1,eta.names=eta.names_text_1)
mydataframe_2 <- summary.table.ebe.npde(ebenpde_obs_2,eta.names=c('ETA.1. IIV','ETA.2. IOV occ.1','ETA.3. IOV occ.1'))
mydataframe_3 <- summary.table.ebe.npde(rbind(ebenpde_obs_3,c(0.92,0.42,-0.78)),eta.names=c('ETA.1. IIV','ETA.2. IOV occ.1','ETA.3. IOV occ.2'))
mydataframe_4 <- summary.table.ebe.npde(ebenpde_obs_6,eta.names=c('ETA.2. IIV','ETA.1. IOV occ.1','ETA.3. IOV occ.1','ETA.4. IOV occ.2','ETA.5. IOV occ.2'))

# Create expected data
exp_mydataframe_1 <- data.frame(c("ETA.1. IIV","ETA.2. IIV","ETA.3. IIV"),
                                c("0.845","-1.060","2.150"),
                                c("0.688","0.844","0.313"),
                                c("16.143","9.960","22.004"),
                                c("0.173","0.009","0.037"),
                                c("0.088","-1.736","0.189"),
                                c("-0.825","1.109","-1.256"),
                                c("0.977","0.001","0.384"),
                                stringsAsFactors = F)
colnames(exp_mydataframe_1) <- c("EBE NPDE","mean","p-value\n(H_0: mean==0)","variance","p-value\n(H_0: var==1)","skewness","kurtosis","p-value\n(normality)")

exp_mydataframe_2 <- data.frame(c("ETA.1. IIV","ETA.2. IOV occ.1","ETA.3. IOV occ.1"),
                                c(5,5,3),
                                c("-0.332","-1.278","2.863"),
                                c("1.000","1.000","0.500"),
                                c("9.789","12.094","20.400"),
                                c("0.518","0.014","0.087"),
                                c("-0.285","-1.453","0.673"),
                                c("-0.782","0.189","-1.500"),
                                c("0.900","0.004","0.199"),
                                stringsAsFactors = F)
colnames(exp_mydataframe_2) <- c("EBE NPDE","Number\nof ID","mean","p-value\n(H_0: mean==0)","variance","p-value\n(H_0: var==1)","skewness","kurtosis","p-value\n(normality)")

exp_mydataframe_3 <- data.frame(c("ETA.1. IIV","ETA.2. IOV occ.1","ETA.3. IOV occ.2"),
                                c(4,5,3),
                                c("0.140","0.138","-0.080"),
                                c("0.875","0.312","0.750"),
                                c("1.168","0.071","0.589"),
                                c("0.823","0.339","0.979"),
                                c("-0.034","-0.431","0.282"),
                                c("-1.868","-1.073","-1.500"),
                                c("0.304","0.787","0.739"),
                                stringsAsFactors = F)
colnames(exp_mydataframe_3) <- c("EBE NPDE","Number\nof ID","mean","p-value\n(H_0: mean==0)","variance","p-value\n(H_0: var==1)","skewness","kurtosis","p-value\n(normality)")

exp_mydataframe_4 <- data.frame(c("ETA.2. IIV","ETA.1. IOV occ.1","ETA.3. IOV occ.1","ETA.4. IOV occ.2","ETA.5. IOV occ.2"),
                                c(5,5,3,0,6),
                                c("-1.278","-0.332","2.863","","0.257"),
                                c("1.000","1.000","0.500","","0.293"),
                                c("12.094","9.789","20.400","","0.356"),
                                c("0.014","0.518","0.087","","0.608"),
                                c("-1.453","-0.285","0.673","","-0.504"),
                                c("0.189","-0.782","-1.500","","-1.539"),
                                c("0.004","0.900","0.199","","0.060"),
                                stringsAsFactors = F)
colnames(exp_mydataframe_4) <- c("EBE NPDE","Number\nof ID","mean","p-value\n(H_0: mean==0)","variance","p-value\n(H_0: var==1)","skewness","kurtosis","p-value\n(normality)")


# Compare expected data with real data
context("Simeval, ebe npde, function summary.table.ebe.npde")
test_that("If function summary.table.ebe.npde works as expected",{
  expect_equal(exp_mydataframe_1,mydataframe_1)
  expect_equal(exp_mydataframe_2,mydataframe_2)
  expect_equal(exp_mydataframe_3,mydataframe_3)
  expect_equal(exp_mydataframe_4,mydataframe_4)
})

#.................................  (3) Test empirical.distance  ....................................    
list_emp_1 <- empirical.distance(ebenpde_obs_1,n.subjects_1)
list_emp_2 <- empirical.distance(ebenpde_obs_2,n.subjects_2)
list_emp_3 <- empirical.distance(ebenpde_obs_3,n.subjects_3)

#unlist
emp_dist_1 <- list_emp_1$emp_distance
emp_dist_2 <- list_emp_2$emp_distance
emp_dist_3 <- list_emp_3$emp_distance
id_eta.names_1 <- list_emp_1$id_eta.names
id_eta.names_2 <- list_emp_2$id_eta.names
id_eta.names_3 <- list_emp_3$id_eta.names

# Create expected data
exp_emp_dist_1 <- matrix(c(2.0501,1.116401,62.0435,133.4141,1.3574,79.311),nrow=6,ncol=1)
exp_emp_dist_2 <- matrix(c(2.0501,1.116401,0.0225,133.4141,0.3364,24.6989),nrow=6,ncol=1)
exp_emp_dist_3 <- matrix(c(2.0501,1.116401,0.0225,0.3373),nrow=4,ncol=1)
exp_id_eta.names_1 <- list(c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),
                               c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."))
exp_id_eta.names_2 <- list(c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),c("ETA.2."),
                               c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1."),c("ETA.1.","ETA.2."))
exp_id_eta.names_3 <- list(c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),c("ETA.2."),c("ETA.1.","ETA.2."))

# Compare expected data with real data
context("Simeval, ebe npde, function empirical.distance")
test_that("If function empirical.distance works as expected",{
  expect_equal(exp_emp_dist_1,emp_dist_1)
  expect_equal(exp_emp_dist_2,emp_dist_2)
  expect_equal(exp_emp_dist_3,emp_dist_3)
  expect_equal(exp_id_eta.names_1,id_eta.names_1)
  expect_equal(exp_id_eta.names_2,id_eta.names_2)
  expect_equal(exp_id_eta.names_3,id_eta.names_3)
})


#.......................................  (4) Test data.for.plots  ...............................................
list_out_tables_1 <- data.for.plots(emp_dist_1,n.subjects_1,eta.names=iiv.eta.names_1)
list_out_tables_2 <- data.for.plots(emp_dist_2,n.subjects_2,eta.names=iiv.eta.names_1)
list_out_tables_3 <- data.for.plots(emp_dist_3,n.subjects_3,eta.names=iiv.eta.names_1)

# Unlist data.for.plots
index_emp_distance_1 <- list_out_tables_1$index_emp_distance
emp_distance_sort_1 <- round(list_out_tables_1$emp_distance_sort,6)
theor_distance_1 <- round(list_out_tables_1$theor_distance,6)
out_distance_1 <- round(list_out_tables_1$out_distance,4)
ebe_npde_quant_1 <- round(list_out_tables_1$ebe_npde_quant,6)

index_emp_distance_2 <- list_out_tables_2$index_emp_distance
emp_distance_sort_2 <- round(list_out_tables_2$emp_distance_sort,6)
theor_distance_2 <- round(list_out_tables_2$theor_distance,6)
out_distance_2 <- round(list_out_tables_2$out_distance,4)
ebe_npde_quant_2 <- round(list_out_tables_2$ebe_npde_quant,6)

index_emp_distance_3 <- list_out_tables_3$index_emp_distance
emp_distance_sort_3 <- round(list_out_tables_3$emp_distance_sort,6)
theor_distance_3 <- round(list_out_tables_3$theor_distance,6)
out_distance_3 <- round(list_out_tables_3$out_distance,6)
ebe_npde_quant_3 <- round(list_out_tables_3$ebe_npde_quant,6)

# Create expected data
exp_index_emp_distance_1 <- list(x=c(1.116401,1.3574,2.0501,62.0435,79.311,133.4141),
                                 ix=c(2,5,1,3,6,4))
exp_emp_distance_sort_1 <- c(1.116401,1.3574,2.0501,62.0435,79.311,133.4141)
exp_theor_distance_1 <- c(0.510101,1.212533,1.947696,2.84179,4.108345,6.666203)
exp_out_distance_1 <- -89.6243
exp_ebe_npde_quant_1 <- c(0.083333,0.25,0.416667,0.583333,0.75,0.916667)

exp_index_emp_distance_2 <- list(x=c(0.0225,0.3364,1.116401,2.0501,24.6989,133.4141),
                                 ix=c(3,5,2,1,6,4))
exp_emp_distance_sort_2 <- c(0.0225,0.3364,1.116401,2.0501,24.6989,133.4141)
exp_theor_distance_2 <- c(0.510101,1.212533,1.947696,2.84179,4.108345,6.666203)
exp_out_distance_2 <- -89.6243
exp_ebe_npde_quant_2 <- c(0.083333,0.25,0.416667,0.583333,0.75,0.916667)

exp_index_emp_distance_3 <- list(x=c(0.0225,0.3373,1.116401,2.0501),
                                 ix=c(3,4,2,1))
exp_emp_distance_sort_3 <- c(0.0225,0.3373,1.116401,2.0501)
exp_theor_distance_3 <- c(0.692358,1.753981,3.109827,5.739413)
exp_out_distance_3 <- 2.608739
exp_ebe_npde_quant_3 <- c(0.125,0.375,0.625,0.875)

# Compare expected data with real data
context("Simeval, ebe npde, function data.for.plots")
test_that("If function data.for.plots works as expected",{
  expect_equal(exp_index_emp_distance_1,index_emp_distance_1)
  expect_equal(exp_emp_distance_sort_1,emp_distance_sort_1)
  expect_equal(exp_theor_distance_1,theor_distance_1)
  expect_equal(exp_out_distance_1,out_distance_1)
  expect_equal(exp_ebe_npde_quant_1,ebe_npde_quant_1)
  
  expect_equal(exp_index_emp_distance_2,index_emp_distance_2)
  expect_equal(exp_emp_distance_sort_2,emp_distance_sort_2)
  expect_equal(exp_theor_distance_2,theor_distance_2)
  expect_equal(exp_out_distance_2,out_distance_2)
  expect_equal(exp_ebe_npde_quant_2,ebe_npde_quant_2)
  
  expect_equal(exp_index_emp_distance_3,index_emp_distance_3)
  expect_equal(exp_emp_distance_sort_3,emp_distance_sort_3)
  expect_equal(exp_theor_distance_3,theor_distance_3)
  expect_equal(exp_out_distance_3,out_distance_3)
  expect_equal(exp_ebe_npde_quant_3,ebe_npde_quant_3)
})

#.....................................   (5) Test plot_1   .................................    
list_plot_1_1 <- plot_1(ebenpde_tmp_1,theor_distance_1,emp_distance_sort_1,index_emp_distance_1,
                 out_distance_1,n.subjects_1,outlying_criteria=-2.7,do_outlier_plot=FALSE,model.filename)
list_plot_1_2 <- plot_1(ebenpde_tmp_2,theor_distance_2,emp_distance_sort_2,index_emp_distance_2,
                 out_distance_2,n.subjects_2,outlying_criteria=2.7,do_outlier_plot=FALSE,model.filename)
list_plot_1_3 <- plot_1(ebenpde_tmp_3,theor_distance_3,emp_distance_sort_3,index_emp_distance_3,
                      out_distance_3,n.subjects_3,outlying_criteria=-2.7,do_outlier_plot=FALSE,model.filename)
# unlist information
flag_1 <- list_plot_1_1$flag
noutlier_1 <- list_plot_1_1$noutlier
outlier_id_row_1 <-list_plot_1_1$outlier_id_row
outlier_ID_1 <- list_plot_1_1$outlier_ID
identityline_1 <- list_plot_1_1$identityline
vector_text_1 <- list_plot_1_1$vector_text

flag_2 <- list_plot_1_2$flag
noutlier_2 <- list_plot_1_2$noutlier
outlier_id_row_2 <-list_plot_1_2$outlier_id_row
outlier_ID_2 <- list_plot_1_2$outlier_ID
identityline_2 <- list_plot_1_2$identityline
vector_text_2 <- list_plot_1_2$vector_text

flag_3 <- list_plot_1_3$flag
noutlier_3 <- list_plot_1_3$noutlier
outlier_id_row_3 <-list_plot_1_3$outlier_id_row
identityline_3 <- list_plot_1_3$identityline

# Create expected data
exp_noutlier_1 <- 1
exp_flag_1 <- 1
exp_outlier_id_row_1 <- 4
exp_outlier_ID_1 <- 14
exp_identityline_1 <- c(1,2,3,4,5,6)
exp_vector_text_1 <- matrix(c("","","","","","14"),nrow = 6,ncol = 1)

exp_noutlier_2 <- 1
exp_flag_2 <- 1
exp_outlier_id_row_2 <- 4
exp_outlier_ID_2 <- 14
exp_identityline_2 <- c(1,2,3,4,5,6)
exp_vector_text_2 <- matrix(c("","","","","","14"),nrow = 6,ncol = 1)

exp_noutlier_3 <- 0
exp_flag_3 <- 0
exp_outlier_id_row_3 <- c()
exp_identityline_3 <- c(1,2,3,4)

# Compare expected data with real data
context("Simeval, ebe npde, function plot_1")
test_that("If function plot_1 works as expected",{
  expect_equal(exp_noutlier_1,noutlier_1)
  expect_equal(exp_flag_1,flag_1)
  expect_equal(exp_outlier_id_row_1,outlier_id_row_1)
  expect_equal(exp_outlier_ID_1,outlier_ID_1)
  expect_equal(exp_identityline_1,identityline_1)
  expect_equal(exp_vector_text_1,vector_text_1)
  
  expect_equal(exp_noutlier_2,noutlier_2)
  expect_equal(exp_flag_2,flag_2)
  expect_equal(exp_outlier_id_row_2,outlier_id_row_2)
  expect_equal(exp_outlier_ID_2,outlier_ID_2)
  expect_equal(exp_identityline_2,identityline_2)
  expect_equal(exp_vector_text_2,vector_text_2)
  
  expect_equal(exp_noutlier_3,noutlier_3)
  expect_equal(exp_flag_3,flag_3)
  expect_equal(exp_outlier_id_row_3,outlier_id_row_3)
  expect_equal(exp_identityline_3,identityline_3)
})

#..................................... (6) Test plot_2  ......................................
list_plot_2_1 <- plot_2(ebenpde_tmp_1,emp_distance_sort_1,theor_distance_1,index_emp_distance_1,
                 noutlier_1,flag_1,n.subjects_1,iiv.eta.names_1,outlying_criteria=-2.7,
                 outlier_id_row_1,do_outlier_plot=FALSE,model.filename)
list_plot_2_2 <- plot_2(ebenpde_tmp_2,emp_distance_sort_2,theor_distance_2,index_emp_distance_2,
                        noutlier_2,flag_2,n.subjects_2,iiv.eta.names_1,outlying_criteria=-2.7,
                        outlier_id_row_2,do_outlier_plot=FALSE,model.filename)
list_plot_2_3 <- plot_2(ebenpde_tmp_3,emp_distance_sort_3,theor_distance_3,index_emp_distance_3,
                        noutlier_3,flag_3,n.subjects_3,iiv.eta.names_1,outlying_criteria=-2.7,
                        outlier_id_row_3,do_outlier_plot=FALSE,model.filename)

# unlist information
vector_theor_dist_2_1 <- round(list_plot_2_1$vector_theor_dist,6)
noutlier_2_1 <- list_plot_2_1$noutlier
outlier_id_row_2_1 <- list_plot_2_1$outlier_id_row
flag1_2_1 <- list_plot_2_1$flag1
out_distance_2_1 <- round(list_plot_2_1$out_distance,7)

vector_theor_dist_2_2 <- round(list_plot_2_2$vector_theor_dist,6)
noutlier_2_2 <- list_plot_2_2$noutlier
outlier_id_row_2_2 <- list_plot_2_2$outlier_id_row
flag1_2_2 <- list_plot_2_2$flag1
out_distance_2_2 <- round(list_plot_2_2$out_distance,5)

vector_theor_dist_2_3 <- round(list_plot_2_3$vector_theor_dist,6)
noutlier_2_3 <- list_plot_2_3$noutlier
outlier_id_row_2_3 <- list_plot_2_3$outlier_id_row
flag1_2_3 <- list_plot_2_3$flag1
out_distance_2_3 <- round(list_plot_2_3$out_distance,5)

# Create expected data
exp_vector_theor_dist_2_1 <- matrix(c(2.365974,4.108345,5.071066,5.739413,6.251389,6.666203),nrow=1,ncol=6)
exp_noutlier_2_1 <- 3
exp_outlier_id_row_2_1 <- c(4,6,3)
exp_flag1_2_1 <- 0
exp_out_distance_2_1 <- c(-51.6609466,-39.8130014,2.1361458,1.9452118,0.8835815)

exp_vector_theor_dist_2_2 <- matrix(c(2.365974,4.108345,5.071066,5.739413,6.251389,6.666203),nrow=1,ncol=6)
exp_noutlier_2_2 <- 2
exp_outlier_id_row_2_2 <- c(4,6)
exp_flag1_2_2 <- 0
exp_out_distance_2_2 <- c(-13.04436,2.60874,2.79637,2.66717,1.65709)

exp_vector_theor_dist_2_3 <- matrix(c(2.365974,4.108345,5.071066,5.739413),nrow=1,ncol=4)
exp_noutlier_2_3 <- 0
exp_outlier_id_row_2_3 <- c()
exp_flag1_2_3 <- 0
exp_out_distance_2_3 <- c(2.79637,2.66653,1.65709)

# Compare expected data with real data
context("Simeval, ebe npde, function plot_2")
test_that("If function plot_2 works as expected",{
  expect_equal(exp_vector_theor_dist_2_1,vector_theor_dist_2_1)
  expect_equal(exp_noutlier_2_1,noutlier_2_1)
  expect_equal(exp_outlier_id_row_2_1,outlier_id_row_2_1)
  expect_equal(exp_flag1_2_1,flag1_2_1)
  expect_equal(exp_out_distance_2_1,out_distance_2_1)
  
  expect_equal(exp_vector_theor_dist_2_2,vector_theor_dist_2_2)
  expect_equal(exp_noutlier_2_2,noutlier_2_2)
  expect_equal(exp_outlier_id_row_2_2,outlier_id_row_2_2)
  expect_equal(exp_flag1_2_2,flag1_2_2)
  expect_equal(exp_out_distance_2_2,out_distance_2_2)
  
  expect_equal(exp_vector_theor_dist_2_3,vector_theor_dist_2_3)
  expect_equal(exp_noutlier_2_3,noutlier_2_3)
  expect_equal(exp_outlier_id_row_2_3,outlier_id_row_2_3)
  expect_equal(exp_flag1_2_3,flag1_2_3)
  expect_equal(exp_out_distance_2_3,out_distance_2_3)
})

#...................................  (7) Test outlier.table.ebe.npde  .........................................
fortable1_1 <- outlier.table.ebe.npde(ebenpde_tmp_1,iiv.eta.names_1,outlier_id_row_2_1)
fortable1_2 <- outlier.table.ebe.npde(ebenpde_tmp_2,iiv.eta.names_1,outlier_id_row_2_2)
fortable1_3 <- outlier.table.ebe.npde(ebenpde_tmp_3,iiv.eta.names_1,outlier_id_row_2_3)

# Create expected data
exp_fortable1_1 <- data.frame(c(14,26,9),c("1,2,3","1,2,3","1,2,3"),c("3.6,-7.46,8.05","-4.9,0.83,7.39","6.73,0.15,-4.09"),stringsAsFactors = F)
colnames(exp_fortable1_1) <- c("ID", "Outlying EBE (ETA numbers)","ETA values")

exp_fortable1_2 <- data.frame(c(14,26),c("1,2,3","1,2"),c("3.6,-7.46,8.05","-4.9,0.83"),stringsAsFactors = F)
colnames(exp_fortable1_2) <- c("ID", "Outlying EBE (ETA numbers)","ETA values")

exp_fortable1_3 <- data.frame(C = c("No EBE NPDE outliers detected"))
names(exp_fortable1_3) <- NULL

# Compare expected data with real data
context("Simeval, ebe npde, function outlier.table.ebe.npde")
test_that("If function outlier.table.ebe.npde works as expected",{
  expect_equal(exp_fortable1_1,fortable1_1)
  expect_equal(exp_fortable1_2,fortable1_2)
  expect_equal(exp_fortable1_3,fortable1_3)
})