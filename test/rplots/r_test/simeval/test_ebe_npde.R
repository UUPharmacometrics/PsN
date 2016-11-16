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
iiv.eta.names_1 <- c('ETA(1)','ETA(2)','ETA(3)')
iiv.eta.names_2 <- c('ETA(2)','ETA(3)')
iiv.eta.names_3 <- c('ETA(1)','ETA(3)')


# iiv.eta.names <- c('ETA(1)','ETA(2)')
# iiv.eta.names_a <- c('ETA(1)','ETA(2)','ETA(3)')
# iiv.eta.names_3 <- c('ETA(1)','ETA(2)','ETA(3)','ETA(4)')
# iiv.eta.names_4 <- c('ETA(1)','ETA(3)')

source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

ebe.npde.file_1 <- paste0(files.w.dir,'ebe_npde_1.csv')
ebe.npde.file_2 <- paste0(files.w.dir,'ebe_npde_NA.csv')
ebe.npde.file_3 <- paste0(files.w.dir,'ebe_npde_NA_NA.csv')

# ebe.npde.file <- paste0(files.w.dir,'ebe_npde.csv')
# ebe.npde.file_1 <- paste0(files.w.dir,'ebe_npde_1.csv')
# ebe.npde.file_2 <- paste0(files.w.dir,'some_missing_ebe_npde.csv')
# ebe.npde.file_3 <- paste0(files.w.dir,'some_missing_ebe_npde_1.csv')
# ebe.npde.file_4 <- paste0(files.w.dir,'some_missing_ebe_npde_2.csv')

###################################     3. Make tests     ###################################
#..................................  (1) Test input_ebe_npde .....................................  
input_data_1 <- input.data(ebe.npde.file=ebe.npde.file_1,iiv.eta.names=iiv.eta.names_1)
input_data_2 <- input.data(ebe.npde.file=ebe.npde.file_2,iiv.eta.names=iiv.eta.names_1)
input_data_3 <- input.data(ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_1)
input_data_4 <- input.data(ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_2)
input_data_5 <- input.data(ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_3)

# list_input <- input.data(ebe.npde.file=ebe.npde.file,iiv.eta.names=iiv.eta.names)
# list_input_a <- input.data(ebe.npde.file=ebe.npde.file_1,iiv.eta.names=iiv.eta.names_a)
# list_input_2 <- input.data(ebe.npde.file=ebe.npde.file_2,iiv.eta.names=iiv.eta.names_a)
# list_input_3 <- input.data(ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_3)
# list_input_4 <- input.data(ebe.npde.file=ebe.npde.file_4,iiv.eta.names=iiv.eta.names_4)

#unlist
ebenpde_tmp_input_1 <- input_data_1$ebenpde_tmp_input
ebenpde_tmp_1 <- input_data_1$ebenpde_tmp
n.subjects_1 <- input_data_1$n.subjects
ebenpde_obs_1 <- input_data_1$ebenpde_obs
iiv.eta.names_1 <- input_data_1$iiv.eta.names
ID_deleted_1 <- input_data_1$ID_deleted

ebenpde_tmp_input_2 <- input_data_2$ebenpde_tmp_input
ebenpde_tmp_2 <- input_data_2$ebenpde_tmp
n.subjects_2 <- input_data_2$n.subjects
ebenpde_obs_2 <- input_data_2$ebenpde_obs
iiv.eta.names_2 <- input_data_2$iiv.eta.names
ID_deleted_2 <- input_data_2$ID_deleted

ebenpde_tmp_input_3 <- input_data_3$ebenpde_tmp_input
ebenpde_tmp_3 <- input_data_3$ebenpde_tmp
n.subjects_3 <- input_data_3$n.subjects
ebenpde_obs_3 <- input_data_3$ebenpde_obs
iiv.eta.names_3 <- input_data_3$iiv.eta.names
ID_deleted_3 <- input_data_3$ID_deleted

ebenpde_tmp_input_4 <- input_data_4$ebenpde_tmp_input
ebenpde_tmp_4 <- input_data_4$ebenpde_tmp
n.subjects_4 <- input_data_4$n.subjects
ebenpde_obs_4 <- input_data_4$ebenpde_obs
iiv.eta.names_4 <- input_data_4$iiv.eta.names
ID_deleted_4 <- input_data_4$ID_deleted

ebenpde_tmp_input_5 <- input_data_5$ebenpde_tmp_input
ebenpde_tmp_5 <- input_data_5$ebenpde_tmp
n.subjects_5 <- input_data_5$n.subjects
ebenpde_obs_5 <- input_data_5$ebenpde_obs
iiv.eta.names_5 <- input_data_5$iiv.eta.names
ID_deleted_5 <- input_data_5$ID_deleted

# ebenpde_tmp <- list_input$ebenpde_tmp
# n.subjects <- list_input$n.subjects
# ebenpde_obs <- list_input$ebenpde_obs
# iiv.eta.names_b <- list_input$iiv.eta.names
# case <- list_input$case
# 
# # ebenpde_tmp_a <- list_input_a$ebenpde_tmp
# n.subjects_a <- list_input_a$n.subjects
# ebenpde_obs_a <- list_input_a$ebenpde_obs
# iiv.eta.names_c <- list_input_a$iiv.eta.names
# case_a <- list_input_a$case
# 
# ebenpde_tmp_2 <- list_input_2$ebenpde_tmp
# n.subjects_2 <- list_input_2$n.subjects
# ebenpde_obs_2 <- list_input_2$ebenpde_obs
# iiv.eta.names_2 <- list_input_2$iiv.eta.names
# case_2 <- list_input_2$case
# 
# ebenpde_tmp_3 <- list_input_3$ebenpde_tmp
# n.subjects_3 <- list_input_3$n.subjects
# ebenpde_obs_3 <- list_input_3$ebenpde_obs
# iiv.eta.names_3 <- list_input_3$iiv.eta.names
# case_3 <- list_input_3$case
# 
# ebenpde_tmp_4 <- list_input_4$ebenpde_tmp
# n.subjects_4 <- list_input_4$n.subjects
# ebenpde_obs_4 <- list_input_4$ebenpde_obs
# iiv.eta.names_4 <- list_input_4$iiv.eta.names
# case_4 <- list_input_4$case

# Create expected input data
exp_ebenpde_tmp_input_1 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
exp_ebenpde_tmp_1 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
exp_n.subjects_1 <- 6
exp_ebenpde_obs_1 <- data.frame(ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
exp_iiv.eta.names_1 <- c("ETA.1.","ETA.2.","ETA.3.")
exp_ID_deleted_1 <- c()

exp_ebenpde_tmp_input_2 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA))
exp_ebenpde_tmp_2 <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA))
exp_n.subjects_2 <- 6
exp_ebenpde_obs_2 <- data.frame(ETA.1.=c(1.2,-0.98,NA,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,NA,0.83),ETA.3.=c(0.74,-0.201,NA,8.05,NA,NA))
exp_iiv.eta.names_2 <- c("ETA.1.","ETA.2.","ETA.3.")
exp_ID_deleted_2 <- c()

exp_ebenpde_tmp_input_3 <-data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,NA,-0.58,NA),ETA.2.=c(-0.25,0.34,0.15,NA,0.03,NA),ETA.3.=c(0.74,-0.201,NA,NA,NA,NA))
exp_ebenpde_tmp_3 <- data.frame(ID=as.integer(c(3,7,9,23)),STAND_EBE=as.integer(c(0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,-0.58),ETA.2.=c(-0.25,0.34,0.15,0.03),ETA.3.=c(0.74,-0.201,NA,NA))
exp_n.subjects_3 <- 4
exp_ebenpde_obs_3 <- data.frame(ETA.1.=c(1.2,-0.98,NA,-0.58),ETA.2.=c(-0.25,0.34,0.15,0.03),ETA.3.=c(0.74,-0.201,NA,NA))
exp_iiv.eta.names_3 <- c("ETA.1.","ETA.2.","ETA.3.")
exp_ID_deleted_3 <- c(14,26)

exp_ebenpde_tmp_input_4 <-data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.2.=c(-0.25,0.34,0.15,NA,0.03,NA),ETA.3.=c(0.74,-0.201,NA,NA,NA,NA))
exp_ebenpde_tmp_4 <- data.frame(ID=as.integer(c(3,7,9,23)),STAND_EBE=as.integer(c(0,0,0,0)),ETA.2.=c(-0.25,0.34,0.15,0.03),ETA.3.=c(0.74,-0.201,NA,NA))
exp_n.subjects_4 <- 4
exp_ebenpde_obs_4 <- data.frame(ETA.2.=c(-0.25,0.34,0.15,0.03),ETA.3.=c(0.74,-0.201,NA,NA))
exp_iiv.eta.names_4 <- c("ETA.2.","ETA.3.")
exp_ID_deleted_4 <- c(14,26)

exp_ebenpde_tmp_input_5 <-data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,NA,NA,-0.58,NA),ETA.3.=c(0.74,-0.201,NA,NA,NA,NA))
exp_ebenpde_tmp_5 <- data.frame(ID=as.integer(c(3,7,23)),STAND_EBE=as.integer(c(0,0,0)),ETA.1.=c(1.2,-0.98,-0.58),ETA.3.=c(0.74,-0.201,NA))
exp_n.subjects_5 <- 3
exp_ebenpde_obs_5 <- data.frame(ETA.1.=c(1.2,-0.98,-0.58),ETA.3.=c(0.74,-0.201,NA))
exp_iiv.eta.names_5 <- c("ETA.1.","ETA.3.")
exp_ID_deleted_5 <- c(9,14,26)


# exp_ebenpde_tmp <- data.frame(ID=as.integer(c(2,11,21,55,56)),STAND_EBE=as.integer(c(0,0,0,0,0)),ETA.1.=c(-1.558,0.54,0.73,0,-2.037),ETA.2.=c(-0.25,0,0.15,-0.46,-0.17))
# exp_n.subjects <- 5
# exp_ebenpde_obs <- data.frame(ETA.1.=c(-1.558,0.54,0.73,0,-2.037),ETA.2.=c(-0.25,0,0.15,-0.46,-0.17))
# exp_iiv.eta.names_b <- c("ETA.1.","ETA.2.")
# exp_case <- ''
# 
# exp_ebenpde_tmp_a <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
# exp_n.subjects_a <- 6
# exp_ebenpde_obs_a <- data.frame(ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
# exp_iiv.eta.names_c <- c("ETA.1.","ETA.2.","ETA.3.")
# 
# exp_ebenpde_tmp_2 <- data.frame(ID=as.integer(c(1,4,5,6,7,9,10,11,13)),STAND_EBE_IIV=c(1.48,0.84,0.49,0.01,2.4,0.2,0.5,3.5,1.06),ETA.1.=c(-1.03,-1.1,0.2,NA,-1.4,NA,NA,0.6,-0.9),ETA.2.=c(0.8,-0.13,-0.4,-0.1,0.9,0.5,0.6,-1.7,-0.5),ETA.3.=c(0.4,1.03,-0.3,NA,0.6,NA,NA,NA,NA))
# exp_n.subjects_2 <- 9
# exp_ebenpde_obs_2 <- data.frame(ETA.1.=c(-1.03,-1.1,0.2,NA,-1.4,NA,NA,0.6,-0.9),ETA.2.=c(0.8,-0.13,-0.4,-0.1,0.9,0.5,0.6,-1.7,-0.5),ETA.3.=c(0.4,1.03,-0.3,NA,0.6,NA,NA,NA,NA))
# exp_iiv.eta.names_2 <- c("ETA.1.","ETA.2.","ETA.3.")
# 
# exp_ebenpde_tmp_3 <- data.frame(ID=as.integer(c(1,4,5,6,7,9,10,11,13)),STAND_EBE_IIV=c(1.48,0.84,0.49,0.01,2.4,0.2,0.5,3.5,1.06),ETA.1.=c(-1.03,-1.1,0.2,NA,-1.4,NA,NA,0.6,-0.9),ETA.2.=c(0.8,-0.13,-0.4,-0.1,0.9,0.5,0.6,-1.7,-0.5),ETA.3.=c(0.4,1.03,-0.3,NA,0.6,NA,NA,NA,NA),ETA.4.=c(0.98,-0.3,0.4,0.31,0.9,0.25,0.46,-0.7,-0.95))
# exp_n.subjects_3 <- 9
# exp_ebenpde_obs_3 <- data.frame(ETA.1.=c(-1.03,-1.1,0.2,NA,-1.4,NA,NA,0.6,-0.9),ETA.2.=c(0.8,-0.13,-0.4,-0.1,0.9,0.5,0.6,-1.7,-0.5),ETA.3.=c(0.4,1.03,-0.3,NA,0.6,NA,NA,NA,NA),ETA.4.=c(0.98,-0.3,0.4,0.31,0.9,0.25,0.46,-0.7,-0.95))
# exp_iiv.eta.names_3 <- c("ETA.1.","ETA.2.","ETA.3.","ETA.4.")
# 
# exp_ebenpde_tmp_4 <- data.frame(ID=as.integer(c(1,4,5,6,7,9,10,11,13)),STAND_EBE_IIV=c(1.48,0.84,0.49,0.01,2.4,0.2,0.5,3.5,1.06),ETA.1.=c(-1.03,-1.1,0.2,NA,-1.4,NA,NA,0.6,-0.9),ETA.3.=c(0.4,1.03,-0.3,NA,0.6,NA,0.3,NA,NA))
# exp_n.subjects_4 <- 9
# exp_ebenpde_obs_4 <- data.frame(ETA.1.=c(-1.03,-1.1,0.2,NA,-1.4,NA,NA,0.6,-0.9),ETA.3.=c(0.4,1.03,-0.3,NA,0.6,NA,0.3,NA,NA))
# exp_iiv.eta.names_4 <- c("ETA.1.","ETA.3.")

# Compare expected input data with real input data
context("Simeval, ebe npde, function input.data")
test_that("If function input.data works as expected",{
    expect_equal(exp_ebenpde_tmp_input_1,ebenpde_tmp_input_1)
    expect_equal(exp_ebenpde_tmp_1,ebenpde_tmp_1)
    expect_equal(exp_n.subjects_1,n.subjects_1)
    expect_equal(exp_ebenpde_obs_1,ebenpde_obs_1)
    expect_equal(exp_iiv.eta.names_1,iiv.eta.names_1)
    
    expect_equal(exp_ebenpde_tmp_input_2,ebenpde_tmp_input_2)
    expect_equal(exp_ebenpde_tmp_2,ebenpde_tmp_2)
    expect_equal(exp_n.subjects_2,n.subjects_2)
    expect_equal(exp_ebenpde_obs_2,ebenpde_obs_2)
    expect_equal(exp_iiv.eta.names_2,iiv.eta.names_2)
    
    expect_equal(exp_ebenpde_tmp_input_3,ebenpde_tmp_input_3)
    expect_equal(exp_ebenpde_tmp_3,ebenpde_tmp_3)
    expect_equal(exp_n.subjects_3,n.subjects_3)
    expect_equal(exp_ebenpde_obs_3,ebenpde_obs_3)
    expect_equal(exp_iiv.eta.names_3,iiv.eta.names_3)
    
    expect_equal(exp_ebenpde_tmp_input_4,ebenpde_tmp_input_4)
    expect_equal(exp_ebenpde_tmp_4,ebenpde_tmp_4)
    expect_equal(exp_n.subjects_4,n.subjects_4)
    expect_equal(exp_ebenpde_obs_4,ebenpde_obs_4)
    expect_equal(exp_iiv.eta.names_4,iiv.eta.names_4)
  
    expect_equal(exp_ebenpde_tmp_input_5,ebenpde_tmp_input_5)
    expect_equal(exp_ebenpde_tmp_5,ebenpde_tmp_5)
    expect_equal(exp_n.subjects_5,n.subjects_5)
    expect_equal(exp_ebenpde_obs_5,ebenpde_obs_5)
    expect_equal(exp_iiv.eta.names_5,iiv.eta.names_5)
    
#   expect_equal(exp_ebenpde_tmp,ebenpde_tmp)
#   expect_equal(exp_n.subjects,n.subjects)
#   expect_equal(exp_ebenpde_obs,ebenpde_obs)
#   expect_equal(exp_iiv.eta.names_b,iiv.eta.names_b)
#   expect_equal(exp_case,case)
#   
#   expect_equal(exp_ebenpde_tmp_a,ebenpde_tmp_a)
#   expect_equal(exp_n.subjects_a,n.subjects_a)
#   expect_equal(exp_ebenpde_obs_a,ebenpde_obs_a)
#   expect_equal(exp_iiv.eta.names_c,iiv.eta.names_c)
#   expect_equal(exp_case,case_a)
#   
#   expect_equal(exp_ebenpde_tmp_2,ebenpde_tmp_2)
#   expect_equal(exp_n.subjects_2,n.subjects_2)
#   expect_equal(exp_ebenpde_obs_2,ebenpde_obs_2)
#   expect_equal(exp_iiv.eta.names_2,iiv.eta.names_2)
#   expect_equal(exp_case,case_2)
#   
#   expect_equal(exp_ebenpde_tmp_3,ebenpde_tmp_3)
#   expect_equal(exp_n.subjects_3,n.subjects_3)
#   expect_equal(exp_ebenpde_obs_3,ebenpde_obs_3)
#   expect_equal(exp_iiv.eta.names_3,iiv.eta.names_3)
#   expect_equal(exp_case,case_3)
#   
#   expect_equal(exp_ebenpde_tmp_4,ebenpde_tmp_4)
#   expect_equal(exp_n.subjects_4,n.subjects_4)
#   expect_equal(exp_ebenpde_obs_4,ebenpde_obs_4)
#   expect_equal(exp_iiv.eta.names_4,iiv.eta.names_4)
#   expect_equal(exp_case,case_4)
})
context("Simeval, ebe npde, expect warning messages from function input.data")
test_that("Expect warnings from function input.data",{
  expect_message(input.data(ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_1))
  expect_message(input.data(ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_2))
  expect_message(input.data(ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_3))
})

# #.................................  (2) Test two.data.cases.R  ...............................   
# list_two.data.cases <- two.data.cases(ebenpde_tmp=ebenpde_tmp_2)
# list_two.data.cases_3 <- two.data.cases(ebenpde_tmp=ebenpde_tmp_3)
# list_two.data.cases_4 <- two.data.cases(ebenpde_tmp=ebenpde_tmp_4)
# 
# # unlist
# ebenpde_tmp_2_1 <- list_two.data.cases[[1]]$ebenpde_tmp
# n.subjects_2_1 <- list_two.data.cases[[1]]$n.subjects
# ebenpde_obs_2_1 <- list_two.data.cases[[1]]$ebenpde_obs
# iiv.eta.names_2_1 <- list_two.data.cases[[1]]$iiv.eta.names
# case_2_1 <- list_two.data.cases[[1]]$case
# 
# ebenpde_tmp_2_2 <- list_two.data.cases[[2]]$ebenpde_tmp
# n.subjects_2_2 <- list_two.data.cases[[2]]$n.subjects
# ebenpde_obs_2_2 <- list_two.data.cases[[2]]$ebenpde_obs
# iiv.eta.names_2_2 <- list_two.data.cases[[2]]$iiv.eta.names
# case_2_2 <- list_two.data.cases[[2]]$case
# 
# ebenpde_tmp_3_1 <- list_two.data.cases_3[[1]]$ebenpde_tmp
# n.subjects_3_1 <- list_two.data.cases_3[[1]]$n.subjects
# ebenpde_obs_3_1 <- list_two.data.cases_3[[1]]$ebenpde_obs
# iiv.eta.names_3_1 <- list_two.data.cases_3[[1]]$iiv.eta.names
# case_3_1 <- list_two.data.cases_3[[1]]$case
# 
# ebenpde_tmp_3_2 <- list_two.data.cases_3[[2]]$ebenpde_tmp
# n.subjects_3_2 <- list_two.data.cases_3[[2]]$n.subjects
# ebenpde_obs_3_2 <- list_two.data.cases_3[[2]]$ebenpde_obs
# iiv.eta.names_3_2 <- list_two.data.cases_3[[2]]$iiv.eta.names
# case_3_2 <- list_two.data.cases_3[[2]]$case
# 
# ebenpde_tmp_4_1 <- list_two.data.cases_4[[1]]$ebenpde_tmp
# n.subjects_4_1 <- list_two.data.cases_4[[1]]$n.subjects
# ebenpde_obs_4_1 <- list_two.data.cases_4[[1]]$ebenpde_obs
# iiv.eta.names_4_1 <- list_two.data.cases_4[[1]]$iiv.eta.names
# case_4_1 <- list_two.data.cases_4[[1]]$case
# 
# # Create expected data
# exp_ebenpde_tmp_2_1 <- data.frame(ID=as.integer(c(1,4,5,7)),STAND_EBE_IIV=c(1.48,0.84,0.49,2.4),ETA.1.=c(-1.03,-1.1,0.2,-1.4),ETA.2.=c(0.8,-0.13,-0.4,0.9),ETA.3.=c(0.4,1.03,-0.3,0.6))
# exp_n.subjects_2_1 <- 4
# exp_ebenpde_obs_2_1 <- data.frame(ETA.1.=c(-1.03,-1.1,0.2,-1.4),ETA.2.=c(0.8,-0.13,-0.4,0.9),ETA.3.=c(0.4,1.03,-0.3,0.6))
# exp_iiv.eta.names_2_1 <- c("ETA.1.","ETA.2.","ETA.3.")
# exp_case_2_1 <- 'Individual based analysis'
# 
# exp_ebenpde_tmp_2_2 <- data.frame(ID=as.integer(c(1,4,5,6,7,9,10,11,13)),STAND_EBE_IIV=c(1.48,0.84,0.49,0.01,2.4,0.2,0.5,3.5,1.06),ETA.2.=c(0.8,-0.13,-0.4,-0.1,0.9,0.5,0.6,-1.7,-0.5))
# exp_n.subjects_2_2 <- 9
# exp_ebenpde_obs_2_2 <- data.frame(ETA.2.=c(0.8,-0.13,-0.4,-0.1,0.9,0.5,0.6,-1.7,-0.5))
# exp_iiv.eta.names_2_2 <- c("ETA.2.")
# exp_case_2_2 <- 'ETA based analysis'
# 
# exp_ebenpde_tmp_3_1 <- data.frame(ID=as.integer(c(1,4,5,7)),STAND_EBE_IIV=c(1.48,0.84,0.49,2.4),ETA.1.=c(-1.03,-1.1,0.2,-1.4),ETA.2.=c(0.8,-0.13,-0.4,0.9),ETA.3.=c(0.4,1.03,-0.3,0.6),ETA.4.=c(0.98,-0.3,0.4,0.9))
# exp_n.subjects_3_1 <- 4
# exp_ebenpde_obs_3_1 <- data.frame(ETA.1.=c(-1.03,-1.1,0.2,-1.4),ETA.2.=c(0.8,-0.13,-0.4,0.9),ETA.3.=c(0.4,1.03,-0.3,0.6),ETA.4.=c(0.98,-0.3,0.4,0.9))
# exp_iiv.eta.names_3_1 <- c("ETA.1.","ETA.2.","ETA.3.","ETA.4.")
# exp_case_3_1 <- 'Individual based analysis'
# 
# exp_ebenpde_tmp_3_2 <- data.frame(ID=as.integer(c(1,4,5,6,7,9,10,11,13)),STAND_EBE_IIV=c(1.48,0.84,0.49,0.01,2.4,0.2,0.5,3.5,1.06),ETA.2.=c(0.8,-0.13,-0.4,-0.1,0.9,0.5,0.6,-1.7,-0.5),ETA.4.=c(0.98,-0.3,0.4,0.31,0.9,0.25,0.46,-0.7,-0.95))
# exp_n.subjects_3_2 <- 9
# exp_ebenpde_obs_3_2 <- data.frame(ETA.2.=c(0.8,-0.13,-0.4,-0.1,0.9,0.5,0.6,-1.7,-0.5),ETA.4.=c(0.98,-0.3,0.4,0.31,0.9,0.25,0.46,-0.7,-0.95))
# exp_iiv.eta.names_3_2 <- c("ETA.2.","ETA.4.")
# exp_case_3_2 <- 'ETA based analysis'
# 
# exp_ebenpde_tmp_4_1 <- data.frame(ID=as.integer(c(1,4,5,7)),STAND_EBE_IIV=c(1.48,0.84,0.49,2.4),ETA.1.=c(-1.03,-1.1,0.2,-1.4),ETA.3.=c(0.4,1.03,-0.3,0.6))
# exp_n.subjects_4_1 <- 4
# exp_ebenpde_obs_4_1 <- data.frame(ETA.1.=c(-1.03,-1.1,0.2,-1.4),ETA.3.=c(0.4,1.03,-0.3,0.6))
# exp_iiv.eta.names_4_1 <- c("ETA.1.","ETA.3.")
# exp_case_4_1 <- 'Individual based analysis'
# 
# # Compare expected data with real data
# context("Simeval, ebe npde, function two.data.cases")
# test_that("If function two.data.cases works as expected",{
#   expect_equal(exp_ebenpde_tmp_2_1,ebenpde_tmp_2_1)
#   expect_equal(exp_n.subjects_2_1,n.subjects_2_1)
#   expect_equal(exp_ebenpde_obs_2_1,ebenpde_obs_2_1)
#   expect_equal(exp_iiv.eta.names_2_1,iiv.eta.names_2_1)
#   expect_equal(exp_case_2_1,case_2_1)
#   expect_equal(exp_ebenpde_tmp_2_2,ebenpde_tmp_2_2)
#   expect_equal(exp_n.subjects_2_2,n.subjects_2_2)
#   expect_equal(exp_ebenpde_obs_2_2,ebenpde_obs_2_2)
#   expect_equal(exp_iiv.eta.names_2_2,iiv.eta.names_2_2)
#   expect_equal(exp_case_2_2,case_2_2)
#   
#   expect_equal(exp_ebenpde_tmp_3_1,ebenpde_tmp_3_1)
#   expect_equal(exp_n.subjects_3_1,n.subjects_3_1)
#   expect_equal(exp_ebenpde_obs_3_1,ebenpde_obs_3_1)
#   expect_equal(exp_iiv.eta.names_3_1,iiv.eta.names_3_1)
#   expect_equal(exp_case_3_1,case_3_1)
#   expect_equal(exp_ebenpde_tmp_3_2,ebenpde_tmp_3_2)
#   expect_equal(exp_n.subjects_3_2,n.subjects_3_2)
#   expect_equal(exp_ebenpde_obs_3_2,ebenpde_obs_3_2)
#   expect_equal(exp_iiv.eta.names_3_2,iiv.eta.names_3_2)
#   expect_equal(exp_case_3_2,case_3_2)
#   
#   expect_equal(exp_ebenpde_tmp_4_1,ebenpde_tmp_4_1)
#   expect_equal(exp_n.subjects_4_1,n.subjects_4_1)
#   expect_equal(exp_ebenpde_obs_4_1,ebenpde_obs_4_1)
#   expect_equal(exp_iiv.eta.names_4_1,iiv.eta.names_4_1)
#   expect_equal(exp_case_4_1,case_4_1)
# })
# 

# .................................  (3) Test summary.table.ebe.npde ...............................   
mydataframe_1 <- summary.table.ebe.npde(ebenpde_obs_1,iiv.eta.names=iiv.eta.names_1)
mydataframe_2 <- summary.table.ebe.npde(ebenpde_obs_2,iiv.eta.names=iiv.eta.names_2)
mydataframe_3 <- summary.table.ebe.npde(ebenpde_obs_3,iiv.eta.names=iiv.eta.names_3)

# Create expected data
exp_mydataframe_1 <- data.frame(factor(c("ETA.1.","ETA.2.","ETA.3.")),
                                as.factor(sprintf("%.5f",c(0.845,-1.06,2.14983))),
                                as.factor(sprintf("%.3f",c(0.688,0.844,0.313))),
                                as.factor(sprintf("%.5f",c(16.14311,9.96008,22.00402))),
                                as.factor(sprintf("%.3f",c(0.173,0.009,0.037))))
colnames(exp_mydataframe_1) <- c("EBE NPDE","mean","p-value (H_0: mean==0)","variance","p-value (H_0: var==1)")

exp_mydataframe_2 <- data.frame(factor(c("ETA.1.","ETA.2.","ETA.3.")),
                                c(5,5,3),
                                as.factor(sprintf("%.5f",c(-0.332,-1.278,2.863))),
                                as.factor(sprintf("%.3f",c(1,1,0.5))),
                                as.factor(sprintf("%.5f",c(9.78892,12.09367,20.4001))),
                                as.factor(sprintf("%.3f",c(0.518,0.014,0.087))))
colnames(exp_mydataframe_2) <- c("EBE NPDE","Number of ID","mean","p-value (H_0: mean==0)","variance","p-value (H_0: var==1)")

exp_mydataframe_3 <- data.frame(factor(c("ETA.1.","ETA.2.","ETA.3.")),
                                c(3,4,2),
                                as.factor(sprintf("%.5f",c(-0.12,0.0675,0.2695))),
                                as.factor(sprintf("%.3f",c(1,0.625,1))),
                                as.factor(sprintf("%.5f",c(1.3468,0.06109,0.44274))),
                                as.factor(sprintf("%.3f",c(0.749,0.461,0.962))))
colnames(exp_mydataframe_3) <- c("EBE NPDE","Number of ID","mean","p-value (H_0: mean==0)","variance","p-value (H_0: var==1)")

# Compare expected data with real data
context("Simeval, ebe npde, function summary.table.ebe.npde")
test_that("If function summary.table.ebe.npde works as expected",{
  expect_equal(exp_mydataframe_1,mydataframe_1)
  expect_equal(exp_mydataframe_2,mydataframe_2)
  expect_equal(exp_mydataframe_3,mydataframe_3)
})

# #.................................  (3) Test summary.table.ebe.npde ...............................   
# mydataframe <- summary.table.ebe.npde(ebenpde_obs,iiv.eta.names=iiv.eta.names)
# mydataframe_a <- summary.table.ebe.npde(ebenpde_obs_a,iiv.eta.names=iiv.eta.names_a)
# 
# # Create expected data
# exp_mydataframe <- data.frame(as.factor(c("ETA(1)","ETA(2)")),
#                               c(5,5),
#                               as.factor(sprintf("%.5f",c(-0.465,-0.146))),
#                               as.factor(sprintf("%.3f",c(0.584,0.201))),
#                               as.factor(sprintf("%.5f",c(1.58003,0.05473))),
#                               as.factor(sprintf("%.3f",c(0.745,0.355))))
# colnames(exp_mydataframe) <- c("EBE NPDE","Amount of ETAs","mean","p-value (H_0: mean==0)","variance","p-value (H_0: var==1)")
# 
# exp_mydataframe_a <- data.frame(factor(c("ETA(1)","ETA(2)","ETA(3)")),
#                                 c(6,6,6),
#                                 as.factor(sprintf("%.5f",c(0.845,-1.06,2.14983))),
#                                 as.factor(sprintf("%.3f",c(0.688,0.844,0.313))),
#                                 as.factor(sprintf("%.5f",c(16.14311,9.96008,22.00402))),
#                                 as.factor(sprintf("%.3f",c(0.173,0.009,0.037))))
# colnames(exp_mydataframe_a) <- c("EBE NPDE","Amount of ETAs","mean","p-value (H_0: mean==0)","variance","p-value (H_0: var==1)")
# 
# # Compare expected data with real data
# context("Simeval, ebe npde, function summary.table.ebe.npde")
# test_that("If function summary.table.ebe.npde works as expected",{
#   expect_equal(exp_mydataframe,mydataframe)
#   expect_equal(exp_mydataframe_a,mydataframe_a)
# })
# 

#.................................  (4) Test empirical.distance  ....................................    
list_emp_1 <- empirical.distance(ebenpde_obs_1,n.subjects_1)
list_emp_2 <- empirical.distance(ebenpde_obs_2,n.subjects_2)
list_emp_3 <- empirical.distance(ebenpde_obs_3,n.subjects_3)

#unlist
emp_dist_1 <- list_emp_1$emp_distance
emp_dist_2 <- list_emp_2$emp_distance
emp_dist_3 <- list_emp_3$emp_distance
id_iiv.eta.names_1 <- list_emp_1$id_iiv.eta.names
id_iiv.eta.names_2 <- list_emp_2$id_iiv.eta.names
id_iiv.eta.names_3 <- list_emp_3$id_iiv.eta.names

# Create expected data
exp_emp_dist_1 <- matrix(c(2.0501,1.116401,62.0435,133.4141,1.3574,79.311),nrow=6,ncol=1)
exp_emp_dist_2 <- matrix(c(2.0501,1.116401,0.0225,133.4141,0.3364,24.6989),nrow=6,ncol=1)
exp_emp_dist_3 <- matrix(c(2.0501,1.116401,0.0225,0.3373),nrow=4,ncol=1)
exp_id_iiv.eta.names_1 <- list(c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),
                               c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."))
exp_id_iiv.eta.names_2 <- list(c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),c("ETA.2."),
                               c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1."),c("ETA.1.","ETA.2."))
exp_id_iiv.eta.names_3 <- list(c("ETA.1.","ETA.2.","ETA.3."),c("ETA.1.","ETA.2.","ETA.3."),c("ETA.2."),c("ETA.1.","ETA.2."))

# Compare expected data with real data
context("Simeval, ebe npde, function empirical.distance")
test_that("If function empirical.distance works as expected",{
  expect_equal(exp_emp_dist_1,emp_dist_1)
  expect_equal(exp_emp_dist_2,emp_dist_2)
  expect_equal(exp_emp_dist_3,emp_dist_3)
  expect_equal(exp_id_iiv.eta.names_1,id_iiv.eta.names_1)
  expect_equal(exp_id_iiv.eta.names_2,id_iiv.eta.names_2)
  expect_equal(exp_id_iiv.eta.names_3,id_iiv.eta.names_3)
})

# #.................................  (4) Test empirical.distance  ....................................    
# emp_dist <- empirical.distance(ebenpde_obs,n.subjects,iiv.eta.names)
# emp_dist_a <- empirical.distance(ebenpde_obs_a,n.subjects_a,iiv.eta.names_a)
# emp_dist_2 <- empirical.distance(ebenpde_obs=ebenpde_obs_2,n.subjects=n.subjects_2,iiv.eta.names=iiv.eta.names_2)
# # Create expected data
# exp_emp_dist <- matrix(c(2.489864,0.2916,0.5554,0.2116,4.178269),nrow=5,ncol=1)
# exp_emp_dist_a <- matrix(c(2.0501,1.116401,62.0435,133.4141,1.3574,79.311),nrow=6,ncol=1)
# # Compare expected data with real data
# context("Simeval, ebe npde, function empirical.distance")
# test_that("If function empirical.distance works as expected",{
#   expect_equal(exp_emp_dist,emp_dist)
#   expect_equal(exp_emp_dist_a,emp_dist_a)
# })
# 

#.......................................  (5) Test data.for.plots  ...............................................
list_out_tables_1 <- data.for.plots(emp_dist_1,n.subjects_1,iiv.eta.names_1)
list_out_tables_2 <- data.for.plots(emp_dist_2,n.subjects_2,iiv.eta.names_2)
list_out_tables_3 <- data.for.plots(emp_dist_3,n.subjects_3,iiv.eta.names_3)

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


# #.......................................  (5) Test data.for.plots  ...............................................
# list_out_tables <- data.for.plots(emp_dist,n.subjects,iiv.eta.names)
# list_out_tables_a <- data.for.plots(emp_dist_a,n.subjects_a,iiv.eta.names_a)
# 
# # Unlist data.for.plots
# index_emp_distance <- list_out_tables$index_emp_distance
# emp_distance_sort <- round(list_out_tables$emp_distance_sort,6)
# theor_distance <- round(list_out_tables$theor_distance,6)
# out_distance <- round(list_out_tables$out_distance,6)
# ebe_npde_quant <- round(list_out_tables$ebe_npde_quant,6)
# 
# index_emp_distance_a <- list_out_tables_a$index_emp_distance
# emp_distance_sort_a <- round(list_out_tables_a$emp_distance_sort,6)
# theor_distance_a <- round(list_out_tables_a$theor_distance,6)
# out_distance_a <- round(list_out_tables_a$out_distance,4)
# ebe_npde_quant_a <- round(list_out_tables_a$ebe_npde_quant,6)
# 
# # Create expected data
# exp_index_emp_distance <- list(x=c(0.2116,0.2916,0.5554,2.489864,4.178269),
#                                ix=c(4,2,3,1,5))
# exp_emp_distance_sort <- c(0.2116,0.2916,0.5554,2.489864,4.178269)
# exp_theor_distance <- c(0.210721,0.71335,1.386294,2.407946,4.60517)
# exp_out_distance <- 0.301865
# exp_ebe_npde_quant <- c(0.1,0.3,0.5,0.7,0.9)
# 
# exp_index_emp_distance_a <- list(x=c(1.116401,1.3574,2.0501,62.0435,79.311,133.4141),
#                                ix=c(2,5,1,3,6,4))
# exp_emp_distance_sort_a <- c(1.116401,1.3574,2.0501,62.0435,79.311,133.4141)
# exp_theor_distance_a<- c(0.510101,1.212533,1.947696,2.84179,4.108345,6.666203)
# exp_out_distance_a <- -89.6243
# exp_ebe_npde_quant_a <- c(0.083333,0.25,0.416667,0.583333,0.75,0.916667)
# 
# # Compare expected data with real data
# context("Simeval, ebe npde, function data.for.plots")
# test_that("If function data.for.plots works as expected",{
#   expect_equal(exp_index_emp_distance,index_emp_distance)
#   expect_equal(exp_emp_distance_sort,emp_distance_sort)
#   expect_equal(exp_theor_distance,theor_distance)
#   expect_equal(exp_out_distance,out_distance)
#   expect_equal(exp_ebe_npde_quant,ebe_npde_quant)
#   expect_equal(exp_index_emp_distance_a,index_emp_distance_a)
#   expect_equal(exp_emp_distance_sort_a,emp_distance_sort_a)
#   expect_equal(exp_theor_distance_a,theor_distance_a)
#   expect_equal(exp_out_distance_a,out_distance_a)
#   expect_equal(exp_ebe_npde_quant_a,ebe_npde_quant_a)
# })
#

#.....................................   (6) Test plot_1   .................................    
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



# #.....................................   (6) Test plot_1   .................................    
# list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
#                  out_distance,n.subjects,outlying_criteria=1,do_outlier_plot=FALSE,model.filename)
# a_list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
#                  out_distance,n.subjects,outlying_criteria=-1,do_outlier_plot=FALSE,model.filename)
# b_list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
#                         out_distance,n.subjects,outlying_criteria=2,do_outlier_plot=FALSE,model.filename)
# c_list_plot_1 <- plot_1(ebenpde_tmp_a,theor_distance_a,emp_distance_sort_a,index_emp_distance_a,
#                         out_distance_a,n.subjects_a,outlying_criteria=-3,do_outlier_plot=FALSE,model.filename)
# # unlist information
# flag <- list_plot_1$flag
# noutlier <- list_plot_1$noutlier
# outlier_id_row <-list_plot_1$outlier_id_row
# outlier_ID <- list_plot_1$outlier_ID
# identityline <- list_plot_1$identityline
# vector_text <- list_plot_1$vector_text
# a_flag <- a_list_plot_1$flag
# a_noutlier <- a_list_plot_1$noutlier
# a_outlier_id_row <-a_list_plot_1$outlier_id_row
# a_identityline <- a_list_plot_1$identityline
# b_flag <- b_list_plot_1$flag
# b_noutlier <- b_list_plot_1$noutlier
# b_outlier_id_row <-b_list_plot_1$outlier_id_row
# b_outlier_ID <- b_list_plot_1$outlier_ID
# b_identityline <- b_list_plot_1$identityline
# b_vector_text <- b_list_plot_1$vector_text
# c_flag <- c_list_plot_1$flag
# c_noutlier <- c_list_plot_1$noutlier
# c_outlier_id_row <- c_list_plot_1$outlier_id_row
# c_outlier_ID <- c_list_plot_1$outlier_ID
# c_identityline <- c_list_plot_1$identityline
# c_vector_text <- c_list_plot_1$vector_text
# # Create expected data
# exp_noutlier <- 1
# exp_flag <- 1
# exp_outlier_id_row <- 5
# exp_outlier_ID <- 56
# exp_identityline <- c(1,2,3,4,5)
# exp_vector_text <- matrix(c("","","","","56"),nrow = 5,ncol = 1)
# exp_a_noutlier <- 0
# exp_a_flag <- 0
# exp_a_outlier_id_row <- NULL
# exp_a_identityline <- c(1,2,3,4,5)
# exp_b_noutlier <- 1
# exp_b_flag <- 1
# exp_b_outlier_id_row <- 5
# exp_b_outlier_ID <- 56
# exp_b_identityline <- c(1,2,3,4,5)
# exp_b_vector_text <- matrix(c("","","","","56"),nrow = 5,ncol = 1)
# exp_c_noutlier <- 1
# exp_c_flag <- 1
# exp_c_outlier_id_row <- 4
# exp_c_outlier_ID <- 14
# exp_c_identityline <- c(1,2,3,4,5,6)
# exp_c_vector_text <- matrix(c("","","","","","14"),nrow = 6,ncol = 1)
# # Compare expected data with real data
# context("Simeval, ebe npde, function plot_1")
# test_that("If function plot_1 works as expected",{
#   expect_equal(exp_noutlier,noutlier)
#   expect_equal(exp_flag,flag)
#   expect_equal(exp_outlier_id_row,outlier_id_row)
#   expect_equal(exp_outlier_ID,outlier_ID)
#   expect_equal(exp_identityline,identityline)
#   expect_equal(exp_vector_text,vector_text)
#   expect_equal(exp_a_noutlier,a_noutlier)
#   expect_equal(exp_a_flag,a_flag)
#   expect_equal(exp_a_outlier_id_row,a_outlier_id_row)
#   expect_equal(exp_a_identityline,a_identityline)
#   expect_equal(exp_b_noutlier,b_noutlier)
#   expect_equal(exp_b_flag,b_flag)
#   expect_equal(exp_b_outlier_id_row,b_outlier_id_row)
#   expect_equal(exp_b_outlier_ID,b_outlier_ID)
#   expect_equal(exp_b_identityline,b_identityline)
#   expect_equal(exp_b_vector_text,b_vector_text)
#   expect_equal(exp_c_noutlier,c_noutlier)
#   expect_equal(exp_c_flag,c_flag)
#   expect_equal(exp_c_outlier_id_row,c_outlier_id_row)
#   expect_equal(exp_c_outlier_ID,c_outlier_ID)
#   expect_equal(exp_c_identityline,c_identityline)
#   expect_equal(exp_c_vector_text,c_vector_text)
# })
# 

#..................................... (7) Test plot_2  ......................................
list_plot_2_1 <- plot_2(ebenpde_tmp_1,emp_distance_sort_1,theor_distance_1,index_emp_distance_1,
                 noutlier_1,flag_1,n.subjects_1,iiv.eta.names_1,outlying_criteria=-2.7,
                 outlier_id_row_1,do_outlier_plot=FALSE,model.filename)
list_plot_2_2 <- plot_2(ebenpde_tmp_2,emp_distance_sort_2,theor_distance_2,index_emp_distance_2,
                        noutlier_2,flag_2,n.subjects_2,iiv.eta.names_2,outlying_criteria=-2.7,
                        outlier_id_row_2,do_outlier_plot=FALSE,model.filename)
list_plot_2_3 <- plot_2(ebenpde_tmp_3,emp_distance_sort_3,theor_distance_3,index_emp_distance_3,
                        noutlier_3,flag_3,n.subjects_3,iiv.eta.names_3,outlying_criteria=-2.7,
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



# #..................................... (7) Test plot_2  ......................................
# list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
#                  noutlier,flag,n.subjects,iiv.eta.names,outlying_criteria=1,
#                  outlier_id_row,do_outlier_plot=FALSE,model.filename)
# a_list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
#                       a_noutlier,a_flag,n.subjects,iiv.eta.names,outlying_criteria=1,
#                       a_outlier_id_row,do_outlier_plot=FALSE,model.filename)
# b_list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
#                         b_noutlier,b_flag,n.subjects,iiv.eta.names,outlying_criteria=2,
#                         b_outlier_id_row,do_outlier_plot=FALSE,model.filename)
# c_list_plot_2 <- plot_2(ebenpde_tmp_a,emp_distance_sort_a,theor_distance_a,index_emp_distance_a,
#                         c_noutlier,c_flag,n.subjects_a,iiv.eta.names_a,outlying_criteria=-3,
#                         c_outlier_id_row,do_outlier_plot=FALSE,model.filename)
# # unlist information
# vector_theor_dist <- round(list_plot_2$vector_theor_dist,6)
# noutlier <- list_plot_2$noutlier
# outlier_id_row <- list_plot_2$outlier_id_row
# flag1 <- list_plot_2$flag1
# out_distance <- round(list_plot_2$out_distance,7)
# a_vector_theor_dist <- round(a_list_plot_2$vector_theor_dist,6)
# a_noutlier <- a_list_plot_2$noutlier
# a_outlier_id_row <- a_list_plot_2$outlier_id_row
# a_flag1 <- a_list_plot_2$flag1
# a_out_distance <- round(a_list_plot_2$out_distance,7)
# b_vector_theor_dist <- round(b_list_plot_2$vector_theor_dist,6)
# b_noutlier <- b_list_plot_2$noutlier
# b_outlier_id_row <- b_list_plot_2$outlier_id_row
# b_flag1 <- b_list_plot_2$flag1
# b_out_distance <- round(b_list_plot_2$out_distance,7)
# c_vector_theor_dist <- round(c_list_plot_2$vector_theor_dist,6)
# c_noutlier <- c_list_plot_2$noutlier
# c_outlier_id_row <- c_list_plot_2$outlier_id_row
# c_flag1 <- c_list_plot_2$flag1
# c_out_distance <- round(c_list_plot_2$out_distance,7)
# # Create expected data
# exp_vector_theor_dist <- matrix(c(1.386294,2.772589,3.583519,4.158883,4.60517),nrow=1,ncol=5)
# exp_noutlier <- 1
# exp_outlier_id_row <- 5
# exp_flag1 <- 0
# exp_out_distance <- c(1.1801747,2.1412034,1.7543239,0.8306343)
# exp_a_vector_theor_dist <- matrix(c(1.386294,2.772589,3.583519,4.158883,4.60517),nrow=1,ncol=5)
# exp_a_noutlier <- 0
# exp_a_outlier_id_row <- NULL
# exp_a_flag1 <- 0
# exp_a_out_distance <- c(1.1801747,2.1412034,1.7543239,0.8306343)
# exp_b_vector_theor_dist <- matrix(c(1.386294,2.772589,3.583519,4.158883,4.60517),nrow=1,ncol=5)
# exp_b_noutlier <- 2
# exp_b_outlier_id_row <- c(5,1)
# exp_b_flag1 <- 0
# exp_b_out_distance <- c(1.1801747,2.1412034,1.7543239,0.8306343)
# exp_c_vector_theor_dist <- matrix(c(2.365974,4.108345,5.071066,5.739413,6.251389,6.666203),nrow=1,ncol=6)
# exp_c_noutlier <- 3
# exp_c_outlier_id_row <- c(4,6,3)
# exp_c_flag1 <- 0
# exp_c_out_distance <- c(-51.6609466,-39.8130014,2.1361458,1.9452118,0.8835815)
# # Compare expected data with real data
# context("Simeval, ebe npde, function plot_2")
# test_that("If function plot_2 works as expected",{
#   expect_equal(exp_vector_theor_dist,vector_theor_dist)
#   expect_equal(exp_noutlier,noutlier)
#   expect_equal(exp_outlier_id_row,outlier_id_row)
#   expect_equal(exp_flag1,flag1)
#   expect_equal(exp_out_distance,out_distance)
#   expect_equal(exp_a_vector_theor_dist,a_vector_theor_dist)
#   expect_equal(exp_a_noutlier,a_noutlier)
#   expect_equal(exp_a_outlier_id_row,a_outlier_id_row)
#   expect_equal(exp_a_flag1,a_flag1)
#   expect_equal(exp_a_out_distance,a_out_distance)
#   expect_equal(exp_b_vector_theor_dist,b_vector_theor_dist)
#   expect_equal(exp_b_noutlier,b_noutlier)
#   expect_equal(exp_b_outlier_id_row,b_outlier_id_row)
#   expect_equal(exp_b_flag1,b_flag1)
#   expect_equal(exp_b_outlier_id_row,b_outlier_id_row)
#   expect_equal(exp_c_vector_theor_dist,c_vector_theor_dist)
#   expect_equal(exp_c_noutlier,c_noutlier)
#   expect_equal(exp_c_outlier_id_row,c_outlier_id_row)
#   expect_equal(exp_c_flag1,c_flag1)
#   expect_equal(exp_c_out_distance,c_out_distance)
# })
# 

#...................................  (8) Test outlier.table.ebe.npde  .........................................
fortable1_1 <- outlier.table.ebe.npde(noutlier_2_1,outlier_id_row_2_1,ebenpde_tmp_1,ebenpde_obs_1,
                           index_emp_distance_1,emp_distance_sort_1,vector_theor_dist_2_1,
                           n.subjects_1,iiv.eta.names_1)
fortable1_2 <- outlier.table.ebe.npde(noutlier_2_2,outlier_id_row_2_2,ebenpde_tmp_2,ebenpde_obs_2,
                                      index_emp_distance_2,emp_distance_sort_2,vector_theor_dist_2_2,
                                      n.subjects_2,iiv.eta.names_2)
fortable1_3 <- outlier.table.ebe.npde(noutlier_2_3,outlier_id_row_2_3,ebenpde_tmp_3,ebenpde_obs_3,
                                      index_emp_distance_3,emp_distance_sort_3,vector_theor_dist_2_3,
                                      n.subjects_3,iiv.eta.names_3)

# Create expected data
exp_fortable1_1 <- data.frame(c(14,26,9),c(-89.624,-51.661,-39.813),c(133.41,79.311,62.044),c(3.6,-4.9,6.73),c(-7.46,0.83,0.15),c(8.05,7.39,-4.09))
colnames(exp_fortable1_1) <- c("ID", "outlying criteria","MD distance","ETA.1.","ETA.2.","ETA.3.")

exp_fortable1_2 <- data.frame(c(14,26),c(-89.624,-13.044),c(133.41,24.699),c(3.6,-4.9),c(-7.46,0.83),c(8.05,NA))
colnames(exp_fortable1_2) <- c("ID", "outlying criteria","MD distance","ETA.1.","ETA.2.","ETA.3.")

exp_fortable1_3 <- data.frame(C = c("No outliers detected"))
names(exp_fortable1_3) <- NULL

# Compare expected data with real data
context("Simeval, ebe npde, function outlier.table.ebe.npde")
test_that("If function outlier.table.ebe.npde works as expected",{
  expect_equal(exp_fortable1_1,fortable1_1)
  expect_equal(exp_fortable1_2,fortable1_2)
  expect_equal(exp_fortable1_3,fortable1_3)
})

# #...................................  (8) Test outlier.table.ebe.npde  .........................................
# fortable1 <- outlier.table.ebe.npde(noutlier,outlier_id_row,ebenpde_tmp,ebenpde_obs,
#                            index_emp_distance,emp_distance_sort,vector_theor_dist,
#                            n.subjects,iiv.eta.names=iiv.eta.names)
# b_fortable1 <- outlier.table.ebe.npde(b_noutlier,b_outlier_id_row,ebenpde_tmp,ebenpde_obs,
#                              index_emp_distance,emp_distance_sort,b_vector_theor_dist,
#                              n.subjects,iiv.eta.names=iiv.eta.names)
# # Create expected data
# exp_fortable1 <- data.frame(c(56),c(0.30186),c(4.1783),c(-2.037),c(-0.17))
# colnames(exp_fortable1) <- c("ID", "outlying criteria","MD distance","ETA(1)","ETA(2)")
# exp_b_fortable1 <- data.frame(c(56,2),c(0.30186,1.1802),c(4.1783,2.4899),c(-2.037,-1.558),c(-0.17,-0.25))
# colnames(exp_b_fortable1) <- c("ID", "outlying criteria","MD distance","ETA(1)","ETA(2)")
# # Compare expected data with real data
# context("Simeval, ebe npde, function outlier.table.ebe.npde")
# test_that("If function outlier.table.ebe.npde works as expected",{
#   expect_equal(exp_fortable1,fortable1)
#   expect_equal(exp_b_fortable1,b_fortable1)
# })
# 
# #...................................  (9) Test ebe.npde.outliers  .........................................
# #input
# # outlier tables
# ebe.npde_outliers_1 <- data.frame(C=56,C=0.30186)
# colnames(ebe.npde_outliers_1) <- c("ID","outlying criteria")
# 
# ebe.npde_outliers_2 <- data.frame(C = c("No outliers detected"))
# colnames(ebe.npde_outliers_2) <- NULL
# 
# list_ebe_outlier_table_1 <- list(ebe.npde_outliers_1)
# list_ebe_outlier_table_2 <- list(ebe.npde_outliers_2)
# 
# # use function
# list_out_1 <- ebe.npde.all.outliers(list_ebe_outlier_table_1,cases='',deleted_ID=NULL)
# list_out_2 <- ebe.npde.all.outliers(list_ebe_outlier_table_2,cases='',deleted_ID=NULL)
# # list_out_3 <- ebe.npde.all.outliers(ebe.npde.file=ebe.npde.file_2,iiv.eta.names=iiv.eta.names_a,outlying_criteria=1,model.filename)
# # list_out_4 <- ebe.npde.all.outliers(ebe.npde.file=ebe.npde.file_3,iiv.eta.names=iiv.eta.names_3,outlying_criteria=2.6,model.filename)
# # list_out_5 <- ebe.npde.all.outliers(ebe.npde.file=ebe.npde.file_4,iiv.eta.names=iiv.eta.names_4,outlying_criteria=1.1,model.filename)
# # list_out_6 <- ebe.npde.all.outliers(ebe.npde.file=ebe.npde.file_4,iiv.eta.names=iiv.eta.names_4,outlying_criteria=1.4,model.filename)
# 
# #unlist
# ebe.npde_outliers_1 <- list_out_1$ebe.npde_outliers
# case_message_1 <- list_out_1$case_message
# 
# ebe.npde_outliers_2 <- list_out_2$ebe.npde_outliers
# case_message_2 <- list_out_2$case_message
# # 
# # ebe.npde_outliers_3 <- list_out_3$ebe.npde_outliers
# # case_message_3 <- list_out_3$case_message
# # 
# # ebe.npde_outliers_4 <- list_out_4$ebe.npde_outliers
# # case_message_4 <- list_out_4$case_message
# # 
# # ebe.npde_outliers_5 <- list_out_5$ebe.npde_outliers
# # case_message_5 <- list_out_5$case_message
# # 
# # ebe.npde_outliers_6 <- list_out_6$ebe.npde_outliers
# # case_message_6 <- list_out_6$case_message
# 
# # Create expected data
# exp_ebe.npde_outliers_1 <- data.frame(c(56),c(0.30186))
# colnames(exp_ebe.npde_outliers_1) <- c("ID","outlying criteria")
# exp_case_message_1 <- ''
# 
# exp_ebe.npde_outliers_2 <- data.frame(C = c("No outliers detected"))
# colnames(exp_ebe.npde_outliers_2) <- NULL
# exp_case_message_2 <- ''
# 
# exp_ebe.npde_outliers_3 <- data.frame(C=c(11),C= as.factor(c("NA")),C=as.factor(c(0.548)))
# colnames(exp_ebe.npde_outliers_3) <- c("ID","EBE NPDE outliers (1)","EBE NPDE outliers (2)")
# exp_case_message_3 <- c("Individual based analysis","ETA based analysis")
# exp_deleted_ID_3 <- c(6,9,10,11,13)
# 
# exp_ebe.npde_outliers_4 <- data.frame(C=c(1,4,5,7,11),C= as.factor(c(2.58,2.127,2.055,2.315,"NA")),C=as.factor(c("","","","",1.698)))
# colnames(exp_ebe.npde_outliers_4) <- c("ID","EBE NPDE outliers (1)","EBE NPDE outliers (2)")
# exp_case_message_4 <- c("Individual based analysis","ETA based analysis")
# 
# exp_ebe.npde_outliers_5 <- data.frame(C = c("No outliers detected"))
# colnames(exp_ebe.npde_outliers_5) <- NULL
# exp_case_message_5 <- c("Individual based analysis")
# 
# exp_ebe.npde_outliers_6 <- data.frame(C=c(1,4,5,7),C= as.factor(c(1.097,0.928,0.888,1.3)))
# colnames(exp_ebe.npde_outliers_6) <- c("ID","EBE NPDE outliers (1)")
# exp_case_message_6 <- c("Individual based analysis")
# 
# # Compare expected data with real data
# context("Simeval, ebe npde, function ebe.npde.outliers")
# test_that("If function ebe.npde.outliers works as expected",{
#   expect_equal(exp_ebe.npde_outliers_1,ebe.npde_outliers_1)
#   expect_equal(exp_case_message_1,case_message_1)
#   expect_equal(exp_ebe.npde_outliers_2,ebe.npde_outliers_2)
#   expect_equal(exp_case_message_2,case_message_2)
# #   expect_equal(exp_ebe.npde_outliers_3,ebe.npde_outliers_3)
# #   expect_equal(exp_case_message_3,case_message_3)
# #   expect_equal(exp_ebe.npde_outliers_4,ebe.npde_outliers_4)
# #   expect_equal(exp_case_message_4,case_message_4)
# #   expect_equal(exp_ebe.npde_outliers_5,ebe.npde_outliers_5)
# #   expect_equal(exp_case_message_5,case_message_5)
# #   expect_equal(exp_ebe.npde_outliers_6,ebe.npde_outliers_6)
# #   expect_equal(exp_case_message_6,case_message_6)
# })