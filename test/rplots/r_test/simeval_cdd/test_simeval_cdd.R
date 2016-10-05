library(testthat)
tool = 'simeval_cdd'

#####################    1.Source functions which are going to be testet   ##################
# create a working directory for R scripts
source("../set.working.directory.R")
rscript.w.dir <- fun.rscript.w.dir()

directory_and_script <- find_r_files_in_subdir(toolname = tool,topdir = rscript.w.dir)

#source functions
for (i in 1:length(directory_and_script)) {
  source(directory_and_script[i])
}
# get cdd functions
directory_and_script <- find_r_files_in_subdir(toolname = 'cdd',topdir = rscript.w.dir)

#source functions
for (i in 1:length(directory_and_script)) {
  source(directory_and_script[i])
}
#get simeval functions
directory_and_script <- find_r_files_in_subdir(toolname = 'simeval',topdir = rscript.w.dir)

#source functions
for (i in 1:length(directory_and_script)) {
  source(directory_and_script[i])
}

###################################    2.Input data    ######################################
source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

all.iofv.file <- paste0(files.w.dir,'raw_all_iofv.csv')
all.iofv.file_1 <- paste0(files.w.dir,'raw_all_iofv_1.csv')
all.iofv.file_2 <- paste0(files.w.dir,'raw_all_iofv_2.csv')
raw.results.file_4 <- paste0(files.w.dir,'raw_results_4.csv')
skipped.id.file_4 <- paste0(files.w.dir,'skipped_4.csv')
###################################     3. Make a test    ###################################
#...........................  (1) Test function influential_outliers_data.R .....................................  
out <- influential_outliers_data(all.iofv.file_2,n.subjects=4,samples=3,
                                 raw.results.file_4,skipped.id.file_4)
out_1 <- influential_outliers_data(all.iofv.file,n.subjects=4,samples=3,
                                   raw.results.file_4,skipped.id.file_4)
out_4 <- influential_outliers_data(all.iofv.file_1,n.subjects=4,samples=3,
                                 raw.results.file_4,skipped.id.file_4)
# unlist
infl_data <- out$infl_data
outl_data <- round(out$outl_data,6)
ID <- out$ID
row_cdd <- out$row_cdd
row_simeval <- out$row_simeval
infl_outl <- out$infl_outl
infl_not_outl <- out$infl_not_outl
outl_not_infl <- out$outl_not_infl
not_outl_not_infl <- out$not_outl_not_infl

infl_data_1 <- out_1$infl_data
outl_data_1 <- round(out_1$outl_data,6)
ID_1 <- out_1$ID
row_cdd_1 <- out_1$row_cdd
row_simeval_1 <- out_1$row_simeval
infl_outl_1 <- out_1$infl_outl
infl_not_outl_1 <- out_1$infl_not_outl
outl_not_infl_1 <- out_1$outl_not_infl
not_outl_not_infl_1 <- out_1$not_outl_not_infl

infl_data_4 <- out_4$infl_data
outl_data_4 <- round(out_4$outl_data,6)
ID_4 <- out_4$ID
row_cdd_4 <- out_4$row_cdd
row_simeval_4 <- out_4$row_simeval
infl_outl_4 <- out_4$infl_outl
infl_not_outl_4 <- out_4$infl_not_outl
outl_not_infl_4 <- out_4$outl_not_infl
not_outl_not_infl_4 <- out_4$not_outl_not_infl

# Create expected input data
exp_infl_data <- data.frame("ID_cdd"=c(35,21,16,11,13,39,37,41,34,31,27,42,23,24,38,19),
                            "delta.ofv"=c(0.01,0.4,0.5,0.54,0.6,0.6,0.7,1.4,1.5,1.9,2.1,2.3,3.2,3.2,5.2,6.3))
exp_outl_data <- data.frame("ID_simeval"=c(19,4,20,1),"iofv_res"=c(-4.758828,0,0.098918,15.305677))
exp_ID <- 19
exp_row_cdd <- 16
exp_row_simeval <- 1
exp_infl_outl <- 19
exp_infl_not_outl <- 38
exp_outl_not_infl <- 1
exp_not_outl_not_infl <- c(4,20)

exp_infl_data_1 <- data.frame("ID_cdd"=c(35,21,16,11,13,39,37,41,34,31,27,42,23,24,38,19),
                            "delta.ofv"=c(0.01,0.4,0.5,0.54,0.6,0.6,0.7,1.4,1.5,1.9,2.1,2.3,3.2,3.2,5.2,6.3))
exp_outl_data_1 <- data.frame("ID_simeval"=c(19,4,20,1),"iofv_res"=c(-1.23551,0,0.098918,1.912686))
exp_ID_1 <- integer(0)
exp_row_cdd_1 <- integer(0)
exp_row_simeval_1 <- integer(0)
exp_infl_outl_1 <- integer(0)
exp_infl_not_outl_1 <- c(19,38)
exp_outl_not_infl_1 <- NULL
exp_not_outl_not_infl_1 <- c(4,20,1)

exp_infl_data_4 <- data.frame("ID_cdd"=c(35,21,16,11,13,39,37,41,34,31,27,42,23,24,38,19),
                            "delta.ofv"=c(0.01,0.4,0.5,0.54,0.6,0.6,0.7,1.4,1.5,1.9,2.1,2.3,3.2,3.2,5.2,6.3))
exp_outl_data_4 <- data.frame("ID_simeval"=c(13,4,20,1),"iofv_res"=c(-0.061071,0,0.098918,14.050084))
exp_ID_4 <- integer(0)
exp_row_cdd_4 <- integer(0)
exp_row_simeval_4 <- integer(0)
exp_infl_outl_4 <- integer(0)
exp_infl_not_outl_4 <- c(19,38)
exp_outl_not_infl_4 <- c(1)
exp_not_outl_not_infl_4 <- c(13,4,20)

# Compare expected input data with real input data
context("Test function influential_outliers_data")
test_that("If function influential_outliers_data works as expected",{
  expect_equal(exp_infl_data,infl_data)
  expect_equal(exp_outl_data,outl_data)
  expect_equal(exp_ID,ID)
  expect_equal(exp_row_cdd,row_cdd)
  expect_equal(exp_row_simeval,row_simeval)
  expect_equal(exp_infl_outl,infl_outl)
  expect_equal(exp_infl_not_outl,infl_not_outl)
  expect_equal(exp_outl_not_infl,outl_not_infl)
  expect_equal(exp_not_outl_not_infl,not_outl_not_infl)
  
  expect_equal(exp_infl_data_1,infl_data_1)
  expect_equal(exp_outl_data_1,outl_data_1)
  expect_equal(exp_ID_1,ID_1)
  expect_equal(exp_row_cdd_1,row_cdd_1)
  expect_equal(exp_row_simeval_1,row_simeval_1)
  expect_equal(exp_infl_outl_1,infl_outl_1)
  expect_equal(exp_infl_not_outl_1,infl_not_outl_1)
  expect_equal(exp_outl_not_infl_1,outl_not_infl_1)
  expect_equal(exp_not_outl_not_infl_1,not_outl_not_infl_1)
  
  expect_equal(exp_infl_data_4,infl_data_4)
  expect_equal(exp_outl_data_4,outl_data_4)
  expect_equal(exp_ID_4,ID_4)
  expect_equal(exp_row_cdd_4,row_cdd_4)
  expect_equal(exp_row_simeval_4,row_simeval_4)
  expect_equal(exp_infl_outl_4,infl_outl_4)
  expect_equal(exp_infl_not_outl_4,infl_not_outl_4)
  expect_equal(exp_outl_not_infl_4,outl_not_infl_4)
  expect_equal(exp_not_outl_not_infl_4,not_outl_not_infl_4)
})