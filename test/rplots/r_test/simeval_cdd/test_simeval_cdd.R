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

all.iofv.file_1 <- paste0(files.w.dir,'raw_all_iofv_1.csv')
all.iofv.file_2 <- paste0(files.w.dir,'raw_all_iofv_2.csv')
all.iofv.file_3 <- paste0(files.w.dir,'raw_all_iofv_3.csv')
all.iofv.file_4 <- paste0(files.w.dir,'raw_all_iofv_4.csv')
all.iofv.file_5 <- paste0(files.w.dir,'raw_all_iofv_5.csv')
skipped.id.file_1 <- paste0(files.w.dir,'skipped_1.csv')
skipped.id.file_2 <- paste0(files.w.dir,'skipped_2.csv')
skipped.id.file_3 <- paste0(files.w.dir,'skipped_3.csv')
skipped.id.file_4 <- paste0(files.w.dir,'skipped_4.csv')
raw.results.file_1 <- paste0(files.w.dir,'raw_results_1.csv')
raw.results.file_2 <- paste0(files.w.dir,'raw_results_2.csv')
raw.results.file_3 <- paste0(files.w.dir,'raw_results_3.csv')
raw.results.file_4 <- paste0(files.w.dir,'raw_results_4.csv')
raw.results.file_5 <- paste0(files.w.dir,'raw_results_5.csv')
raw.results.file_6 <- paste0(files.w.dir,'raw_results_6.csv')
raw.results.file_7 <- paste0(files.w.dir,'raw_results_7.csv')
raw.results.file_8 <- paste0(files.w.dir,'raw_results_8.csv')
ebe.npde.file_1 <- paste0(files.w.dir,'ebe_npde_iiv_iov.csv')
ebe.npde.file_2 <- paste0(files.w.dir,'ebe_npde_iiv_iov_1.csv')
ebe.npde.file_3 <- paste0(files.w.dir,'ebe_npde_1.csv')
ebe.npde.file_4 <- paste0(files.w.dir,'ebe_npde_2.csv')
ebe.npde.file_5 <- paste0(files.w.dir,'ebe_npde_3.csv')
ebe.npde.file_6 <- paste0(files.w.dir,'ebe_npde_4.csv')
residual.outliers.file_1 <- paste0(files.w.dir,'residual_outliers_1.csv')
residual.outliers.file_2 <- paste0(files.w.dir,'residual_outliers_2.csv')

###################################     3. Make a test    ###################################
#...........................  (1) Test function influential_outliers_data.R .....................................  
out <- influential_outliers_data(all.iofv.file=all.iofv.file_4,n.subjects=16,samples=3,
                                 raw.results.file=raw.results.file_4,skipped.id.file=skipped.id.file_4,cutoff_delta.ofv=3.84)
out_1 <- influential_outliers_data(all.iofv.file_2,n.subjects=16,samples=3,
                                 raw.results.file_4,skipped.id.file_4,cutoff_delta.ofv=3.84)
out_2 <- influential_outliers_data(all.iofv.file_4,n.subjects=16,samples=3,
                                 raw.results.file_4,skipped.id.file_4,cutoff_delta.ofv=2)
out_3 <- influential_outliers_data(all.iofv.file_3,n.subjects=6,samples=3,
                                   raw.results.file_3,skipped.id.file_3,cutoff_delta.ofv=3.84)
out_5 <- influential_outliers_data(all.iofv.file_4,n.subjects=16,samples=3,
                                   raw.results.file_5,skipped.id.file_4,cutoff_delta.ofv=3.1) # failed ID in outliers
out_6 <- influential_outliers_data(all.iofv.file_4,n.subjects=16,samples=3,
                                   raw.results.file_2,skipped.id.file_4,cutoff_delta.ofv=3.1) # failed ID but no in outliers
out_7 <- influential_outliers_data(all.iofv.file_3,n.subjects=6,samples=3,
                                   raw.results.file_3,skipped.id.file_3,cutoff_delta.ofv=6.5) # no outliers no infl

# unlist
infl_data <- out$infl_data
outl_data <- round(out$outl_data,3)
table_for_plot <- round(out$table_for_plot,3)
ID <- out$ID
row <- out$row
infl_outl <- out$infl_outl
infl_not_outl <- out$infl_not_outl
outl_not_infl <- out$outl_not_infl
not_outl_not_infl <- out$not_outl_not_infl
fail_ID_text <- out$fail_ID_text
deleted_outliers_text <- out$deleted_outliers_text
row_infl_not_outl <- out$row_infl_not_outl
row_outl_not_infl <- out$row_outl_not_infl

infl_data_1 <- out_1$infl_data
outl_data_1 <- round(out_1$outl_data,3)
table_for_plot_1 <- round(out_1$table_for_plot,3)
ID_1 <- out_1$ID
row_1 <- out_1$row
infl_outl_1 <- out_1$infl_outl
infl_not_outl_1 <- out_1$infl_not_outl
outl_not_infl_1 <- out_1$outl_not_infl
not_outl_not_infl_1 <- out_1$not_outl_not_infl
fail_ID_text_1 <- out_1$fail_ID_text
deleted_outliers_text_1 <- out_1$deleted_outliers_text
row_infl_not_outl_1 <- out_1$row_infl_not_outl
row_outl_not_infl_1 <- out_1$row_outl_not_infl

infl_data_2 <- out_2$infl_data
outl_data_2 <- round(out_2$outl_data,3)
table_for_plot_2 <- round(out_2$table_for_plot,3)
ID_2 <- out_2$ID
row_2 <- out_2$row
infl_outl_2 <- out_2$infl_outl
infl_not_outl_2 <- out_2$infl_not_outl
outl_not_infl_2 <- out_2$outl_not_infl
not_outl_not_infl_2 <- out_2$not_outl_not_infl
fail_ID_text_2 <- out_2$fail_ID_text
deleted_outliers_text_2 <- out_2$deleted_outliers_text
row_infl_not_outl_2 <- out_2$row_infl_not_outl
row_outl_not_infl_2 <- out_2$row_outl_not_infl

infl_data_3 <- out_3$infl_data
outl_data_3 <- round(out_3$outl_data,3)
table_for_plot_3 <- round(out_3$table_for_plot,3)
ID_3 <- out_3$ID
row_3 <- out_3$row
infl_outl_3 <- out_3$infl_outl
infl_not_outl_3 <- out_3$infl_not_outl
outl_not_infl_3 <- out_3$outl_not_infl
not_outl_not_infl_3 <- out_3$not_outl_not_infl
fail_ID_text_3 <- out_3$fail_ID_text
deleted_outliers_text_3 <- out_3$deleted_outliers_text
row_infl_not_outl_3 <- out_3$row_infl_not_outl
row_outl_not_infl_3 <- out_3$row_outl_not_infl

infl_data_5 <- out_5$infl_data
outl_data_5 <- round(out_5$outl_data,3)
table_for_plot_5 <- round(out_5$table_for_plot,3)
ID_5 <- out_5$ID
row_5 <- out_5$row
infl_outl_5 <- out_5$infl_outl
infl_not_outl_5 <- out_5$infl_not_outl
outl_not_infl_5 <- out_5$outl_not_infl
not_outl_not_infl_5 <- out_5$not_outl_not_infl
fail_ID_text_5 <- out_5$fail_ID_text
deleted_outliers_text_5 <- out_5$deleted_outliers_text
row_infl_not_outl_5 <- out_5$row_infl_not_outl
row_outl_not_infl_5 <- out_5$row_outl_not_infl

infl_data_6 <- out_6$infl_data
outl_data_6 <- round(out_6$outl_data,3)
table_for_plot_6 <- round(out_6$table_for_plot,3)
ID_6 <- out_6$ID
row_6 <- out_6$row
infl_outl_6 <- out_6$infl_outl
infl_not_outl_6 <- out_6$infl_not_outl
outl_not_infl_6 <- out_6$outl_not_infl
not_outl_not_infl_6 <- out_6$not_outl_not_infl
fail_ID_text_6 <- out_6$fail_ID_text
deleted_outliers_text_6 <- out_6$deleted_outliers_text
row_infl_not_outl_6 <- out_6$row_infl_not_outl
row_outl_not_infl_6 <- out_6$row_outl_not_infl

infl_data_7 <- out_7$infl_data
outl_data_7 <- round(out_7$outl_data,3)
table_for_plot_7 <- round(out_7$table_for_plot,3)
ID_7 <- out_7$ID
row_7 <- out_7$row
infl_outl_7 <- out_7$infl_outl
infl_not_outl_7 <- out_7$infl_not_outl
outl_not_infl_7 <- out_7$outl_not_infl
not_outl_not_infl_7 <- out_7$not_outl_not_infl
fail_ID_text_7 <- out_7$fail_ID_text
deleted_outliers_text_7 <- out_7$deleted_outliers_text
row_infl_not_outl_7 <- out_7$row_infl_not_outl
row_outl_not_infl_7 <- out_7$row_outl_not_infl

# Create expected data
exp_infl_data <- data.frame("ID_cdd"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                            "delta.ofv"=c(0.54,0.6,2.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,2.4,2.3))
exp_outl_data <- data.frame("ID_simeval"=c(35,41,24,39,21,27,13,34,42,37,11,23,31,19,16,38),
                            "iofv_res"=c(-15.254,-4.759,-2.323,-1.705,-1.558,-0.716,-0.218,0.083,0.099,0.951,1.124,1.165,1.847,1.999,7.976,15.306))
exp_table_for_plot <- data.frame("ID"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                                 "cdd_delta.ofv"=c(0.54,0.6,2.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,2.4,2.3),
                                 "simeval_iofv_res"=c(1.124,-0.218,7.976,1.999,-1.558,1.165,-2.323,-0.716,1.847,0.083,-15.254,0.951,15.306,-1.705,-4.759,0.099))
exp_ID <- 38
exp_row <- 13
exp_infl_outl <- 38
exp_infl_not_outl <- 19
exp_outl_not_infl <- c(35,41,16)
exp_not_outl_not_infl <- c(11,13,21,23,24,27,31,34,37,39,42)
exp_fail_ID_text <- ''
exp_deleted_outliers_text <- ''
exp_row_infl_not_outl <- c(4)
exp_row_outl_not_infl <- c(11,15,3)

exp_infl_data_1 <- data.frame("ID_cdd"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                            "delta.ofv"=c(0.54,0.6,2.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,2.4,2.3))
exp_outl_data_1 <- data.frame("ID_simeval"=c(24,39,21,27,38,35,13,34,42,41,37,23,31,19,16,11),
                            "iofv_res"=c(-2.323,-1.705,-1.558,-0.716,-0.657,-0.462,-0.218,0.083,0.099,0.174,0.951,1.165,1.847,1.999,9.238,28.598))
exp_table_for_plot_1 <- data.frame("ID"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                                 "cdd_delta.ofv"=c(0.54,0.6,2.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,2.4,2.3),
                                 "simeval_iofv_res"=c(28.598,-0.218,9.238,1.999,-1.558,1.165,-2.323,-0.716,1.847,0.083,-0.462,0.951,-0.657,-1.705,0.174,0.099))
exp_ID_1 <- integer(0)
exp_row_1 <- NULL
exp_infl_outl_1 <- integer(0)
exp_infl_not_outl_1 <- c(19,38)
exp_outl_not_infl_1 <- c(16,11)
exp_not_outl_not_infl_1 <- c(13,21,23,24,27,31,34,35,37,39,41,42)
exp_fail_ID_text_1 <- ''
exp_deleted_outliers_text_1 <- ''
exp_row_infl_not_outl_1 <- c(4,13)
exp_row_outl_not_infl_1 <- c(3,1)

exp_infl_data_2 <- data.frame("ID_cdd"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                              "delta.ofv"=c(0.54,0.6,2.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,2.4,2.3))
exp_outl_data_2 <-  data.frame("ID_simeval"=c(35,41,24,39,21,27,13,34,42,37,11,23,31,19,16,38),
                               "iofv_res"=c(-15.254,-4.759,-2.323,-1.705,-1.558,-0.716,-0.218,0.083,0.099,0.951,1.124,1.165,1.847,1.999,7.976,15.306))
exp_table_for_plot_2 <- data.frame("ID"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                                   "cdd_delta.ofv"=c(0.54,0.6,2.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,2.4,2.3),
                                   "simeval_iofv_res"=c(1.124,-0.218,7.976,1.999,-1.558,1.165,-2.323,-0.716,1.847,0.083,-15.254,0.951,15.306,-1.705,-4.759,0.099))
exp_ID_2 <- c(41,16,38)
exp_row_2 <- c(15,3,13)
exp_infl_outl_2 <- c(41,16,38)
exp_infl_not_outl_2 <- c(19,23,24,27,42)
exp_outl_not_infl_2 <- 35
exp_not_outl_not_infl_2 <- c(11,13,21,31,34,37,39)
exp_fail_ID_text_2 <- ''
exp_deleted_outliers_text_2 <- ''
exp_row_infl_not_outl_2 <- c(4,6,7,8,16)
exp_row_outl_not_infl_2 <- 11

exp_infl_data_3 <- data.frame("ID_cdd"=c(11,13,16,19,21),
                              "delta.ofv"=c(0.54,0.6,0.5,6.3,0.4))
exp_outl_data_3 <- data.frame("ID_simeval"=c(21,13,16,11,19),
                              "iofv_res"=c(-1.558,-0.218,0.562,1.124,1.999))
exp_table_for_plot_3 <- data.frame("ID"=c(11,13,16,19,21),
                                   "cdd_delta.ofv"=c(0.54,0.6,0.5,6.3,0.4),
                                   "simeval_iofv_res"=c(1.124,-0.218,0.562,1.999,-1.558))
exp_ID_3 <- integer(0)
exp_row_3 <- NULL
exp_infl_outl_3 <- integer(0)
exp_infl_not_outl_3 <- 19
exp_outl_not_infl_3 <- NULL
exp_not_outl_not_infl_3 <- c(11,13,16,21)
exp_fail_ID_text_3 <- 23
exp_deleted_outliers_text_3 <- ''
exp_row_infl_not_outl_3 <- c(4)
exp_row_outl_not_infl_3 <- NULL
  
exp_infl_data_5 <- data.frame("ID_cdd"=c(11,19,21,23,24,27,31,34,39,42),
                            "delta.ofv"=c(0.54,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.6,2.3))
exp_outl_data_5 <- data.frame("ID_simeval"=c(24,39,21,27,34,42,11,23,31,19),
                            "iofv_res"=c(-2.323,-1.705,-1.558,-0.716,0.083,0.099,1.124,1.165,1.847,1.999))
exp_table_for_plot_5 <- data.frame("ID"=c(11,19,21,23,24,27,31,34,39,42),
                                 "cdd_delta.ofv"=c(0.54,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.6,2.3),
                                 "simeval_iofv_res"=c(1.124,1.999,-1.558,1.165,-2.323,-0.716,1.847,0.083,-1.705,0.099))
exp_ID_5 <- integer(0)
exp_row_5 <- NULL
exp_infl_outl_5 <- integer(0)
exp_infl_not_outl_5 <- c(19,23,24)
exp_outl_not_infl_5 <- numeric(0)
exp_not_outl_not_infl_5 <- c(11,21,27,31,34,39,42)
exp_fail_ID_text_5 <-"13, 16, 35, 37, 38, 41"
exp_deleted_outliers_text_5 <- "16, 35, 38, 41"
exp_row_infl_not_outl_5 <- c(2,4,5)
exp_row_outl_not_infl_5 <- NULL

exp_infl_data_6 <-  data.frame("ID_cdd"=c(16,19,23,24,27,35,38,39,41),
                           "delta.ofv"=c(0.5,6.3,3.2,3.2,2.1,0.01,5.2,0.6,1.4))
exp_outl_data_6 <- data.frame("ID_simeval"=c(35,41,24,39,27,23,19,16,38),
                            "iofv_res"=c(-15.254,-4.759,-2.323,-1.705,-0.716,1.165,1.999,7.976,15.306))
exp_table_for_plot_6 <- data.frame("ID"=c(16,19,23,24,27,35,38,39,41),
                                 "cdd_delta.ofv"=c(0.5,6.3,3.2,3.2,2.1,0.01,5.2,0.6,1.4),
                                 "simeval_iofv_res"=c(7.976,1.999,1.165,-2.323,-0.716,-15.254,15.306,-1.705,-4.759))
exp_ID_6 <- 38
exp_row_6 <- 7
exp_infl_outl_6 <- 38
exp_infl_not_outl_6 <- c(19,23,24)
exp_outl_not_infl_6 <- c(35,41,16)
exp_not_outl_not_infl_6 <- c(27,39)
exp_fail_ID_text_6 <- "11, 13, 21, 31, 34, 37, 42"
exp_deleted_outliers_text_6 <- ""
exp_row_infl_not_outl_6 <- c(2,3,4)
exp_row_outl_not_infl_6 <- c(6,9,1)

exp_infl_data_7 <- data.frame("ID_cdd"=c(11,13,16,19,21),
                              "delta.ofv"=c(0.54,0.6,0.5,6.3,0.4))
exp_outl_data_7 <- data.frame("ID_simeval"=c(21,13,16,11,19),
                              "iofv_res"=c(-1.558,-0.218,0.562,1.124,1.999))
exp_table_for_plot_7 <- data.frame("ID"=c(11,13,16,19,21),
                                   "cdd_delta.ofv"=c(0.54,0.6,0.5,6.3,0.4),
                                   "simeval_iofv_res"=c(1.124,-0.218,0.562,1.999,-1.558))
exp_ID_7 <- integer(0)
exp_row_7 <- NULL
exp_infl_outl_7 <- integer(0)
exp_infl_not_outl_7 <- integer(0)
exp_outl_not_infl_7 <- NULL
exp_not_outl_not_infl_7 <- c(11,13,16,19,21)
exp_fail_ID_text_7 <- 23
exp_deleted_outliers_text_7 <- ''
exp_row_infl_not_outl_7 <- NULL
exp_row_outl_not_infl_7 <- NULL

# Compare expected data with real data
context("CDD/Simeval, function influential_outliers_data")
test_that("If function influential_outliers_data works as expected",{
  expect_equal(exp_infl_data,infl_data)
  expect_equal(exp_outl_data,outl_data)
  expect_equal(exp_table_for_plot,table_for_plot)
  expect_equal(exp_ID,ID)
  expect_equal(exp_row,row)
  expect_equal(exp_infl_outl,infl_outl)
  expect_equal(exp_infl_not_outl,infl_not_outl)
  expect_equal(exp_outl_not_infl,outl_not_infl)
  expect_equal(exp_not_outl_not_infl,not_outl_not_infl)
  expect_equal(exp_fail_ID_text,fail_ID_text)
  expect_equal(exp_deleted_outliers_text,deleted_outliers_text)
  expect_equal(exp_row_infl_not_outl,row_infl_not_outl)
  expect_equal(exp_row_outl_not_infl,row_outl_not_infl)
  
  expect_equal(exp_infl_data_1,infl_data_1)
  expect_equal(exp_outl_data_1,outl_data_1)
  expect_equal(exp_table_for_plot_1,table_for_plot_1)
  expect_equal(exp_ID_1,ID_1)
  expect_equal(exp_row_1,row_1)
  expect_equal(exp_infl_outl_1,infl_outl_1)
  expect_equal(exp_infl_not_outl_1,infl_not_outl_1)
  expect_equal(exp_outl_not_infl_1,outl_not_infl_1)
  expect_equal(exp_not_outl_not_infl_1,not_outl_not_infl_1)
  expect_equal(exp_fail_ID_text_1,fail_ID_text_1)
  expect_equal(exp_deleted_outliers_text_1,deleted_outliers_text_1)
  expect_equal(exp_row_infl_not_outl_1,row_infl_not_outl_1)
  expect_equal(exp_row_outl_not_infl_1,row_outl_not_infl_1)
  
  expect_equal(exp_infl_data_2,infl_data_2)
  expect_equal(exp_outl_data_2,outl_data_2)
  expect_equal(exp_table_for_plot_2,table_for_plot_2)
  expect_equal(exp_ID_2,ID_2)
  expect_equal(exp_row_2,row_2)
  expect_equal(exp_infl_outl_2,infl_outl_2)
  expect_equal(exp_infl_not_outl_2,infl_not_outl_2)
  expect_equal(exp_outl_not_infl_2,outl_not_infl_2)
  expect_equal(exp_not_outl_not_infl_2,not_outl_not_infl_2)
  expect_equal(exp_fail_ID_text_2,fail_ID_text_2)
  expect_equal(exp_deleted_outliers_text_2,deleted_outliers_text_2)
  expect_equal(exp_row_infl_not_outl_2,row_infl_not_outl_2)
  expect_equal(exp_row_outl_not_infl_2,row_outl_not_infl_2)
  
  expect_equal(exp_infl_data_3,infl_data_3)
  expect_equal(exp_outl_data_3,outl_data_3)
  expect_equal(exp_table_for_plot_3,table_for_plot_3)
  expect_equal(exp_ID_3,ID_3)
  expect_equal(exp_row_3,row_3)
  expect_equal(exp_infl_outl_3,infl_outl_3)
  expect_equal(exp_infl_not_outl_3,infl_not_outl_3)
  expect_equal(exp_outl_not_infl_3,outl_not_infl_3)
  expect_equal(exp_not_outl_not_infl_3,not_outl_not_infl_3)
  expect_equal(exp_fail_ID_text_3,fail_ID_text_3)
  expect_equal(exp_deleted_outliers_text_3,deleted_outliers_text_3)
  expect_equal(exp_row_infl_not_outl_3,row_infl_not_outl_3)
  expect_equal(exp_row_outl_not_infl_3,row_outl_not_infl_3)
  
  expect_equal(exp_infl_data_5,infl_data_5)
  expect_equal(exp_outl_data_5,outl_data_5)
  expect_equal(exp_table_for_plot_5,table_for_plot_5)
  expect_equal(exp_ID_5,ID_5)
  expect_equal(exp_row_5,row_5)
  expect_equal(exp_infl_outl_5,infl_outl_5)
  expect_equal(exp_infl_not_outl_5,infl_not_outl_5)
  expect_equal(exp_outl_not_infl_5,outl_not_infl_5)
  expect_equal(exp_not_outl_not_infl_5,not_outl_not_infl_5)
  expect_equal(exp_fail_ID_text_5,fail_ID_text_5)
  expect_equal(exp_deleted_outliers_text_5,deleted_outliers_text_5)
  expect_equal(exp_row_infl_not_outl_5,row_infl_not_outl_5)
  expect_equal(exp_row_outl_not_infl_5,row_outl_not_infl_5)
  
  expect_equal(exp_infl_data_6,infl_data_6)
  expect_equal(exp_outl_data_6,outl_data_6)
  expect_equal(exp_table_for_plot_6,table_for_plot_6)
  expect_equal(exp_ID_6,ID_6)
  expect_equal(exp_row_6,row_6)
  expect_equal(exp_infl_outl_6,infl_outl_6)
  expect_equal(exp_infl_not_outl_6,infl_not_outl_6)
  expect_equal(exp_outl_not_infl_6,outl_not_infl_6)
  expect_equal(exp_not_outl_not_infl_6,not_outl_not_infl_6)
  expect_equal(exp_fail_ID_text_6,fail_ID_text_6)
  expect_equal(exp_deleted_outliers_text_6,deleted_outliers_text_6)
  expect_equal(exp_row_infl_not_outl_6,row_infl_not_outl_6)
  expect_equal(exp_row_outl_not_infl_6,row_outl_not_infl_6)
  
  expect_equal(exp_infl_data_7,infl_data_7)
  expect_equal(exp_outl_data_7,outl_data_7)
  expect_equal(exp_table_for_plot_7,table_for_plot_7)
  expect_equal(exp_ID_7,ID_7)
  expect_equal(exp_row_7,row_7)
  expect_equal(exp_infl_outl_7,infl_outl_7)
  expect_equal(exp_infl_not_outl_7,infl_not_outl_7)
  expect_equal(exp_outl_not_infl_7,outl_not_infl_7)
  expect_equal(exp_not_outl_not_infl_7,not_outl_not_infl_7)
  expect_equal(exp_fail_ID_text_7,fail_ID_text_7)
  expect_equal(exp_deleted_outliers_text_7,deleted_outliers_text_7)
  expect_equal(exp_row_infl_not_outl_7,row_infl_not_outl_7)
  expect_equal(exp_row_outl_not_infl_7,row_outl_not_infl_7)
})

#...........................  (2) Test function ebe.cook.score.data.R .....................................  
list_data_1 <- ebe_cook.score_data(ebe.npde.file=ebe.npde.file_1,eta.names=c('ETA.1.','ETA.2.','ETA.3.','ETA.4.','ETA.5.','ETA.6.'),
                                   outlying_criteria=3, 
                                   raw.results.file=raw.results.file_1,skipped.id.file=skipped.id.file_1,cutoff_cook=0.8) #outl, infl
list_data_2 <- ebe_cook.score_data(ebe.npde.file=ebe.npde.file_2,eta.names=c('ETA.1.','ETA.2.','ETA.3.','ETA.4.','ETA.5.','ETA.6.'),
                                   outlying_criteria=3, 
                                   raw.results.file=raw.results.file_1,skipped.id.file=skipped.id.file_1,cutoff_cook=0.8) #no outl, infl
list_data_3 <- ebe_cook.score_data(ebe.npde.file=ebe.npde.file_1,eta.names=c('ETA.1.','ETA.2.','ETA.3.','ETA.4.','ETA.5.','ETA.6.'),
                                   outlying_criteria=3, 
                                   raw.results.file=raw.results.file_1,skipped.id.file=skipped.id.file_1,cutoff_cook=0.95) #oult, no infl
list_data_4 <- ebe_cook.score_data(ebe.npde.file=ebe.npde.file_2,eta.names=c('ETA.1.','ETA.2.','ETA.3.','ETA.4.','ETA.5.','ETA.6.'),
                                   outlying_criteria=3, 
                                   raw.results.file=raw.results.file_1,skipped.id.file=skipped.id.file_1,cutoff_cook=0.95) #no outlier, no infl

# unlist
infl_data_1 <- list_data_1$infl_data
outl_data_1 <- list_data_1$outl_data
table_for_plot_1 <- list_data_1$table_for_plot
ID_1 <- list_data_1$ID
row_1 <- list_data_1$row
infl_outl_1 <- list_data_1$infl_outl
infl_not_outl_1 <- list_data_1$infl_not_outl
outl_not_infl_1 <- list_data_1$outl_not_infl
not_outl_not_infl_1 <- list_data_1$not_outl_not_infl
row_infl_not_outl_1 <- list_data_1$row_infl_not_outl
row_outl_not_infl_1 <- list_data_1$row_outl_not_infl

infl_data_2 <- list_data_2$infl_data
outl_data_2 <- list_data_2$outl_data
table_for_plot_2 <- list_data_2$table_for_plot
ID_2 <- list_data_2$ID
row_2 <- list_data_2$row
infl_outl_2 <- list_data_2$infl_outl
infl_not_outl_2 <- list_data_2$infl_not_outl
outl_not_infl_2 <- list_data_2$outl_not_infl
not_outl_not_infl_2 <- list_data_2$not_outl_not_infl
row_infl_not_outl_2 <- list_data_2$row_infl_not_outl
row_outl_not_infl_2 <- list_data_2$row_outl_not_infl

infl_data_3 <- list_data_3$infl_data
outl_data_3 <- list_data_3$outl_data
table_for_plot_3 <- list_data_3$table_for_plot
ID_3 <- list_data_3$ID
row_3 <- list_data_3$row
infl_outl_3 <- list_data_3$infl_outl
infl_not_outl_3 <- list_data_3$infl_not_outl
outl_not_infl_3 <- list_data_3$outl_not_infl
not_outl_not_infl_3 <- list_data_3$not_outl_not_infl
row_infl_not_outl_3 <- list_data_3$row_infl_not_outl
row_outl_not_infl_3 <- list_data_3$row_outl_not_infl

infl_data_4 <- list_data_4$infl_data
outl_data_4 <- list_data_4$outl_data
table_for_plot_4 <- list_data_4$table_for_plot
ID_4 <- list_data_4$ID
row_4 <- list_data_4$row
infl_outl_4 <- list_data_4$infl_outl
infl_not_outl_4 <- list_data_4$infl_not_outl
outl_not_infl_4 <- list_data_4$outl_not_infl
not_outl_not_infl_4 <- list_data_4$not_outl_not_infl
row_infl_not_outl_4 <- list_data_4$row_infl_not_outl
row_outl_not_infl_4 <- list_data_4$row_outl_not_infl

# Create expected data
exp_infl_data_1 <- data.frame("ID_cdd"=c(23,14,9,7,3,26),"cook_scores"=c(0.82,0.92,0.81,0.2,0.24,0.6))
exp_outl_data_1 <- data.frame("ID_simeval"=c(3,7,9,14,23,26),"EBE_NPDE"=c(2.5,2.9,0.43,8.05,0.58,4.9))
exp_table_for_plot_1  <- data.frame("ID"=c(3,7,9,14,23,26),"cdd_cook_scores"=c(0.24,0.2,0.81,0.92,0.82,0.6),
                                    "simeval_EBE_NPDE"=c(2.5,2.9,0.43,8.05,0.58,4.9))
exp_ID_1 <- c(14)
exp_row_1 <- c(4)
exp_infl_outl_1 <- c(14)
exp_infl_not_outl_1 <- c(23,9)
exp_outl_not_infl_1 <- c(26)
exp_not_outl_not_infl_1 <- c(3,7)
exp_row_infl_not_outl_1 <- c(5,3)
exp_row_outl_not_infl_1 <- c(6)

exp_infl_data_2 <- data.frame("ID_cdd"=c(14,7,3),"cook_scores"=c(0.92,0.2,0.24))
exp_outl_data_2 <- data.frame("ID_simeval"=c(3,7,14),"EBE_NPDE"=c(2.5,2.9,2.6))
exp_table_for_plot_2  <- data.frame("ID"=c(3,7,14),"cdd_cook_scores"=c(0.24,0.2,0.92),
                                    "simeval_EBE_NPDE"=c(2.5,2.9,2.6))
exp_ID_2 <- numeric(0)
exp_row_2 <- c()
exp_infl_outl_2 <- numeric(0)
exp_infl_not_outl_2 <- c(14)
exp_outl_not_infl_2 <- c()
exp_not_outl_not_infl_2 <- c(3,7)
exp_row_infl_not_outl_2 <- c(3)
exp_row_outl_not_infl_2 <- c()

exp_infl_data_3 <- data.frame("ID_cdd"=c(23,14,9,7,3,26),"cook_scores"=c(0.82,0.92,0.81,0.2,0.24,0.6))
exp_outl_data_3 <- data.frame("ID_simeval"=c(3,7,9,14,23,26),"EBE_NPDE"=c(2.5,2.9,0.43,8.05,0.58,4.9))
exp_table_for_plot_3  <- data.frame("ID"=c(3,7,9,14,23,26),"cdd_cook_scores"=c(0.24,0.2,0.81,0.92,0.82,0.6),
                                    "simeval_EBE_NPDE"=c(2.5,2.9,0.43,8.05,0.58,4.9))
exp_ID_3 <- NULL
exp_row_3 <- c()
exp_infl_outl_3 <- NULL
exp_infl_not_outl_3 <- NULL
exp_outl_not_infl_3 <- c(14,26)
exp_not_outl_not_infl_3 <- c(3,7,9,23)
exp_row_infl_not_outl_3 <- c()
exp_row_outl_not_infl_3 <- c(4,6)

exp_infl_data_4 <- data.frame("ID_cdd"=c(14,7,3),"cook_scores"=c(0.92,0.2,0.24))
exp_outl_data_4 <- data.frame("ID_simeval"=c(3,7,14),"EBE_NPDE"=c(2.5,2.9,2.6))
exp_table_for_plot_4  <- data.frame("ID"=c(3,7,14),"cdd_cook_scores"=c(0.24,0.2,0.92),
                                    "simeval_EBE_NPDE"=c(2.5,2.9,2.6))
exp_ID_4 <- numeric(0)
exp_row_4 <- NULL
exp_infl_outl_4 <- numeric(0)
exp_infl_not_outl_4 <- numeric(0)
exp_outl_not_infl_4 <- NULL
exp_not_outl_not_infl_4 <- c(3,7,14)
exp_row_infl_not_outl_4 <- c()
exp_row_outl_not_infl_4 <- c()

# Compare expected data with real data
context("CDD/Simeval, function influential_outliers_data")
test_that("If function influential_outliers_data works as expected",{
  expect_equal(exp_infl_data_1,infl_data_1)
  expect_equal(exp_outl_data_1,outl_data_1)
  expect_equal(exp_table_for_plot_1,table_for_plot_1)
  expect_equal(exp_ID_1,ID_1)
  expect_equal(exp_row_1,row_1)
  expect_equal(exp_infl_outl_1,infl_outl_1)
  expect_equal(exp_infl_not_outl_1,infl_not_outl_1)
  expect_equal(exp_outl_not_infl_1,outl_not_infl_1)
  expect_equal(exp_not_outl_not_infl_1,not_outl_not_infl_1)
  expect_equal(exp_row_infl_not_outl_1,row_infl_not_outl_1)
  expect_equal(exp_row_outl_not_infl_1,row_outl_not_infl_1)
  
  expect_equal(exp_infl_data_2,infl_data_2)
  expect_equal(exp_outl_data_2,outl_data_2)
  expect_equal(exp_table_for_plot_2,table_for_plot_2)
  expect_equal(exp_ID_2,ID_2)
  expect_equal(exp_row_2,row_2)
  expect_equal(exp_infl_outl_2,infl_outl_2)
  expect_equal(exp_infl_not_outl_2,infl_not_outl_2)
  expect_equal(exp_outl_not_infl_2,outl_not_infl_2)
  expect_equal(exp_not_outl_not_infl_2,not_outl_not_infl_2)
  expect_equal(exp_row_infl_not_outl_2,row_infl_not_outl_2)
  expect_equal(exp_row_outl_not_infl_2,row_outl_not_infl_2)
  
  expect_equal(exp_infl_data_3,infl_data_3)
  expect_equal(exp_outl_data_3,outl_data_3)
  expect_equal(exp_table_for_plot_3,table_for_plot_3)
  expect_equal(exp_ID_3,ID_3)
  expect_equal(exp_row_3,row_3)
  expect_equal(exp_infl_outl_3,infl_outl_3)
  expect_equal(exp_infl_not_outl_3,infl_not_outl_3)
  expect_equal(exp_outl_not_infl_3,outl_not_infl_3)
  expect_equal(exp_not_outl_not_infl_3,not_outl_not_infl_3)
  expect_equal(exp_row_infl_not_outl_3,row_infl_not_outl_3)
  expect_equal(exp_row_outl_not_infl_3,row_outl_not_infl_3)

  expect_equal(exp_infl_data_4,infl_data_4)
  expect_equal(exp_outl_data_4,outl_data_4)
  expect_equal(exp_table_for_plot_4,table_for_plot_4)
  expect_equal(exp_ID_4,ID_4)
  expect_equal(exp_row_4,row_4)
  expect_equal(exp_infl_outl_4,infl_outl_4)
  expect_equal(exp_infl_not_outl_4,infl_not_outl_4)
  expect_equal(exp_outl_not_infl_4,outl_not_infl_4)
  expect_equal(exp_not_outl_not_infl_4,not_outl_not_infl_4)
  expect_equal(exp_row_infl_not_outl_4,row_infl_not_outl_4)
  expect_equal(exp_row_outl_not_infl_4,row_outl_not_infl_4)
})

context("CDD/Simeval, messages from function influential_outliers_data")
test_that("messages shows up",{
  expect_message(ebe_cook.score_data(ebe.npde.file=ebe.npde.file_2,eta.names=c('ETA.1.','ETA.2.','ETA.3.','ETA.4.','ETA.5.','ETA.6.'),
                                     outlying_criteria=3,raw.results.file=raw.results.file_1,
                                     skipped.id.file=skipped.id.file_1,cutoff_cook=0.8))  #no outl, infl
  expect_message(ebe_cook.score_data(ebe.npde.file=ebe.npde.file_2,eta.names=c('ETA.1.','ETA.2.','ETA.3.','ETA.4.','ETA.5.','ETA.6.'),
                                     outlying_criteria=3,raw.results.file=raw.results.file_1,
                                     skipped.id.file=skipped.id.file_1,cutoff_cook=0.95)) #no outlier, no infl
})

#...........................  (3) Test function outlier.infl.table.R .....................................  
table_list_1 <- outlier_infl_table(all.iofv.file=all.iofv.file_1,n.subjects=6,samples=3,ebe.npde.file=ebe.npde.file_3,
                                 eta.names=c("ETA.1.","ETA.2.","ETA.3."),outlying_criteria=2,
                                 residual.outliers.file=residual.outliers.file_1,
                                 raw.results.file=raw.results.file_6,skipped.id.file=skipped.id.file_2,cutoff_delta.ofv=3.84)

table_list_2 <- outlier_infl_table(all.iofv.file=all.iofv.file_1,n.subjects=6,samples=3,ebe.npde.file=ebe.npde.file_3,
                                 eta.names=c("ETA.1.","ETA.2.","ETA.3."),outlying_criteria=2,
                                 residual.outliers.file=residual.outliers.file_1,
                                 raw.results.file=raw.results.file_7,skipped.id.file=skipped.id.file_2,cutoff_delta.ofv=3.84)

table_list_3 <- outlier_infl_table(all.iofv.file=all.iofv.file_5,n.subjects=6,samples=3,ebe.npde.file=ebe.npde.file_4,
                                   eta.names=c("ETA.1.","ETA.2.","ETA.3."),outlying_criteria=2,
                                   residual.outliers.file=residual.outliers.file_2,
                                   raw.results.file=raw.results.file_7,skipped.id.file=skipped.id.file_2,cutoff_delta.ofv=3.84)

table_list_4 <- outlier_infl_table(all.iofv.file=all.iofv.file_5,n.subjects=6,samples=3,ebe.npde.file=ebe.npde.file_4,
                                   eta.names=c("ETA.1.","ETA.2.","ETA.3."),outlying_criteria=2,
                                   residual.outliers.file=residual.outliers.file_2,
                                   raw.results.file=raw.results.file_8,skipped.id.file=skipped.id.file_2,cutoff_delta.ofv=3.84)

table_list_5 <- outlier_infl_table(all.iofv.file=all.iofv.file_1,n.subjects=6,samples=3,ebe.npde.file=ebe.npde.file_4,
                                   eta.names=c("ETA.1.","ETA.2.","ETA.3."),outlying_criteria=2,
                                   residual.outliers.file=residual.outliers.file_2,
                                   raw.results.file=raw.results.file_8,skipped.id.file=skipped.id.file_2,cutoff_delta.ofv=3.84)

# table_list_6 <- outlier_infl_table(all.iofv.file=all.iofv.file_5,n.subjects=6,samples=3,ebe.npde.file=ebe.npde.file_4,
#                                    eta.names=c("ETA.1.","ETA.2.","ETA.3."),outlying_criteria=2,
#                                    residual.outliers.file="residual.outliers.file_2",
#                                    raw.results.file=raw.results.file_7,skipped.id.file=skipped.id.file_2,cutoff_delta.ofv=3.84)

# unlist
col_amount_1 <- table_list_1$col_amount
all_outlier_table_1 <- table_list_1$all_outlier_table
all_infl_indiv_table_1 <- table_list_1$all_infl_indiv_table
outl_infl_table_1 <- table_list_1$outl_infl_table

col_amount_2 <- table_list_2$col_amount
all_outlier_table_2 <- table_list_2$all_outlier_table
all_infl_indiv_table_2 <- table_list_2$all_infl_indiv_table
outl_infl_table_2 <- table_list_2$outl_infl_table

col_amount_3 <- table_list_3$col_amount
all_outlier_table_3 <- table_list_3$all_outlier_table
all_infl_indiv_table_3 <- table_list_3$all_infl_indiv_table
outl_infl_table_3 <- table_list_3$outl_infl_table

col_amount_4 <- table_list_4$col_amount
all_outlier_table_4 <- table_list_4$all_outlier_table
all_infl_indiv_table_4 <- table_list_4$all_infl_indiv_table
outl_infl_table_4 <- table_list_4$outl_infl_table

col_amount_5 <- table_list_5$col_amount
all_outlier_table_5 <- table_list_5$all_outlier_table
all_infl_indiv_table_5 <- table_list_5$all_infl_indiv_table
outl_infl_table_5 <- table_list_5$outl_infl_table

# Create expected data
exp_col_amount_1 <- 8
exp_all_outlier_table_1 <- data.frame(c('3','7','9','14','23'),c('NA','9.806','','-4.759',''),c('','','1,2,3','1,2,3',''),
                                      c('1','','','','1'),c('','','','','2'),stringsAsFactors = F)
colnames(exp_all_outlier_table_1) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","IWRES outliers","CWRES outliers")

exp_all_infl_indiv_table_1 <- data.frame(c('3','14','23','26'),c('6.3','4.2','',''),c('','0.82','1.6','2.1'),
                                         c('','2.6','',''),stringsAsFactors = F)
colnames(exp_all_infl_indiv_table_1) <- c("ID","Delta OFV influentials","Cook score influentials","Cov ratio influentials")

exp_outl_infl_table_1 <- data.frame(c('3','7','9','14','23','26'),c('NA','9.806','','-4.759','',''),c('','','1,2,3','1,2,3','',''),
                                    c('1','','','','1',''),c('','','','','2',''),c('6.3','','','4.2','',''),c('','','','0.82','1.6','2.1'),
                                    c('','','','2.6','',''),stringsAsFactors = F)
colnames(exp_outl_infl_table_1) <- c("ID","OFV (SD)","EBE NPDE\n(ETA nr.)","IWRES","CWRES",
                                       "Delta OFV","Cook\nscores","Cov\nratios")

exp_col_amount_2 <- 8
exp_all_outlier_table_2 <- data.frame(c('3','7','9','14','23'),c('NA','9.806','','-4.759',''),c('','','1,2,3','1,2,3',''),
                                      c('1','','','','1'),c('','','','','2'),stringsAsFactors = F)
colnames(exp_all_outlier_table_2) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","IWRES outliers","CWRES outliers")

exp_all_infl_indiv_table_2 <- data.frame(c('3','7','14'),c('6.3','NA','4.2'),c('','',''),
                                         c('','3.8',''),stringsAsFactors = F)
colnames(exp_all_infl_indiv_table_2) <- c("ID","Delta OFV influentials","Cook score influentials","Cov ratio influentials")

exp_outl_infl_table_2 <- data.frame(c('3','7','9','14','23'),c('NA','9.806','','-4.759',''),c('','','1,2,3','1,2,3',''),
                                    c('1','','','','1'),c('','','','','2'),c('6.3','NA','','4.2','NA'),c('','','','',''),
                                    c('','3.8','','',''),stringsAsFactors = F)
colnames(exp_outl_infl_table_2) <- c("ID","OFV (SD)","EBE NPDE\n(ETA nr.)","IWRES","CWRES",
                                     "Delta OFV","Cook\nscores","Cov\nratios")

exp_col_amount_3 <- 8
exp_all_outlier_table_3 <- data.frame(C = c("No outliers detected"))
names(exp_all_outlier_table_3) <- NULL

exp_all_infl_indiv_table_3 <- data.frame(c('3','7','14'),c('6.3','NA','4.2'),c('','',''),
                                         c('','3.8',''),stringsAsFactors = F)
colnames(exp_all_infl_indiv_table_3) <- c("ID","Delta OFV influentials","Cook score influentials","Cov ratio influentials")

exp_outl_infl_table_3 <- data.frame(c('3','7','14'),c('','',''),c('','',''),
                                    c('','',''),c('','',''),c('6.3','NA','4.2'),c('','',''),
                                    c('','3.8',''),stringsAsFactors = F)
colnames(exp_outl_infl_table_3) <- c("ID","OFV (SD)","EBE NPDE\n(ETA nr.)","IWRES","CWRES",
                                     "Delta OFV","Cook\nscores","Cov\nratios")

exp_col_amount_4 <- 8
exp_all_outlier_table_4 <- data.frame(C = c("No outliers detected"))
names(exp_all_outlier_table_4) <- NULL

exp_all_infl_indiv_table_4 <- data.frame(C = c("No influential individuals detected"))
names(exp_all_infl_indiv_table_4) <- NULL

exp_outl_infl_table_4 <- data.frame(C = c("No influential individuals and outliers detected"))
names(exp_outl_infl_table_4) <- NULL

exp_col_amount_5 <- 8
exp_all_outlier_table_5 <- data.frame(c('7','14'),c('9.806','-4.759'),c('',''),
                                      c('',''),c('',''),stringsAsFactors = F)
colnames(exp_all_outlier_table_5) <- c("ID","OFV outliers (SD)","EBE NPDE outliers (ETA numbers)","IWRES outliers","CWRES outliers")

exp_all_infl_indiv_table_5 <- data.frame(C = c("No influential individuals detected"))
names(exp_all_infl_indiv_table_5) <- NULL

exp_outl_infl_table_5 <- data.frame(c('7','14'),c('9.806','-4.759'),c('',''),
                                    c('',''),c('',''),c('',''),c('',''),
                                    c('',''),stringsAsFactors = F)
colnames(exp_outl_infl_table_5) <- c("ID","OFV (SD)","EBE NPDE\n(ETA nr.)","IWRES","CWRES",
                                     "Delta OFV","Cook\nscores","Cov\nratios")

# Compare expected data with real data
context("CDD/Simeval, function outlier_infl_table")
test_that("If function outlier_infl_table works as expected",{
  expect_equal(exp_col_amount_1,col_amount_1)
  expect_equal(exp_all_outlier_table_1,all_outlier_table_1)
  expect_equal(exp_all_infl_indiv_table_1,all_infl_indiv_table_1)
  expect_equal(exp_outl_infl_table_1,outl_infl_table_1)
  
  expect_equal(exp_col_amount_2,col_amount_2)
  expect_equal(exp_all_outlier_table_2,all_outlier_table_2)
  expect_equal(exp_all_infl_indiv_table_2,all_infl_indiv_table_2)
  expect_equal(exp_outl_infl_table_2,outl_infl_table_2)
  
  expect_equal(exp_col_amount_3,col_amount_3)
  expect_equal(exp_all_outlier_table_3,all_outlier_table_3)
  expect_equal(exp_all_infl_indiv_table_3,all_infl_indiv_table_3)
  expect_equal(exp_outl_infl_table_3,outl_infl_table_3)

  expect_equal(exp_col_amount_4,col_amount_4)
  expect_equal(exp_all_outlier_table_4,all_outlier_table_4)
  expect_equal(exp_all_infl_indiv_table_4,all_infl_indiv_table_4)
  expect_equal(exp_outl_infl_table_4,outl_infl_table_4)

  expect_equal(exp_col_amount_5,col_amount_5)
  expect_equal(exp_all_outlier_table_5,all_outlier_table_5)
  expect_equal(exp_all_infl_indiv_table_5,all_infl_indiv_table_5)
  expect_equal(exp_outl_infl_table_5,outl_infl_table_5)
})

