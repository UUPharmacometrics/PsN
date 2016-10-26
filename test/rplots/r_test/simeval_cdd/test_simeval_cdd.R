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
raw.results.file_4 <- paste0(files.w.dir,'raw_results_4.csv')
skipped.id.file_4 <- paste0(files.w.dir,'skipped_4.csv')
raw.results.file_1 <- paste0(files.w.dir,'raw_results_1.csv')
skipped.id.file_1 <- paste0(files.w.dir,'skipped_1.csv')
raw.results.file_3 <- paste0(files.w.dir,'raw_results_3.csv')
skipped.id.file_3 <- paste0(files.w.dir,'skipped_3.csv')
###################################     3. Make a test    ###################################
#...........................  (1) Test function influential_outliers_data.R .....................................  
out <- influential_outliers_data(all.iofv.file_4,n.subjects=16,samples=3,
                                 raw.results.file_4,skipped.id.file_4)
out_1 <- influential_outliers_data(all.iofv.file_2,n.subjects=16,samples=3,
                                 raw.results.file_4,skipped.id.file_4)
out_2 <- influential_outliers_data(all.iofv.file_1,n.subjects=26,samples=2,
                                 raw.results.file_1,skipped.id.file_1)
out_3 <- influential_outliers_data(all.iofv.file_3,n.subjects=6,samples=3,
                                   raw.results.file_3,skipped.id.file_3)

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

infl_data_1 <- out_1$infl_data
outl_data_1 <- round(out_1$outl_data,3)
table_for_plot_1 <- round(out_1$table_for_plot,3)
ID_1 <- out_1$ID
row_1 <- out_1$row
infl_outl_1 <- out_1$infl_outl
infl_not_outl_1 <- out_1$infl_not_outl
outl_not_infl_1 <- out_1$outl_not_infl
not_outl_not_infl_1 <- out_1$not_outl_not_infl

infl_data_2 <- out_2$infl_data
outl_data_2 <- round(out_2$outl_data,3)
table_for_plot_2 <- round(out_2$table_for_plot,3)
ID_2 <- out_2$ID
row_2 <- out_2$row
infl_outl_2 <- out_2$infl_outl
infl_not_outl_2 <- out_2$infl_not_outl
outl_not_infl_2 <- out_2$outl_not_infl
not_outl_not_infl_2 <- out_2$not_outl_not_infl

infl_data_3 <- out_3$infl_data
outl_data_3 <- round(out_3$outl_data,3)
table_for_plot_3 <- round(out_3$table_for_plot,3)
ID_3 <- out_3$ID
row_3 <- out_3$row
infl_outl_3 <- out_3$infl_outl
infl_not_outl_3 <- out_3$infl_not_outl
outl_not_infl_3 <- out_3$outl_not_infl
not_outl_not_infl_3 <- out_3$not_outl_not_infl

# Create expected input data
exp_infl_data <- data.frame("ID_cdd"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                            "delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3))
exp_outl_data <- data.frame("ID_simeval"=c(35,41,24,39,21,27,13,34,42,37,11,23,31,19,16,38),
                            "iofv_res"=c(-15.254,-4.759,-2.323,-1.705,-1.558,-0.716,-0.218,0.083,0.099,0.951,1.124,1.165,1.847,1.999,7.976,15.306))
exp_table_for_plot <- data.frame("ID"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                                 "cdd_delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3),
                                 "simeval_iofv_res"=c(1.124,-0.218,7.976,1.999,-1.558,1.165,-2.323,-0.716,1.847,0.083,-15.254,0.951,15.306,-1.705,-4.759,0.099))
exp_ID <- 38
exp_row <- 13
exp_infl_outl <- 38
exp_infl_not_outl <- 19
exp_outl_not_infl <- c(35,41,16)
exp_not_outl_not_infl <- c(11,13,21,23,24,27,31,34,37,39,42)

exp_infl_data_1 <- data.frame("ID_cdd"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                            "delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3))
exp_outl_data_1 <- data.frame("ID_simeval"=c(24,39,21,27,38,35,13,34,42,41,37,23,31,19,16,11),
                            "iofv_res"=c(-2.323,-1.705,-1.558,-0.716,-0.657,-0.462,-0.218,0.083,0.099,0.174,0.951,1.165,1.847,1.999,9.238,28.598))
exp_table_for_plot_1 <- data.frame("ID"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42),
                                 "cdd_delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3),
                                 "simeval_iofv_res"=c(28.598,-0.218,9.238,1.999,-1.558,1.165,-2.323,-0.716,1.847,0.083,-0.462,0.951,-0.657,-1.705,0.174,0.099))
exp_ID_1 <- integer(0)
exp_row_1 <- NULL
exp_infl_outl_1 <- integer(0)
exp_infl_not_outl_1 <- c(19,38)
exp_outl_not_infl_1 <- c(16,11)
exp_not_outl_not_infl_1 <- c(13,21,23,24,27,31,34,35,37,39,41,42)

exp_infl_data_2 <- data.frame("ID_cdd"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42,43,45,48,49,51,52,55,60,61,63),
                            "delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3,4.7,0.21,0.9,3.1,1,6.1,2.4,1.3,0.7,0.8))
exp_outl_data_2 <- data.frame("ID_simeval"=c(35,41,24,21,13,39,19,45,23,61,63,27,51,55,11,42,60,34,43,49,31,48,16,52,37,38),
                            "iofv_res"=c(-39.598,-12.619,-1.896,-0.932,-0.893,-0.707,-0.534,-0.508,-0.171,-0.164,-0.079,0.072,0.138,0.181,0.272,0.376,0.508,0.849,0.957,1.195,1.845,1.974,6.771,14.394,14.849,63.94))
exp_table_for_plot_2 <- data.frame("ID"=c(11,13,16,19,21,23,24,27,31,34,35,37,38,39,41,42,43,45,48,49,51,52,55,60,61,63),
                                 "cdd_delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2,3.2,2.1,1.9,1.5,0.01,0.7,5.2,0.6,1.4,2.3,4.7,0.21,0.9,3.1,1,6.1,2.4,1.3,0.7,0.8),
                                 "simeval_iofv_res"=c(0.272,-0.893,6.771,-0.534,-0.932,-0.171,-1.896,0.072,1.845,0.849,-39.598,14.849,63.94,-0.707,-12.619,0.376,0.957,-0.508,1.974,1.195,0.138,14.394,0.181,0.508,-0.164,-0.079))
exp_ID_2 <- c(52,38)
exp_row_2 <- c(22,13)
exp_infl_outl_2 <- c(52,38)
exp_infl_not_outl_2 <- 19
exp_outl_not_infl_2 <- c(35,41,16,37)
exp_not_outl_not_infl_2 <- c(11,13,21,23,24,27,31,34,39,42,43,45,48,49,51,55,60,61,63)

exp_infl_data_3 <- data.frame("ID_cdd"=c(11,13,16,19,21,23),
                              "delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2))
exp_outl_data_3 <- data.frame("ID_simeval"=c(21,13,16,11,23,19),
                              "iofv_res"=c(-1.558,-0.218,0.562,1.124,1.165,1.999))
exp_table_for_plot_3 <- data.frame("ID"=c(11,13,16,19,21,23),
                                   "cdd_delta.ofv"=c(0.54,0.6,0.5,6.3,0.4,3.2),
                                   "simeval_iofv_res"=c(1.124,-0.218,0.562,1.999,-1.558,1.165))
exp_ID_3 <- integer(0)
exp_row_3 <- NULL
exp_infl_outl_3 <- integer(0)
exp_infl_not_outl_3 <- 19
exp_outl_not_infl_3 <- NULL
exp_not_outl_not_infl_3 <- c(11,13,16,21,23)

# Compare expected input data with real input data
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
  
  expect_equal(exp_infl_data_1,infl_data_1)
  expect_equal(exp_outl_data_1,outl_data_1)
  expect_equal(exp_table_for_plot_1,table_for_plot_1)
  expect_equal(exp_ID_1,ID_1)
  expect_equal(exp_row_1,row_1)
  expect_equal(exp_infl_outl_1,infl_outl_1)
  expect_equal(exp_infl_not_outl_1,infl_not_outl_1)
  expect_equal(exp_outl_not_infl_1,outl_not_infl_1)
  expect_equal(exp_not_outl_not_infl_1,not_outl_not_infl_1)
  
  expect_equal(exp_infl_data_2,infl_data_2)
  expect_equal(exp_outl_data_2,outl_data_2)
  expect_equal(exp_table_for_plot_2,table_for_plot_2)
  expect_equal(exp_ID_2,ID_2)
  expect_equal(exp_row_2,row_2)
  expect_equal(exp_infl_outl_2,infl_outl_2)
  expect_equal(exp_infl_not_outl_2,infl_not_outl_2)
  expect_equal(exp_outl_not_infl_2,outl_not_infl_2)
  expect_equal(exp_not_outl_not_infl_2,not_outl_not_infl_2)
  
  expect_equal(exp_infl_data_3,infl_data_3)
  expect_equal(exp_outl_data_3,outl_data_3)
  expect_equal(exp_table_for_plot_3,table_for_plot_3)
  expect_equal(exp_ID_3,ID_3)
  expect_equal(exp_row_3,row_3)
  expect_equal(exp_infl_outl_3,infl_outl_3)
  expect_equal(exp_infl_not_outl_3,infl_not_outl_3)
  expect_equal(exp_outl_not_infl_3,outl_not_infl_3)
  expect_equal(exp_not_outl_not_infl_3,not_outl_not_infl_3)
})
