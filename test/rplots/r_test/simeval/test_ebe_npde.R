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
pdf.file.name <- 'PsN_ebe_npde_plots.pdf'
model.filename <- "run1.mod"
all.eta.names <- c('ETA(1)','ETA(2)')
all.eta.names_a <- c('ETA(1)','ETA(2)','ETA(3)')

source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

ebe.npde.file <- paste0(files.w.dir,'ebe_npde.csv')
ebe.npde.file_1 <- paste0(files.w.dir,'ebe_npde_1.csv')
###################################     3. Make tests     ###################################
#..................................  (1) Test input_ebe_npde .....................................  
list_input <- input.data(ebe.npde.file=ebe.npde.file,n.eta=2,all.eta.names=all.eta.names)
list_input_a <- input.data(ebe.npde.file=ebe.npde.file_1,n.eta=3,all.eta.names=all.eta.names_a)

#unlist
ebenpde_tmp <- list_input$ebenpde_tmp
n.subjects <- list_input$n.subjects
ebenpde_obs <- list_input$ebenpde_obs

ebenpde_tmp_a <- list_input_a$ebenpde_tmp
n.subjects_a <- list_input_a$n.subjects
ebenpde_obs_a <- list_input_a$ebenpde_obs

# Create expected input data
exp_ebenpde_tmp <- data.frame(ID=as.integer(c(2,11,21,55,56)),STAND_EBE=as.integer(c(0,0,0,0,0)),ETA.1.=c(-1.558,0.54,0.73,0,-2.037),ETA.2.=c(-0.25,0,0.15,-0.46,-0.17))
exp_n.subjects <- 5
exp_ebenpde_obs <- data.frame(ETA.1.=c(-1.558,0.54,0.73,0,-2.037),ETA.2.=c(-0.25,0,0.15,-0.46,-0.17))
exp_ebenpde_tmp_a <- data.frame(ID=as.integer(c(3,7,9,14,23,26)),STAND_EBE=as.integer(c(0,0,0,0,0,0)),ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))
exp_n.subjects_a <- 6
exp_ebenpde_obs_a <- data.frame(ETA.1.=c(1.2,-0.98,6.73,3.6,-0.58,-4.9),ETA.2.=c(-0.25,0.34,0.15,-7.46,0.03,0.83),ETA.3.=c(0.74,-0.201,-4.09,8.05,1.01,7.39))

# Compare expected input data with real input data
context("Test function input.data")
test_that("If function input.data works as expected",{
  expect_equal(exp_ebenpde_tmp,ebenpde_tmp)
  expect_equal(exp_n.subjects,n.subjects)
  expect_equal(exp_ebenpde_obs,ebenpde_obs)
  expect_equal(exp_ebenpde_tmp_a,ebenpde_tmp_a)
  expect_equal(exp_n.subjects_a,n.subjects_a)
  expect_equal(exp_ebenpde_obs_a,ebenpde_obs_a)
})

#.................................  (2) Test summary.table.ebe.npde ...............................   
mydataframe <- summary.table.ebe.npde(ebenpde_obs,all.eta.names=all.eta.names,n.eta=2)
mydataframe_a <- summary.table.ebe.npde(ebenpde_obs_a,all.eta.names=all.eta.names_a,n.eta=3)

# Create expected data
exp_mydataframe <- data.frame(as.factor(c("ETA(1)","ETA(2)")),
                              as.factor(sprintf("%.5f",c(-0.465,-0.146))),
                              as.factor(sprintf("%.3f",c(0.584,0.201))),
                              as.factor(sprintf("%.5f",c(1.58003,0.05473))),
                              as.factor(sprintf("%.3f",c(0.745,0.355))))
colnames(exp_mydataframe) <- c("EBE NPDE","mean","p-value (H_0: mean==0)","variance","p-value (H_0: var==1)")

exp_mydataframe_a <- data.frame(factor(c("ETA(1)","ETA(2)","ETA(3)")),
                              as.factor(sprintf("%.5f",c(0.845,-1.06,2.14983))),
                              as.factor(sprintf("%.3f",c(0.688,0.844,0.313))),
                              as.factor(sprintf("%.5f",c(16.14311,9.96008,22.00402))),
                              as.factor(sprintf("%.3f",c(0.173,0.009,0.037))))
colnames(exp_mydataframe_a) <- c("EBE NPDE","mean","p-value (H_0: mean==0)","variance","p-value (H_0: var==1)")
# Compare expected data with real data
context("Test function summary.table.ebe.npde")
test_that("If function summary.table.ebe.npde works as expected",{
  expect_equal(exp_mydataframe,mydataframe)
  expect_equal(exp_mydataframe_a,mydataframe_a)
})

#.................................  (3) Test empirical.distance  ....................................    
emp_dist <- empirical.distance(ebenpde_obs,n.subjects,n.eta=2)
emp_dist_a <- empirical.distance(ebenpde_obs_a,n.subjects_a,n.eta=3)
# Create expected data
exp_emp_dist <- matrix(c(2.489864,0.2916,0.5554,0.2116,4.178269),nrow=5,ncol=1)
exp_emp_dist_a <- matrix(c(2.0501,1.116401,62.0435,133.4141,1.3574,79.311),nrow=6,ncol=1)
# Compare expected data with real data
context("Test function empirical.distance")
test_that("If function empirical.distance works as expected",{
  expect_equal(exp_emp_dist,emp_dist)
  expect_equal(exp_emp_dist_a,emp_dist_a)
})

#.......................................  (4) Test data.for.plots  ...............................................
list_out_tables <- data.for.plots(emp_dist,n.subjects,n.eta=2)
list_out_tables_a <- data.for.plots(emp_dist_a,n.subjects_a,n.eta=3)

# Unlist data.for.plots
index_emp_distance <- list_out_tables$index_emp_distance
emp_distance_sort <- round(list_out_tables$emp_distance_sort,6)
theor_distance <- round(list_out_tables$theor_distance,6)
out_distance <- round(list_out_tables$out_distance,6)
ebe_npde_quant <- round(list_out_tables$ebe_npde_quant,6)

index_emp_distance_a <- list_out_tables_a$index_emp_distance
emp_distance_sort_a <- round(list_out_tables_a$emp_distance_sort,6)
theor_distance_a <- round(list_out_tables_a$theor_distance,6)
out_distance_a <- round(list_out_tables_a$out_distance,4)
ebe_npde_quant_a <- round(list_out_tables_a$ebe_npde_quant,6)

# Create expected data
exp_index_emp_distance <- list(x=c(0.2116,0.2916,0.5554,2.489864,4.178269),
                               ix=c(4,2,3,1,5))
exp_emp_distance_sort <- c(0.2116,0.2916,0.5554,2.489864,4.178269)
exp_theor_distance <- c(0.210721,0.71335,1.386294,2.407946,4.60517)
exp_out_distance <- 0.301865
exp_ebe_npde_quant <- c(0.1,0.3,0.5,0.7,0.9)

exp_index_emp_distance_a <- list(x=c(1.116401,1.3574,2.0501,62.0435,79.311,133.4141),
                               ix=c(2,5,1,3,6,4))
exp_emp_distance_sort_a <- c(1.116401,1.3574,2.0501,62.0435,79.311,133.4141)
exp_theor_distance_a<- c(0.510101,1.212533,1.947696,2.84179,4.108345,6.666203)
exp_out_distance_a <- -89.6243
exp_ebe_npde_quant_a <- c(0.083333,0.25,0.416667,0.583333,0.75,0.916667)

# Compare expected data with real data
context("Test function data.for.plots")
test_that("If function data.for.plots works as expected",{
  expect_equal(exp_index_emp_distance,index_emp_distance)
  expect_equal(exp_emp_distance_sort,emp_distance_sort)
  expect_equal(exp_theor_distance,theor_distance)
  expect_equal(exp_out_distance,out_distance)
  expect_equal(exp_ebe_npde_quant,ebe_npde_quant)
  expect_equal(exp_index_emp_distance_a,index_emp_distance_a)
  expect_equal(exp_emp_distance_sort_a,emp_distance_sort_a)
  expect_equal(exp_theor_distance_a,theor_distance_a)
  expect_equal(exp_out_distance_a,out_distance_a)
  expect_equal(exp_ebe_npde_quant_a,ebe_npde_quant_a)
})

#.....................................   (5) Test plot_1   .................................    
list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                 out_distance,n.subjects,outlying_criteria=1,do_outlier_plot=FALSE,model.filename)
a_list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                 out_distance,n.subjects,outlying_criteria=-1,do_outlier_plot=FALSE,model.filename)
b_list_plot_1 <- plot_1(ebenpde_tmp,theor_distance,emp_distance_sort,index_emp_distance,
                        out_distance,n.subjects,outlying_criteria=2,do_outlier_plot=FALSE,model.filename)
c_list_plot_1 <- plot_1(ebenpde_tmp_a,theor_distance_a,emp_distance_sort_a,index_emp_distance_a,
                        out_distance_a,n.subjects_a,outlying_criteria=-3,do_outlier_plot=FALSE,model.filename)
# unlist information
flag <- list_plot_1$flag
noutlier <- list_plot_1$noutlier
outlier_id_row <-list_plot_1$outlier_id_row
outlier_ID <- list_plot_1$outlier_ID
identityline <- list_plot_1$identityline
vector_text <- list_plot_1$vector_text
a_flag <- a_list_plot_1$flag
a_noutlier <- a_list_plot_1$noutlier
a_outlier_id_row <-a_list_plot_1$outlier_id_row
a_identityline <- a_list_plot_1$identityline
b_flag <- b_list_plot_1$flag
b_noutlier <- b_list_plot_1$noutlier
b_outlier_id_row <-b_list_plot_1$outlier_id_row
b_outlier_ID <- b_list_plot_1$outlier_ID
b_identityline <- b_list_plot_1$identityline
b_vector_text <- b_list_plot_1$vector_text
c_flag <- c_list_plot_1$flag
c_noutlier <- c_list_plot_1$noutlier
c_outlier_id_row <- c_list_plot_1$outlier_id_row
c_outlier_ID <- c_list_plot_1$outlier_ID
c_identityline <- c_list_plot_1$identityline
c_vector_text <- c_list_plot_1$vector_text
# Create expected data
exp_noutlier <- 1
exp_flag <- 1
exp_outlier_id_row <- 5
exp_outlier_ID <- 56
exp_identityline <- c(1,2,3,4,5)
exp_vector_text <- matrix(c("","","","","56"),nrow = 5,ncol = 1)
exp_a_noutlier <- 0
exp_a_flag <- 0
exp_a_outlier_id_row <- NULL
exp_a_identityline <- c(1,2,3,4,5)
exp_b_noutlier <- 1
exp_b_flag <- 1
exp_b_outlier_id_row <- 5
exp_b_outlier_ID <- 56
exp_b_identityline <- c(1,2,3,4,5)
exp_b_vector_text <- matrix(c("","","","","56"),nrow = 5,ncol = 1)
exp_c_noutlier <- 1
exp_c_flag <- 1
exp_c_outlier_id_row <- 4
exp_c_outlier_ID <- 14
exp_c_identityline <- c(1,2,3,4,5,6)
exp_c_vector_text <- matrix(c("","","","","","14"),nrow = 6,ncol = 1)
# Compare expected data with real data
context("Test function plot_1")
test_that("If function plot_1 works as expected",{
  expect_equal(exp_noutlier,noutlier)
  expect_equal(exp_flag,flag)
  expect_equal(exp_outlier_id_row,outlier_id_row)
  expect_equal(exp_outlier_ID,outlier_ID)
  expect_equal(exp_identityline,identityline)
  expect_equal(exp_vector_text,vector_text)
  expect_equal(exp_a_noutlier,a_noutlier)
  expect_equal(exp_a_flag,a_flag)
  expect_equal(exp_a_outlier_id_row,a_outlier_id_row)
  expect_equal(exp_a_identityline,a_identityline)
  expect_equal(exp_b_noutlier,b_noutlier)
  expect_equal(exp_b_flag,b_flag)
  expect_equal(exp_b_outlier_id_row,b_outlier_id_row)
  expect_equal(exp_b_outlier_ID,b_outlier_ID)
  expect_equal(exp_b_identityline,b_identityline)
  expect_equal(exp_b_vector_text,b_vector_text)
  expect_equal(exp_c_noutlier,c_noutlier)
  expect_equal(exp_c_flag,c_flag)
  expect_equal(exp_c_outlier_id_row,c_outlier_id_row)
  expect_equal(exp_c_outlier_ID,c_outlier_ID)
  expect_equal(exp_c_identityline,c_identityline)
  expect_equal(exp_c_vector_text,c_vector_text)
})

#..................................... (6) Test plot_2  ......................................
list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                 noutlier,flag,n.subjects,n.eta=2,outlying_criteria=1,
                 outlier_id_row,do_outlier_plot=FALSE,model.filename)
a_list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                      a_noutlier,a_flag,n.subjects,n.eta=2,outlying_criteria=1,
                      a_outlier_id_row,do_outlier_plot=FALSE,model.filename)
b_list_plot_2 <- plot_2(ebenpde_tmp,emp_distance_sort,theor_distance,index_emp_distance,
                        b_noutlier,b_flag,n.subjects,n.eta=2,outlying_criteria=2,
                        b_outlier_id_row,do_outlier_plot=FALSE,model.filename)
c_list_plot_2 <- plot_2(ebenpde_tmp_a,emp_distance_sort_a,theor_distance_a,index_emp_distance_a,
                        c_noutlier,c_flag,n.subjects_a,n.eta=3,outlying_criteria=-3,
                        c_outlier_id_row,do_outlier_plot=FALSE,model.filename)
# unlist information
vector_theor_dist <- round(list_plot_2$vector_theor_dist,6)
noutlier <- list_plot_2$noutlier
outlier_id_row <- list_plot_2$outlier_id_row
flag1 <- list_plot_2$flag1
out_distance <- round(list_plot_2$out_distance,6)
a_vector_theor_dist <- round(a_list_plot_2$vector_theor_dist,6)
a_noutlier <- a_list_plot_2$noutlier
a_outlier_id_row <- a_list_plot_2$outlier_id_row
a_flag1 <- a_list_plot_2$flag1
a_out_distance <- round(a_list_plot_2$out_distance,6)
b_vector_theor_dist <- round(b_list_plot_2$vector_theor_dist,6)
b_noutlier <- b_list_plot_2$noutlier
b_outlier_id_row <- b_list_plot_2$outlier_id_row
b_flag1 <- b_list_plot_2$flag1
b_out_distance <- round(b_list_plot_2$out_distance,6)
c_vector_theor_dist <- round(c_list_plot_2$vector_theor_dist,6)
c_noutlier <- c_list_plot_2$noutlier
c_outlier_id_row <- c_list_plot_2$outlier_id_row
c_flag1 <- c_list_plot_2$flag1
c_out_distance <- round(c_list_plot_2$out_distance,6)
# Create expected data
exp_vector_theor_dist <- matrix(c(1.386294,2.772589,3.583519,4.158883,4.60517),nrow=1,ncol=5)
exp_noutlier <- 1
exp_outlier_id_row <- 5
exp_flag1 <- 0
exp_out_distance <- c(1.180175,2.141203,1.754324,0.830634)
exp_a_vector_theor_dist <- matrix(c(1.386294,2.772589,3.583519,4.158883,4.60517),nrow=1,ncol=5)
exp_a_noutlier <- 0
exp_a_outlier_id_row <- NULL
exp_a_flag1 <- 0
exp_a_out_distance <- c(1.180175,2.141203,1.754324,0.830634)
exp_b_vector_theor_dist <- matrix(c(1.386294,2.772589,3.583519,4.158883,4.60517),nrow=1,ncol=5)
exp_b_noutlier <- 2
exp_b_outlier_id_row <- c(5,1)
exp_b_flag1 <- 0
exp_b_out_distance <- c(1.180175,2.141203,1.754324,0.830634)
exp_c_vector_theor_dist <- matrix(c(2.365974,4.108345,5.071066,5.739413,6.251389,6.666203),nrow=1,ncol=6)
exp_c_noutlier <- 3
exp_c_outlier_id_row <- c(4,6,3)
exp_c_flag1 <- 0
exp_c_out_distance <- c(-51.660947,-39.813001,2.136146,1.945212,0.883581)
# Compare expected data with real data
context("Test function plot_2")
test_that("If function plot_2 works as expected",{
  expect_equal(exp_vector_theor_dist,vector_theor_dist)
  expect_equal(exp_noutlier,noutlier)
  expect_equal(exp_outlier_id_row,outlier_id_row)
  expect_equal(exp_flag1,flag1)
  expect_equal(exp_out_distance,out_distance)
  expect_equal(exp_a_vector_theor_dist,a_vector_theor_dist)
  expect_equal(exp_a_noutlier,a_noutlier)
  expect_equal(exp_a_outlier_id_row,a_outlier_id_row)
  expect_equal(exp_a_flag1,a_flag1)
  expect_equal(exp_a_out_distance,a_out_distance)
  expect_equal(exp_b_vector_theor_dist,b_vector_theor_dist)
  expect_equal(exp_b_noutlier,b_noutlier)
  expect_equal(exp_b_outlier_id_row,b_outlier_id_row)
  expect_equal(exp_b_flag1,b_flag1)
  expect_equal(exp_b_outlier_id_row,b_outlier_id_row)
  expect_equal(exp_c_vector_theor_dist,c_vector_theor_dist)
  expect_equal(exp_c_noutlier,c_noutlier)
  expect_equal(exp_c_outlier_id_row,c_outlier_id_row)
  expect_equal(exp_c_flag1,c_flag1)
  expect_equal(exp_c_out_distance,c_out_distance)
})

#...................................  (7) Test outlier.table.ebe.npde  .........................................
fortable1 <- outlier.table.ebe.npde(noutlier,outlier_id_row,ebenpde_tmp,ebenpde_obs,
                           index_emp_distance,emp_distance_sort,vector_theor_dist,
                           n.subjects,all.eta.names=all.eta.names,n.eta=2)
b_fortable1 <- outlier.table.ebe.npde(b_noutlier,b_outlier_id_row,ebenpde_tmp,ebenpde_obs,
                             index_emp_distance,emp_distance_sort,b_vector_theor_dist,
                             n.subjects,all.eta.names=all.eta.names,n.eta=2)
# Create expected data
exp_fortable1 <- data.frame(c(56),c(0.30186),c(4.1783),c(-2.037),c(-0.17))
colnames(exp_fortable1) <- c("ID", "outlying criteria","MD distance","ETA(1)","ETA(2)")
exp_b_fortable1 <- data.frame(c(56,2),c(0.30186,1.1802),c(4.1783,2.4899),c(-2.037,-1.558),c(-0.17,-0.25))
colnames(exp_b_fortable1) <- c("ID", "outlying criteria","MD distance","ETA(1)","ETA(2)")
# Compare expected data with real data
context("Test function outlier.table.ebe.npde")
test_that("If function outlier.table.ebe.npde works as expected",{
  expect_equal(exp_fortable1,fortable1)
  expect_equal(exp_b_fortable1,b_fortable1)
})


#...................................  (8) Test ebe.npde.outliers  .........................................
ebe.npde_outliers <- ebe.npde.outliers(ebe.npde.file=ebe.npde.file,n.eta=2,outlying_criteria=1,model.filename)
ebe.npde_outliers_a <- ebe.npde.outliers(ebe.npde.file=ebe.npde.file,n.eta=2,outlying_criteria=-2,model.filename)
# Create expected data
exp_ebe.npde_outliers <- data.frame(c(56),c(0.30186))
colnames(exp_ebe.npde_outliers) <- c("ID","outlying criteria")
exp_ebe.npde_outliers_a <- data.frame(C = c("No outliers detected"))
colnames(exp_ebe.npde_outliers_a) <- NULL

# Compare expected data with real data
context("Test function ebe.npde.outliers")
test_that("If function ebe.npde.outliers works as expected",{
  expect_equal(exp_ebe.npde_outliers,ebe.npde_outliers)
  expect_equal(exp_ebe.npde_outliers_a,ebe.npde_outliers_a)
})