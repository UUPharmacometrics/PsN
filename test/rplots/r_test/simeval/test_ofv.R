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
# create a working directory for R scripts
source("../set.working.directory.R")
files.w.dir <- fun.files.w.dir(toolname = tool)

raw.results.file <- paste0(files.w.dir,'raw_results_run1.csv')
iofv.file <- paste0(files.w.dir,'summary_iofv.csv')
all.iofv.file <- paste0(files.w.dir,'raw_all_iofv.csv')
all.iofv.file_1 <- paste0(files.w.dir,'raw_all_iofv_1.csv')

###################################     3. Make tests     ###################################

#..........................  (1) Test function   ...................................
out_p_ofv_ppc <- p_ofv_ppc(raw.results.file=raw.results.file)

# unlist information
rawres_input <- out_p_ofv_ppc$rawres_input
rawres <- out_p_ofv_ppc$rawres
pOFV_obs <- out_p_ofv_ppc$pOFV_obs
pOFV_sim <- out_p_ofv_ppc$pOFV_sim
sort_pOFV_sim <- out_p_ofv_ppc$sort_pOFV_sim
newxlim <- out_p_ofv_ppc$newxlim
# Create expected data
exp_rawres_input <- data.frame(run_type=factor(c("original",c(rep("simulation",3))),levels=c("original","simulation")),
                                              ofv=c(-30.72,NA,-36.3,-39.708),
                                              EI1=c(NA,NA,NA,NA))
exp_rawres <- data.frame(run_type=factor(c("original",c(rep("simulation",2))),levels=c("original","simulation")),
                         ofv=c(-30.72,-36.3,-39.708),
                         EI1=c(NA,NA,NA))
exp_pOFV_obs <- -30.72
exp_pOFV_sim <- c(-36.3,-39.708)
exp_sort_pOFV_sim <- c(-39.708,-36.3)
exp_newxlim <- c(-39.708,-30.72)
# Compare expected data with real data
context("Simeval, ofv, function p_ofv_ppc")
test_that("If function p_ofv_ppc works as expected",{
  expect_equal(exp_rawres_input,rawres_input)
  expect_equal(exp_rawres,rawres)
  expect_equal(exp_pOFV_obs,pOFV_obs)
  expect_equal(exp_pOFV_sim,pOFV_sim)
  expect_equal(exp_sort_pOFV_sim,sort_pOFV_sim)
  expect_equal(exp_newxlim,newxlim)
})

#..........................  (2) Test function_i_ofv_npde  ...................................
out_i_ofv_npde <- i_ofv_npde(iofv.file=iofv.file)

# unlist information
iOFV <- out_i_ofv_npde$iOFV
iOFV_npde <- out_i_ofv_npde$iOFV_npde
ylimi <- round(out_i_ofv_npde$ylimi,6)
xlimit_min <- out_i_ofv_npde$xlimit_min
xlimit_max <- out_i_ofv_npde$xlimit_max

# Create expected data
exp_iOFV <- data.frame(ID=as.integer(c(2,4,6,8,11)),OBSERVED=c(2,5,-3,-8,-1.2),NPDE=c(1.8,0,NA,-0.8,-3.71))
exp_iOFV_npde <- c(1.8,0,-0.8,-3.71)
exp_ylimi <- 3.190073
exp_xlimit_min <- -3.71
exp_xlimit_max <- 3

# Compare expected data with real data
context("Simeval, ofv, function i_ofv_npde")
test_that("If function i_ofv_npde works as expected",{
  expect_equal(exp_iOFV,iOFV)
  expect_equal(exp_iOFV_npde,iOFV_npde)
  expect_equal(exp_ylimi,ylimi)
  expect_equal(exp_xlimit_min,xlimit_min)
  expect_equal(exp_xlimit_max,xlimit_max)
})

#..........................  (3) Test function i_ofv_res  ...................................
out_i_ofv_res <- i_ofv_res(all.iofv.file=all.iofv.file_1,n.subjects=4,samples=3)
out_i_ofv_res_a <- i_ofv_res(all.iofv.file=all.iofv.file,n.subjects=4,samples=3)

# unlist information
all.iOFV_sim <- out_i_ofv_res$all.iOFV_sim
iOFV_res <- round(out_i_ofv_res$iOFV_res,6)
x <- round(out_i_ofv_res$result$x,6)
ix <- out_i_ofv_res$result$ix
iOFV_res_ord <- round(out_i_ofv_res$iOFV_res_ord,6)
id_sorted <- out_i_ofv_res$id_sorted
outlier_ID <- out_i_ofv_res$outlier_ID
vector_text <- out_i_ofv_res$vector_text
ofv_outliertable <- round(out_i_ofv_res$ofv_outliertable,6)
outlier_median <- round(out_i_ofv_res$outlier_median,6)

outlier_ID_a <- out_i_ofv_res_a$outlier_ID
ofv_outliertable_a <- out_i_ofv_res_a$ofv_outliertable
outlier_median_a <- out_i_ofv_res_a$outlier_median
vector_text_a <- out_i_ofv_res_a$vector_text

# Create expected data
exp_all.iOFV_sim <- data.frame(ID=as.integer(c(1,4,13,20)),ORIGINAL=c(2.57,-7.4,-25.24,3.9),
                       sample.1=c(-1.2,2.4,-7.13,5.74),sample.2=c(-2,-4.4,-4.98,-2.11),
                       sample.3=c(45.68,-4,1.08,3.5))
exp_iOFV_res <- matrix(c(0.138095,0.167399,-1.579118,-2.568298,-0.786214,-0.891042,-4.253819,-4.758828,-6.182249,-0.455022,1.486239,0.098918),nrow=3,ncol=4)
exp_x <- c(-4.758828,-0.891042,0.098918,0.138095)
exp_ix <- c(3,2,4,1)
exp_iOFV_res_ord <- matrix(c(-4.253819,-4.758828,-6.182249,-2.568298,-0.786214,-0.891042,-0.455022,1.486239,0.098918,0.138095,0.167399,-1.579118),nrow=3,ncol=4)
exp_id_sorted <- matrix(c(13,4,20,1),4,1)
exp_outlier_ID <- c(13)
exp_vector_text <- matrix(c("13","","",""),nrow = 4,ncol = 1)
exp_ofv_outliertable <- data.frame(ID=c(13),MEDIAN=c(-4.758828))
exp_outlier_median <- c(-4.758828)

exp_outlier_ID_a <- NULL
exp_ofv_outliertable_a <- data.frame()
exp_outlier_median_a <- NULL
exp_vector_text_a <- matrix(c("","","",""),nrow = 4,ncol = 1)
# Compare expected data with real data
context("Simeval, ofv, function i_ofv_res")
test_that("If function i_ofv_res works as expected",{
  expect_equal(exp_all.iOFV_sim,all.iOFV_sim)
  expect_equal(exp_iOFV_res,iOFV_res)
  expect_equal(exp_x,x)
  expect_equal(exp_ix,ix)
  expect_equal(exp_iOFV_res_ord,iOFV_res_ord)
  expect_equal(exp_id_sorted,id_sorted)
  expect_equal(exp_outlier_ID,outlier_ID)
  expect_equal(exp_vector_text,vector_text)
  expect_equal(exp_ofv_outliertable,ofv_outliertable)
  expect_equal(exp_outlier_median,outlier_median)
  expect_equal(exp_outlier_ID_a,outlier_ID_a)
  expect_equal(exp_ofv_outliertable_a,ofv_outliertable_a)
  expect_equal(exp_outlier_median_a,outlier_median_a)
  expect_equal(exp_vector_text_a,vector_text_a)
})

#..........................  (4) Test function i_ofv_ppc  ...................................
list_i_ofv_ppc <- i_ofv_ppc(all.iofv.file=all.iofv.file,samples=3,
                           outlier_ID=c(13,4))
out_i_ofv_ppc_a <- i_ofv_ppc(all.iofv.file=all.iofv.file,samples=3,
                           outlier_ID=outlier_ID_a)

# unlist information
all.iOFV_sim <- list_i_ofv_ppc$all.iOFV_sim # check input data
outlier_data <- list_i_ofv_ppc$outlier_data

iOFV_sim_1 <- list_i_ofv_ppc[[1]]$iOFV_sim
iOFV_obs_1 <- list_i_ofv_ppc[[1]]$iOFV_obs
sort_iOFV_sim_1 <- list_i_ofv_ppc[[1]]$sort_iOFV_sim
newxlim_1 <- list_i_ofv_ppc[[1]]$newxlim

iOFV_sim_2 <- list_i_ofv_ppc[[2]]$iOFV_sim
iOFV_obs_2 <- list_i_ofv_ppc[[2]]$iOFV_obs
sort_iOFV_sim_2 <- list_i_ofv_ppc[[2]]$sort_iOFV_sim
newxlim_2 <- list_i_ofv_ppc[[2]]$newxlim

# Create expected data
exp_all.iOFV_sim <- data.frame(ID=as.integer(c(1,4,13,20)),ORIGINAL=c(2.57,-7.4,-10.24,3.9),
                                 sample.1=c(-1.2,2.4,-7.13,5.74),sample.2=c(-2,-7.4,-4.98,-2.11),
                                 sample.3=c(-5.68,NA,1.08,3.5))
exp_outlier_data <- data.frame(ID=as.integer(c(4,13)),ORIGINAL=c(-7.4,-10.24),
                               sample.1=c(2.4,-7.13),sample.2=c(-7.4,-4.98),
                               sample.3=c(NA,1.08))
exp_iOFV_sim_1 <- c(2.4,-7.4)
exp_iOFV_obs_1 <- -7.4
exp_sort_iOFV_sim_1 <- c(-7.4,2.4)
exp_newxlim_1 <- c(-7.4,2.4)
exp_iOFV_sim_2 <- c(-7.13,-4.98,1.08)
exp_iOFV_obs_2 <- -10.24
exp_sort_iOFV_sim_2 <- c(-7.13,-4.98,1.08)
exp_newxlim_2 <- c(-10.24,1.08)

exp_out_i_ofv_ppc_a <- NULL
# Compare expected data with real data
context("Simeval, ofv, function i_ofv_ppc")
test_that("If function i_ofv_ppc works as expected",{
  expect_equal(exp_all.iOFV_sim,all.iOFV_sim)
  expect_equal(exp_outlier_data,outlier_data)
  expect_equal(exp_iOFV_sim_1,iOFV_sim_1)
  expect_equal(exp_iOFV_obs_1,iOFV_obs_1)
  expect_equal(exp_sort_iOFV_sim_1,sort_iOFV_sim_1)
  expect_equal(exp_newxlim_1,newxlim_1)
  expect_equal(exp_iOFV_sim_2,iOFV_sim_2)
  expect_equal(exp_iOFV_obs_2,iOFV_obs_2)
  expect_equal(exp_sort_iOFV_sim_2,sort_iOFV_sim_2)
  expect_equal(exp_newxlim_2,newxlim_2)
  expect_equal(exp_out_i_ofv_ppc_a,out_i_ofv_ppc_a)
  
})

#..........................  (5) Test function kld_i_ofv  ...................................
out_kld_i_ofv <- kld_i_ofv(all.iofv.file=all.iofv.file,n.subjects=4,samples=3,n=5)

# unlist information
all.iOFV_sim <- out_kld_i_ofv$all.iOFV_sim
iOFV_obs <- out_kld_i_ofv$iOFV_obs
iOFV_min <- out_kld_i_ofv$iOFV_min
iOFV_max <- out_kld_i_ofv$iOFV_max
final_grid <- out_kld_i_ofv$final_grid
iOFV_kernel <- round(out_kld_i_ofv$iOFV_kernel,6)
iOFV_kernel_obs <- round(out_kld_i_ofv$iOFV_kernel_obs,6)
iOFV_kernel_average <- round(out_kld_i_ofv$iOFV_kernel_average,6)
KLD_sim <- round(out_kld_i_ofv$KLD_sim,6)
KLD_obs <- round(out_kld_i_ofv$KLD_obs,6)
newxlim <- round(out_kld_i_ofv$newxlim,6)
# Create expected data
exp_all.iOFV_sim <- data.frame(ID=as.integer(c(1,4,13,20)),ORIGINAL=c(2.57,-7.4,-10.24,3.9),
                               sample.1=c(-1.2,2.4,-7.13,5.74),sample.2=c(-2,-7.4,-4.98,-2.11),
                               sample.3=c(-5.68,NA,1.08,3.5))
exp_iOFV_obs <- c(2.57,-7.4,-10.24,3.9)
exp_iOFV_min <- matrix(c(-7.13,-7.4,-5.68),nrow = 3,ncol = 1)
exp_iOFV_max <- matrix(c(5.74,-2,3.5),nrow = 3,ncol = 1)
exp_final_grid <- c(-5.68,-2)
exp_iOFV_kernel <- matrix(c(0.041413,0.042854,0.044928,0.048147,0.052397,0.101216,0.109579,0.12115,0.131188,0.127168,0.055085,0.053696,0.048923,0.045349,0.047204),nrow = 5,ncol = 3)
exp_iOFV_kernel_obs <- c(0.040333,0.039340,0.038642,0.038466,0.038905)
exp_iOFV_kernel_average <- matrix(c(0.065905,0.06871,0.071667,0.074895,0.07559),nrow = 5,ncol = 1)
exp_KLD_sim <- matrix(c(0.001174,0.001519,0.01124),nrow = 3,ncol = 1)
exp_KLD_obs <- 0.003206
exp_newxlim <- c(0.001174,0.01124)
# Compare expected data with real data
context("Simeval, ofv, function kld_i_ofv")
test_that("If function kld_i_ofv works as expected",{
  expect_equal(exp_all.iOFV_sim,all.iOFV_sim)
  expect_equal(exp_iOFV_obs,iOFV_obs)
  expect_equal(exp_iOFV_min,iOFV_min)
  expect_equal(exp_iOFV_max,iOFV_max)
  expect_equal(exp_final_grid,final_grid)
  expect_equal(exp_iOFV_kernel,iOFV_kernel)
  expect_equal(exp_iOFV_kernel_obs,iOFV_kernel_obs)
  expect_equal(exp_iOFV_kernel_average,iOFV_kernel_average)
  expect_equal(exp_KLD_sim,KLD_sim)
  expect_equal(exp_KLD_obs,KLD_obs)
  expect_equal(exp_newxlim,newxlim)
})

#..........................  (6) Test function kld  ...................................
list_kld_a <- KLD(iOFV_kernel_average,iOFV_kernel[,1],base=2)
list_kld_b <- KLD(iOFV_kernel_average,iOFV_kernel_obs,base=2)
list_kld_c <- KLD(matrix(c(0,0.3,0.49,0.201),nrow = 4,ncol = 1),c(0.029,0.041,0.05,0.037))
list_kld_d <- KLD(c(0,0.3,0.49),c(0.029,0.041,0.05,0.037),base=1) # if function stops, returns error
list_kld_e <- KLD(c(3.4,NA,0.923),c(0.5,2.4,6.201)) # if function stops, returns error

# unlist information
base_a <- list_kld_a$base
n1_a <- list_kld_a$n1
n2_a <- list_kld_a$n2
px.normalized_a <- round(list_kld_a$px.normalized,6)
py.normalized_a <- round(list_kld_a$py.normalized,6)
KLD.px.py_a <- round(list_kld_a$KLD.px.py,6)
KLD.py.px_a <- round(list_kld_a$KLD.py.px,6)
mean.KLD_a <- round(list_kld_a$mean.KLD,7)
sum.KLD.px.py_a <- round(list_kld_a$sum.KLD.px.py,6)
sum.KLD.py.px_a <- round(list_kld_a$sum.KLD.py.px,6)
mean.sum.KLD_a <- round(list_kld_a$mean.sum.KLD,6)
intrinsic.discrepancy_a <- round(list_kld_a$intrinsic.discrepancy,6)

base_b <- list_kld_b$base
n1_b <- list_kld_b$n1
n2_b <- list_kld_b$n2
px.normalized_b <- round(list_kld_b$px.normalized,6)
py.normalized_b <- round(list_kld_b$py.normalized,6)
KLD.px.py_b <- round(list_kld_b$KLD.px.py,6)
KLD.py.px_b <- round(list_kld_b$KLD.py.px,6)
mean.KLD_b <- round(list_kld_b$mean.KLD,7)
sum.KLD.px.py_b <- round(list_kld_b$sum.KLD.px.py,6)
sum.KLD.py.px_b <- round(list_kld_b$sum.KLD.py.px,6)
mean.sum.KLD_b <- round(list_kld_b$mean.sum.KLD,6)
intrinsic.discrepancy_b <- round(list_kld_b$intrinsic.discrepancy,6)

base_c <- round(list_kld_c$base,6)
n1_c <- list_kld_c$n1
n2_c <- list_kld_c$n2
px.normalized_c <- round(list_kld_c$px.normalized,6)
py.normalized_c <- round(list_kld_c$py.normalized,6)
KLD.px.py_c <- round(list_kld_c$KLD.px.py,6)
KLD.py.px_c <- round(list_kld_c$KLD.py.px,6)
mean.KLD_c <- round(list_kld_c$mean.KLD,6)
sum.KLD.px.py_c <- round(list_kld_c$sum.KLD.px.py,6)
sum.KLD.py.px_c <- round(list_kld_c$sum.KLD.py.px,6)
mean.sum.KLD_c <- round(list_kld_c$mean.sum.KLD,6)
intrinsic.discrepancy_c <- round(list_kld_c$intrinsic.discrepancy,6)

# Create expected data
exp_base_a <- 2
exp_n1_a <- 5
exp_n2_a <- 5
exp_px.normalized_a <- c(0.184728,0.192591,0.200879,0.209927,0.211875)
exp_py.normalized_a <- c(0.180261,0.186533,0.195561,0.209573,0.228072)
exp_KLD.px.py_a <- c(0.006524,0.008879,0.007776,0.000512,-0.022517)
exp_KLD.py.px_a <- c(-0.006366,-0.0086,-0.00757,-0.000511,0.024238)
exp_mean.KLD_a <- c(0.0000789,0.0001396,0.0001029,0.0000004,0.0008607)
exp_sum.KLD.px.py_a <- 0.001173
exp_sum.KLD.py.px_a <- 0.001192
exp_mean.sum.KLD_a <- 0.001183
exp_intrinsic.discrepancy_a <- 0.001173

exp_base_b <- 2
exp_n1_b <- 5
exp_n2_b <- 5
exp_px.normalized_b <- c(0.184728,0.192591,0.200879,0.209927,0.211875)
exp_py.normalized_b <- c(0.206111,0.201036,0.197469,0.19657,0.198813)
exp_KLD.px.py_b <- c(-0.02919,-0.011925,0.004961,0.01991,0.01945)
exp_KLD.py.px_b <- c(0.032568,0.012448,-0.004877,-0.018643,-0.018251)
exp_mean.KLD_b <- c(0.0016894,0.0002615,0.0000421,0.0006334,0.0005995)
exp_sum.KLD.px.py_b <- 0.003207
exp_sum.KLD.py.px_b <- 0.003245
exp_mean.sum.KLD_b <- 0.003226
exp_intrinsic.discrepancy_b <- 0.003207

exp_base_c <- 2.718282
exp_n1_c <- 4
exp_n2_c <- 4
exp_px.normalized_c <- c(0.19213,0.259349,0.313617,0.234903)
exp_py.normalized_c <- c(0.184713,0.261146,0.318471,0.235669)
exp_KLD.px.py_c <- c(0.007564,-0.001791,-0.004817,-0.000764)
exp_KLD.py.px_c <- c(-0.007272,0.001804,0.004891,0.000767)
exp_mean.KLD_c <- c(0.000146,0.000006,0.000037,0.000001)
exp_sum.KLD.px.py_c <- 0.000192
exp_sum.KLD.py.px_c <- 0.00019
exp_mean.sum.KLD_c <- 0.000191
exp_intrinsic.discrepancy_c <- 0.00019

# Compare expected data with real data
context("Simeval, ofv, function kld")
test_that("If function kld works as expected",{
  expect_equal(exp_base_a,base_a)
  expect_equal(exp_n1_a,n1_a)
  expect_equal(exp_n2_a,n2_a)
  expect_equal(exp_px.normalized_a,px.normalized_a)
  expect_equal(exp_py.normalized_a,py.normalized_a)
  expect_equal(exp_KLD.px.py_a,KLD.px.py_a)
  expect_equal(exp_KLD.py.px_a,KLD.py.px_a)
  expect_equal(exp_mean.KLD_a,mean.KLD_a)
  expect_equal(exp_sum.KLD.px.py_a,sum.KLD.px.py_a)
  expect_equal(exp_sum.KLD.py.px_a,sum.KLD.py.px_a)
  expect_equal(exp_mean.sum.KLD_a,mean.sum.KLD_a)
  expect_equal(exp_intrinsic.discrepancy_a,intrinsic.discrepancy_a)
  expect_equal(exp_base_b,base_b)
  expect_equal(exp_n1_b,n1_b)
  expect_equal(exp_n2_b,n2_b)
  expect_equal(exp_px.normalized_b,px.normalized_b)
  expect_equal(exp_py.normalized_b,py.normalized_b)
  expect_equal(exp_KLD.px.py_b,KLD.px.py_b)
  expect_equal(exp_KLD.py.px_b,KLD.py.px_b)
  expect_equal(exp_mean.KLD_b,mean.KLD_b)
  expect_equal(exp_sum.KLD.px.py_b,sum.KLD.px.py_b)
  expect_equal(exp_sum.KLD.py.px_b,sum.KLD.py.px_b)
  expect_equal(exp_mean.sum.KLD_b,mean.sum.KLD_b)
  expect_equal(exp_intrinsic.discrepancy_b,intrinsic.discrepancy_b)
  expect_equal(exp_base_c,base_c)
  expect_equal(exp_n1_c,n1_c)
  expect_equal(exp_n2_c,n2_c)
  expect_equal(exp_px.normalized_c,px.normalized_c)
  expect_equal(exp_py.normalized_c,py.normalized_c)
  expect_equal(exp_KLD.px.py_c,KLD.px.py_c)
  expect_equal(exp_KLD.py.px_c,KLD.py.px_c)
  expect_equal(exp_mean.KLD_c,mean.KLD_c)
  expect_equal(exp_sum.KLD.px.py_c,sum.KLD.px.py_c)
  expect_equal(exp_sum.KLD.py.px_c,sum.KLD.py.px_c)
  expect_equal(exp_mean.sum.KLD_c,mean.sum.KLD_c)
  expect_equal(exp_intrinsic.discrepancy_c,intrinsic.discrepancy_c)
  expect_equal("ERROR: px and py must have the same length.",list_kld_d)
  expect_equal("ERROR: px and py must have finite values.",list_kld_e)
})