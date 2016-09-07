library(testthat)
# Used libraries
library(PerformanceAnalytics)
require("PEIP")
# Test files
test_file("cdd/test_cdd_1.R")
test_file("cdd/test_cdd_2.R")
test_file("simeval/test_ebe_npde.R")
test_file("simeval/test_cwres_iwres.R")
test_file("simeval/test_ofv.R")
test_file("simeval/test_outlier_report_table.R")