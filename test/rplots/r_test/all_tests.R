options(warn=-1) # don't show warnings
suppressMessages(library(testthat))
# Used libraries
suppressMessages(library(PerformanceAnalytics))
suppressMessages(require("PEIP"))
# Test files
test_file("cdd/test_cdd_1.R")
test_file("cdd/test_cdd_2.R")
test_file("simeval/test_ebe_npde.R")
test_file("simeval/test_cwres_iwres.R")
test_file("simeval/test_ofv.R")
test_file("simeval/test_outlier_report_table.R")
test_file("execute/test_data.obj.obsi.R")
test_file("simeval_cdd/test_simeval_cdd.R")
test_file("npfit/test_npfit.R")
test_file("compare/test_two_tools_functions.R")