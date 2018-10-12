options(warn=-1) # don't show warnings
suppressMessages(library(testthat))
# Used libraries
suppressMessages(library(PerformanceAnalytics))
suppressMessages(require("PEIP"))
# Test files
test_file("cdd/test_cdd_1.R")
test_file("cdd/test_cdd_2.R")
test_file("execute/test_data.obj.obsi.R")
test_file("npfit/test_npfit.R")
test_file("compare/test_two_tools_functions.R")
test_file("bootstrap/test_bootstrap.R")
test_file("vpc/test_vpc_mixture.R")
test_file("scm/test_scm.R")
test_file("common/test_common_fun.R")
test_file("qa/test_qa.R")