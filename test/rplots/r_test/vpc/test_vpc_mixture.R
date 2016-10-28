library(testthat)

suppressMessages(have_vpc <- require("vpc"))

check_vpc <- function() {
    if (!have_vpc) {
        skip("vpc pacakge not available")
    }
}


source("../../../../R-scripts/vpc/vpc_mixtures.R")

context("vpc_mixtures subpopulations_from_nonmem_phm function")
a <- data.frame(ID=c(1,1,2,2), SUBPOP=c(1,2,1,2), PMIX=c(1,0,0,1))
test_that("phm with probabilities 1 and 0, 1 replicate",{
    check_vpc()
    res <- subpopulations_from_nonmem_phm(a, 1)
    expect_equal(res, data.frame(sim=c(1,1), ID=c(1,2), SUBPOP=c(1,2)))
})

a <- data.frame(ID=c(1,1,2,2), SUBPOP=c(1,2,1,2), PMIX=c(0,1,1,0))
test_that("phm with probabilities 1 and 0, 2 replicates",{
    check_vpc()
    res <- subpopulations_from_nonmem_phm(a, 2)
    facit <- data.frame(sim=c(1,1,2,2), ID=c(1,2,1,2), SUBPOP=c(2,1,2,1))
    expect_equal(res, data.frame(sim=c(1,1,2,2), ID=c(1,2,1,2), SUBPOP=c(2,1,2,1)))
})
