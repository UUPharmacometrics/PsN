#!/usr/bin/Rscript

library(MASS)

rm(list=ls(all=TRUE))

lower_triangle_vector <- function(A) {
    # Get rowwise vector of lower triangle of square matrix A
    t(A)[upper.tri(A, diag=TRUE)]   
}

symmetric_from_flattened <- function(flattened) {
    # The triangular root of the number of elements in flattened is the size of the full matrix
    size <- trunc((sqrt(8 * length(flattened) + 1) - 1) / 2)   # floor(sqrt(2*T_n)) might work but couldn't find proof
    
    upper <- matrix(0, size, size)
    # Set the flattened vector into the lower triangle of the matrix
    upper[upper.tri(upper, diag=TRUE)] <- flattened
    symm <- t(upper)
    upper[lower.tri(upper, diag=TRUE)] <- 0
    symm <- symm + upper
}

#calc_EBE_mean_cov <- function(A) {
#    EBE_mean <- colMeans(A)
#    rowsub <- t(t(A) - EBE_mean)  # Subtract EBE_mean from each row of A
#    R <- qr.R(qr(rowsub))
#    EBE_cov <- t(R) %*% R
#    list(mean=EBE_mean, cov=EBE_cov)
#}


influential_individuals <- function(phiname) {
    N_iter <- 200    # Number of Monte Carlo iterations

    phi_all <- read.table(phiname, skip=1, header=TRUE)
    N_ind_all <- nrow(phi_all)
    etamask <- grepl("^ETA", colnames(phi_all))
    etcmask <- grepl("^ETC", colnames(phi_all))
    N_eta <- sum(etamask)
    result_table <- matrix(0, N_ind_all, 3)     # ID, Cookscore, covratio

    for (k in 0:N_ind_all) {        # Which ID should be deleted. 0 means keep all
        cat("Starting iteration", k, "\n")
        if (k == 0) {
            N_ind <- N_ind_all
            phi <- phi_all
        } else {
            N_ind <- N_ind_all - 1
            phi <- phi_all[-c(k),]  # Remove the k:th individual
        }

        # Calculate mu and cov
        mu <- matrix(0, N_ind, N_eta)
        cov <- array(0, c(N_ind, N_eta, N_eta))
        for (i in 1:N_ind) {
            mu[i,] <- as.numeric(phi[i, etamask])
            cov[i,,] <- symmetric_from_flattened(as.numeric(phi[i, etcmask]))
        }

        B <- matrix(0, N_iter, N_eta + sum(etcmask))

        for (j in 1:N_iter) {
            # Sample A_j
            A_j <- matrix(0, N_ind, N_eta)

            for (i in 1:N_ind) {
                A_j[i, ] <- mvrnorm(1, mu[i,], cov[i,,])
            }

            EBE_mean <- colMeans(A_j)
            EBE_cov <- cov(A_j)
            # The b:th row of B_original is combination of mean and lower triangle of cov
            B[j,] <- c(EBE_mean, lower_triangle_vector(EBE_cov))
        }

        cov_B_k <- cov(B)
    
        EBE_observed_mean <- colMeans(mu)       # A_observed and mu are the same
        EBE_observed_cov <- cov(mu)
        parameters_k <- c(EBE_observed_mean, lower_triangle_vector(EBE_observed_cov))
        if (k == 0) {
            parameters_orig <- parameters_k
        }
    
        # cov ratio
        if (k == 0) {
            det_cov_orig <- det(cov_B_k)
        } else {
            cov_ratio <- sqrt(det(cov_B_k) / det_cov_orig)
        }

        # Cook score
        if (k == 0) {
            inv_cov_orig <- solve(cov_B_k)
        } else {
            cook_score <- sqrt(t(parameters_k - parameters_orig) %*% inv_cov_orig %*% (parameters_k - parameters_orig))
        }

        if (k != 0) {
            result_table[k,] <- c(phi_all[k, 2], cook_score, cov_ratio)
        }
    }

    result_table <- as.data.frame(result_table)
    colnames(result_table) <- c("ID", "Cook score", "Cov ratio")

    return(result_table)
}

phiname = 'pheno.phi'
result <- influential_individuals(phiname)

# Ideas. Test to calculate cov using R function directly
# Check how A_orig is really used

# TODO: Plots?