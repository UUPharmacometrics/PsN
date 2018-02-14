suppressMessages(library(vpc))
suppressMessages(library(dplyr))
library(ggplot2)
library(xpose)


vpc_mixtures <- function(obs, sim, numsims, mixcol="MIXNUM", dv="DV", phm_obs, phm_sim) {
    # Put in replicate numbers in sim table
    sim$sim <- rep(1:numsims, each=nrow(sim) / numsims)

    if (!missing(phm_sim)) {
        phm_table <- subpopulations_from_nonmem_phm(phm_sim, numsims)
        sim <- dplyr::full_join(sim, phm_table)
        phm_table_obs <- subpopulations_from_nonmem_phm(phm_obs, 1)
        obs <- dplyr::full_join(obs, phm_table_obs)
        mixcol <- 'SUBPOP'
        method <- 'Randomized Mixture'
    } else {
        method <- 'MIXEST Mixture'
    }

    num_ids <- length(unique(obs$ID))

    unique_subpops <- sort(unique(c(obs[[mixcol]], sim[[mixcol]])))

    table_list <- list()

    for (i in unique_subpops) {
        subobs <- filter_(obs, paste0(mixcol, "==", i))
        subsim <- filter_(sim, paste0(mixcol, "==", i))
        if (nrow(subsim) == 0) {
            next
        }
        vpc <- vpc::vpc(obs=subobs, sim=subsim, obs_cols=list(dv=dv), sim_cols=list(dv=dv))

        obs_ids <- length(unique(subobs$ID))
        perc_obs_ids <- (obs_ids / num_ids) * 100
    
        ids_per_sim <- subsim %>% group_by(sim) %>% summarise(count=length(unique(ID)))
        ids_per_sim <- ids_per_sim$count
        lower_quantile <- (quantile(ids_per_sim, probs=0.05) / num_ids) * 100
        upper_quantile <- (quantile(ids_per_sim, probs=0.95) / num_ids) * 100

        title <- sprintf("%s SUBPOP=%d\nORIGID=%.1f%% SIMID=[%.0f%%, %.0f%%] (5%%, 95%% percentiles)",
                         method, i, perc_obs_ids, lower_quantile, upper_quantile)
        vpc <- vpc + ggtitle(title)
        table_list[[i]] <- vpc
    }
    
    return(table_list)
}


# Takes a NONMEM phm file as input and outputs
# a data.frame with ID and SUBPOP columns
# The SUBPOP is a random sample given the PMIX probabilities
# nrep is the number of replicates
# phm can either be a file name of a phm file or a data.frame
subpopulations_from_nonmem_phm <- function(phm, nrep) {
    if (is.character(phm)) {
        phm_table <- xpose::read_nm_files(file=phm)
    } else {
        phm_table <- phm
    }

    ind_table <- dplyr::bind_rows(phm_table[['data']])   # One table for all replicates
    ind_table <- data.frame(ID=ind_table$ID, SUBPOP=ind_table$SUBPOP, PMIX=ind_table$PMIX)  # Keep only interesting columns
    ind_table$sim <- rep(1:nrep, each=nrow(ind_table) / nrep) # number the replicates

    result <- data.frame(ind_table %>% group_by(sim, ID) %>% summarize(SUBPOP=sample(SUBPOP, size=1, prob=PMIX)))

    return(result)
}
