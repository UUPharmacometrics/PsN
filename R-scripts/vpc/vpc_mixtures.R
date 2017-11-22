suppressMessages(library(vpc))
suppressMessages(library(dplyr))
library(ggplot2)

#The Simple Asymtotic Method:
#Where n is the sample size,
#p is the proportion,
#z is the z value for the % interval (i.e. 1.96 provides the 95% CI)
#and cc is whether a continuity correction should be applied.
simpasym <- function(n, p, z=1.96, cc=TRUE) {
    out <- list()
    if (cc) {
        out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
        out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
    } else {
        out$lb <- p - z*sqrt((p*(1-p))/n)
        out$ub <- p + z*sqrt((p*(1-p))/n)
    }
    out
}

vpc_mixtures <- function(obs, sim, numsims, mixcol="MIXNUM", dv="DV", phm) {
    # Put in replicate numbers in sim table
    sim$sim <- rep(1:numsims, each=nrow(sim) / numsims)

    if (!missing(phm)) {
        phm_table <- subpopulations_from_nonmem_phm(phm, numsims)
        sim <- full_join(sim, phm_table)
        names(obs)[names(obs) == mixcol] <- 'SUBPOP'    # rename mixcol in obs
        mixcol <- 'SUBPOP'
    }

    num_ids <- length(unique(obs$ID))

    numsubs <- max(max(obs[mixcol]), max(sim[mixcol]))

    table_list <- list()

    for (i in 1:numsubs) {
        subobs <- filter_(obs, paste0(mixcol, "==", i))
        subsim <- filter_(sim, paste0(mixcol, "==", i))
        if (nrow(subsim) == 0) {
            next
        }
        vpc <- vpc::vpc(obs=subobs, sim=subsim, obs_cols=list(dv=dv), sim_cols=list(dv=dv))

        obs_ids <- length(unique(subobs$ID))
        perc_obs_ids <- (obs_ids / num_ids) * 100
    
        ids_per_sim <- subsim %>% group_by(sim) %>% summarise(count=length(unique(ID)))
        sim_ids <- sum(ids_per_sim$count)
        perc_sim_ids <- (sim_ids / (numsims * num_ids)) * 100

        ci <- simpasym(numsims * num_ids, perc_sim_ids / 100)

        title <- sprintf("MIXTURE VPC ORIGID=%.1f%% SIMID=[%.1f%%, %.1f%%] (95%% CI)", perc_obs_ids, ci[[1]] * 100, ci[[2]] * 100)
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
        phm <- read.table(name, skip=1, header=TRUE)
    }

    phm <- data.frame(ID=phm$ID, SUBPOP=phm$SUBPOP, PMIX=phm$PMIX)  # Keep only interesting columns
    phm <- phm[rep(seq_len(nrow(phm)), nrep),]  # One phm per replicate
    phm$sim <- rep(1:nrep, each=nrow(phm) / nrep) # put in the SIM column

    result <- data.frame(phm %>% group_by(sim, ID) %>% summarize(SUBPOP=sample(SUBPOP, size=1, prob=PMIX)))

    return(result)
}
