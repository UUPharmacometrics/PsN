library(dplyr, quietly=TRUE)
library(ggplot2)

# .phm is output after an Estimation in NONMEM

#library(devtools)
#dev_mode(on=TRUE, path=getOption("devtools.path"))
#install_local("./vpc", quick=TRUE, dependencies=FALSE, quiet=TRUE)
library(vpc)

#phm <- read.table("example/example3.phm",  skip=1, header=TRUE)
#subpops <- phm %>% group_by(ID) %>% summarise(SUBPOP=which.max(PMIX))

#The Simple Asymtotic Method:
#Where n is the sample size,
#p is the proportion,
#z is the z value for the % interval (i.e. 1.96 provides the 95% CI)
#and cc is whether a continuity correction should be applied.
simpasym <- function(n, p, z=1.96, cc=TRUE) {
    out <- list()
    if(cc) {
        out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
        out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
    } else {
        out$lb <- p - z*sqrt((p*(1-p))/n)
        out$ub <- p + z*sqrt((p*(1-p))/n)
    }
    out
}

vpc_mixtures <- function(obs, sim, numsims, mixcol="MIXNUM", dv="DV") {

    num_ids <- length(unique(obs$ID))

    # Put in replicate numbers in sim table
    sim$sim <- rep(1:numsims, each=nrow(sim) / numsims)

    numsubs <- max(max(obs[mixcol]), max(sim[mixcol]))

    table_list <- list()

    for (i in 1:numsubs) {
        subobs <- filter_(obs, paste0(mixcol, "==", i))
        subsim <- filter_(sim, paste0(mixcol, "==", i))
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
subpopulations_from_nonmem_phm <- function(name, nrep) {
    phm <- read.table(name, skip=1, header=TRUE)

    nind <- length(unique(phm$ID))
    nsubpop <- max(phm$SUBPOP)

    rnd <- runif(nind)
    rnd <- rep(rnd, nsubpop)
    mask <- rnd <= phm$PMIX
    return(data.frame(ID=unique(phm$ID), SUBPOP=phm$SUBPOP[mask]))
}


#phm <- subpopulations_from_nonmem_phm("example/example3.phm")

#obs <- read_table_nm("example/esttab")
#sim <- read_table_nm("example/simtab")

#sim <- full_join(sim, phm)
#obs <- full_join(obs, phm)

#plots <- vpc_mixtures(obs=obs, sim=sim, numsims=20, mixcol="SUBPOP", dv="CONC")

#for (p in plots) {
#    print(p)
#}



#dev_mode(on=FALSE)
