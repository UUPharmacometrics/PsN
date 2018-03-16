suppressMessages(library(dplyr))
library(ggplot2)
suppressMessages(library(vpc))

# Read a nonmem table into a data.frame
# Currently assumes that there is a header and the regular TABLE lines.
# Adds a "replicate" column with the number of the replicate counting from 1
# This is not optimized for speed or for memory.
# Using xpose (again)
read_nonmem_table <- function(filename) {
    lines <- readLines(filename)
#    header <- lines[2]

    numreplicates <- 0
    for (line in lines) {
        if (startsWith(line, "TABLE")) {
            numreplicates <- numreplicates + 1
        }
    }
    table <- xpose::read_nm_tables(filename)
#    filter_fn <- function(line) { !startsWith(line, "TABLE") && line != header }
#    lines <- Filter(filter_fn, lines)

#    column_names <- strsplit(header, "\\s+")[[1]][-1]  # Get array of column names
#    table <- read.table(text=lines, col.names=column_names)
    table$replicate <- rep(1:numreplicates, each=nrow(table) / numreplicates)

    return(table)
}

# Input: A table with ID, SUBPOP, PMIX and replicate columns
#        This can be read in from for example a .phm file from NONMEM
# For each replicate for each ID find the SUBPOP with highest PMIX.
# Create a table with ID, MIXEST, replicate with only one row per ID/replicate
most_probable_mixture_subpop <- function(df) {
    df %>%
        dplyr::group_by(ID, replicate) %>%
        dplyr::summarize(MIXEST=SUBPOP[which.max(PMIX)]) %>%
        dplyr::arrange(replicate)
}

# Input: A table with ID, SUBPOP, PMIX columns
#        This can be read in from for example a .phm file from NONMEM
# For each SUBPOP calculate the mean PMIX over all individuals
# Output is a table with SUBPOP and PMIX where PMIX is now an average
average_probability_per_mixture_subpops <- function(df) {
    df %>%
        dplyr::group_by(SUBPOP) %>%
        dplyr::summarise(PMIX=mean(PMIX))
}

# The input is a table with ID, SUBPOP, PMIX and replicate columns
# The function outputs a table with one randomly (given PMIX) selected SUBPOP for each ID, replicate pair
randomize_mixture_subpops <- function(df) {
    df %>%
        group_by(replicate, ID) %>%
        summarise(SUBPOP=sample(SUBPOP, size=1, prob=PMIX))
}

# Input is obs - observation table
#   sim - simulation table
#   obs_mixture - The mixture data for the observations table with ID, SUBPOP, PMIX and replicate
#   bins - Array of bins for the vpc if needed specifically
#   dv - name of dv. default DV
#   randomize - Default is to only use the subpop with maximum probability for each ID
#               This option randomizes using the probabilities in the input
# Can return error message instead of plot for a subpopulation
mixture_vpc <- function(obs, sim, obs_mixture, sim_mixture, bins, dv="DV", randomize=FALSE) {
    #numsims <- unique(sim_mixture$replicate)
    
    if (randomize) {
        randomized_sim <- randomize_mixture_subpops(sim_mixture)
        sim <- dplyr::full_join(sim, randomized_sim)
        randomized_obs <- randomize_mixture_subpops(obs_mixture)
        obs <- dplyr::full_join(obs, randomized_obs)
        mixcol <- 'SUBPOP'
        method_name <- 'Randomized Mixture'
    } else {
        mixest_sim <- most_probable_mixture_subpop(sim_mixture)
        sim <- dplyr::full_join(sim, mixest_sim)
        mixest_obs <- most_probable_mixture_subpop(obs_mixture)
        obs <- dplyr::full_join(obs, mixest_obs)
        mixcol <- 'MIXEST'
        method_name <- 'MIXEST Mixture'
    }
    
    colnames(sim)[colnames(sim)=="replicate"] <- "sim"  # Rename replicate column to sim for vpc
    
    num_ids <- length(unique(obs$ID))
    unique_subpops <- sort(unique(c(obs[[mixcol]], sim[[mixcol]])))
    table_list <- list()

    for (i in unique_subpops) {
        subobs <- filter_(obs, paste0(mixcol, "==", i))
        subsim <- filter_(sim, paste0(mixcol, "==", i))
        if (nrow(subsim) == 0) {
            table_list[[i]] <- paste0("There are no individuals simulated to belong to subpopulation ", i)
            next
        }
        if (nrow(subobs) == 0) {
            table_list[[i]] <- paste0("There are no individuals in subpopulation ", i, " in the original model")
            next
        }
        if (missing(bins)) {
            vpc <- vpc::vpc(obs=subobs, sim=subsim, obs_cols=list(dv=dv), sim_cols=list(dv=dv))
        } else {
            vpc <- vpc::vpc(obs=subobs, sim=subsim, obs_cols=list(dv=dv), sim_cols=list(dv=dv), bins=bins)
        }

        ids_per_sim <- subsim %>% dplyr::group_by(sim) %>% dplyr::summarise(count=length(unique(ID)))
        ids_per_sim <- ids_per_sim$count
        lower_quantile <- (quantile(ids_per_sim, probs=0.05) / num_ids) * 100
        upper_quantile <- (quantile(ids_per_sim, probs=0.95) / num_ids) * 100

        mean_probs <- average_probability_per_mixture_subpops(obs_mixture)
        pmix_value <- mean_probs$PMIX[i] * 100

        obs_ids <- length(unique(subobs$ID))
        orig_value <- (obs_ids / num_ids) * 100

        title <- sprintf("%s SUBPOP=%d     PMIX=%.1f%%\nORIGID=%.1f%% SIMID=[%.0f%%, %.0f%%] (5%%, 95%% percentiles)",
                         method_name, i, pmix_value, orig_value, lower_quantile, upper_quantile)
        vpc <- vpc + ggtitle(title)
        table_list[[i]] <- vpc
    }
    
    return(table_list)
}