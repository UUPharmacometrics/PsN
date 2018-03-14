plot_structural_vpc <- function(obs,sim,shift_tab,idv) {
  sim_shifted <- sim %>%
    dplyr::mutate(bin_index = findInterval(unlist(.[[toupper(idv)]]), shift_tab$bin_min),  # in which bin did observation go
           shift = shift_tab$shift[bin_index],
           DV = DV - shift)
  
  obs_combined <- dplyr::bind_rows(before = obs, corrected = obs, .id = "type")
  sim_combined <- dplyr::bind_rows(before = sim, corrected = sim_shifted, .id = "type")
  
  p <- vpc::vpc (sim = sim_combined, 
                 obs = obs_combined,
                 obs_cols = list(
                   dv = "DV",                             # these column names are the default,
                   idv = toupper(idv)),                         # update these if different.
                 sim_cols = list(
                   dv = "DV",
                   idv = toupper(idv)),
                 bins = "percentiles",
                 n_bins = 10,
                 stratify = "type"
  )+
    theme_bw() +
    facet_wrap(~type)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}  