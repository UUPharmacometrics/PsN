hist_of_cdd_dofv <- function(values,quiet=F) {
  values <- values[!is.na(values)]
  values_table <- data.frame(dOFV=values,stringsAsFactors = F)
  if(length(values)>0) {
    x_scaling <- rbind(values_table,3.84)
    p <- ggplot(values_table,aes(values_table$dOFV)) +
      geom_histogram(bins=20,
                     fill='white',
                     col="black",
                     closed = "left",
                     boundary = 0) +
      labs(x="dOFV",y="Frequency") +
      geom_vline(xintercept=3.84,color="red") +
      theme_bw() +
      scale_x_continuous(breaks = sort(c(with(x_scaling, labeling::extended(range(x_scaling)[1], range(x_scaling)[2], m = 5)),3.84)))
    return(p)
  } else {
    if(!quiet) {
      message("WARNING: In function hist_of_cdd_dofv values vector is empty or all values are NA!")
    }
  }
}