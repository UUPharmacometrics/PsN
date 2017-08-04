plot_boxcox_shape <- function(data_table,eta_table) {
  
    get_x_min_max <- function(data_table,eta_table,y_pec=0.01) {
      for(i in unique(data_table$ETA_name)) {
        data_table_per_eta <- data_table %>% filter(ETA_name %in% i)
        # 1 procent of the density values
        y_min_limit <- min(data_table_per_eta$density) + y_pec*abs(max(data_table_per_eta$density)-min(data_table_per_eta$density))
        spec <- data_table_per_eta %>% filter(density>y_min_limit)
        x_max <- max(spec$eta)
        x_min <- min(spec$eta)

        # check if y_min_limit should be smaller because of the real boxcox eta values
        if(any(eta_table$value > x_max)) {
          x_max <- max(eta_table$value)
        }
        if(any(eta_table$value < x_min)) {
          x_min <- min(eta_table$value)
        }
        data_table_per_eta <- data_table_per_eta %>% filter(eta>=x_min,eta<=x_max)

        if(i==unique(data_table$ETA_name)[1]) {
          data_table_new <- data_table_per_eta
        } else {
          data_table_new <- rbind(data_table_new,data_table_per_eta)
        }
      }
      return(data_table_new)
    }
    data_table_new <- get_x_min_max(data_table,eta_table)
    
    #make a plot
    n_pages <- ceiling(length(unique(data_table_new$ETA_name))/12)
    p <- list()
    for(i in seq_len(n_pages)) {
      p[[i]] <- ggplot(data_table_new,aes(x=eta,y=density,fill=type)) +
        geom_area(alpha=0.3) +
        theme_bw() +
        labs(x="",y="") +
        scale_fill_manual("",values=c("ETA"="black","ETAT"="blue"),labels=c("Untransformed density","Boxcox transformed density")) +
        theme(legend.position = "top")
      
      if(length(unique(data_table_new$ETA_name)) > 4) {
        p[[i]] <- p[[i]] + facet_wrap_paginate(~ETA_name,nrow=4,ncol=3,scales="free",page=i)
      } else {
        p[[i]] <- p[[i]] + facet_wrap_paginate(~ETA_name,ncol=2,scales="free")
      }
      
      if(!missing(eta_table)) {
        if(nrow(eta_table)>0) {
          p[[i]] <- p[[i]] + geom_rug(data=eta_table,aes(x=value),sides="b",inherit.aes = F)
        }
      }
    }

  return(p)
}