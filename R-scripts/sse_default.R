library(ggplot2)
library(plyr)
source(paste0(rscripts.directory,"/common/plot.table.R"),echo=TRUE)

theme_set(theme_bw(12))						# set standard theme
remove_na_dofvs <- TRUE 					# should dofvs with NA be removed? 
max_power <- 0.99 								# until which value should the power curve be drawn
alpha <- 0.05											# type-I error rate 
diagnostics <- rplots.level>1 		# should diagnostics be created
# end of user options
options(stringsAsFactors = FALSE)

#parametric power estimation function
ppe <- function(dofvs, df=1){
  dofvs <- dofvs[dofvs>0]
  opt.fun<-function(ncp)-sum(dchisq(dofvs,df,ncp,log=T))
  init <- mean(dofvs)-df
  fit <- optim(par=init, fn=opt.fun, lower=0, method="L-BFGS-B")
  ncp <- fit$par
  power <- pchisq(qchisq(1-alpha,df=df,ncp=0),df=df,ncp=ncp,lower.tail=F)
  return(list(power=power, ncp=ncp))
}

# this function does the actual parameter estimation, it will estimate ncp & df parameters
# if they are set to NA
chi_square_est <- function(dofvs, ncp = NA, df=1){
  dofvs <- dofvs[dofvs>0]
  if(is.na(ncp) && is.na(df)){
    opt.fun<-function(ncp, df)-sum(dchisq(dofvs,df,ncp,log=T))
    init <- c(mean(dofvs)-1, 1) # TODO: use better initals of this is really used (based on var)
  }else if(is.na(ncp)){
    opt.fun<-function(ncp)-sum(dchisq(dofvs,df,ncp,log=T))
    init <- mean(dofvs)-df
  }else if(is.na(df)){
    opt.fun<-function(df)-sum(dchisq(dofvs,df,ncp,log=T))
    init <- mean(dofvs)-ncp
  }
  fit <- optim(par=init, fn=opt.fun, lower=init*0+1e-16, method="L-BFGS-B")
  return(fit$par)
}

#parametric power estimation function
ppe <- function(dofvs, df=1){
  ncp <- chi_square_est(dofvs, df=df)
  power <- pchisq(qchisq(1-alpha,df=df,ncp=0),df=df,ncp=ncp,lower.tail=F)
  return(list(power=power, ncp=ncp))
}

#df estimation function assuming ncp=0
df_est <- function(dofvs, ncp=0, df = 1){
  df_est <- chi_square_est(dofvs, df=NA, ncp=0)
  type_1 <- pchisq(qchisq(1-alpha,df=df,ncp=0), df = df_est, ncp=0, lower.tail = F)
  return(list(df = df_est, type_1=type_1))
}

ppe_subjects <- function(dofvs, ncp=NULL, df=1, n.subjects, pred.n.subjects){
  if(is.null(ncp)){
    ppe.fit <- ppe(dofvs, df)
    ncp <- ppe.fit$ncp
  }
  ncps <- ncp/n.subjects*pred.n.subjects  
  power <- pchisq(qchisq(1-alpha,df=df,ncp=0),df=df,ncp=ncps,lower.tail=F)
  return(power)
}

param_boot_ppe <- function(ncp, nmc_samples, df=1, n.boot=1000, value="ncp",
                           sum.fun=quantile, sum.fun.args=list(probs=c(0.025,0.975))){
  bsamples <- switch(value,
                     ncp =  replicate(n.boot, ppe(rchisq(nmc_samples, df, ncp), df = df)[["ncp"]]),
                     df = replicate(n.boot, df_est(rchisq(nmc_samples, df, ncp), ncp = ncp)[["df"]]))
  if(is.null(sum.fun)){
    return(bsamples)
  }else{
    sum.fun.args$x <- bsamples
    return(do.call(sum.fun, args=sum.fun.args))     
  }
}


# construct full reduced pairs based on models provided
model_names_indicies <- regexpr(".*(?=\\..*)", model.file.names, perl = T)
model_names <- substr(model.file.names, model_names_indicies, model_names_indicies+attr(model_names_indicies, "match.length")-1)

reduced_pattern <- ifelse(grepl("_(f|full)$", model_names), 
                          gsub("_(f|full)$","_(r|red|reduced)\\\\d*",model_names),
                          paste0(model_names, "_(r|red|reduced)\\d*")) 			 

reduced_matches <- sapply(reduced_pattern, grepl, x=model_names, USE.NAMES = F)

if(any(reduced_matches)){
  order_matching <- F
  #matching based on filename
  comb_index <- which(reduced_matches, arr.ind = T)
  
}else{
  #matching based on order
  order_matching <- T
  comb_index <- matrix(seq_len(length(model.file.names)%/%2*2), ncol=2, byrow = T,dimnames = list(NULL, c("col","row")))
}

model_pairs <- data.frame(full=model.rawres.names[comb_index[,"col"]],
                          full_mod=model_names[comb_index[,"col"]],
                          red=model.rawres.names[comb_index[,"row"]],
                          red_mod=model_names[comb_index[,"row"]],
                          subjects=model.subjects[comb_index[,"col"]],
                          df=model.estimated.params[comb_index[,"col"]]-model.estimated.params[comb_index[,"row"]])

if(order_matching && any(model_pairs$df<0)){
  temp <- model_pairs[,c("full", "full_mod")]
  model_pairs[,c("full", "full_mod")] <- model_pairs[,c("red","red_mod")]
  model_pairs[,c("red","red_mod")] <- temp
  model_pairs$df <- -model_pairs$df
}

if(nrow(model_pairs)==0) stop("No valid model pair provided!")

raw_results <- read.csv(raw.results.file)


dofvs_df <- ddply(model_pairs, names(model_pairs), function(model_pair){
  ofv_full <- subset(raw_results, hypothesis==model_pair[1,"full"], select=c(sample, ofv))
  ofv_reduced <- subset(raw_results, hypothesis==model_pair[1,"red"], select=c(sample, ofv))
  names(ofv_full) <- c("SIM","FULL")
  names(ofv_reduced) <- c("SIM", "RED")
  combined <- merge(ofv_full, ofv_reduced, all.x=T)
  delta_ofvs <- with(combined, RED-FULL)
  type_1 <- F
  if(median(delta_ofvs)<0) type_1 <- T
  if(remove_na_dofvs) delta_ofvs <- delta_ofvs[!is.na(delta_ofvs)]
  data.frame(dofv=delta_ofvs, 
             name=sprintf("%s vs. %s (%i DF)", model_pair[1,"full_mod"], model_pair[1,"red_mod"],model_pair[1,"df"]),
             type_1 = type_1)
})


est_results <- plyr::ddply(dofvs_df, .(name), function(dofv_df){
  if(dofv_df$type_1[1]){
    parameter <- "df"
    dofv_df$dofv <- -dofv_df$dofv
    est_results <- df_est(dofv_df$dofv)
    estimate <- est_results[[parameter]]
    prob <- est_results$type_1
    bootstrap_ci <- param_boot_ppe(ncp = 0, 
                                   nmc_samples = nrow(dofv_df), 
                                   df = estimate, value = parameter)  
    max_subjects <- NA
  }else{
    parameter <- "ncp"
    est_results <- ppe(dofv_df$dofv)
    estimate <- est_results[[parameter]]
    bootstrap_ci <- param_boot_ppe(ncp = estimate, 
                                   nmc_samples = nrow(dofv_df), 
                                   df = dofv_df$df[1], value = parameter)
    prob <- est_results$power
    # determine range for sample size curves
    max_subjects <- uniroot(function(n_subjects) 
      ppe_subjects(ncp=estimate, df=dofv_df$df[1], 
                   n.subjects=dofv_df$subjects[1], 
                   pred.n.subjects=n_subjects)-max_power,
      lower=0, upper=2*max_power/prob*dofv_df$subjects[1], extendInt="upX", tol=0.01)$root
  }
  n_negative <- sum(dofv_df$dofv<0) 
  # estimate df parameter
  # calculate df CI using parametric bootstrap
  
  data.frame(df  = dofv_df$df[1], subjects = dofv_df$subjects[1],
             samples = nrow(dofv_df), n_negative = n_negative, max_dofv = max(dofv_df$dofv),
             type_1 = alpha, prob = prob, max_subjects = max_subjects,
             parameter = parameter,  estimate = estimate, 
             ci_lower = bootstrap_ci[[1]], ci_upper=bootstrap_ci[[2]])
})


# calculate power curves
power_curves <- ddply(subset(est_results, parameter=="ncp"), .(name), function(df){
  subjects <- seq(0, ceiling(max(est_results$max_subjects)))
  # determine uncertainty via bootstrap
  power <- sapply(c(df$estimate[1],df$ci_lower[1], df$ci_upper[1]), function(ncp) ppe_subjects(ncp=ncp, df=df$df[1], 
                                                                                               n.subjects=df$subjects[1],
                                                                                               pred.n.subjects=subjects))
  data.frame(subjects = subjects, 
             power=power[,1], 
             power_lower=power[,2], 
             power_upper=power[,3])
})

output_table <- plyr::ddply(est_results, .(), function(df)
  data.frame(Comparison=df$name, 
             `#dOFV` = df$samples,
             `#(dOFV<0)` = df$n_negative,
             Parameter=df$parameter,
             `Estimate [CI]` = sprintf("%.1f [%.1f-%.1f]", df$estimate, df$ci_lower, df$ci_upper), check.names = F))
output_table$`.id` <- NULL

pdf(pdf.filename)
plot.table(output_table)


if(nrow(power_curves)){
  p <- ggplot(power_curves, aes(subjects, power, colour=name, fill=name))+
    geom_ribbon(aes(ymin=power_lower, ymax=power_upper), alpha=0.3, colour=NA)+
    geom_line(size=1)+
    geom_hline(yintercept=.8, linetype="dashed")+
    scale_x_continuous("Study size")+
    scale_y_continuous("Power", labels=function(breaks) paste0(breaks*100,"%"), breaks=seq(0,1, by=.2))+
    theme(legend.position=c(1,0), legend.justification=c(1,0), legend.title=element_blank())+
    ggtitle(sprintf("PPE Power Curve%s",ifelse(nrow(model_pairs)>1,"s","")))
  
  print(p)
}


if(diagnostics){
  
  diag_curves <- ddply(est_results, .(name), function(dofv_df){
    # estimate ncp parameter
    #ppe_results <- ppe(dofv_df$dofv, dofv_df$df[1])
    #ncp_ci <- param_boot_ppe(ppe_results$ncp, length(dofv_df$dofv), df=dofv_df$df[1], sum.fun.args = list(probs=c(0.025,0.975)))
    grid <- seq(0, dofv_df$max_dofv[1], length=100)
    fun <- switch(dofv_df$parameter,
                  ncp = function(est) pchisq(grid, df=dofv_df$df[1], ncp=est),
                  df = function(est) pchisq(grid, df=est, ncp=0))
    cum_dist <- sapply(c(dofv_df$estimate, dofv_df$ci_lower, dofv_df$ci_upper), fun)
    data.frame(quantile=grid, prob=cum_dist[,1], prob_low=cum_dist[,3], prob_high=cum_dist[,2], parameter=dofv_df$parameter)
  })
  
  d_ply(diag_curves,.(name),function(diag_df){
    dofvs_df <- subset(dofvs_df, name==diag_df$name[1])
    if(diag_df$parameter[1]=="df") dofvs_df <- transform(dofvs_df, dofv=-dofv)
    p <- ggplot()+
      geom_ribbon(data=diag_df, mapping=aes(x=quantile,  ymin=prob_low, ymax=prob_high),fill="lightgray")+
      geom_line(data=diag_df, mapping=aes(x=quantile,  y=prob),linetype="dashed", size=1)+
      stat_ecdf(data=dofvs_df, aes(dofv), geom="step", colour="darkred", size=1)+
      scale_x_continuous("Quantile")+
      scale_y_continuous("Probability", labels=function(breaks) paste0(breaks*100,"%"))+
      ggtitle(sprintf("Diagnostic: %s",diag_df$name[1]))
    
    print(p)
  })
  
  
}

dev.off()