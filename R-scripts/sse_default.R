library(PsNR)
library(magrittr)
library(methods)
library(ggplot2)
library(plyr)

#add R_info to the meta file
R_info(directory=working.directory)
meta <- PsNR::metadata(working.directory)

theme_set(theme_bw(12))						# set standard theme
remove_na_dofvs <- TRUE 					# should dofvs with NA be removed? 
max_power <- 0.99 								# until which value should the power curve be drawn
alpha <- 0.05											# type-I error rate 
diagnostics <- PsNR::rplots_level(meta) > 1 		# should diagnostics be created
# end of user options
options(stringsAsFactors = FALSE)

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

# the script performs type I error assessment if the simulation model has the suffix _base/_reduced/_red/_r
type_1 <- grepl("_(base|r|red|reduced)", basename(PsNR::model_path(meta)))

# default for matching during a power assessment is to take the first estimated model as the full and all 
# other models as alternatives for a reduced model
# during a type 1 error assessment the first estimated model is taken to be the reduced model and all
# other models as alternatives for the full model
pair_indicies <- data.frame(full = 1, red = 2:length(model.file.names))
if(type_1) colnames(pair_indicies) <- rev(colnames(pair_indicies))
# this pairing by position can be overwritten by adhering to the following naming convention 
# full model: name_full,name_f,name_est,name reduced model: name_reduced, name_red, name_r, name_base
model_names <- tools::file_path_sans_ext(model.file.names)
reduced_pattern <- ifelse(grepl("_(f|full|est)$", model_names), 
                          gsub("_(f|full|est)$","_(r|red|reduced|base)\\\\d*",model_names),
                          paste0(model_names, "_(r|red|reduced|base)\\d*")) 			 
reduced_matches <- sapply(reduced_pattern, grepl, x=model_names, USE.NAMES = F)

if(any(reduced_matches)){
  # matching based on filename
  matched_pair_indicies <- which(reduced_matches, arr.ind = T)
  colnames(matched_pair_indicies) <- c("red", "full")
  pair_indicies <- subset(pair_indicies, !(red %in% matched_pair_indicies | full %in% matched_pair_indicies))
  pair_indicies <- rbind(pair_indicies, matched_pair_indicies)
}

model_pairs <- data.frame(full=model.rawres.names[pair_indicies[,"full"]],
                          full_mod=model_names[pair_indicies[,"full"]],
                          red=model.rawres.names[pair_indicies[,"red"]],
                          red_mod=model_names[pair_indicies[,"red"]],
                          subjects=model.subjects[pair_indicies[,"full"]],
                          df=model.estimated.params[pair_indicies[,"full"]]-model.estimated.params[pair_indicies[,"red"]])

if(nrow(model_pairs)==0) stop("No valid model pair provided!")

raw_results <- read.csv(raw.results.file)


dofvs_df <- ddply(model_pairs, names(model_pairs), function(model_pair){
  ofv_full <- subset(raw_results, hypothesis==model_pair[1,"full"], select=c(sample, ofv))
  ofv_reduced <- subset(raw_results, hypothesis==model_pair[1,"red"], select=c(sample, ofv))
  names(ofv_full) <- c("SIM","FULL")
  names(ofv_reduced) <- c("SIM", "RED")
  combined <- merge(ofv_full, ofv_reduced, all.x=T)
  delta_ofvs <- with(combined, RED-FULL)
  if(remove_na_dofvs) delta_ofvs <- delta_ofvs[!is.na(delta_ofvs)]
  data.frame(dofv=delta_ofvs, 
             name=sprintf("%s vs. %s (%i DF)", model_pair[1,"full_mod"], model_pair[1,"red_mod"],model_pair[1,"df"]),
             type_1 = type_1)
})


est_results <- plyr::ddply(dofvs_df, .(name), function(dofv_df){
  n_negative <- sum(dofv_df$dofv<0) 
  if(dofv_df$type_1[1]){
    parameter <- "df"
    est_results <- df_est(dofv_df$dofv)
    estimate <- est_results[[parameter]]
    prob <- est_results$type_1
    bootstrap_ci <- param_boot_ppe(ncp = 0, 
                                   nmc_samples = nrow(dofv_df)-n_negative, 
                                   df = estimate, value = parameter)  
    max_subjects <- NA
  }else{
    parameter <- "ncp"
    est_results <- ppe(dofv_df$dofv)
    estimate <- est_results[[parameter]]
    bootstrap_ci <- param_boot_ppe(ncp = estimate, 
                                   nmc_samples = nrow(dofv_df)-n_negative, 
                                   df = dofv_df$df[1], value = parameter)
    prob <- est_results$power
    # determine range for sample size curves
    max_subjects <- uniroot(function(n_subjects) 
      ppe_subjects(ncp=estimate, df=dofv_df$df[1], 
                   n.subjects=dofv_df$subjects[1], 
                   pred.n.subjects=n_subjects)-max_power,
      lower=0, upper=2*max_power/prob*dofv_df$subjects[1], extendInt="upX", tol=0.01)$root
  }
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
  if(type_1){
    data.frame(Comparison=df$name, 
               `#dOFV` = df$samples,
               `#(dOFV<0)` = df$n_negative,
               `df [CI]` = sprintf("%.1f [%.1f-%.1f]", df$estimate, df$ci_lower, df$ci_upper),
               `Type-I` = round(df$prob*100,1),
               check.names = F)    
  }else{
    data.frame(Comparison=df$name, 
               `#dOFV` = df$samples,
               `#(dOFV<0)` = df$n_negative,
               Parameter=df$parameter,
               `ncp [CI]` = sprintf("%.1f [%.1f-%.1f]", df$estimate, df$ci_lower, df$ci_upper),
               `Power` = round(df$prob*100,1),
               check.names = F)
  }
)
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
    grid <- seq(0, dofv_df$max_dofv[1], length=100)
    fun <- switch(dofv_df$parameter,
                  ncp = function(est) pchisq(grid, df=dofv_df$df[1], ncp=est),
                  df = function(est) pchisq(grid, df=est, ncp=0))
    cum_dist <- sapply(c(dofv_df$estimate, dofv_df$ci_lower, dofv_df$ci_upper), fun)
    data.frame(quantile=grid, prob=cum_dist[,1], prob_low=cum_dist[,3], prob_high=cum_dist[,2], parameter=dofv_df$parameter)
  })
  
  d_ply(diag_curves,.(name),function(diag_df){
    dofvs_df <- subset(dofvs_df, name==diag_df$name[1])
    #if(diag_df$parameter[1]=="df") dofvs_df <- transform(dofvs_df, dofv=-dofv)
    p <- ggplot()+
      geom_ribbon(data=diag_df, mapping=aes(x=quantile,  ymin=prob_low, ymax=prob_high),fill="lightgray")+
      geom_line(data=diag_df, mapping=aes(x=quantile,  y=prob),linetype="dashed", size=1)+
      stat_ecdf(data=dofvs_df, aes(dofv), geom="step", colour="darkred", size=1)+
      scale_x_continuous("dOFV")+
      scale_y_continuous("Cumulative Probability", labels=function(breaks) paste0(breaks*100,"%"))+
      ggtitle(sprintf("Diagnostic: %s",diag_df$name[1]))
    
    print(p)
  })
  
  
}

dev.off()
