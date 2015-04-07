
library(ggplot2)
library(plyr)


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
	bsamples <- replicate(n.boot, ppe(rchisq(nmc_samples, df, ncp),df)[[value]])
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
	#matching based on filename
	comb_index <- which(reduced_matches, arr.ind = T)
	
}else{
	#matching based on order
	comb_index <- matrix(seq_len(length(model.file.names)%/%2*2), ncol=2, byrow = T,dimnames = list(NULL, c("col","row")))
}

model_pairs <- data.frame(full=model.rawres.names[comb_index[,"col"]],
													full_mod=model_names[comb_index[,"col"]],
													red=model.rawres.names[comb_index[,"row"]],
													red_mod=model_names[comb_index[,"row"]],
													subjects=model.subjects[comb_index[,"col"]],
													df=model.estimated.params[comb_index[,"col"]]-model.estimated.params[comb_index[,"row"]])

if(any(model_pairs$df<1)){
	d_ply(subset(model_pairs, df<1), .(), function(df) 
		warning(sprintf("Hypothesis test with H0:\"%s\" and H1:\"%s\" has less than 1 DF and is skipped!\n", df$full_mod, df$red_mod), call. = F))
	model_pairs <- subset(model_pairs, df>=1)
}

if(nrow(model_pairs)==0) stop("No valid model pair provided!")

raw_results <- read.csv(raw.results.file)

# calculate delta ofvs 
dofvs_df <- ddply(model_pairs, names(model_pairs), function(model_pair){
	ofv_full <- subset(raw_results, hypothesis==model_pair[1,"full"], select=c(sample, ofv))
	ofv_reduced <- subset(raw_results, hypothesis==model_pair[1,"red"], select=c(sample, ofv))
	names(ofv_full) <- c("SIM","FULL")
	names(ofv_reduced) <- c("SIM", "RED")
	combined <- merge(ofv_full, ofv_reduced, all.x=T)
	delta_ofvs <- with(combined, RED-FULL)
	if(remove_na_dofvs) delta_ofvs <- delta_ofvs[!is.na(delta_ofvs)]
	data.frame(dofv=delta_ofvs, 
						 name=sprintf("%s vs. %s (%i DF)", model_pair[1,"full_mod"], model_pair[1,"red_mod"],model_pair[1,"df"]))
})

# determine ncp parameter and find the maximum number of subjects 
npc_subj_max <- ddply(dofvs_df, .(name), function(dofv_df){
	# estimate ncp parameter
	ppe_results <- ppe(dofv_df$dofv, dofv_df$df[1])
	# determine range for sample size curves
	max_subjects <- uniroot(function(n_subjects) ppe_subjects(ncp=ppe_results$ncp, df=dofv_df$df[1], 
																														n.subjects=dofv_df$subjects[1],
																														pred.n.subjects=n_subjects)-max_power,
													lower=0, upper=2*max_power/ppe_results$power*dofv_df$subjects[1], extendInt="upX", tol=0.01)$root
	data.frame(ncp=ppe_results$ncp, subjects_ref=dofv_df$subjects[1], max_subjects=max_subjects, 
						 nsamples=length(dofv_df$dofv), df=dofv_df$df[1])
})

# calculate power curves
power_curves <- ddply(npc_subj_max, .(name), function(df){
	subjects <- seq(0, ceiling(max(npc_subj_max$max_subjects)))
	# determine uncertainty via bootstrap
	ncp_ci <- param_boot_ppe(df$ncp[1], df$nsamples[1], df=df$df[1])
	power <- sapply(c(df$ncp[1],ncp_ci), function(ncp) ppe_subjects(ncp=ncp, df=df$df[1], 
																																	n.subjects=df$subjects_ref[1],
																																	pred.n.subjects=subjects))
	
	data.frame(subjects = subjects, power=power[,1], power_lower=power[,2], power_upper=power[,3])
})

pdf(pdf.filename)
p <- ggplot(power_curves, aes(subjects, power, colour=name, fill=name))+
	geom_ribbon(aes(ymin=power_lower, ymax=power_upper), alpha=0.3, colour=NA)+
	geom_line(size=1)+
	geom_hline(yintercept=.8, linetype="dashed")+
	scale_x_continuous("Study size")+
	scale_y_continuous("Power", labels=function(breaks) paste0(breaks*100,"%"), breaks=seq(0,1, by=.2))+
	theme(legend.position=c(1,0), legend.justification=c(1,0), legend.title=element_blank())+
	ggtitle(sprintf("PPE Power Curve%s",ifelse(nrow(model_pairs)>1,"s","")))

print(p)


if(diagnostics){
	
	diag_curves <- ddply(dofvs_df, .(name), function(dofv_df){
		# estimate ncp parameter
		ppe_results <- ppe(dofv_df$dofv, dofv_df$df[1])
		ncp_ci <- param_boot_ppe(ppe_results$ncp, length(dofv_df$dofv), df=dofv_df$df[1], sum.fun.args = list(probs=c(0.025,0.975)))
		grid <- seq(0, max(dofv_df$dofv), length=100)
		cum_dist <- sapply(c(ppe_results$ncp, ncp_ci), function(ncp) pchisq(grid, df=dofv_df$df[1], ncp=ncp))
		data.frame(quantile=grid, prob=cum_dist[,1], prob_low=cum_dist[,3], prob_high=cum_dist[,2])
	})
	
	d_ply(diag_curves,.(name),function(diag_df){
		p <- ggplot()+
			geom_ribbon(data=diag_df, mapping=aes(x=quantile,  ymin=prob_low, ymax=prob_high),fill="lightgray")+
			geom_line(data=diag_df, mapping=aes(x=quantile,  y=prob),linestyle="dashed", size=1)+
			stat_ecdf(data=subset(dofvs_df, name==diag_df$name[1]), aes(dofv), geom="step", colour="darkred", size=1)+
			scale_x_continuous("Quantile")+
			scale_y_continuous("Probability", labels=function(breaks) paste0(breaks*100,"%"))+
			ggtitle(sprintf("PPE Diagnostic: %s",diag_df$name[1]))
		
		print(p)
	})
	
	
}

dev.off()