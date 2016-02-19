# randtest plot
pvalue <- 0.05 #for randtest.hist

if (have.base.model){

library(grid)
library(xpose4) 
pdf(file=pdf.filename,title=pdf.title)

#does not work for undef deltaofv
#xpose4 built-in, need library(xpose4)
#print(randtest.hist(raw.results.file,df=length(extra.thetas),p.val=pvalue)) 

#if (rplots.level > 1){
# required packages
library(ggplot2)
library(reshape2)
library(gridExtra)
library(scales)
library(MASS)
library(plotrix)
library(plyr)


# Check if runs with unsuccessful minimization will be ignored
ignore.usm <- F


modcol <- extra.thetas
modnm  <- extra.thetas

# Height of the png files
hth <- 4      # Optimized for 3 panels
# Width of the png files
wth <- 7      # Optimized for 3 panels


# ggplot set up
ggOpt <- list(xlab("\nValue"),ylab("Count\n"),
theme(plot.title = element_text(size=9, face="bold"),
plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
panel.background = element_rect(fill="grey95"),
axis.title.x = element_text(size=9,face="bold"),
axis.title.y = element_text(size=9,face="bold"),
axis.text.x  = element_text(size=8,face="bold",color="black",angle=45,vjust=1,hjust=1),
axis.text.y  = element_text(size=8,face="bold",color="black",hjust=0.5),
strip.text.x  = element_text(size=9,face="bold",color="black",hjust=0.5),
legend.position = "bottom", legend.direction = "horizontal",
legend.key.size=unit(0.5,"cm"),
legend.text = element_text(size=8,face="bold"),
legend.title = element_blank()))

## KS test
KStestOFV <- function(objdf=NULL,dfRange=seq(0.0001,5,0.01),flNm=NULL,ignore.usm=F,alpha=0.95){
if (is.null(flNm)) flNm <- match.call(expand.dots=F)[[2]]
if (is.null(objdf)){stop("No input file was provided")}

# Check if only successful minimization runs will be included
if (ignore.usm) objdf <- subset(objdf, minimization_successful==1)

objdf$deltaofv <- objdf$ofv-objdf[objdf$model=="base","ofv"]

truedelofv <- objdf[objdf$model=="input","deltaofv"]
objdf <- na.omit(subset(objdf, select=c(deltaofv), model!="base" & model!="input"))
names(objdf) <- "deltaOFV"

# Create a new column for modified deltaOFV to convert positive deltaOFV (if any) to zero
objdf$mod.deltaOFV <- objdf$deltaOFV
# Store how many positive deltaOFV was found in the data set
nposdofv <- nrow(subset(objdf, deltaOFV>0))
if (nposdofv>0) objdf[objdf$mod.deltaOFV>0,]$mod.deltaOFV <- 0

deltaOFV    <- data.frame(deltaOFV = -1*objdf[objdf$deltaOFV<=0,]$deltaOFV)
mod.deltaOFV <- data.frame(mod.deltaOFV = -1*objdf$mod.deltaOFV)
ndata     <- nrow(deltaOFV)
nmoddata  <- nrow(mod.deltaOFV)
dof       <- dfRange

# Create CDF and perform K-S test to determine the correct DoF fro deltaOFV
for (i in 1:length(dof)){
if (i == 1){
set.seed(657652)
chidf  <- rchisq(ndata,df=dof[i])
cmdf   <- summarize(deltaOFV, value=unique(sort(deltaOFV)), "abs(deltaOFV)"=ecdf(deltaOFV)(value), CHISQ=ecdf(chidf)(value))
ksDist <- ks.test(cmdf[,2], cmdf$CHISQ)[[1]]
pval   <- ks.test(cmdf[,2], cmdf$CHISQ)[[2]]
df     <- dof[i]
}else{
set.seed(657652)
chidf   <- rchisq(ndata,df=dof[i])
tcmdf   <- summarize(deltaOFV, value=unique(sort(deltaOFV)), "abs(deltaOFV)"=ecdf(deltaOFV)(value), CHISQ=ecdf(chidf)(value))
tksDist <- ks.test(tcmdf[,2], tcmdf$CHISQ)[[1]]
tpval   <- ks.test(tcmdf[,2], tcmdf$CHISQ)[[2]]
if (tksDist < ksDist){
cmdf <- tcmdf; ksDist <- tksDist; pval <- tpval; df <- dof[i]
}
}
}

# Create CDF and perform K-S test to determine the correct DoF fro mod.deltaOFV
for (i in 1:length(dof)){
if (i == 1){
set.seed(657652)
modchidf  <- rchisq(nmoddata,df=dof[i])
modcmdf   <- summarize(mod.deltaOFV, value=unique(sort(mod.deltaOFV)), "abs(mod.deltaOFV)"=ecdf(mod.deltaOFV)(value), CHISQ=ecdf(modchidf)(value))
modksDist <- ks.test(modcmdf[,2], modcmdf$CHISQ)[[1]]
modpval   <- ks.test(modcmdf[,2], modcmdf$CHISQ)[[2]]
moddf     <- dof[i]
}else{
set.seed(657652)
modchidf <- rchisq(nmoddata,df=dof[i])
tcmdf    <- summarize(mod.deltaOFV, value=unique(sort(mod.deltaOFV)), "abs(mod.deltaOFV)"=ecdf(mod.deltaOFV)(value), CHISQ=ecdf(modchidf)(value))
tksDist  <- ks.test(tcmdf[,2], tcmdf$CHISQ)[[1]]
tpval    <- ks.test(tcmdf[,2], tcmdf$CHISQ)[[2]]
if (tksDist < modksDist){
modcmdf <- tcmdf; modksDist <- tksDist; modpval <- tpval; moddf <- dof[i]
}
}
}

# Plot deltaOFV comparison
df       <- signif(df,3); pval <- signif(pval,2); ksDist <- signif(ksDist,2); ksCrit <- signif(1.358/sqrt(ndata), 2)
result   <- ifelse(ksDist > ksCrit, "Fail", "Pass")
dKSscore <- ksDist - ksCrit
dofvqn   <- unname(quantile(deltaOFV$deltaOFV, alpha))
set.seed(657652)
chidf    <- rchisq(ndata, df)
chsqn    <- unname(quantile(chidf, alpha))
vdf      <- data.frame(CHISQcritical=chsqn,deltaOFVcritical=dofvqn,deltaOFVtrue=-1*truedelofv)
vdf      <- melt(vdf); names(vdf) <- c("quant","val")

cmdf  <- melt(cmdf,id=c("value"),c("abs(deltaOFV)","CHISQ"),value.name="CDF")
gcdf  <- ggplot(cmdf, aes(value,CDF,color=variable)) +
xlab("\nabs(deltaOFV), positive excluded") + ylab("CDF\n") +
theme(plot.title = element_text(size=8, face="bold"),
axis.title.x = element_text(size=8,face="bold"),
axis.title.y = element_text(size=8,face="bold"),
axis.text.x  = element_text(size=8,face="bold",color="black",angle=90),
axis.text.y  = element_text(size=8,face="bold",color="black",hjust=0),
legend.text  = element_text(size=6,face="bold"),
legend.position = "bottom",
legend.key.size = unit(0.5,"cm"),
legend.title = element_blank()) +
geom_vline(data=vdf,aes(xintercept=val, color=factor(quant), lty=factor(quant)), size=1) +
geom_point(size=1) + guides(col = guide_legend(nrow = 2))

# Plot mod.deltaOFV comparison
moddf        <- signif(moddf,3); modpval <- signif(modpval,2); modksDist <- signif(modksDist,2); modksCrit <- signif(1.358/sqrt(nmoddata), 2)
modresult    <- ifelse(modksDist > modksCrit, "Fail", "Pass")
moddKSscore  <- modksDist - modksCrit
moddofvqn    <- unname(quantile(mod.deltaOFV$mod.deltaOFV, alpha))
set.seed(657652)
modchidf     <- rchisq(nmoddata, moddf)
modchsqn     <- unname(quantile(modchidf, alpha))
modvdf       <- data.frame(CHISQcritical=modchsqn,mod.deltaOFVcritical=moddofvqn,deltaOFVtrue=-1*truedelofv)
modvdf       <- melt(modvdf); names(modvdf) <- c("quant","val")

modcmdf  <- melt(modcmdf,id=c("value"),c("abs(mod.deltaOFV)","CHISQ"),value.name="CDF")
modgcdf  <- ggplot(modcmdf, aes(value,CDF,color=variable)) +
xlab("\nabs(deltaOFV), positive set to 0") + ylab("CDF\n") +
theme(plot.title = element_text(size=8, face="bold"),
axis.title.x = element_text(size=8,face="bold"),
axis.title.y = element_text(size=8,face="bold"),
axis.text.x  = element_text(size=8,face="bold",color="black",angle=90),
axis.text.y  = element_text(size=8,face="bold",color="black",hjust=0),
legend.text  = element_text(size=6,face="bold"),
legend.position = "bottom",
legend.key.size = unit(0.5,"cm"),
legend.title = element_blank()) +
geom_vline(data=modvdf,aes(xintercept=val, color=factor(quant), lty=factor(quant)), size=1) +
geom_point(size=1) + guides(col = guide_legend(nrow = 2))

# Compare PDF of deltaOFV with estimated chi-sq obtained from K-S test
bin   <- hist(deltaOFV$deltaOFV, plot=F)$breaks
dofv  <- hist(deltaOFV$deltaOFV, plot=F)$density/sum(hist(deltaOFV$deltaOFV, plot=F)$density)
imdf  <- data.frame(bin=numeric(0),deltaOFV=numeric(0))
for (i in 1:length(dofv)){imdf <- rbind(imdf, data.frame(bin=mean(bin[i:(i+1)]),deltaOFV=dofv[i]))}
set.seed(657652); dchidf <- dchisq(imdf$bin,df)
pmdf  <- cbind(imdf, CHISQ=dchidf)
colnames(pmdf)[2] <- "abs(deltaOFV)"
pmdf  <- melt(pmdf,id=c("bin"),c("abs(deltaOFV)","CHISQ"),value.name="PDF")

gpdf <- ggplot(pmdf, aes(bin,PDF,color=variable)) +
geom_smooth(method="loess",se=F,size=1) +
xlab("\nabs(deltaOFV), positive excluded") + ylab("PDF\n") +
theme(plot.title = element_text(size=8, face="bold"),
axis.title.x = element_text(size=8,face="bold"),
axis.title.y = element_text(size=8,face="bold"),
axis.text.x  = element_text(size=8,face="bold",color="black",angle=90),
axis.text.y  = element_text(size=8,face="bold",color="black",hjust=0),
legend.text  = element_text(size=6,face="bold"),
legend.position = "bottom",
legend.key.size = unit(0.5,"cm"),
legend.title = element_blank()) +
geom_vline(xintercept=chsqn, color="skyblue", size=1, lty=2) +
geom_vline(xintercept=dofvqn, color="red", size=1, lty=2) + guides(col = guide_legend(nrow = 2))

# Compare PDF of mod.deltaOFV with estimated chi-sq obtained from K-S test
bin   <- hist(mod.deltaOFV$mod.deltaOFV, plot=F)$breaks
dofv  <- hist(mod.deltaOFV$mod.deltaOFV, plot=F)$density/sum(hist(mod.deltaOFV$mod.deltaOFV, plot=F)$density)
imdf  <- data.frame(bin=numeric(0),mod.deltaOFV=numeric(0))
for (i in 1:length(dofv)){imdf <- rbind(imdf, data.frame(bin=mean(bin[i:(i+1)]),mod.deltaOFV=dofv[i]))}
set.seed(657652); dchidf <- dchisq(imdf$bin,df)
pmdf  <- cbind(imdf, CHISQ=dchidf)
colnames(pmdf)[2] <- "abs(mod.deltaOFV)"
pmdf  <- melt(pmdf,id=c("bin"),c("abs(mod.deltaOFV)","CHISQ"),value.name="PDF")

modgpdf <- ggplot(pmdf, aes(bin,PDF,color=variable)) +
geom_smooth(method="loess",se=F,size=1) +
xlab("\nabs(deltaOFV), positive set to 0") + ylab("PDF\n") +
theme(plot.title = element_text(size=8, face="bold"),
axis.title.x = element_text(size=8,face="bold"),
axis.title.y = element_text(size=8,face="bold"),
axis.text.x  = element_text(size=8,face="bold",color="black",angle=90),
axis.text.y  = element_text(size=8,face="bold",color="black",hjust=0),
legend.text  = element_text(size=6,face="bold"),
legend.position = "bottom",
legend.key.size = unit(0.5,"cm"),
legend.title = element_blank()) +
geom_vline(xintercept=chsqn, color="skyblue", size=1, lty=2) +
geom_vline(xintercept=dofvqn, color="red", size=1, lty=2) + guides(col = guide_legend(nrow = 2))


myGrob <- textGrob(bquote(atop(,"df"[chi^2]~"="~.(df)~", D"[KS]~"="~.(ksDist)~", D"[critical]~"="~.(ksCrit)~", pvalue="~.(pval))),
gp=gpar(cex=0.7,fontface="bold"))

return(list(KSresult=result,ndata=ndata,KSscore=ksDist,KScritical=ksCrit,dKSscore=dKSscore,
df=df,EMPcritical=dofvqn,CHIcritical=chsqn,nposdofv=nposdofv,modKSresult=modresult,
nmoddata=nmoddata,modKSscore=modksDist,modKScritical=modksCrit,moddKSscore=moddKSscore,
moddf=moddf,modEMPcritical=moddofvqn,modCHIcritical=modchsqn,cutoff=alpha,
gcdf=gcdf,modgcdf=modgcdf,gpdf=gpdf,modgpdf=modgpdf,myGrob=myGrob))
}

sel <- c("model","ofv","deltaofv",modcol)
pnm <- c("model","OFV","deltaOFV",modnm)
mod <- paste0(mod.prefix,xpose.runno)
tabout <- data.frame()





# Read randtest raw result file
odf <- read.csv(raw.results.file)
odf <- subset(odf,!is.na(deltaofv))
# Check if only successful minimization runs will be included
if (ignore.usm){
 df <- subset(odf, minimization_successful==1)
}else{
df <- odf
}

# Select the useful columns and rename
df <- subset(df, select=sel)
names(df) <- pnm

# Calculate deltaOFV values
df$deltaOFV <- df$OFV-df[df$model=="base","OFV"]

# Create a new column for modified deltaOFV to convert positive deltaOFV (if any) to zero
df$mod.deltaOFV <- df$deltaOFV

# Store how many positive deltaOFV was found in the data set
nposdofv <- nrow(subset(df, deltaOFV>0 & model!="input"))
if (nposdofv>0) df[df$mod.deltaOFV>0 & df$model!="input","mod.deltaOFV"] <- 0

# Create a new data frame for vertical lines to show the reduced and full model values
tmp <- subset(df, model=="base" | model=="input")
tmp$model <- factor(tmp$model, levels=c("base","input"), labels=c("reduced model","full model"))
vdf <- melt(tmp, id.vars = c("model","OFV"))

# Melted data frame for plotting purpose
mdf <- subset(melt(na.omit(df[-c(1:2),]), id.vars = c("model","OFV")), abs(value)<10^5)

# Re-order the factor levels
vdf$variable <- factor(vdf$variable, levels=c("deltaOFV","mod.deltaOFV",modnm), labels=c("deltaOFV, positive excluded","deltaOFV, positive set to 0",modnm))
mdf$variable <- factor(mdf$variable, levels=c("deltaOFV","mod.deltaOFV",modnm), labels=c("deltaOFV, positive excluded","deltaOFV, positive set to 0",modnm))





# Plot the distributions using ggplot
#
p0 <- ggplot(mdf,aes(value))+geom_histogram(fill="white",color="black")+ggOpt+
geom_vline(data=vdf, aes(xintercept=value, color=factor(model)), show_guide=T, size=1)+
facet_wrap(~variable,scales="free",ncol=2)+
ggtitle(paste("Distribution of deltaOFV and model parameter\n",mod,"\n",
paste("No. of positive deltaOFV is ",nposdofv," out of ",nrow(na.omit(df[-c(1:2),])),sep=""),"\n",sep=""))

ksrecord <- KStestOFV(objdf = odf, flNm = mod)
print(p0)
grid.arrange(ksrecord$gcdf,ksrecord$modgcdf,ksrecord$gpdf,ksrecord$modgpdf,ncol=2,
main=ksrecord$myGrob)





tabout <- rbind(tabout, data.frame(Model=mod,KSresult=ksrecord$KSresult,ndata=ksrecord$ndata,
KSscore=ksrecord$KSscore,KScritical=ksrecord$KScritical,dKSscore=ksrecord$dKSscore,df=ksrecord$df,
EMPcritical=ksrecord$EMPcritical,CHIcritical=ksrecord$CHIcritical,nposdofv=ksrecord$nposdofv,
modKSresult=ksrecord$modKSresult,nmoddata=ksrecord$nmoddata,modKSscore=ksrecord$modKSscore,
modKScritical=ksrecord$modKScritical,moddKSscore=ksrecord$moddKSscore,moddf=ksrecord$moddf,
modEMPcritical=ksrecord$modEMPcritical,modCHIcritical=ksrecord$modCHIcritical,cutoff=ksrecord$cutoff))


write.table(tabout, file="randtestTable.tsv", sep="\t", row.names=F, col.names=T, quote=F)
#} end if level>1
	dev.off()
}



