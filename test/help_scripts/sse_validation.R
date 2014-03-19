#Kajsa 2014-03-19
#used for generating true answer numbers for sse unit tests
# easy to modify for new test cases

#clear everything
rm(list=ls())
options(digits=7) #same as tool.pm print_results
initials <- matrix()
rawres <- data.frame()
if (TRUE){
  #simulation initial values for mox  
  #theta=27.5,13,0.2,.1,.23
  #omega=.3,.1,.3,.3,.3,.3
  #sigma=1
  moxinitials <- c(27.5,13,0.2,0.1,0.23,0.3,0.1,0.3,0.3,0.3,0.3,1)
    initials<-matrix(rep(moxinitials,times=5),nrow=5,byrow=T)
    rawres <- read.csv(file='raw_results_moxonidine.csv',as.is=T)
}else{
    #simulation initial values for pheno
    initials <- read.csv(file='rawres_for_sse.csv',as.is=T)
    initials <- as.matrix(initials[2:6,5:9])
    rawres <- read.csv(file='raw_results_pheno.csv',as.is=T)
}
oind <- which(names(rawres)== 'ofv')
sind <- which(names(rawres)== 'SIGMA.1.1.')

rawres1 <- rawres[1:5,oind:sind]
rawres2 <- rawres[6:10,oind:sind]

params1 <- as.matrix(rawres[1:5,(oind+1):sind])
params2 <- as.matrix(rawres[6:10,(oind+1):sind])

mean1 <- apply(X=rawres1, MARGIN=2, FUN=mean)
median1 <- apply(X=rawres1, MARGIN=2, FUN=median)
sd1 <- apply(X=rawres1, MARGIN=2, FUN=sd)
max1 <- apply(X=rawres1, MARGIN=2, FUN=max)
min1 <- apply(X=rawres1, MARGIN=2, FUN=min)

mean2 <- apply(X=rawres2, MARGIN=2, FUN=mean)
median2 <- apply(X=rawres2, MARGIN=2, FUN=median)
sd2 <- apply(X=rawres2, MARGIN=2, FUN=sd)
max2 <- apply(X=rawres2, MARGIN=2, FUN=max)
min2 <- apply(X=rawres2, MARGIN=2, FUN=min)

#rmse
squarederrs1 <- ((params1 - initials)**2)
squarederrs2 <- ((params2 - initials)**2)
squaredrelerr1 <- (((params1 - initials) / initials)**2) #element-wise
squaredrelerr2 <- (((params2 - initials) / initials)**2) #element-wise
rmse1 <- sqrt(apply(X=squarederrs1,MARGIN=2, FUN=mean))
rmse2 <- sqrt(apply(X=squarederrs2,MARGIN=2, FUN=mean))
relrmse1 <- 100*sqrt(apply(X=squaredrelerr1,MARGIN=2, FUN=mean))
relrmse2 <- 100*sqrt(apply(X=squaredrelerr2,MARGIN=2, FUN=mean))

#bias
bias1 <- (apply(X=(params1 - initials),MARGIN=2, FUN=mean))
bias2 <- (apply(X=(params2 - initials),MARGIN=2, FUN=mean))
relbias1 <- 100*(apply(X=((params1 - initials)/initials),MARGIN=2, FUN=mean))
relbias2 <- 100*(apply(X=((params2 - initials)/initials),MARGIN=2, FUN=mean))
relabsbias1 <- 100*(apply(X=((params1 - initials)/(abs(initials))),MARGIN=2, FUN=mean))
relabsbias2 <- 100*(apply(X=((params2 - initials)/(abs(initials))),MARGIN=2, FUN=mean))


#skewness
meanmat1<-as.matrix(rawres1)
for (S in 1:5){
  meanmat1[S,] <- t(as.matrix(mean1))
}
meanmat2<-as.matrix(rawres1)
for (S in 1:5){
  meanmat2[S,] <- t(as.matrix(mean2))
}

skew1 <- (5/12)*(apply(X=((rawres1 - meanmat1)**3),MARGIN=2,FUN=sum))/((apply(X=rawres1, MARGIN=2, FUN=sd))**3)
skew2 <- (5/12)*(apply(X=((rawres2 - meanmat2)**3),MARGIN=2,FUN=sum))/((apply(X=rawres2, MARGIN=2, FUN=sd))**3)
#kurtosis
kurt1 <- (30/24)*(apply(X=((rawres1 - meanmat1)**4),MARGIN=2,FUN=sum))/((apply(X=rawres1, MARGIN=2, FUN=sd))**4)-(3*16)/6
kurt2 <- (30/24)*(apply(X=((rawres2 - meanmat2)**4),MARGIN=2,FUN=sum))/((apply(X=rawres2, MARGIN=2, FUN=sd))**4)-(3*16)/6

stats1a <- data.frame(mean1,median1,sd1,max1,min1,skew1,kurt1)
apply(X=stats1a,MARGIN=2,FUN=function(x){cat('[');cat(x,sep=',');cat("];\n")})
stats1b <- data.frame(rmse1,relrmse1,bias1,relbias1,relabsbias1)
apply(X=stats1b,MARGIN=2,FUN=function(x){cat('[');cat(x,sep=',');cat("];\n")})

stats2a <- data.frame(mean2,median2,sd2,max2,min2,skew2,kurt2)
apply(X=stats2a,MARGIN=2,FUN=function(x){cat('[');cat(x,sep=',');cat("];\n")})
stats2b <- data.frame(rmse2,relrmse2,bias2,relbias2,relabsbias2)
apply(X=stats2b,MARGIN=2,FUN=function(x){cat('[');cat(x,sep=',');cat("];\n")})




