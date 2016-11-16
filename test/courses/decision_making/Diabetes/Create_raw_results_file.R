##..............................................................................
## Created by Steve Choy
## Oct 16, 2013
##.............................................................................
## Last modified: 2013.10.10
## Last changes: 
##.............................................................................
## Create raw results file from WHIG model with n=500 
##.............................................................................
setwd("C:/Users/Steve Choy/Dropbox/Steve Choy Janssen Diabetes/Documents/Decision making course")

test <- read.csv("raw_results_run360_clean.csv",head=T,sep=",")

# take first row of an example raw results file
test4 <- test[1,] 

# add in drug effect and rearranging rows
test4[,"TH20.DRUG"] <- 1.25 
test4 <- test4[,c(1:39,306,40:305)]
test4$EI91 <- NA
test4$seTH20.DRUG <- 0
test4 <- test4[,c(1:130,308,131:307)]

# repeat rows of model 0
test5 <- test4[rep(1:nrow(test4),each=501),] 

# renumber models to go from 0 to 500
test5[,1] <- 0:500

# dataset for increased D&E, 2* THETA for TH12 TH18 (2*EFB is done in run52.mod)
test52 <- test5
test52$"TH12.EFDE.PLAC" <- test52$"TH12.EFDE.PLAC" * 2
test52$"TH18.EFPL.PLAC" <- test52$"TH18.EFPL.PLAC" * 2

pseudoRaw <- function(dataset,start,end,rows=500) {
  df <- dataset
  i <- start
  rows2 <- rows + 1
  while(i <= end) {
    name1 <- colnames(df[i])
    name2 <- paste("se",name1,sep="")
    df[c(2:rows2),name1] <- rnorm(rows,df[1,name1],df[1,name2])
    print(paste("Randomised ",name1,sep=""))
    i <- i + 1
  }
  return(df)
}

names(test5)
test6 <- pseudoRaw(test5,21,110)  # raw_results for run51
test52.ed <- pseudoRaw(test52,21,110) # raw_results for D&E++

test6 <- pseudoRaw(test5,21,40)  # raw_results for run51
test52.ed <- pseudoRaw(test52,21,40) # raw_results for D&E++

head(test6)



#Need to re-open and replace the names of headers to have parentheses
file.to.write <- test6
file.name <- "raw_results_pseudo_run51_noOMEGA.csv"

write.csv(file.to.write,file=file.name, row.names=F)
print(paste("Outputted data to ",file.name,sep=""))

