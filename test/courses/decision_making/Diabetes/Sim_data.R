## Created by Mia Kjellsson
## August 16, 2011
##-----------------------------------------------------------------------------
## Last modified:  13.10.10 Steve Choy
## Changes introduced: Modified to fit with WHIG model
##-----------------------------------------------------------------------------
## Create a simulated dataset for TIME at 3m, 6m, 9m, 1y 
##
##-----------------------------------------------------------------------------

#setwd("C:/Users/Steve Choy/Dropbox/Steve Choy Janssen Diabetes/Documents/Decision making course")

#..............................................................................
#       Dataset frame
#..............................................................................
 ID   <- c(1:4000)
 TIME <- c(0,84,182,252,365)

 obs.1 <- data.frame(merge(ID,TIME))
 names(obs.1) <- c("ID","TIME")
 obs.1$CMT   <- 1

obs.2 <- data.frame(merge(ID,TIME))
names(obs.2) <- c("ID","TIME")
obs.2$CMT   <- 2

obs.3 <- data.frame(merge(ID,TIME))
names(obs.3) <- c("ID","TIME")
obs.3$CMT   <- 3

obs.4 <- data.frame(merge(ID,TIME))
names(obs.4) <- c("ID","TIME")
obs.4$CMT   <- 4

obs <- rbind(obs.1,obs.2,obs.3,obs.4)
obs <- obs[order(obs$ID,obs$TIME),]

obs$DV    <- NA   # it is actually LDV
#obs$LDV    <- NA
#obs$MDV   <- 0
obs$BSL   <- 49

obs <- obs[,c(1,2,5,3,4)]
obs[is.na(obs)] <- 0
obs$DRUG <- ifelse(obs$ID <= 2000,0,1)

#obs.test <- subset(obs, DRUG == 1)
colnames(obs)[1] <- "#ID"

print("created observations")



#..................................................................
#  Printing data to file, need to re-open and rename ID to #ID and save
#..................................................................
 write.csv(obs,file="Sim_WHIG_369m.csv", row.names=F)
print("printed data to Sim_WHIG_369m.csv")


