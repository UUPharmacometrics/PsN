##..............................................................................
## Created by Steve Choy
## Oct 18, 2013
##.............................................................................
## Last modified: 
## Last changes: 
##.............................................................................
## Reads in a csv containing vpc output values and do boxplots
##.............................................................................

library(ggplot2)
library(gridExtra)


d <- read.csv("vpc_output.csv",skip=1,sep=",",head=T)

d.0 <- subset(d, TRT == 0)
d.1 <- subset(d, TRT == 1)

d.1$dCFB.RUN51.84 <- d.1[,1] - d.0[,1]
d.1$dCFB.RUN51.182 <- d.1[,2] - d.0[,2]
d.1$dCFB.RUN51.252 <- d.1[,3] - d.0[,3]

d.1$dCFB.RUN52.84 <- d.1[,4] - d.0[,4]
d.1$dCFB.RUN52.182 <- d.1[,5] - d.0[,5]
d.1$dCFB.RUN52.252 <- d.1[,6] - d.0[,6]

d.1$dCFB.RUN53.84 <- d.1[,7] - d.0[,7]
d.1$dCFB.RUN53.182 <- d.1[,8] - d.0[,8]
d.1$dCFB.RUN53.252 <- d.1[,9] - d.0[,9]

d.1$dCFB.RUN54.84 <- d.1[,10] - d.0[,10]
d.1$dCFB.RUN54.182 <- d.1[,11] - d.0[,11]
d.1$dCFB.RUN54.252 <- d.1[,12] - d.0[,12]

# RUN 51 - Normal D&E
summary(d.1[14])
summary(d.1[15])
summary(d.1[16])
mean(d.1$dCFB.RUN51.84)
mean(d.1$dCFB.RUN51.182)
mean(d.1$dCFB.RUN51.252)

# RUN 52 - Extreme D&E
summary(d.1[17])
summary(d.1[18])
summary(d.1[19])
mean(d.1$dCFB.RUN52.84)
mean(d.1$dCFB.RUN52.182)
mean(d.1$dCFB.RUN52.252)

# RUN 53 - No D&E
summary(d.1[20])
summary(d.1[21])
summary(d.1[22])
mean(d.1$dCFB.RUN53.84)
mean(d.1$dCFB.RUN53.182)
mean(d.1$dCFB.RUN53.252)

# RUN 54 - Bad D&E
summary(d.1[23])
summary(d.1[24])
summary(d.1[25]) # The biggest mean dCFB here
mean(d.1$dCFB.RUN54.84)
mean(d.1$dCFB.RUN54.182)
mean(d.1$dCFB.RUN54.252)

#bplot <- function(yvar1,tit) {
#  p <- ggplot(d, aes(x=factor(TRT),get(yvar1)) +
#       geom_boxplot(aes(fill = factor(TRT))) +
#       guides(fill = FALSE) +
#       geom_abline(intercept=0, slope=0) +
#       labs(title = tit, y= "d CFB (%)", x = "Treatment")
  #return(print(p))
#}

p.1 <- ggplot(d, aes(factor(TRT),y=RUN51.CFB.84)) +
       geom_boxplot(aes(fill = factor(TRT))) +
       guides(fill = FALSE) +
       geom_abline(intercept=0, slope=0) +
       ylim(min(d[1:3]),max(d[1:3])) +
       labs(title = "Normal D&E, 3m", y= "d CFB (%)", x = "Treatment")
p.1


p.2 <- ggplot(d, aes(factor(TRT),y=RUN51.CFB.182)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[1:3]),max(d[1:3])) +
  labs(title = "Normal D&E, 6m", y= "d CFB (%)", x = "Treatment")

p.2

p.3 <- ggplot(d, aes(factor(TRT),y=RUN51.CFB.252)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[1:3]),max(d[1:3])) +
  labs(title = "Normal D&E, 9m", y= "d CFB (%)", x = "Treatment")
p.3

p.4 <- ggplot(d, aes(factor(TRT),y=RUN52.CFB.84)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[4:6]),max(d[4:6])) +
  labs(title = "Extreme D&E, 3m", y= "d CFB (%)", x = "Treatment")
p.4


p.5 <- ggplot(d, aes(factor(TRT),y=RUN52.CFB.182)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[4:6]),max(d[4:6])) +
  labs(title = "Extreme D&E, 6m", y= "d CFB (%)", x = "Treatment")

p.5

p.6 <- ggplot(d, aes(factor(TRT),y=RUN52.CFB.252)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[4:6]),max(d[4:6])) +
  labs(title = "Extreme D&E, 9m", y= "d CFB (%)", x = "Treatment")
p.6

p.7 <- ggplot(d, aes(factor(TRT),y=RUN53.CFB.84)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[7:9]),max(d[7:9])) +
  labs(title = "No D&E, 3m", y= "d CFB (%)", x = "Treatment")
p.7


p.8 <- ggplot(d, aes(factor(TRT),y=RUN53.CFB.182)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[7:9]),max(d[7:9])) +
  labs(title = "No D&E, 6m", y= "d CFB (%)", x = "Treatment")

p.8

p.9 <- ggplot(d, aes(factor(TRT),y=RUN53.CFB.252)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[7:9]),max(d[7:9])) +
  labs(title = "No D&E, 9m", y= "d CFB (%)", x = "Treatment")
p.9

p.10 <- ggplot(d, aes(factor(TRT),y=RUN54.CFB.84)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[10:12]),max(d[10:12])) +
  labs(title = "Bad D&E, 3m", y= "d CFB (%)", x = "Treatment")
p.10


p.11 <- ggplot(d, aes(factor(TRT),y=RUN54.CFB.182)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[10:12]),max(d[10:12])) +
  labs(title = "Bad D&E, 6m", y= "d CFB (%)", x = "Treatment")

p.11

p.12 <- ggplot(d, aes(factor(TRT),y=RUN54.CFB.252)) +
  geom_boxplot(aes(fill = factor(TRT))) +
  guides(fill = FALSE) +
  geom_abline(intercept=0, slope=0) +
  ylim(min(d[10:12]),max(d[10:12])) +
  labs(title = "Bad D&E, 9m", y= "d CFB (%)", x = "Treatment")
p.12


#grid.arrange(p.1,p.2,p.3, ncol=3, nrow=1)
#grid.arrange(p.4,p.5,p.6, ncol=3, nrow=1)
#grid.arrange(p.7,p.8,p.9, ncol=3, nrow=1)
#grid.arrange(p.10,p.11,p.12, ncol=3, nrow=1)


pdf(file="Boxplot.pdf")
grid.arrange(p.1,p.2,p.3, ncol=3, nrow=1)
grid.arrange(p.4,p.5,p.6, ncol=3, nrow=1)
grid.arrange(p.7,p.8,p.9, ncol=3, nrow=1)
grid.arrange(p.10,p.11,p.12, ncol=3, nrow=1)
dev.off()
