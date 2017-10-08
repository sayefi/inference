

tData<-ToothGrowth

head(tData)



summary(tData)
unique(tData$dose)

tapply(tData,tData$supp,mean(tData$len))

library(dplyr)
tDataGrouped<-group_by(tData,supp,dose)

tDataSummary<-summarise_all(tDataGrouped,mean)

library(ggplot2)

g<-ggplot(data=tDataSummary,aes(y=len,x=factor(dose),fill=supp))
g<-g+geom_col(position = "dodge")
# g<-g+coord_flip()
g<-g+ scale_fill_discrete(name="Type of Suppliment")
g<-g+theme(legend.position=c(.1,.8))
g<-g+labs(x="Dose", y="Average Teeth length")
g<-g+ggtitle(label="Both suppliments contributed to teeth growth") 
print(g)


## Compare effectiveness of the suppliments
### H0 - Suppliments doesn't an impact on teeth length
### H1 - Suppliment OJ works better than suppliment VC

meanOJ<-mean(tData[tData$supp=="OJ",]$len)
meanVC<-mean(tData[tData$supp=="VC",]$len)

sdOJ<-sd(tData[tData$supp=="OJ",]$len)
sdVC<-sd(tData[tData$supp=="VC",]$len)

meanOJ
meanVC
sdOJ
sdVC

meanVC-meanOJ

pt(1,58)

t.test(tData[tData$supp=="OJ",]$len, y = tData[tData$supp=="VC",]$len,
       alternative ="greater",
       mu = meanOJ-meanVC, paired = FALSE, var.equal = T,
       conf.level = 0.80)

## P-value =0.5 H0..false

power.t.test(n = 30, delta = meanOJ-meanVC, sd = (sdOJ+sdVC)/2, 
             sig.level = 0.05,
             type = "two.sample",
             alternative = "one.sided")


## Power=0.60 Not enough for H1

power.t.test(power = .80, delta = meanOJ-meanVC, sd = (sdOJ+sdVC)/2, 
             sig.level = 0.05,
             type = "two.sample",
             alternative = "one.sided")

## For Power=0.80, n should be more than 51

power.t.test()

tData[tData$supp=="VC",]

subset(tData,factor(supp)="VC")

str(tData)

library(reshape2)
tDataCast<-dcast(tData,dose~len)

tDataCast
head(tDataCast)

tDataOJ<-subset(tData,supp="OJ")
mean(tDataOJ$len)
