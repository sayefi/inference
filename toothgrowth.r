

tData<-ToothGrowth

head(tData)



summary(tData)
unique(tData$dose)
uniqu(tData$supp)

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
### H0 - Both suppliments have similar effect on teeth growth 
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

t.test(tData[tData$supp=="OJ",]$len-tData[tData$supp=="VC",]$len)

## P-value =0.00255 H0..false

t.test(len~supp,data=tData,paired=T,var.equal=T)

## P-value =0.00255 H0..false when its a single sample
## but, thats unlikely
## The response is the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs.
## OJ - Orange Juice
## VC - Vitamin C

t.test(len~supp,data=tData,paired=F,var.equal=T)
## P-value =0.06 H0..can't be rejected, when its two different sample

## P-value =0.00255 H0..false

# t.test(tData[tData$supp=="OJ",]$len, y = tData[tData$supp=="VC",]$len,
#        alternative ="greater",
#        mu = meanOJ-meanVC, paired = FALSE, var.equal = T,
#        conf.level = 0.80)

## P-value =0.5 H0..false

power.t.test(n = 30, delta = meanOJ-meanVC, sd = sqrt((sdOJ^2+sdVC^2)/2), 
             sig.level = 0.05,
             type = "two.sample",
             alternative = "one.sided")


## Power=0.60 Not enough for H1

power.t.test(power = .90, delta = meanOJ-meanVC, sd = sqrt((sdOJ^2+sdVC^2)/2), 
             sig.level = 0.05,
             type = "two.sample",
             alternative = "one.sided")

## For Power=0.90, n should be more than 71



tDataDose12<-tData[tData$dose!=1.0,]

t.test(len~dose,data=tDataDose12,paired=F,var.equal=T)


meanD.5<-mean(tData[tData$dose==.5,]$len)
meanD1<-mean(tData[tData$dose==1,]$len)
meanD2<-mean(tData[tData$dose==2.0,]$len)

sdD.5<-sd(tData[tData$dose==.5,]$len)
sdD1<-sd(tData[tData$dose==1,]$len)
sdD2<-sd(tData[tData$dose==2.0,]$len)


power.t.test(n=20, delta = meanD2-meanD.5, sd = sqrt((sdD2^2+sdD.5^2)/2), 
             sig.level = 0.05,
             type = "two.sample",
             alternative = "one.sided")

power.t.test(n=20, delta = meanD2-meanD1, sd = sqrt((sdD2^2+sdD1^2)/2), 
             sig.level = 0.05,
             type = "two.sample",
             alternative = "one.sided")

power.t.test(n=20, delta = meanD1-meanD.5, sd = sqrt((sdD.5^2+sdD1^2)/2), 
             sig.level = 0.05,
             type = "two.sample",
             alternative = "one.sided")


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
