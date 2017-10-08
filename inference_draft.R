n<-40;lambda<-0.2;set.seed(1000)
x<-rexp(n,lambda)
ms<-mean(x); sds<-sd(x); vars<-var(x)


sim<-1000
simData<-matrix(sample(x,n*sim,replace=T),sim,n)
means<-apply(simData,1,mean); 


vars<-apply(simData,1,var); sds<-apply(simData,1,sd)


means
xx



par(mfrow=c(2,1))

par(mfrow=c(1,1))

set.seed(2000)
xx<-rexp(sim,lambda)
hist(means, col=rgb(0,0,1,0.5), breaks=30,xlim=c(-.1,15),ylim=c(0,100),
     main="Distribution of exponentials and its sample means",
     xlab="x",
     ylab="Fequency")
hist(xx,col=rgb(1,0,0,0.5),alpha=1,add=T,breaks = 131)
curve(1000*dnorm(x,mean=1/lambda,sd=1/lambda), lty=2,lwd=3,
      col="gray",from=-1,to=12, n=100,add=TRUE)
legend("topright", c("Dist. of means","Dist of 1000 exponentials","Normal Distribution"),
       cex=0.8, col=c("blue","red","gray"), lty=5, lwd=4, bty="n")





# Histogram Colored (blue and red)
hist(h1, col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,200), main=”Overlapping Histogram”, xlab=”Variable”)
hist(h2, col=rgb(0,0,1,0.5), add=T)
box()



hist(xx,)


h<-hist(means,breaks = 131)


hh<-h$density;xx<-h$mids
plot(xx,y=hh/sqrt(40),type ="l",lty=2,col="red",xlim=c(-1,12), ylim=c(0,300),
     main="Distribution of Sample mean in contrust with normal dist.",
     xlab="x",
     ylab="Fequency")

curve(1000*dnorm(x,mean=1/lambda,sd=1/lambda), lty=2,
      col="darkblue",from=-1,to=12, n=100,add=TRUE)

curve(dchisq(x, df = 4), col = 2, lty = 2, lwd = 2, add = TRUE)


# hist(x,breaks = 10, 
#      main="Distribution of 40 exponetials (sample)")
# text(x=10,y=10,paste("Sample mean:",round(ms,2),"\nStandard Deviation:",round(sds,2)))
```

* Step 3: Generate simulation data (1000 simulations, 40 exponentials)
+ Calculate mean and variance for each simulation
+ Plot histogram for mean and variance

```{r}

par(mfrow=c(1,2))
hist(means,breaks = 20,
     main=paste("Dist.of sample mean"),
     xlab="Sample Means")
abline(v=mean(means),lty=5,col="blue")
abline(v=1/lambda,lty=5,col="red")
legend("topright", c(paste("S. Mean:",round(mean(means),2)),paste("T.Mean:",1/lambda)),
       cex=0.55, col=c("blue","red"), lty=5, lwd=2, bty="n");
hist(vars,breaks = 20,xlab="Sample Variances",
     main=paste("Dist. of sample Variance"))
abline(v=mean(vars),lty=5,col="blue")
text(38,100,cex=.8, paste("Sample & pop.\n variance: ~",round(mean(vars),0)))
abline(v=(1/lambda)^2,lty=5,col="red")