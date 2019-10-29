poisR <-
function(N=200)
{
dump("poisR","c:\\StatBook\\poisR.r")

#set.seed(3)
#n=834
#marriage=sample(x=c(1,0),size=n,rep=T,prob=c(.7,.3))
#income=round(exp(rnorm(n=n,mean=log(70),sd=.4)))
#age=round(runif(n,min=16,max=85))
#gender=sample(x=c(1,0),size=n,rep=T,prob=c(.5,.5))
#rate=5.75-.2*marriage-.05*age-.01*income+.4*gender
#rate=rate[rate>0]
#trviol=rpois(n=n,lambda=rate)
#da=as.data.frame(cbind(trviol,marriage,gender,age,income))
#write.csv(da,"c:\\StatBook\\Traffic.Viol.csv",row.names=F)

da=read.csv("c:\\StatBook\\Traffic.Viol.csv",header=T)
o=glm(trviol~marriage+gender+age+income,data=da,family=poisson)
a=coef(o)
print(summary(o))
lam=exp(a[1]+a[2]+a[3]+a[4]*50+80*a[5])
cat("The expected number of violations during a 5-year period\nfor a married man of 50 years old with income $80K =",round(lam,1),"\n")
minc=mean(da$income);mage=mean(da$age)
print(c(minc,mage))
ag=seq(from=15,to=60,length=N)
inc=seq(from=20,to=200,length=N)
lamba=exp(a[1]+a[2]+a[3]+a[4]*mage+a[5]*inc)
par(mfrow=c(1,1),mar=c(3.5,3.5,4,1))
plot(inc,lamba,type="l",lwd=3,ylim=c(1,4),axes=F,xlab="",ylab="")
mtext(side=1,"Income, thousands dollars",cex=1.4,line=2.5)
mtext(side=2,"Expected number of traffic violations",cex=1.4,line=2.25)
mtext(side=3,"Age of driver",cex=1.4,line=2.25)
lambi=exp(a[1]+a[2]+a[3]+a[4]*ag+a[5]*minc)
lines(inc,lambi,lty=2,lwd=3)
axis(side=1,seq(from=20,to=200,by=20))
axis(side=2,c(1,2,3,4))
axis(side=3,seq(from=20,to=200,by=20),labels=as.character(seq(from=15,to=60,by=5)))
legend(20,1.7,c("Dependence on income (average age)","Dependence on age (average income)"),lty=1:2,lwd=3,bg="gray95",cex=1.25)
}
