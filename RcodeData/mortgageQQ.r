mortgageQQ <-
function(lambda=.95)
{
dump("mortgageQQ","c:\\StatBook\\mortgageQQ.r")
da=read.csv("c:\\StatBook\\mortgageROC.csv",header=T)
FamilyIncome=da$FamilyIncome
Default=da$Default
Y=FamilyIncome[Default=="yes"];X=FamilyIncome[Default=="no"]
ZYX=c((Y-mean(Y))/sd(Y),(X-mean(X))/sd(X))
ZYX=ZYX[order(ZYX)]

LY=log10(Y);LX=log10(X)
ZLYX=c((LY-mean(LY))/sd(LY),(LX-mean(LX))/sd(LX))
ZLYX=ZLYX[order(ZLYX)]

par(mfrow=c(1,2),mar=c(4,4,3,1),cex.main=1.5)
n=length(FamilyIncome)    
thq=qnorm(((1:n)-.5)/n)
q=seq(from=-8,to=8,length=1000) # quantiles must cover wide range
pthq=pnorm(q)
Zl=qnorm((1+lambda)/2)
lb=qnorm(pthq-Zl*sqrt(pthq*(1-pthq)/n))
ub=qnorm(pthq+Zl*sqrt(pthq*(1-pthq)/n))

plot(thq,ZYX,xlab="",ylab="")
mtext(side=1,"Theoretical quantile",cex=1.25,line=2.75)
mtext(side=2,"Empirical quantile",cex=1.25,line=2.75)
segments(-4,-4,4,4,lty=2)
title("Original income data")

lines(q,lb,col=2);lines(q,ub,col=2)

plot(thq,ZLYX,xlab="",ylab="")
mtext(side=1,"Theoretical quantile",cex=1.25,line=2.75)
mtext(side=2,"Empirical quantile",cex=1.25,line=2.75)
lines(q,lb,col=2);lines(q,ub,col=2)
segments(-4,-4,4,4,lty=2)
title("log10 income data")

}
