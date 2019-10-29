twocph <-
function(Nst=100,alpha=.05)
{
dump("twocph","c:\\StatBook\\twocph.r")
da=read.csv("c:\\StatBook\\twocph.csv",header=T,stringsAsFactors=F)
min=da[,1];Vt=da[,2]
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(min,Vt,type="p",xlab="",ylab="")
mtext(side=1,"Time elapsed from drug injection, minutes",line=2.5,cex=1.25)
mtext(side=2,"Contrast agent concentration, mg/l",line=2.75,cex=1.25)
#Starting values
alphs=rep(seq(from=0.001,to=1,length=Nst),Nst) #Nst=number of grid points
betas=rep(seq(from=0.001,to=1,length=Nst),each=Nst)
E=(1-exp(-alphs%*%t(min)))*exp(-betas%*%t(min))
RSS=(E%*%Vt)^2/rowSums(E^2)
maxRSS=max(RSS)
a0=alphs[RSS==maxRSS];b0=betas[RSS==maxRSS]
E=(1-exp(-a0*min))*exp(-b0*min)
V00=sum(Vt*E)/sum(E^2)
cat("Starting values:",V00,a0,b0)

VtM=function(V0,alpha,beta,x) # Two-compartment function
{
	e1=exp(-alpha*x);e2=exp(-beta*x)
	Vt.ret=V0*(1-e1)*e2
	dVdV0=(1-e1)*e2
	dVda=V0*e1*e2*x
	dVdb=-Vt.ret*x
	attr(Vt.ret,"gradient")=cbind(dVdV0,dVda,dVdb)
	return(Vt.ret)
}

out=nls(Vt~VtM(V0,alpha,beta,x=min),start=list(V0=V00,alpha=a0,beta=b0))
sighat=summary(out)$sigma
C=sighat^2*summary(out)$cov.unscaled
print(summary(out))
a=coef(out)
x=seq(from=0,to=120,length=1000)
Vall=VtM(a[1],a[2],a[3],x)
Jhat=attr(Vall,"gradient")
SEy=sqrt(sighat^2+diag(Jhat%*%C%*%t(Jhat)))
Za1=qnorm(1-alpha/2)
lines(x,Vall+Za1*SEy,col=2,lty=2)
lines(x,Vall-Za1*SEy,col=2,lty=2)
lines(x,Vall,lwd=2)
legend(70,350,c("Fitted value","95% confidence interval"),lwd=2,col=1:2,lty=1:2,bg="gray94",cex=1.5)
topt=log(1+a[2]/a[3])/a[2]
Vopt=a[1]*(1-exp(-a[2]*topt))*exp(-a[3]*topt)
segments(topt,-10,topt,Vopt,lty=2)
points(topt,Vopt,cex=1.25)
text(topt+1,1,paste("Optimal time for imaging=",round(topt)," minutes after injection"),adj=0,font=4)


Cab=C[2:3,2:3]
dtoptda=(a[2]/(a[2]+a[3])-log(a[2]+a[3])+log(a[3]))/a[2]^2
dtoptdb=(1/(a[2]+a[3])-1/a[3])/a[2]
der=c(dtoptda,dtoptdb)
vartopt=t(der)%*%Cab%*%der
SEopt=sqrt(vartopt)
cat("topt =",topt,", SE topt =",SEopt,"\n")

RAT=22/20
C=a[2]*(a[2]+a[3])*RAT
T0=-log((a[3]-C)/(a[2]+a[3]))/a[2]
for(i in 1:10)
{
	den=a[3]+a[2]*exp(-(a[2]+a[3])*T0)-(a[2]+a[3])*exp(-a[2]*T0)-C
	num=a[2]*(a[2]+a[3])*(exp(-(a[2]+a[3])*T0)-exp(-(a[2]*T0)))
	TK=T0+den/num
	if(abs(TK-T0)<0.0001) break
	T0=TK
}

#> D(expression(b+a*exp(-T*(a+b))-(a+b)*exp(-a*T)),'a')
#exp(-T * (a + b)) - a * (exp(-T * (a + b)) * T) - (exp(-a * T) - (a + b) * (exp(-a * T) * T))

#> D(expression(b+a*exp(-T*(a+b))-(a+b)*exp(-a*T)),'b')
#1 - a * (exp(-T * (a + b)) * T) - exp(-a * T)
#> D(expression(b+a*exp(-T*(a+b))-(a+b)*exp(-a*T)),'T')
#-(a * (exp(-T * (a + b)) * (a + b)) - (a + b) * (exp(-a * T) * a))

d9.48da=exp(-T0*(a[2]+a[3]))-a[2]*(exp(-T0*(a[2]+a[3]))*T0)-(exp(-a[2]*T0)-(a[2]+a[3])*(exp(-a[2]*TK)*TK))
d9.48db=1-a[2]*(exp(-T0*(a[2]+a[3]))*T0)-exp(-a[2]*T0)
d9.48dT=-(a[2]*(exp(-T0*(a[2]+a[3]))*(a[2]+a[3]))-(a[2]+a[3])*(exp(-a[2]*T0)*a[2]))
dab=c(d9.48da,d9.48db)/d9.48dT
SET=sqrt(t(dab)%*%Cab%*%dab)
cat("T =",T0,", SET =",SET,"\n")
}
