walD <-
function(job=1)
{
dump("walD","c:\\StatBook\\walD.r")
da=read.csv("c:\\StatBook\\blackfriday.csv")
n=nrow(da)	
if(job==1)
{
	par(mfrow=c(2,2))
	plot(da$age,da$time.hour,main="Time at Wallmart as a function of age")
	plot(da$man,da$time.hour,main="Time at Wallmart as a function of gender")
	plot(da$age,log(da$time.hour),main="Time at Wallmart as a function of age")
	plot(da$man,log(da$time.hour),main="Time at Wallmart as a function of gender")
}
if(job==2)
{
	y=da$time.hour
    X=cbind(rep(1,n),da$age,da$age^2,da$man)
	lamb0=1/mean(y)
	print("Lambda from exp. distribution with no predictors:");print(lamb0)
    bet=c(log(lamb0),0,0,0)
    r=t(X)%*%rep(1,n)
	eps=0.0001;maxit=100
	cov=solve(t(X)%*%X)
	print("FS iteration for MLE: iter, loglik, beta0, beta1, beta2, beta3")
	for(it in 1:maxit)
    {
        Xb=X%*%bet
        d=as.vector(y*exp(Xb))
        tZd=t(X)%*%(d*X)
        loglik=sum(bet*r)-sum(d)
        delta=cov%*%(r-t(X)%*%d)
        if(max(abs(delta))<eps) break
        bet=bet+delta
		print(c(it,loglik,bet))        
    }
	se=sqrt(diag(cov))
	pv=2*(1-pnorm(abs(bet)/se))
	out=as.data.frame(cbind(bet,se, pv));names(out)=c("ML betas","SE","P-value")
	print(out)
	
	#Gamma link glm 
	y[y==0]=.0001
	o=glm(y~X-1,family=Gamma(link='log'))
	print(summary(o))	
}

if(job==3)
{
	bet=c(0.884806939,-0.129022535,0.001277193,-0.087489089)
	par(mfrow=c(1,3),cex.main=2)
	ti=seq(from=0,to=18,length=100)
	alpha=.5
	age=18:80
	Z1a=qnorm(1-alpha/2) #crit Z-value
	exptF=exp(-bet[1]-bet[2]*age-bet[3]*age^2)
	X=cbind(rep(1,n),da$age,da$age^2,da$man)
	cov13=solve(t(X)%*%X)[1:3,1:3]
	X13=cbind(rep(1,length(age)),age,age^2)
	var.age=diag(X13%*%cov13%*%t(X13))
	low.age=-bet[1]-bet[2]*age-bet[3]*age^2-Z1a*sqrt(var.age)
	up.age=-bet[1]-bet[2]*age-bet[3]*age^2+Z1a*sqrt(var.age)
	plot(age,exptF,type="l",ylim=c(0,12),col=1,lwd=3,xlab="",ylab="",main="The expected time to shop")
	lines(age,exp(low.age));lines(age,exp(up.age))
	mtext(side=2,"Expected time to shop after 6 AM",cex=1.25,line=2.5)
	mtext(side=1,"Age",cex=1.25,line=3)
	
	
	age=20
	LMAB.wo=exp(bet[1]+bet[2]*age+bet[3]*age^2)
	prWO20=1-exp(-LMAB.wo*ti)
	age=50
	LMAB.wo=exp(bet[1]+bet[2]*age+bet[3]*age^2)
	prWO50=1-exp(-LMAB.wo*ti)	
	matplot(ti,cbind(prWO20,prWO50),type="l",col=1,lwd=3,xlab="",ylab="",main="The time cdf for females",ylim=c(0,1))
	mtext(side=1,"Time after 6 AM, hours",line=3,cex=1.25)
	mtext(side=2,"Proportion of shoppers",line=2.5,cex=1.25)
	legend(7,.25,c("20 years old","50 years old"),col=1,lty=1:2,cex=1.75,lwd=3,bg="gray90")
	
	age=50
	LMAB.me=exp(bet[1]+bet[2]*age+bet[3]*age^2)
	prW=1-exp(-LMAB.me*ti)
	LMAB.me=exp(bet[1]+bet[2]*age+bet[3]*age^2+bet[4])
	prM=1-exp(-LMAB.me*ti)	
	matplot(ti,cbind(prW,prM),type="l",col=1,lwd=3,xlab="",ylab="",main="Femals vs. males",ylim=c(0,1))
	mtext(side=1,"Time after 6 AM, hours",line=3,cex=1.25)
	mtext(side=2,"Proportion of shoppers",line=2.5,cex=1.25)
	legend(7,.25,c("Females","Males"),col=1,lty=1:2,cex=1.75,lwd=3,bg="gray90")
}
}
