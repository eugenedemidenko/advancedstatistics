dint <-
function(job=1,N=1000000)
{
dump("dint","c:\\StatBook\\dint.r")
if(job==1) #uniform distribution
{
	X=runif(n=N,min=-1,max=1)
	Y=runif(n=N,min=-1,max=1)
	cc=as.numeric(X^2+Y^2<1)
	I=4*mean(cc*exp(-(3*X^2-4*X*Y+2*Y^2)))
	print(I)
}
if(job==2) #normal distribution, regenerate
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	sds=seq(from=.3,to=3,by=.1);ns=length(sds)
	Ins1=Ins2=rep(NA,ns)
	for(i in 1:ns)
	{
		X=rnorm(N,sd=sds[i])
		Y=rnorm(N,sd=sds[i])
		cc=as.numeric(X^2+Y^2<1)
		Ins1[i]=mean(cc*exp(-(3*X^2-4*X*Y+2*Y^2))/dnorm(X,sd=sds[i])/dnorm(Y,sd=sds[i]))
	}
	
	X0=rnorm(N);Y0=rnorm(N)
	for(i in 1:ns)
	{
		X=sds[i]*X0;Y=sds[i]*Y0
		cc=as.numeric(X^2+Y^2<1)
		Ins2[i]=mean(cc*exp(-(3*X^2-4*X*Y+2*Y^2))/dnorm(X,sd=sds[i])/dnorm(Y,sd=sds[i]))
	}
	matplot(sds,cbind(Ins1,Ins2),type="b",col=1,xlab="SD",ylab="Integral approximation")
	segments(0,1.374,max(sds),1.374,col=2)
	legend(sds[1],max(cbind(Ins1,Ins2)),c("1: regenerated","2: generated once"))
}
}
