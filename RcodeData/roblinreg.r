roblinreg <-
function(n=c(10,20),sigma=3,dft=3,dfchi=2,nSim=100000)
{
dump("roblinreg","c:\\StatBook\\roblinreg.r")
nn=length(n)
alpha=.05
bs=seq(from=-1,to=1,length=30);NBS=length(bs)
intrc=1
pow=matrix(nrow=NBS,ncol=3)
par(mfrow=c(1,nn))
simlr=function(y)
{
	n=length(y)
	x=1:n
	out=summary(lm(y~x))$coefficients
	return(out[2,4])
}

for(i in 1:nn)
{
	x=1:n[i]
	for(k in 1:NBS)
	{
		X=matrix(rep(intrc+bs[k]*x,each=nSim),ncol=n[i])
		for(j in 1:3)
		{
			if(j==1) eps=sigma*matrix(rnorm(nSim*n[i]),ncol=n[i]) #normal
			if(j==2) eps=sigma*matrix(rt(nSim*n[i],df=dft),ncol=n[i])/sqrt(dft/(dft-2)) #t
			if(j==3) eps=sigma*(matrix(rchisq(nSim*n[i],df=dfchi),ncol=n[i])-dfchi)/sqrt(2*dfchi) #chi
			Y=X+eps
			pv=apply(Y,1,simlr)
			pow[k,j]=mean(pv<alpha)			
		}
	}
	matplot(bs,pow,col=1,lwd=2,type="l")
}

}
