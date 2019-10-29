roblinreg2 <-
function(n=c(10,20),sigma=2,dft=3,dfchi=2,nSim=100000)
{
dump("roblinreg2","c:\\StatBook\\roblinreg2.r")
nn=length(n)
alpha=.05
bs=seq(from=-.5,to=.5,length=100);NBS=length(bs)
intrc=1
pow=matrix(nrow=NBS,ncol=3)
par(mfrow=c(1,nn),mar=c(3.5,3.5,3,1),cex.main=1.5)

for(i in 1:nn)
{
	x=1:n[i];avx=mean(x);s2x=var(x)*(n[i]-1)
	X=matrix(rep(x,each=nSim),ncol=n[i])
	for(k in 1:NBS)
	{
		for(j in 1:3)
		{
			if(j==1) eps=sigma*matrix(rnorm(nSim*n[i]),ncol=n[i]) #normal
			if(j==2) eps=sigma*matrix(rt(nSim*n[i],df=dft),ncol=n[i])/sqrt(dft/(dft-2)) #t
			if(j==3) eps=sigma*(matrix(rchisq(nSim*n[i],df=dfchi),ncol=n[i])-dfchi)/sqrt(2*dfchi) #chi
			Y=intrc+bs[k]*X+eps
			ry=rowMeans(Y)
			b.hat=(rowSums(Y*X)-ry*avx*n[i])/s2x
			int.hat=ry-b.hat*avx
			res=Y-int.hat-b.hat%*%t(x)
			sig.hat=sqrt(rowSums(res^2)/(n[i]-2))
			pv=2*pt(-abs(b.hat)/sig.hat*sqrt(s2x),df=n[i]-2)
			pow[k,j]=mean(pv<alpha)		
		}
	}
	matplot(bs,pow,lwd=2,col=1,ylim=c(0,1),type="l",xlab="",ylab="")
	segments(bs[1],alpha,bs[NBS],alpha,col="gray90")
	mtext(side=1,"Alternative slope coefficient",line=2.5,cex=1.4)
	mtext(side=2,"Power",line=2.5,cex=1.4)
	title(paste("Sample size, n =",n[i]))
	legend(x=bs[1],.95,c("Normal",paste("t-distribution, dft = ",dft,sep=""),paste("chi-square distribution, dfchi = ",dfchi,sep="")),lty=1:3,lwd=2,bg="gray90",cex=1.25)
}

}
