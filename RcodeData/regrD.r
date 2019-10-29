regrD <-
function(alpha=1,beta=1,mux=2,sdx=1,sigma=2.5,nSim=100000)
{
dump("regrD","c:\\StatBook\\regrD.r")
ols.slope=function(X,Y) #vectorized OLS
{
	meanX=rowMeans(X);meanX2=rowSums(X^2)
	meanY=rowMeans(Y)
	meanXY=rowSums(X*Y)
	b.est=(meanXY-n*meanX*meanY)/(meanX2-n*meanX^2)	
	b.est=b.est[order(b.est)]
	return(b.est)
}

par(mfrow=c(1,2),mar=c(4,4,3,1))
lx=c(-5,-3);ly=c(-.03,-.01);k=1
cdfE=(1:nSim)/nSim
for(n in c(4,10))
{
	se.b=sigma/sdx/sqrt(n)
	
	X=matrix(rnorm(nSim*n,mean=mux,sd=sdx),ncol=n) #normal
	Y=alpha+beta*X+matrix(rnorm(nSim*n,mean=0,sd=sigma),ncol=n)
	b=ols.slope(X,Y)
	fin=pnorm((b-beta)/se.b)
	plot(b,cdfE-fin,xlim=c(beta-5*se.b,beta+5*se.b),type="l",lwd=3,xlab="",ylab="")
	title(paste("Sample size =",n))
	mtext(side=1,"Slope estimate",line=2.5,cex=1.5)
	mtext(side=2,"Difference with normal cdf",line=2.5,cex=1.5)
	X=matrix(runif(nSim*n,min=mux-sqrt(3)*sdx,max=mux+sqrt(3)*sdx),ncol=n)
	Y=alpha+beta*X+matrix(rnorm(nSim*n,mean=0,sd=sigma),ncol=n)
	b=ols.slope(X,Y)
	fin=pnorm((b-beta)/se.b)
	lines(b,cdfE-fin,type="l",lwd=2)
	X=matrix(sample(x=c(mux-sdx,mux+sdx),size=nSim*n,prob=c(.5,.5),rep=T),ncol=n)
	Y=alpha+beta*X+matrix(rnorm(nSim*n,mean=0,sd=sigma),ncol=n)
	b=ols.slope(X,Y)		
	b=b[b<Inf];b=b[!is.na(b)]
	nd=length(b)
	fin=pnorm((b-beta)/se.b)
	lines(b,(1:nd)/nd-fin,type="l")		
	legend(lx[k],ly[k],c("Normal","Uniform","Discrete"),lty=1,lwd=c(3,2,1),cex=1.5,bg=gray(.9))
	k=k+1
}
	
}
