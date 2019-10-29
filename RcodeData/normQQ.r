norm3QQ <-
function(i1=1,i2=2)
{
	dump("norm3QQ","c:\\StatBook\\norm3QQ.r")
	symb=c("HPQ","HD","MS","M","VZ","T","S","C","TGT","WMT","GM","XOM","F","YHOO","IBM","GOOGL","MSFT")
	nmST=c("Hewlett-Packard","Home Depo","Morgan Stanley","Macys","Verizon","AT&T","Sprint","Citigroup","Target","Walmart","General Motors","Exxon","Ford Motor","Yahoo","IBM","Google","Microsoft")
	ns=length(symb)
	all=as.data.frame(matrix(NA,ncol=ns,nrow=10000))
	names(all)=symb
	min.ni=10000
	for(i in 1:ns)
	{
		tabi=read.csv(paste("c:\\StatBook\\stocks\\",symb[i],".csv",sep=""),stringsAsFactors=F)
		ni=nrow(tabi)
		if(min.ni>ni) min.ni=ni
		all[1:ni,i]=tabi[,7]
	}
	all=all[seq(from=min.ni,to=1,by=-1),]
	LRT=log(all[2:min.ni,]/all[1:(min.ni-1),])
	n=nrow(LRT)
	
	par(mfrow=c(1,2),mar=c(3,3,2,1),cex.main=1.5)
	covLRT=cov(LRT)
	ecovLRT=eigen(covLRT,sym=T)
	iOM12=ecovLRT$vectors%*%diag(1/sqrt(ecovLRT$values),ns,ns)%*%t(ecovLRT$vectors)
	Z=as.matrix((LRT-rep(1,n)%*%t(colMeans(LRT))))
	Z=Z%*%iOM12
	lambda=.95
	for(i in c(i1,i2))
	{
		z=Z[,i]
		z=z[order(z)]
		rz=range(z)
		thq=qnorm(((1:n)-.5)/n)
		plot(thq,z,xlim=range(z),xlab="",ylab="",main=paste("Component",i))
		mtext(side=1,"Theoretical quantile",line=2,cex=1.25)
		mtext(side=2,"Empirical quantile",line=2,cex=1.25)
		q=seq(from=rz[1],to=rz[2],length=1000)
		pthq=pnorm(q)
		Zl=qnorm((1+lambda)/2)
		lb=qnorm(pthq-Zl*sqrt(pthq*(1-pthq)/n))
		ub=qnorm(pthq+Zl*sqrt(pthq*(1-pthq)/n))
		segments(rz[1],rz[1],rz[2],rz[2],col=3,lwd=3)
		lines(q,lb,col=2)
		lines(q,ub,col=2)	
	}
	
	
}
