cauchy.google <-
function(job=1,stock=16,n=50,theta=1,lambda=2,itMAX=10,nSim=10000)
{
dump("cauchy.google","c:\\StatBook\\cauchy.google.r")
if(job==1) #vectorized simulations
{
	Y=matrix(theta+rcauchy(nSim*n)*lambda,ncol=n)
	th=la=rep(NA,nSim)
	th=apply(Y,1,median)
	la=apply(abs(Y-th),1,median)
	for(it in 1:itMAX)
	{
		y=(Y-th)/la
		th=th+4*la/n*rowSums(y/(1+y^2))
		la=la+2*la/n*(2*rowSums(y^2/(1+y^2))-n)
	}	
	print(c(mean(th),mean(la)))
	print(c(var(th),var(la),2*lambda^2/n))
}
if(job==2)
{
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
	y=all[,stock];n=length(y)
	n1=n-1
	r=(y[2:n]-y[1:n1])/y[1:n1]
	
	png("c:\\StatBook\\cauchy.google_2a.png",width=1000,height=500)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(1:n1,r,type="l",xlab="",ylab="")
	mtext(side=1,"August 20, 2004 through March 4, 2016, days",cex=1.25,line=2.75)
	mtext(side=2,"Stock price return",cex=1.25,line=2.75)
	mr=mean(r)
	segments(-10,mr,n1+10,mr,col=2)
	SD=sd(r)
	segments(-10,mr+SD,n1+10,mr+SD,lty=2,col=2)
	segments(-10,mr-SD,n1+10,mr-SD,lty=2,col=2)
	dev.off()
	
	png("c:\\StatBook\\cauchy.google_2b.png",width=500,height=500)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	res=r[order(r)]
	res=(res-mean(res))/sd(res)
	thq=qnorm((1:n1-.5)/n1)
	plot(thq,res,xlim=range(res),ylim=range(res),xlab="",ylab="")
	segments(-10,-10,10,10,col=3,lwd=3)
	mtext(side=1,"Theoretical quantile",cex=1.5,line=2.75)
	mtext(side=2,"Empirical quantile",cex=1.5,line=2.5)
	q=seq(from=-10,to=10,length=2000)
	pthq=pnorm(q)
	lambda=.95
	Zl=qnorm((1+lambda)/2)
	lb=qnorm(pthq-Zl*sqrt(pthq*(1-pthq)/n1))
	ub=qnorm(pthq+Zl*sqrt(pthq*(1-pthq)/n1))
	lines(q,lb,col=2);lines(q,ub,col=2)
	dev.off()
	
	th=median(r)
	la=median(abs(r-th))
	for(it in 1:itMAX)
	{
		y=(r-th)/la
		th=th+4*la/n*sum(y/(1+y^2))
		la=la+2*la/n*(2*sum(y^2/(1+y^2))-n)
	}	
	print(c(th,la))
	png("c:\\StatBook\\cauchy.google_2c.bmp",width=1000,height=500)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(1:n1,r,type="l",xlab="",ylab="")
	mtext(side=1,"August 20, 2004 through March 4, 2016, days",cex=1.25,line=2.75)
	mtext(side=2,"Stock price return",cex=1.25,line=2.75)
	segments(-10,th,n1+10,th,col=2)
	mr=mean(r);SD=sd(r)
	segments(-10,mr+SD,n1+10,mr+SD,col=2)
	segments(-10,mr-SD,n1+10,mr-SD,col=2)
	segments(-10,th+la,n1+10,th+la,col=3)
	segments(-10,th-la,n1+10,th-la,col=3)
	legend(200,.15,c("Normal SD","Cauchy scale"),col=2:3,cex=2,lwd=3,bg="gray90")
	dev.off()
	
	png("c:\\StatBook\\cauchy.google_2d.png",width=1000,height=500)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(1:n1,r,type="l",xlab="",ylab="")
	mtext(side=1,"August 20, 2004 through March 4, 2016, days",cex=1.25,line=2.75)
	mtext(side=2,"Stock price return",cex=1.25,line=2.75)
	segments(-10,th,n1+10,th,col=2)
	mr=mean(r);SD=sd(r)
	segments(-10,mr+SD,n1+10,mr+SD,lty=2,col=2)
	segments(-10,mr-SD,n1+10,mr-SD,lty=2,col=2)
	segments(-10,th+la,n1+10,th+la,lty=2,col=3)
	segments(-10,th-la,n1+10,th-la,lty=2,col=3)
	legend(200,.15,c("Normal SD","Cauchy scale"),col=2:3,cex=2,lty=2,bg="gray90")
	dev.off()
	
	bmp("c:\\StatBook\\cauchy.google_2e.bmp",width=800,height=500)
print(c(mr,SD,th,la))
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	ret=seq(from=-.1,to=0,length=300)
	prNORM=pnorm((ret-mr)/SD)
	prCAUCH=pcauchy((ret-th)/la)
	matplot(100*ret,cbind(prNORM,prCAUCH),col=1:2,lty=1:2,lwd=3,type="l",xlab="",ylab="")
	for(ip in seq(from=.025,to=.5,by=.025)) segments(-100,ip,0,ip,col="gray80")
	mtext(side=1,"Stock return, %",cex=1.25,line=2.75)
	mtext(side=2,"Probability",cex=1.25,line=2.75)
	legend(-10,.45,c("Normal distribution","Cauchy distribution"),col=1:2,lty=1:2,lwd=3,cex=2,bg="gray90")
	dev.off()
}
}
