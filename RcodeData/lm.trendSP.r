lm.trendSP <-
function(job=1,stock=16)
{
dump("lm.trendSP","c:\\StatBook\\lm.trendSP.r")
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
if(job==1)
{
	par(mfrow=c(1,2),mar=c(4,4,3,1))
	y=all[,stock]
	n=length(y)
	x=1:n
	plot(x,y,type="b",xlab="",ylab="")
	mtext(side=1,"Days after August 19, 2004",cex=1.5,line=2.75)
	mtext(side=2,"$ per share",cex=1.5,line=2.5)
	title(paste(nmST[stock],"stock price"))
	lm.out=lm(y~x)
	print(summary(lm.out))
	a=coef(lm.out)
	lines(x,a[1]+a[2]*x,lwd=3)
	res=lm.out$residuals
	res=res[order(res)]
	res=(res-mean(res))/sd(res)
	thq=qnorm((1:n-.5)/n)
	plot(thq,res,xlim=range(res),ylim=range(res),xlab="",ylab="")
	segments(-4,-4,4,4,col=3,lwd=3)
	mtext(side=1,"Theoretical quantile",cex=1.5,line=2.5)
	mtext(side=2,"Empirical quantile",cex=1.5,line=2.5)
	title("Q-q plot of residuals")
	q=seq(from=-3,to=3,length=200)
	pthq=pnorm(q)
	lambda=.95
	Zl=qnorm((1+lambda)/2)
	lb=qnorm(pthq-Zl*sqrt(pthq*(1-pthq)/n))
	ub=qnorm(pthq+Zl*sqrt(pthq*(1-pthq)/n))
	lines(q,lb,col=2);lines(q,ub,col=2)	
}
if(job==2)
{
	gjd=read.csv("c:\\StatBook\\GOOGLEjandec.csv",stringsAsFactors=F)
	gjd=gjd[seq(from=nrow(gjd),to=1,by=-1),]
	y.real=gjd[,7]
	nr=length(y.real)
	y=all[,stock]
	n=length(y)
	x=1:n
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(x,y,xlim=c(1,n+245+nr),ylim=c(200,1000),type="b",xlab="",ylab="")
	mtext(side=1,"Days after August 19, 2004",cex=1.5,line=2.75)
	mtext(side=2,"$ per share",cex=1.5,line=2.5)
	lm.out=lm(y~x)
	xp=1:(n+245+nr)
	print(summary(lm.out))
	a=coef(lm.out)
	xr=(n+246):(n+245+nr)
	RMSE=summary(lm.out)$sigma*sqrt(1+1/n+(xr-mean(x))^2/var(x)/(n-1))
	lines(xr,a[1]+a[2]*xr+RMSE,lwd=2)
	lines(xr,a[1]+a[2]*xr-RMSE,lwd=2)
	lines(xp,a[1]+a[2]*xp,lwd=3)
	points((n+246):(n+245+nr),y.real,col=2,cex=2)
}
}
