cimcorSP <-
function(job=1)
{
dump("cimcorSP","c:\\StatBook\\cimcorSP.r")
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
if(job==1)
{
	par(mfrow=c(ns,ns),mar=c(0,0,0,0),omi=c(.15,.15,.15,.15))
	for(i in seq(from=ns,to=1,by=-1))
	for(j in 1:ns)
	{
		x=LRT[,i];y=LRT[,j]
		plot(x,y,pch=16,cex=.25,xlim=c(-.1,.1),ylim=c(-.1,.1),xlab="",ylab="",axes=F)	
		abline(lsfit(y=y,x=x),col=2,lwd=3)
		if(i==1) mtext(side=1,symb[j],cex=.75,font=2)
		if(j==1) mtext(side=2,symb[i],cex=.75,font=2)
		if(i==ns) mtext(side=3,symb[j],cex=.75,font=2)
		if(j==ns) mtext(side=4,symb[i],cex=.75,font=2)
	}		
}
if(job==2)
{
	par(mfrow=c(1,1),mar=c(3,3,3,3),cex.axis=.75)
	R=cor(LRT);R[R==1]=NA
	image(1:ns,1:ns,R,col=c("cyan","green","yellow","red"),breaks=c(0,.25,.5,.75,1),xlab="",ylab="",axes=F)
	axis(side=1,at=1:ns,labels=symb,font=2)
	axis(side=2,at=1:ns,labels=symb,srt=90,font=2)
	axis(side=3,at=1:ns,labels=symb,font=2)
	axis(side=4,at=1:ns,labels=symb,font=2)
	for(i in 1:ns)
		text(rep(i,ns),1:ns,round(R[i,],2),font=2)
}
if(job==3)
{
	par(mfrow=c(1,1),mar=c(3,3,3,3),cex.axis=.75)
	R=cor(LRT)
	iR=iiR=solve(R)
	for(i in 1:ns)
	for(j in 1:ns)
	 iR[i,j]=-iiR[i,j]/sqrt(abs(iiR[i,i]*iiR[j,j]))
	diag(iR)=rep(NA,ns)
	image(1:ns,1:ns,iR,col=c("cyan","green","yellow","red"),breaks=c(-.25,0,.25,.5,.75),xlab="",ylab="",axes=F)
	axis(side=1,at=1:ns,labels=symb,font=2)
	axis(side=2,at=1:ns,labels=symb,srt=90,font=2)
	axis(side=3,at=1:ns,labels=symb,font=2)
	axis(side=4,at=1:ns,labels=symb,font=2)
	for(i in 1:ns)
		text(rep(i,ns),1:ns,round(iR[i,],2),font=2)
}
}
