vartestSP <-
function(stock1=2,stock2=6,alpha=.05)
{
dump("vartestSP","c:\\StatBook\\vartestSP.r")
symb=c("HPQ","HD","MS","M","VZ","T","S","C","TGT","WMT","GM","XOM","F","YHOO","IBM","GOOGL","MSFT")
nmST=c("Hewlett-Packard","Home Depot","Morgan Stanley","Macys","Verizon","AT&T","Sprint","Citigroup","Target","Walmart","General Motors","Exxon","Ford Motor","Yahoo","IBM","Google","Microsoft")
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

par(mfrow=c(1,2),mar=c(3.5,3.75,2,1),cex.main=1.5)
st=c(stock1,stock2)
xy=LRT[,st]
n=nrow(xy);va=rep(0,2)
for(i in 1:2)
{
	me=mean(xy[,i]);va[i]=var(xy[,i]);SD=round(sd(xy[,i]),4)
	plot(1:n,xy[,i],type="l",ylim=c(-.05,.05),main=paste(nmST[st[i]],", SD = ",SD,sep=""),xlab="",ylab="")
	mtext(side=1,"Time, day",line=2.5,cex=1.5)
	mtext(side=2,"Return on the log scale with CI",line=2.5,cex=1.5)
	segments(-1,me,n,me,col=2,lwd=3)
	segments(-1,me+1.96*SD,n,me+1.96*SD,col=2)
	segments(-1,me-1.96*SD,n,me-1.96*SD,col=2)
}
nu=va[1]/va[2]
pval=pf(nu,df1=n-1,df2=n-1,lower.tail=F)+pf(1/nu,df1=n-1,df2=n-1)
cat("lower nu = ",qf(alpha/2,df1=n-1,df2=n-1),", uppper nu = ",qf(alpha/2,df1=n-1,df2=n-1,lower.tail=F),sep="")
cat(", nu=var1/var2=",nu,", p-value = ",pval,sep="")


}
