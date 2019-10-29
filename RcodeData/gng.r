gng <-
function(stock=16,maxit=100,eps=.00001)
{
dump("gng","c:\\StatBook\\gng.r")

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
y=(y[2:n]-y[1:n1])/y[1:n1]
n=n-1
#starting values
theta=0;SD=sd(y)
sigma=sd(y[abs(y)<2*SD])
sigmaN=sd(y[abs(y)>2*SD])
delta=length(y[abs(y)>2*SD])/n
p=c(theta,delta,sigma,sigmaN)
for(it in 1:maxit)
{
	f1=dnorm(y,mean=theta,sd=sigma);f2=dnorm(y,mean=theta,sd=sigmaN)
	f12=(1-p[2])*f1+p[2]*f2
	ll=sum(log(f12))
	d1=((1-p[2])*f1/p[3]^2+p[2]*f2/p[4]^2)*(y-p[1])
	d2=-f1+f2
	d3=(1-p[2])/p[3]^3*f1*((y-p[1])^2-p[3]^2)
	d4=p[2]/p[4]^3*f2*((y-p[1])^2-p[4]^2)
	d=cbind(d1/f12,d2/f12,d3/f12,d4/f12)
	H=t(d)%*%d
	iH=solve(H)
	grad=t(d)%*%rep(1,n)
	print(c(it,ll,p,sqrt(sum(grad^2))))
	p.new=p+iH%*%grad
	if(max(abs(p-p.new))<eps) break
	p=p.new
}
SEp=sqrt(diag(iH))
Zvalue=p.new/SEp
print(cbind(p.new,SEp,Zvalue))
theta=p[1];delta=p[2];sigma=p[3];sigmaN=p[4]
par(mfrow=c(1,2),mar=c(4,4,3,1))
x=seq(from=-.2,to=.2,length=1000)
matplot(x,cbind(dnorm(x,mean=theta,sd=sigma),dnorm(x,mean=theta,sd=sigmaN)),type="l",lwd=3,lty=1:2,xlab="",ylab="")
title("Density components of the Gaussian mixture")
mtext(side=1,"Stock return, %",cex=1.25,line=2.75)
mtext(side=2,"Density",cex=1.25,line=2.75)
rug(y)
text(.1,20,paste("d =",round(delta,3)),font=5,cex=1.5)
legend(-.2,30,c("1: SD = 0.012","2: SD = 0.05"),col=1:2,lwd=3,lty=1:2,bg="gray90")
mr=0.0007966704;SD=0.0158611336;th=0.0006029807;la=0.0073432496
ret=seq(from=-.1,to=0,length=300)
prNORM=pnorm((ret-mr)/SD)
prCAUCH=pcauchy((ret-th)/la)
prGM1=pnorm((ret-theta)/sigma)
prGM2=pnorm((ret-theta)/sigmaN)
matplot(100*ret,cbind(prNORM,prCAUCH,prGM1,prGM2),col=1:4,lty=1:4,lwd=3,type="l",xlab="",ylab="")
title("Probability of obtaining a negative return")
for(ip in seq(from=.025,to=.5,by=.025)) segments(-100,ip,0,ip,col="gray80")
mtext(side=1,"Stock return, %",cex=1.25,line=2.75)
mtext(side=2,"Probability",cex=1.25,line=2.75)
legend(-10,.45,c("Normal distribution","Cauchy distribution","1st component GM","2nd component GM"),col=1:4,lty=1:4,lwd=3,cex=1,bg="gray90")
 
}
