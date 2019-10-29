pidistr <-
function()
{
dump("pidistr","c:\\StatBook\\pidistr.r")
pidig=read.csv("c:\\StatBook\\pi100000digits.csv",col.names=F)
edig=read.csv("c:\\StatBook\\e10000digits.csv",col.names=F)
par(mfrow=c(1,1),mar=c(4,4,1,1))
pr.pi=pr.e=rep(0,10)
for(i in 1:10)
{
	pr.pi[i]=mean(pidig==(i-1))
	pr.e[i]=mean(edig==(i-1))
}	
plot(0:9,pr.pi,type="h",lwd=5,ylim=c(0,.15),xlab="",ylab="",axes=F)
segments(0:9+.1,rep(0,10),0:9+.1,pr.e,lwd=5,lty=2)
axis(side=1,0:9)
axis(side=2,seq(from=0,to=0.15,by=.025))
mtext(side=1,"Digits of pi",cex=1.5,line=2.5)
mtext(side=2,"Probability",cex=1.5,line=2.5)
legend(0,.15,c("pi","e"),lty=1:2,lwd=5,bg=gray(.9),cex=1.5)

pidig=as.vector(edig[,1])
n=length(pidig)
print(n)
d=rep(0,10)
pv=rep(0,n-20+1)
for(i in 20:n)
{
	x=pidig[1:i]
	for(j in 0:9) d[j+1]=mean(x==j)
	Q=i/.1*sum((d-.1)^2)
	pv[i-49]=1-pchisq(Q,df=9)	

}
pv


}
