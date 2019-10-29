benfordEXP <-
function()
{
dump("benfordEXP","c:\\StatBook\\BenfordEXP.r")
lambdas=c(1,.1,.01);nl=length(lambdas)
par(mfrow=c(1,2))
d=1:9
dmat=as.data.frame(matrix(ncol=4,nrow=9))
row.names(dmat)=1:9
names(dmat)=c("Benford","lambda=1","lambda=0.1","lambda=0.01")
plot(1:9,log10(d+1)-log10(d),ylim=c(0,.4),type="l",lwd=2,xlab="",ylab="")
title("Distribution of the 1st digit")
mtext(side=1,"Digits",line=2.75,cex=1.5)
mtext(side=2,"Probability",line=2.5,cex=1.5)
dmat[,1]=log10(d+1)-log10(d)
for(i in 1:3)
{
	PrD=(1/d^(lambdas[i]/log(10))-1/(d+1)^(lambdas[i]/log(10)))/(1-exp(-lambdas[i]))
	lines(d,PrD,lty=i+1,lwd=2)
	dmat[,i+1]=PrD
}
legend(4,.4,c("Benford","lambda=1","lambda=0.1","lambda=0.01"),lty=1:3,lwd=2,cex=1.5,bg=gray(.92))
print(dmat)

x=seq(from=1,to=200,length=1000);nx=length(x)
fmat=matrix(nrow=nx,ncol=3)
for(i in 1:3)
	fmat[,i]=lambdas[i]/x^(lambdas[i]/log(10)+1)/log(10)

matplot(x,fmat,ylim=c(0,.01),type="l",lty=2:3,lwd=2,col=1,xlab="",ylab="")
title("Densities of originated distributions")
mtext(side=1,"x",line=2.75,cex=1.5)
mtext(side=2,"Density",line=2.5,cex=1.5)
legend(75,.01,c("lambda=1","lambda=0.1","lambda=0.01"),lwd=2,lty=2:4,cex=1.5,bg=gray(.92))
}
