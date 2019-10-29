piest <-
function(sigma=1,n=200,minT=5,maxT=20,nSim=500000)
{
dump("piest","c:\\StatBook\\piest.r")
d=matrix(rep(runif(n,min=minT,max=maxT),nSim),byrow=T,ncol=n)
D=d+matrix(rnorm(nSim*n,mean=0,sd=sigma),ncol=n)
C=pi*d+matrix(rnorm(nSim*n,mean=0,sd=sigma),ncol=n)
E=rowMeans(C*D);HCD=rowMeans(C^2-D^2)
piML=(HCD+sqrt(HCD^2+4*E^2))/2/E
num=rowMeans(C);den=rowMeans(D)
piRAT=num/den
prRAT=mean(abs(piRAT-pi)<.01)
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(density(piML),type="l",lwd=2,xlab="",ylab="",main="")
mtext(side=1,"pi-hat",cex=1.5,line=2.75)
mtext(side=2,"Density",cex=1.5,line=2.5)
prML=mean(abs(piML-pi)<.01)
lines(density(piRAT),lty=2,lwd=2)
piNAIV=rowMeans(C/D)
prNAIV=mean(abs(piNAIV-pi)<.01)
lines(density(piNAIV),lty=3,lwd=2)
piOLS=rowMeans(C*D)/rowMeans(D^2)
prLS=mean(abs(piOLS-pi)<.01)
lines(density(piOLS),lty=4,lwd=2)
segments(pi,-1,pi,1000,col=3,lty=2)
legend("topleft",c("RAT","MLE","NAIVE","OLS"),col=1,lty=c(2,1,3,4),lwd=2,cex=1.5,bg=gray(.9))
pr=c(prRAT,prML,prNAIV,prLS)
m=c(median(piRAT)-pi,median(piML)-pi,median(piNAIV)-pi,median(piOLS)-pi)
mse=c(mean((piRAT-pi)^2),mean((piML-pi)^2),mean((piNAIV-pi)^2),mean((piOLS-pi)^2))
da=as.data.frame(cbind(m,sqrt(mse),pr))
row.names(da)=c("RAT","MLE","NAIVE","OLS")
names(da)=c("Bias","RMSE","Pr(abs(Est-pi)<0.01)")
print(da)
}