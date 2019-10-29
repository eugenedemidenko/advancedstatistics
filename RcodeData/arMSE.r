arMSE <-
function(NSIM=100000,ro=2)
{
dump("arMSE","c:\\StatBook\\arMSE.r")
Ar.true=pi*ro^2
SDro=seq(from=.1,to=.5,by=.1);nsd=length(SDro)
RMSE1=RMSE2=RMSE3=rep(NA,nsd)
par(mfrow=c(1,2),mar=c(4,4,3,1))
for(n in c(2,5))
{
	for(i in 1:nsd)
	{
		r=matrix(rnorm(n*NSIM,mean=ro,sd=SDro[i]),ncol=n,nrow=NSIM)
		A1=pi*rowMeans(r^2)
		A2=pi*rowMeans(r)^2
		#var.r=apply(X=r,MARGIN=1,FUN=var)
		var.r=(rowSums(r^2)-n*rowMeans(r)^2)/(n-1)
		A3=A2-pi*var.r/n
		RMSE1[i]=sqrt(mean((A1-Ar.true)^2))
		RMSE2[i]=sqrt(mean((A2-Ar.true)^2))
		RMSE3[i]=sqrt(mean((A3-Ar.true)^2))
	}
	all=cbind(RMSE1,RMSE2,RMSE3)/SDro
	matplot(SDro,all,type="o",col=1,lwd=2,xlab="",ylab="Relative RMSE",main=paste("Sample size, n =",n))
	expression(paste("True SD =",sigma))
	mtext(side=1,expression(paste("True SD = ",sigma)),cex=1.5,line=2.5)
	legend(SDro[1],max(all),c("RMSE1","RMSE2","RMSE3"),lty=1:3,cex=1.25,lwd=3)
	cat("\nn =",n,"\n")
	print(cbind(SDro,RMSE1,RMSE2,RMSE3))
}

}
