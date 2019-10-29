gammaNLS <-
function(n=100,alpha=1,lambda=2,nSim=10000,ss=4)
{
dump("gammaNLS","c:\\StatBook\\gammaNLS.r")
set.seed(ss)
Fi=(1:n-.5)/n
ab1=ab2=ab3=ab4=matrix(ncol=2,nrow=nSim)
for(isim in 1:nSim)
{
	y=rgamma(n,alpha,lambda)
	y=y[order(y)]
	ab1[isim,]=c(mean(y)^2/var(y),mean(y)/var(y))
	ab2[isim,]=coef(nls(Fi~pgamma(y,a,l),start=c(a=alpha,l=lambda)))	
	out=nls(Fi~pgamma(y,a,l),start=c(a=alpha,l=lambda),weights=1/Fi/(1-Fi))
	ab3[isim,]=coef(out)
	pr=pgamma(y,ab3[isim,1],ab3[isim,2])
	w=1/(pr*(1-pr)+1/n)
	ab4[isim,]=coef(nls(Fi~pgamma(y,a,l),start=c(a=alpha,l=lambda),weights=w))	
}
out=as.data.frame(matrix(ncol=4,nrow=4),row.names=c("MM","NLS-cdf","NLS-cdf ew","NLS-cdf rw"))
names(out)=c("Bias alpha","Bias lambda","RMSE alpha","RMSE lambda")
out[1,1]=mean(ab1[,1])-alpha;out[1,2]=mean(ab1[,2])-lambda
out[1,3]=sqrt(mean((ab1[,1]-alpha)^2));out[1,4]=sqrt(mean((ab1[,2]-lambda)^2))

out[2,1]=mean(ab2[,2])-alpha;out[2,2]=mean(ab2[,2])-lambda
out[2,3]=sqrt(mean((ab2[,2]-lambda)^2));out[2,4]=sqrt(mean((ab2[,2]-lambda)^2))

out[3,1]=mean(ab3[,2])-alpha;out[3,2]=mean(ab3[,2])-lambda
out[3,3]=sqrt(mean((ab3[,2]-lambda)^2));out[3,4]=sqrt(mean((ab3[,2]-lambda)^2))

out[4,1]=mean(ab4[,2])-alpha;out[4,2]=mean(ab4[,2])-lambda
out[4,3]=sqrt(mean((ab4[,2]-lambda)^2));out[4,4]=sqrt(mean((ab4[,2]-lambda)^2))

out
}
