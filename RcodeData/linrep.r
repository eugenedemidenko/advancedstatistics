linrep <-
function(N=30)
{
dump("linrep","c:\\StatBook\\linrep.r")
ni=round(runif(n=N,min=30,max=60))
mui=round(runif(n=N,min=450,max=550))
beta=30
sigma=20
y=x=matrix(nrow=N,ncol=max(ni))
for(i in 1:N)
{
	xi=sample(x=c(0,1),size=ni[i],rep=T,prob=c(.5,.5))
	yi=mui[i]+beta*xi+rnorm(ni[i],sd=sigma)
	y[i,1:ni[i]]=yi;x[i,1:ni[i]]=xi
}

#linear model y=X*beta+eps
sni=sum(ni)
X=matrix(0,nrow=sni,ncol=N+1)
Y=rep(0,sni)
k=1
for(i in 1:N)
{
	Y[k:(k+ni[i]-1)]=y[i,1:ni[i]]
	X[k:(k+ni[i]-1),i]=rep(1,ni[i])
	X[k:(k+ni[i]-1),N+1]=x[i,1:ni[i]]
	k=k+ni[i]
}
o=lm(Y~X-1)
print(summary(o))

#slope model on differences

X=rep(0,sni)
k=1
for(i in 1:N)
{
	Y[k:(k+ni[i]-1)]=y[i,1:ni[i]]-mean(y[i,1:ni[i]])
	X[k:(k+ni[i]-1)]=x[i,1:ni[i]]-mean(x[i,1:ni[i]])	
	k=k+ni[i]
}
beta.hat=sum(Y*X)/sum(X^2)
resster=sqrt(sum((Y-beta.hat*X)^2)/(sni-N-1))
tval=beta.hat/resster*sqrt(sum(X^2))
pval=2*pt(abs(tval),df=sni-N-1,lower.tail=F)
cat("beta.hat=",beta.hat,"\nRes. st.error=",resster,"\nt value=",tval,"\nPr(>|t|)=",pval)
cat("\n")
}
