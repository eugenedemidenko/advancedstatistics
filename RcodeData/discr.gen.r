discr.gen <-
function(Ngen=100000,n=10,x=rnorm(n,sd=3),p=runif(n))
{
dump("discr.gen","c:\\StatBook\\discr.gen.r")
par(mfrow=c(1,1),mar=c(4,4,3,1))
n=length(p)
iox=order(x);p=p[iox];x=x[iox] # order x and p
p=p/sum(p)
csp=cumsum(p)
Y=runif(Ngen)
X=rep(0,Ngen)
X[Y<p[1]]=x[1]
for(i in 2:n) X[csp[i-1] <= Y & Y < csp[i]]=x[i]
plot(x,p,type="h",ylab="Probability")
pemp=rep(0,n)
for(i in 1:n) pemp[i]=mean(X==x[i])
segments(x+.01,rep(0,n),x+.01,pemp,lty=2)
legend(x[1],max(p),c("Theoretical probabilities","Empirical probabilities"),lty=1:2,cex=1.25)
title(paste("Generation of a discrete distribution with n = ",n, " using ", Ngen," simulations",sep=""))
print(cbind(x,p,pemp))
}
