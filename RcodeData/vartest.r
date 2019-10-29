vartest <-
function(n=20,s20=1,alpha=0.05,mu=-1,N=200,nSim=100000)
{
dump("vartest","c:\\StatBook\\vartest.r")

varq=function(n,alpha,eps=1e-05,maxit=100)
{
    n1=n-1
    q1=qchisq(alpha/2,df=n1)
    q2=qchisq(1-alpha/2,df=n1)
    M=matrix(ncol=2,nrow=2)
    m=rep(NA,2)
    for(it in 1:maxit)
    {
       m[1]=n1*(log(q2)-log(q1))-(q2-q1)
       m[2]=pchisq(q1,df=n1)-pchisq(q2,df=n1)+(1-alpha)
       M[1,1]=n1/q1-1
       M[1,2]=-n1/q2+1
       M[2,1]=-dchisq(q1,df=n1)
       M[2,2]=dchisq(q2,df=n1)
       delta=solve(M)%*%m
       if(max(abs(delta))<eps)break
       q1=q1+delta[1]
       q2=q2+delta[2]
    }
return(c(q1,q2))
}
par(mfrow=c(1,1),mar=c(3.5,3.5,1,1))
s2=seq(from=.8,to=1.2,length=N)
q1=qchisq(alpha/2,df=n-1);q2=qchisq(1-alpha/2,df=n-1)
pow.eqtail=1+pchisq(q1*s20/s2,df=n-1)-pchisq(q2*s20/s2,df=n-1)
plot(s2,pow.eqtail,type="l",lwd=3,ylab="",xlab="")
mtext(side=1,"Alternative variance",cex=1.3,line=2.5)
mtext(side=2,"Power, probability",cex=1.3,line=2.5)
segments(-1,.05,3,.05);segments(s20,-1,s20,.08)

q=varq(n=n,alpha=alpha,eps=1e-05,maxit=100)
pow.unb=1+pchisq(q[1]*s20/s2,df=n-1)-pchisq(q[2]*s20/s2,df=n-1)
lines(s2,pow.unb,lwd=3,lty=2)

s2=c(.85,.97,1.1);ns2=length(s2);prob1=prob2=rep(NA,ns2)
for(i in 1:ns2)
{
	X=matrix(rnorm(nSim*n,mean=mu,sd=sqrt(s2[i])),ncol=n)
	v=apply(X,1,var)*(n-1)/s20
	prob1[i]=1-mean(v<q2 & v>q1)
	prob2[i]=1-mean(v<q[2] & v>q[1])
}
points(s2,prob1,pch=2,cex=2)
points(s2,prob2,pch=1,cex=2)
legend(.83,.1,c("Unbiased test (non equal-tail probabilities)","Biased test (equal-tail probabilities)"),lwd=3,lty=1:2,col=1,pch=c(1:2),cex=1.5,bg="gray90")
meas=c(138.508,138.368,138.372,138.305,138.383,138.391,138.424,138.349,138.428,138.344,138.335,138.341)
print(meas)
n=length(meas)
q=varq(n=n,alpha=0.05,eps=1e-05,maxit=100)
print(q)
sm=sd(meas)
print(c(sm,.1*sqrt(q[1]/(n-1)),.1*sqrt(q[2]/(n-1))))
print(.1*sqrt(qchisq(.05,df=n-1)/(n-1)))
}
