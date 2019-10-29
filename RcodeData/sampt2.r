sampt2 <-
function(eff.size=-1,alpha=.02,p=.8,NMAX=100)
{
dump("sampt2","c:\\StatBook\\sampt2.r")
N=6:NMAX
pow1=pt(qt(alpha,df=N-2),df=N-2,ncp=eff.size*sqrt(N)/2)
t1a=qt(1-alpha/2,df=N-2)
pow2=pt(-t1a,df=N-2,ncp=eff.size*sqrt(N)/2)+1-pt(t1a,df=N-2,ncp=eff.size*sqrt(N)/2)
matplot(N,cbind(pow1,pow2),type="l")
segments(-1,p,max(N),p)
a=abs(pow1-p)
N1=N[a==min(a)]
a=abs(pow2-p)
N2=N[a==min(a)]
title(paste("N1 =",N1,", N2 =",N2))
}
