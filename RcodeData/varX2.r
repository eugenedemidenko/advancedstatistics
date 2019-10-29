varX2 <-
function(mu=1,s2=2,nSim=1000000)
{
dump("varX2","c:\\StatBook\\varX2.r")
X=rnorm(nSim,mean=mu,sd=sqrt(s2))
theor.var=4*mu^2*s2+2*s2^2
emp.var=var(X^2)
cat("empirical variance=",emp.var," theoretical variance=",theor.var,"\n")
}
