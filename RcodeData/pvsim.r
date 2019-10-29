pvsim <-
function(incA=60,pvalue.obs=0.0588,SD=10,n=100,nSurv=10000)
{ 
#incA=state offical family income, the null hypothesis value
#Z.obs=observed Z-statistic
#pvalue.obs=observed p-value based on the survey of 100 families in town
#SD=standard deviation on family income in town and state, known
#n=town survey sample size
#nSurv=imaginary survey sample size in a typical state town
dump("pvsim","c:\\StatBook\\pvsim.r")
Z.obs=qnorm(pvalue.obs/2,lower.tail=F)
z.nSurv=pv.nSurv=rep(NA,nSurv)
for(i in 1:nSurv) #imaginary nSurv surveys
{
    inc=rnorm(n,mean=incA,sd=SD) # n random incomes from a typical town
    av=mean(inc)
    Z=(av-incA)/SD*sqrt(n)
	z.nSurv[i]=Z
    pv.nSurv[i]=2*(1-pnorm(abs(Z))) 
}
cat("Observed Z = Z.obs =",Z.obs,"\nProportion of Z-values in a typical state town for which abs(Z)>=Z.obs =",mean(abs(z.nSurv)>=Z.obs),"\n") 
cat("Observed p-value = pvalue.obs =",pvalue.obs,"\nProportion of p-values in a typical state town < pvalue.obs =",mean(pv.nSurv<pvalue.obs),"\n") 
}
