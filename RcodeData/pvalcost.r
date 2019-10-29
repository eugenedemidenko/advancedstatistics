pvalcost <-
function(N=1000,n=23,sigma=12)
{ #N=the number of imaginable surveys with n students in each survey
    dump("pvalcost","c:\\StatBook\\pvalcost.r")
    mu0=17 # the null value
    n=23 #number of students in each survey
    Tstat=rep(NA,N)
    for(i in 1:N)
    {
		Y=rnorm(n,mean=17,sd=sigma)
		Y.bar=mean(Y);sigma.hat=sd(Y)
		Tstat[i]=sqrt(n)*(Y.bar-17)/sigma.hat
    }
    cat("\nProportion of abs(Tstat)>2.398 =",mean(abs(Tstat)>2.398),"\n")     
}
