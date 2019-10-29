cov01 <-
function(delta=.1,N=10000,n=50,nSim=1000)
{
dump("cov01","c:\\StatBook\\cov01.r")
ncov=rep(0,nSim)
deltaN=round(delta*N)
for(isim in 1:nSim)
{
	INT=1:N
	for(i in 1:n)
	{
		X=sample(x=1:N,size=1,rep=T,prob=rep(1/N,N))
		lefX=max(X-deltaN/2,1);upX=min(X+deltaN/2,N)	
		INT=INT[INT<lefX | INT>upX]		
		if(length(INT)==0) { ncov[isim]=1; break}	
	}	
}
prn1=(1-(1-delta)^n)^(n-1)
cat("Empirical probability =",mean(ncov)," Approximation =",prn1,"\n")
}
