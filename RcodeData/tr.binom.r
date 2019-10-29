tr.binom <-
function(n=10,p=.3,lambda=.9)
{
dump("tr.binom","c:\\StatBook\\tr.binom.r")
minDIF=n+1
for(m in 0:n)
for(M in m:n)
{
	pr.within=pbinom(M,n,prob=p)-pbinom(m,n,prob=p)
	if(pr.within>=lambda)
	{
		if(M-m<minDIF)
		{
			m.opt=m
			M.opt=M
			minDIF=M-m
			pr.within.real=pr.within
		}
	}
}
c(m.opt,M.opt,pr.within.real)
}
