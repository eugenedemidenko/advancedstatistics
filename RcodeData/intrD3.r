intrD3 <-
function(nSim=10000)
{
dump("intrD3","c:\\StatBook\\intrD3.r")
z=rep(1,4)
rea=rep(0,nSim)
for(isim in 1:nSim)
{
	A=runif(1,min=-1,max=1)
	B=runif(1,min=-1,max=1)
	z[2]=B;z[3]=A
	r=polyroot(z)
	rea[isim]=Re(r[abs(Im(r))<10^-8])
}
paste("Empiricial variance =",var(rea))
}
