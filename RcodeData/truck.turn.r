truck.turn <-
function(tr.tu=10,car.time=3,Nmax=1000,nSim=10000)
{
dump("truck.turn","c:\\StatBook\\truck.turn.r")
nsec=rep(0<nSim)
for(isim in 1:nSim)
{
	X=rexp(Nmax,rate=1/car.time)
	cX=cumsum(X)
	nsec[isim]=min(cX[X>tr.tu])
}
nsec=nsec[order(nsec)]
xl=paste("Time to wait until first",tr.tu,"seconds break between two cars, seconds")  
plot(nsec,(1:nSim-.5)/nSim,type="l",xlab=xl,ylab="CDF, probability",main=paste("Median time =",median(round(nsec)),"seconds"))
mean(nsec)
}
