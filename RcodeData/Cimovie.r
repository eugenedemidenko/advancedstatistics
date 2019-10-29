CImovie <-
function(mu.true=1,sigma=2,n=10,Nsim=100)
{
dump("CImovie","c:\\StatBook\\CImovie.r")
par(mfrow=c(1,2),mar=c(4,4,3,1))
Lmu=Umu=lw=rep(NA,Nsim)
for(lambda in c(.5,.95))
{
	plot(1,1,xlim=c(-3,5),ylim=c(0,Nsim),type="l",axes=F,xlab="Mean",ylab=paste("Sample of",n))
	axis(side=2,at=c(1,10,20,30,40,50,60,70,80,90,100),srt=90)
	axis(side=1,at=seq(from=-3,to=5,by=1))	
	segments(mu.true,-10,mu.true,Nsim,col=2,lwd=3)
	tcrit=qt((1+lambda)/2,df=n-1)	
	for(isim in 1:Nsim)
	{
		y=rnorm(n,mean=mu.true,sd=sigma)
		Lmu[isim]=mean(y)-tcrit*sd(y)/sqrt(n)
		Umu[isim]=mean(y)+tcrit*sd(y)/sqrt(n)
		lw[isim]=1;if(mu.true<Umu[isim] & mu.true>Lmu[isim]) lw[isim]=3
		segments(Lmu[isim],isim-1,Umu[isim],isim-1,lwd=lw[isim])
	}
	title(paste("Nominal confidence level =",lambda,", % covered =",mean(lw==3)*100))
}
}
