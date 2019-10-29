sbE <-
function(job=1,m=20,p=0.45,n=15,N=100,ss=3)
{
	dump("sbE", "c:\\statbook\\sbE.r")
	set.seed(ss)
	if(job==1)
	{
		par(mfrow = c(1, 1), mar = c(3.5, 3.5, 3, 1))
		plot(1:N, 1:N, type = "n", xlab = "", ylab = "")
		mtext(side = 1, "Player #1", line = 2.5, cex = 1.25)
		mtext(side = 2, "Player #2", line = 2.5, cex = 1.25)
		mtext(side=3,paste("p=",p,", n=",n,", m=",m,sep=""),cex=1.5,line=.5)
		polygon(x = c(0, 0, N - n, N, N, n, 0), y = c(0, n, N, N, N - n, 0, 0), col="grey90")
		lines(x = c(0, 0, N - n, N, N, n, 0), y = c(0, n, N, N, N - n, 0, 0))
		segments(0, 0, N, N)
		points(0, 0, pch = 16, cex = 1.25)
		nwins=0
		for(i in 1:m) {
			y1 <- y2 <- 0
			for(k in 1:(2 * N)) {
				if(runif(1) > p)
					x <- 0
				else x <- 1
				segments(y1, y2, y1 + x, y2 + 1 - x)
				y1 <- y1 + x
				y2 <- y2 + 1 - x
				if(abs(y1 - y2) >= n) {
					points(y1, y2, pch = 17, cex = 1.25)
					break
				}
				if(y1==100 | y2==100)
				{
					points(y1, y2, pch = 17, cex = 1.25)				
					break
				}
			}
			if(y1>y2) nwins=nwins+1
		}
		text(70,15,paste("Numbers of wins for the 1st player =",nwins))
	}
	if(job==2)
	{
		par(mfrow=c(1,2),mar=c(4,4,3,1))
		ns=1:15
		nns=length(ns)
		nwins=matrix(0,nrow=nns,ncol=2)
		pps=c(.3,.4)
		for(ipp in 1:2)
		for(iii in 1:nns)
		{
			nwins[iii,ipp]=0
			for(i in 1:m) {
				y1 <- y2 <- 0
				for(k in 1:(2 * N)) {
					if(runif(1) > pps[ipp]) x <- 0 else x <- 1
					y1 <- y1 + x; y2 <- y2 + 1 - x
					if(abs(y1 - y2) >= ns[iii]) break
					if(y1==100 | y2==100) break				
				}
				if(y1>y2) nwins[iii,ipp]=nwins[iii,ipp]+1
			}
			nwins[iii,ipp]=nwins[iii,ipp]/m
		}		
		matplot(ns,nwins,col=1,type="b",xlab="n",ylab="Probability",main="Probability as a function of n")
		legend(9,.4,c("1: p=0.3","2: p=0.4"),cex=1.25,bg="gray90")
		ps=seq(from=.25,to=.75,by=.25/8)
		ns=c(5,10)
		nps=length(ps)
		nwins=matrix(0,nrow=nps,ncol=2)
		for(ins in 1:2)
		for(iii in 1:nps)
		{
			nwins[iii,ins]=0
			for(i in 1:m) {
				y1 <- y2 <- 0
				for(k in 1:(2 * N)) {
					if(runif(1) > ps[iii]) x <- 0 else x <- 1
					y1 <- y1 + x; y2 <- y2 + 1 - x
					if(abs(y1 - y2) >= ns[ins]) break
					if(y1==100 | y2==100) break				
				}
				if(y1>y2) nwins[iii,ins]=nwins[iii,ins]+1
			}
			nwins[iii,ins]=nwins[iii,ins]/m
		}		
		matplot(ps,nwins,col=1,type="b",xlab="p",ylab="Probability",main="Probability as a function of p")	
		legend(.25,1,c("1: n=5","2: n=10"),cex=1.25,bg="gray90")
	}
}
