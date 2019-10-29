webhitsQ <-
function()
{
    dump("webhitsQ","c:\\statbook\\webhitsQ.r")
	par(mfrow=c(1,1),mar=c(4.5,4.2,1,1),cex.lab=1.25)
    x<-scan("c:\\statbook\\comwebhits.dat") # read the data
    x<-x[order(x)] # order observations
    n<-length(x)
	q14=x[n/4];q12=x[n/2];q34=x[3*n/4]
    Fx<-(1:n)/n # values for cdf
    plot(x,Fx,type="s",lwd=3,xlab="Time of the website hit (h)",ylab="Probability, cdf")
    text(5,1,paste("Number of hits during 24 hours =",n),adj=0,cex=1.5)
	q=c(q14,q12,q34);prQ=c(1/4,1/2,3/4)
	for(i in 1:3)
		lines(x=c(-1,q[i],q[i]),y=c(prQ[i],prQ[i],-1),col=i+1,lty=i+1,lwd=2)	
	
	legend(15,.2,c("First quartile","Second quartile (median)","Third quartile"),lty=2:4,col=2:4,lwd=2,bg="gray98",cex=1.25)
	return(c(q14,q34))
}
