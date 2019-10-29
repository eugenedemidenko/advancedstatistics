my1sr <-
function(alpha=0.5)
{
	dump("my1sr","c:\\StatBook\\my1sr.r")
    x<-seq(from=0,to=pi/2,length=100)
    y1<-cos(x)
    y2<-alpha*x
    plot(x,y1,type="l",xlab="x",ylab="y")
	lines(x,y2,lwd=3) # the result is in Figure <ref>grr</ref>
    legend(1,1,c("cos","linear"),lty=1,lwd=c(1,3),cex=1.5) # indicate the functions
    adif<-abs(y1-y2)
    indmin<-which.min(abs(y1-y2)) # index of minimum
    x0<-x[indmin] # an approx. solution
    points(x0,alpha*x0,cex=2,pch=16) # large solid point
	segments(x0,-1,x0,alpha*x0,lty=2)
	mt=paste("Graphical solution of equation cos(x) = ",alpha,"*x\nx = ",round(x0,2),sep="")
	title(mt)
    return(x0)
}
