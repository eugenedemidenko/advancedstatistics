webhits <-
function()
{
    dump("webhits","c:\\statbook\\webhits.r")
	par(mfrow=c(1,1),mar=c(4,4,1,1))
    x<-scan("c:\\statbook\\comwebhits.dat") # read the data
    x<-x[order(x)] # order observations
    n<-length(x)
    Fx<-(1:n)/n # values for cdf
    plot(x,Fx,type="s",xlab="Time of the website hit, hour",ylab="Probability, cdf")
    text(17,.2,paste("Number of hits during 24 hours =",n))
}
