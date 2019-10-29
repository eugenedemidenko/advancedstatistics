heightweight <-
function(job=1)
{
dump("heightweight","c:\\StatBook\\heightweight.r")
da=read.csv("c:\\StatBook\\HeightWeight.csv",header=T)
url="http://wiki.stat.ucla.edu/socr/index.php/SOCR_Data_Dinov_020108_HeightsWeights"
h=da[,2];w=da[,3]
n=length(h)
if(job==1)
{
	plot(h,w,pch=16,cex=.5,xlab="Height, inches",ylab="Weight, pounds")
	title(paste("Weight versus height among",n,"individuals"))
	mtext(side=3,url,line=.25,cex=.85)
}
if(job==2)
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(h,w,pch=16,cex=.5,xlab="Height, inches",ylab="Weight, pounds")
	o=lm(w~h);a=coef(o)
	lines(50:80,a[1]+a[2]*(50:80),lwd=4,col=2)
	o=lm(h~w);a=coef(o)
	lines(60:80,((60:80)-a[1])/a[2],lwd=4,col=3,lty=2)
	legend(60,172,c("Regression of weight on height","Regression of height on weight"),col=2:3,lwd=4,lty=1:2,bg=gray(.95))
}
}
