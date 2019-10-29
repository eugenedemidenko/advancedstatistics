truckR <-
function(job=1)
{
dump("truckR","c:\\StatBook\\truckR.r")
da=read.csv("c:\\StatBook\\truckR.data.csv")
n=nrow(da);ti=1:n
if(job==1)
{
	par(mfrow=c(1,3),mar=c(4,4,1,1))
	plot(1:n,da$revenue,type="b",xlab="",ylab="",cex=1.25,lwd=3)
	mtext(side=1,"Time",cex=1.25,line=2.5)
	mtext(side=2,"Revenue",cex=1.25,line=2.5)

	plot(1:n,da$truc.dr,type="b",xlab="",ylab="",cex=1.25,lwd=3)
	mtext(side=1,"Time",cex=1.25,line=2.5)
	mtext(side=2,"Truck drives",cex=1.25,line=2.5)

	plot(da$truc.dr,da$revenue,xlab="",ylab="",cex=1.25,lwd=3)
	mtext(side=1,"Truck drivers",cex=1.25,line=2.5)
	mtext(side=2,"Revenue",cex=1.25,line=2.5)
	abline(lsfit(x=da$truc.dr,y=da$revenue),lwd=3)
	r2=cor(da$truc.dr,da$revenue)^2
	text(1.1,240,paste("R-squared =",round(r2,2)),adj=0,font=2,cex=1.75)
}
if(job==2)
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	x=lm(truc.dr~ti,data=da)$residuals
	y=lm(revenue~ti,data=da)$residuals
	plot(x,y,xlab="",ylab="",cex=1.25,pch=16)
	abline(lsfit(x=x,y=y),lwd=3)
	mtext(side=1,"Truck residuals",cex=1.25,line=2.5)
	mtext(side=2,"Revenue residuals",cex=1.25,line=2.5)
	r2=cor(x,y)^2
	text(-1,6,paste("R-squared =",round(r2,3)),adj=0,font=2,cex=1.5)
}
if(job==3) #Generalized coefficient of determination
{
	soFULL=summary(lm(revenue~truc.dr+ti,data=da))
	print(soFULL)
	R2F=soFULL$r.squared
	soNULL=summary(lm(revenue~ti,data=da))
	print(soNULL)
	R20=soNULL$r.squared
	R2G=(R2F-R20)/(1-R20)
	print(paste("R2G =",R2G))


}
}
