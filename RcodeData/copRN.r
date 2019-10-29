copRN <-
function(N=100)
{
dump("copRN","c:\\StatBook\\copRN.r")
exRN=function(x,y,ro)
	1/sqrt(2*pi*(1-ro^2))*exp(.5*qnorm(y)^2-.5/(1-ro^2)*(x^2-2*ro*x*qnorm(y)+qnorm(y)^2))
EYx=function(x,y,ro)
	y/sqrt(1-ro^2)*exp(.5*x^2+.5*qnorm(y)^2-.5/(1-ro^2)*(x^2-2*ro*x*qnorm(y)+qnorm(y)^2)) 
x=seq(from=-2.5,to=2.5,length=N)
y=seq(from=0,to=1,length=N)
par(mfrow=c(1,3),mar=c(3,3,3,1))
regr=rep(NA,N)
for(ro in c(-0.7,0,0.7))
{
	fxy=outer(x,y,ro=ro,FUN=exRN)
	contour(x,y,fxy,main=paste("ro =",ro))
	for(i in 1:N)
		regr[i]=integrate(EYx,x=x[i],ro=ro,lower=0,upper=1)$value
	lines(x,regr,lwd=3)	
}

}
