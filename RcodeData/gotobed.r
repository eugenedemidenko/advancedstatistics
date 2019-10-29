gotobed <-
function()
{
dump("gotobed","c:\\StatBook\\gotobed.r")
bes0=function(theta,mu=0,kappa) exp(kappa*cos(theta-mu))/(2*pi)
bes1=function(theta,mu=0,kappa) cos(theta-mu)*exp(kappa*cos(theta-mu))/(2*pi)
bes2=function(theta,mu=0,kappa) cos(theta-mu)^2*exp(kappa*cos(theta-mu))/(2*pi)

hour=c(23.1,0.3,22.6,21.5,23.7,1.2,0.7,22.1,23.3)
n=length(hour)
theta=(pi*(30-hour)/12)%%(2*pi)# convert hours to polar angles
ct=cos(theta);st=sin(theta)
sq=sqrt(sum(ct)^2+sum(st)^2)
R.bar=sq/n
mu.ML=acos(sum(ct)/sq)
kappa.ML=0.5/(1-R.bar)
hgotobed=30-12*mu.ML/pi #back convert to hours
print(c(mu.ML,hgotobed,R.bar,kappa.ML))
for(it in 1:10)
{
	I0=integrate(bes0,kappa=kappa.ML,lower=0,upper=2*pi)$value
	I1=integrate(bes1,kappa=kappa.ML,lower=0,upper=2*pi)$value
	I2=integrate(bes2,kappa=kappa.ML,lower=0,upper=2*pi)$value
	delta=(I1-R.bar*I0)/(I2-R.bar*I1)
	if(abs(delta)<0.00001) break
	kappa.ML=kappa.ML-delta
	print(c(it,kappa.ML,I1-R.bar*I0))
}

prgb=integrate(bes0,kappa=kappa.ML,mu=mu.ML,lower=0,upper=pi/2)$value/I0
return(prgb)

}
