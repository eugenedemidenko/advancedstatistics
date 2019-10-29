autocrash <-
function(kappa=2*pi*10,acof=4,nk=200)
{
dump("autocrash","c:\\StatBook\\autocrash.r")
bes0=function(theta,mu=0,kappa) exp(kappa*cos(theta-mu))/(2*pi)
I0=integrate(bes0,kappa=kappa,lower=0,upper=2*pi)$value
da=read.csv("c:\\StatBook\\autocrash.csv",header=T)
day=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
hour=da$TIME_OF_DAY/100
hf=floor(hour)
hour=hf+(hour-hf)*10/6 #convert to decimal
w=da$DAY_OF_WEEK
colw=c("red","deeppink","darkmagenta","blue","cyan","green","coral")
tt=seq(from=0,to=2*pi,length=200)
par(mfrow=c(1,1),mar=c(.1,.1,.1,.1))
plot(1,1,xlim=c(-2.5,2),ylim=c(-2,2),type="n",xlab="",ylab="",axes=F)
lines(cos(tt),sin(tt),lwd=3)
polygon(cos(tt),sin(tt))
for(i in 1:24)
{
	ii=i
	ci=cos(pi/2-pi/12*ii);si=sin(pi/2-pi/12*ii)
	segments(.9*ci,.9*si,ci,si)
	text(.85*ci,.85*si,ii,font=2,cex=1.25)
}

th=seq(from=0,to=2*pi,length=nk)
kd=rep(0,nk)
for(id in 1:7)
{
	hid=hour[w==id]
	theta=(pi*(30-hid)/12)%%(2*pi)# convert hours to polar angles
	for(i in 1:nk)
		kd[i]=mean(exp(kappa*cos(th[i]-theta))/(2*pi*I0))
	rk=range(kd)
	amplf=1+kd*acof	
	lines(cos(th)*amplf,sin(th)*amplf,col=colw[id],lwd=3)
}
legend(-2.5,2,day,col=colw,lty=1,lwd=4,cex=1.2,bg="gray90")

}
