salmark <-
function(NG=200)
{
dump(c("salmark","bvn.density.my","Eyx"),"c:\\StatBook\\salmark.r")
d=read.csv("c:\\StatBook\\Forbes2000.csv",stringsAsFactors=F)
par(mfrow=c(1,1),mar=c(4,4,1,1))
s=log10(d$sales);m=log10(d$marketvalue)
n=length(s)
plot(s,m,xlab="",ylab="",axes=F,ylim=c(-2,3),xlim=c(-2,3))
mtext(side=1,"Sales, billions of dollars",cex=1.3,line=2.75,font=2)
mtext(side=2,"Market value, billions of dollars",cex=1.3,line=2.75,font=2)
dol=c("$10M","$100M","$1B","10B","$100B","$300B")
axis(side=1,at=c(-2,-1,0,1,2,3),labels=dol)
axis(side=2,at=c(-2,-1,0,1,2,3),labels=dol)
text(-2,3,"2000 Forbes world's biggest companies",font=4,cex=2,adj=0)
rug(side=1,s,col=3);rug(side=2,m,col=3)
ds=density(s);lines(ds$x,ds$y-2-.04*(3+2),col=4,lwd=2)
dm=density(m);lines(-2-.04*(3+2)+dm$y,dm$x,col=4,lwd=2)
xg=yg=seq(from=-2,to=3,length=NG)
hx=hy=.3
kxy=bvn.density.my(x.data=s,y.data=m,x=xg,y=yg,hx=hx,hy=hy)
contour(xg,yg,kxy,add=T,col=2,lwd=2)
points(s,m)
locr=Eyx(yx=cbind(m,s),ro=cor(s,m),x=seq(from=-1,to=2,length=100),hx=hx,hy=hy)
lines(locr[,1],locr[,2],lwd=2)
lines(locr[,1],locr[,2]+1.96*locr[,3])
lines(locr[,1],locr[,2]-1.96*locr[,3])
lines(lowess(s,m),lwd=2,lty=2)
}
bvn.density.my <-
function(x.data,y.data,x,y,hx,hy,ro=cor(x.data,y.data))
{
    #x.data and y.data are coordinates of n data points
    # x and y are coordinates of the grid where f=kernel density is evaluated
    dump("bvn.density.my","c:\\StatBook\\bvn.density.my.r")
    n=length(x.data);nx=length(x);ny=length(y)
	ey=rep(1,ny);ex=rep(1,nx)
	Kxy=matrix(0,nrow=nx,ncol=ny)
	for(i in 1:n)
	{
		xi=(x-x.data[i])/hx;yi=(y-y.data[i])/hy
		earg=-.5/(1-ro^2)*(xi^2%*%t(ey)-2*ro*xi%*%t(yi)+ex%*%t(yi^2))
		Kxy=Kxy+exp(earg)
	}
	Kxy=Kxy/n/2/pi/hx/hy/sqrt(1-ro^2)
    return(Kxy)
}
Eyx <-
function(yx,ro=0,x,hx=1,hy=1)
{
# Computes the nonparametric conditional mean E(Y|X=x) and its SE
# yx is the nx2 data point (y,x)
# x=array for which E(Y|X=x) is evaluated
# hx,hy bandwidth for y and x
dump("Eyx","c:\\StatBook\\Eyx.r")
Nx=length(x)
eNx=rep(1,Nx);en=rep(1,nrow(yx))
M=(x%*%t(en)-eNx%*%t(yx[,2]))/hx
fi=dnorm(M);den=fi%*%en
Eyx=((eNx%*%t(yx[,1])+ro*hy*M)*fi)%*%en/den
SEyx=sqrt((1-ro^2)*hy^2+((Eyx%*%t(en)-eNx%*%t(yx[,1])-ro*hy*M)^2*fi)%*%en/den)
return(cbind(x,Eyx,SEyx))
}
