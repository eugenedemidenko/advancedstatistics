cubMLE <-
function(N=500,theta=.5,sigma=1,nSim=100000)
{
dump("cubMLE","c:\\StatBook\\cubMLE.r")
par(mfrow=c(1,2),mar=c(4,4,3,1))
n=2
x=runif(n)
Ey=theta+theta^2*x
a=2*sum(x^2);b=3*sum(x)
y1=seq(from=Ey[1]-3*sigma,to=Ey[1]+3*sigma,length=N)
y2=seq(from=Ey[2]-3*sigma,to=Ey[2]+3*sigma,length=N)
uN=rep(1,N)
nd=1/(2*pi)*exp(-.5*(y1-Ey[1])^2%*%t(uN)-.5*uN%*%t((y2-Ey[2])^2))
image(y1,y2,nd,col=gray((0:255)/255),xlab="",ylab="")
title("Bivariate normal density and contours of the D function")
mtext(side=1,"y1",line=2.5,cex=1.5);mtext(side=2,"y2",line=2.5,cex=1.5)
cc=n-2*y1%*%t(rep(x[1],N))-2*rep(x[2],N)%*%t(y2)
d=-uN%*%t(y2)-y1%*%t(uN)
D=18*a*b*cc*d-4*b^3*d+b^2*cc^2-4*a*cc^3-27*a^2*d^2
contour(y1,y2,D,add=T,col="yellow")
contour(y1,y2,D,add=T,level=0,lwd=4,col="yellow")
points(Ey[1],Ey[2])
ce=2-2*x[1]*Ey[1]-2*x[2]*Ey[2]
de=-Ey[1]-Ey[2]
A=c(de,ce,b,a)
x=polyroot(A)
print("Roots at the mean value:")
print(x)
# Simulations

n=seq(from=2,to=20,by=1)
NN=length(n)
pr=matrix(0,ncol=2,nrow=NN)
theta=c(-.5,.5)
for(it in 1:2)
for(i in 1:NN)
{
	X=matrix(runif(n[i]*nSim),ncol=n[i])
	Y=theta[it]+theta[it]^2*X+sigma*matrix(rnorm(n[i]*nSim),ncol=n[i])
	a=2*rowSums(X^2);b=3*rowSums(X)
	cc=n[i]-2*rowSums(X*Y);d=-rowSums(Y)
	D=18*a*b*cc*d-4*b^3*d+b^2*cc^2-4*a*cc^3-27*a^2*d^2
	pr[i,it]=mean(D<=0)		
}
matplot(n,pr,type="b",col=1,xlab="",ylab="")
title("Probability of a unique solution of the score equation")
mtext(side=1,"n",line=2.5,cex=1.5);mtext(side=2,"Probability",line=2.5,cex=1.5)
legend(10,.7,c("1: theta = -0.5","2: theta = 0.5"),bg=gray(.9),cex=1.75)
}
