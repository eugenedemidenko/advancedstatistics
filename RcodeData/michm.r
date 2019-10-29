michm <-
function(job=1,a=100,b=1,x=1:10,sigma=2,ss=3,nSim=1000,alpha=.05,N=100)
{
dump(c("michm","polsum","polmultk","polmult","MMpolyn"),"c:\\StatBook\\michm.r")
n=length(x)
if(job==0) # plot 4 typical samples 
{
	par(mfrow=c(2,2),mar=c(3,3,3,1))
	xx=seq(from=0,to=n,length=100)
	for(i in 1:4)
	{
		y=a*x/(b+x)+rnorm(n,sd=sigma)
		plot(x,y,xlim=c(0,n))
		lines(xx,a*xx/(b+xx),lwd=3)
		o=nls(y~a*x/(b+x),start=c(a=a,b=b))
		ab=coef(o)
		lines(xx,ab[1]*xx/(ab[2]+xx),lwd=3,lty=2)	
	}
}
if(job==1) # Compare nls with the polynomial solution using MMpolyn
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	# Puromycin velocity data, Bates and Watts
	x = c(0.02, 0.02, 0.06, 0.06, 0.11, 0.11, 0.22, 0.22, 0.56, 0.56, 1.1, 1.1)
    y = c(76, 47, 97, 107, 123, 139, 159, 152, 191, 201, 207, 200)
	n=length(x)
	plot(x,y,xlab="",ylab="",xlim=c(0,1.2),ylim=c(0,250))
	mtext(side=1,"x",line=2.5,cex=1.5);mtext(side=2,"y",line=2.5,cex=1.5)
	# Initial guess #1
	o=nls(y~a*x/(b+x),start=c(a=mean(y),b=0))
	ab=coef(o)
	xx=seq(from=0,to=1.2,length=N);xxr=xx[order(-xx)]
	covab=vcov(o)
	j=cbind(xx/(ab[2]+xx),-ab[1]*xx/(ab[2]+xx)^2)
	SEy=sqrt(summary(o)$sigma^2+diag(j%*%covab%*%t(j)))
	Z1a=qnorm(1-alpha/2)
	low=ab[1]*xx/(ab[2]+xx)-Z1a*SEy;up=ab[1]*xx/(ab[2]+xx)+Z1a*SEy
	polygon(x=c(0,xx,xx[N],xxr),y=c(low[1],up,low[N],low[order(-xx)]),col="gray93")
	points(x,y)
	lines(xx,ab[1]*xx/(ab[2]+xx),lwd=3)	
	print(summary(o))
	S.hat=summary(o)$sigma^2*(n-2)
	S.inf=sum(lm(y~x-1)$residuals^2)
	p.slope=1-pchisq(n*log(S.inf/S.hat),df=1)
	S.0=var(y)*(n-1)
	p.const=1-pchisq(n*log(S.0/S.hat),df=1)
	cat("p-value slope =",p.slope,", p-value const =",p.const,"\n")
	r=MMpolyn(x=x,y=y)
	xr=x/(r+x)
	ar=sum(y*xr)/sum(xr^2) #OLS estimate of a 
	cat("\nSolution from MMpolyn: a=",ar,", b =",r,"\n")	
}

if(job==2) # An example of failure to estimate
{
	x=1:5;n=length(x)
	y=c(51.341490,40.469845,4.263204,21.807834,102.533238)
	
	par(mfrow=c(1,2),mar=c(4,4,3,1),cex.main=1.5)
	r=MMpolyn(x=x,y=y)
	sol=as.data.frame(matrix(ncol=3,nrow=2),row.names=c("Solution1","Solution2"))
	sol[,2]=r
	names(sol)=c("a","b","RSS")
	xr=x/(x+r[1]);sol[1,1]=sum(y*xr)/sum(xr^2);sol[1,3]=sum((y-sol[1,1]*xr)^2)
	xr=x/(x+r[2]);sol[2,1]=sum(y*xr)/sum(xr^2);sol[2,3]=sum((y-sol[2,1]*xr)^2)
	cat("Solution from MMpolyn:\n");print(sol)
	S.inf=sum(y^2)-sum(y*x)^2/sum(x^2)
	cat("RSS at infinity =",S.inf,"\n")	
	plot(x,y,xlim=c(0,n),cex=1.5,pch=16,xlab="",ylab="",main="False NLS fit")
	legend(0,100,c("Local minimum fit","Local maximum fit"),lty=1:2,lwd=3,bg="gray90",cex=1.5)
	mtext(side=1,"x",cex=1.25,line=2.5)
	mtext(side=2,"y",cex=1.25,line=2.5)
	xx=seq(from=0,to=n,length=100)	
	for(j in 1:length(r))
	{
		xr=x/(x+r[j])
		ar=sum(y*xr)/sum(xr^2)
		lines(xx,ar*xx/(r[j]+xx),lwd=3,lty=j)
	}		
	o=try(nls(y~a*x/(b+x),start=c(a=100,b=3)))
	if(attr(o,"class")!="try-error") 
	{
		print(summary(o))
		ab=coef(o)
		lines(xx,ab[1]*xx/(ab[2]+xx),lwd=3,lty=2)	
	}
	rs=RSS=seq(from=0,to=3,length=1000)
	for(i in 1:1000)
	{
		xr=x/(x+rs[i])
		ar=sum(y*xr)/sum(xr^2)
		RSS[i]=sum((y-ar*x/(x+rs[i]))^2)			
	}
	plot(rs,RSS,type="l",lwd=3,xlab="",ylab="",main="Residual sum of squares")
	mtext(side=1,"b",cex=1.25,line=2.5)
	mtext(side=2,"RSS",cex=1.25,line=2.5)
	RSS.fit=rep(0,2)
	for(j in 1:2)
	{
		xr=x/(x+r[j])
		ar=sum(y*xr)/sum(xr^2)
		RSS.fit[j]=sum((y-ar*x/(x+r[j]))^2)			
	}
	points(r,RSS.fit,pch=16:17,cex=2)
	for(j in 1:2) segments(r[j],-1,r[j],RSS.fit[j],lty=2)
	legend(.5,5540,c("Local minimum","Local maximum"),pch=16:17,bg="gray90",cex=1.75)
}

}
polsum <-
function(a,b) # sum of two polynomials
{ 
n=length(a);m=length(b)
#a[1]+a[2]*x+...+a[n]*x^(n-1)
#b[1]+b[2]*x+...+b[m]*x^(m-1)

if(n>=m){d=a;d[1:m]=d[1:m]+b}
else {d=b;d[1:n]=d[1:n]+a}
d
}
polmultk <-
function(a,b,k) #multiplication of a polynomial on b*x^k
{
#a[1]+a[2]*x+...+a[n]*x^(n-1) times b*x^k
n=length(a)
d=rep(0,n+k)
d[(k+1):(n+k)]=a*b
d
}
polmult <-
function(a,b) #multiplication of two polynomials
{
#a[1]+a[2]*x+....+a[n]*x^(n-1) times b[1]+b[2]*x+....+b[m]*x^(m-1)
n=length(a);m=length(b)
d=rep(0,n+m-1)
for(i in 1:m)
d=polsum(d,polmultk(a=a,b=b[i],k=i-1))
d
}
MMpolyn <-
function(x,y)
{
n=length(x)
#returns all positive solutions of the normal equation in the Michaelis-Menten regression model

#compute coefficients in the ascending power form of a 4*(n-1)-1 polynomial as the solution of the normal equation in the Michaelis-Menten regression model
P=Q=R=H=0
for(i in 1:n)
{
	p=q=r=h=1
	for(j in 1:n)
	if(j!=i)
	{
		p=polmult(a=p,b=c(x[j]^2,2*x[j],1))					
		q=polmult(a=q,b=c(x[j]^2,2*x[j],1))		
		r=polmult(a=r,b=c(x[j],1))		
		h=polmult(a=h,b=c(x[j]^3,3*x[j]^2,3*x[j],1))		
	}
	P=polsum(P,polmult(a=x[i]*y[i],b=p))
	Q=polsum(Q,polmult(a=x[i]^2,b=q))
	R=polsum(R,polmult(a=x[i]*y[i],b=r))
	H=polsum(H,polmult(a=x[i]^2,b=h))				
}
P1=polmult(a=P,b=Q);P2=polmult(a=R,b=H)
POL=polsum(P1,-P2)
POL=POL[1:(length(POL)-1)] # the last coefficient = 0

r=polyroot(POL) #find all roots, real and complex
r=Re(r[abs(Im(r))<0.0000001]) # keep only real roots 
r=r[r>0] # keep only positive roots
return(r)
}
