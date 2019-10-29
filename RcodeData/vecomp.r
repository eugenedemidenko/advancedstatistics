vecomp <-
function(job=1,N=10000)
{
dump("vecomp","c:\\StatBook\\vecomp.r")
#use 5 methods to compute exp(-(3*x^2-4*x*y+2*y^2)) for integral approximation
x=seq(from=-1,to=1,length=N);y=seq(from=-1,to=1,length=N)
M=matrix(ncol=N,nrow=N)
one=rep(1,N)
print(date())
if(job==1) # double loop
{	
	for(i in 1:N)
	for(j in 1:N)
	M[i,j]=3*x[i]^2-4*x[i]*y[j]+2*y[j]^2	
}
if(job==2) # single loop
{
	for(i in 1:N)
		M[i,]=3*x[i]^2-4*x[i]*y+2*y^2	
}
if(job==3) #matrix compputation
{	
	M=3*x^2%*%t(one)-4*x%*%t(y)+2*one%*%t(y^2)	
}
if(job==4) # double grid 
{	
	xx=rep(x,N);yy=rep(y,each=N)
	M=matrix(3*xx^2-4*xx*yy+2*yy^2,ncol=N,nrow=N)	
}
if(job==5)  # expand grid 
{	
	xy=expand.grid(x,y)
	M=matrix(3*xy[,1]^2-4*xy[,1]*xy[,2]+2*xy[,2]^2,ncol=N,nrow=N)
}
if(job==6) # outer
{
	qf=function(x,y) 3*x^2-4*x*y+2*y^2	
	M=outer(X=x,Y=y,FUN=qf)
}
print(date())	
M=exp(-M)
x2y2=x^2%*%t(one)+one%*%t(y^2)
INT=sum(M[x2y2<1])*(max(x)-min(x))*(max(y)-min(y))/N^2
return(INT)
}
