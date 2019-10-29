mn3 <-
function(sq=T,nPoints=100,mu=c(-1,2,3),Omega=matrix(c(3,-1,1,-1,2,1,1,1,2),ncol=3,nrow=3))
{
    dump("mn3","c:\\StatBook\\mn3.r")
    p3=function(X3,theta=15,phi=30,r=10,mainL)
    {
       op=persp(x=range(X3[,1]),y=range(X3[,2]),z=matrix(ncol=2,nrow=2),
       zlim=range(X3[,3]),xlab="x1",ylab="x2",zlab="x3",
       theta=theta,phi=phi,r=r,main=mainL)
       p3=trans3d(x=X3[,1], y=X3[,2], z=X3[,3], pmat=op)
       points(p3,pch=16,cex=1.25)
       n=length(X3[,1])
       p2=trans3d(x=X3[,1], y=X3[,2], z=rep(min(X3[,3]),n), pmat=op)
       segments(p3$x,p3$y,p2$x,p2$y)
       points(p2,pch=4,cex=.5,col=2)
    }
    if(sq)
    {
        egOM=eigen(Omega,symmetric=T) #matrix spectral decomposition
        sqL=diag(sqrt(egOM$values),3,3)
        cOm=egOM$vectors%*%sqL%*%t(egOM$vectors)
    }
    else
     cOm=chol(Omega) # Cholesky factor matrix R
    Z=matrix(rnorm(nPoints*3),ncol=3,nrow=nPoints)
    un=rep(1,nPoints)
    X3=Z%*%cOm+un%*%t(mu)
    thetas=c(45,120,250)
    par(mfrow=c(1,3),mar=c(1,0,2,0))
    for(theta in thetas)
        p3(X3,r=100,theta=theta,mainL=paste("theta =",theta))
    # Testing 
    print("True mu:");print(mu)
    empM=colMeans(X3)
    print("Empirical mu:");print(empM)
    print("True Omega:")
    print(Omega)
    X3c=X3-un%*%t(empM)
    empOmega=t(X3c)%*%X3c/nPoints
    print("Empirical Omega:")
	#print(var(X3)*(nPoints-1)/nPoints)
    empOmega
}
