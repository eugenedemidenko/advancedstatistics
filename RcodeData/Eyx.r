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
