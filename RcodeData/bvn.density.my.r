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
