nLINEXP <-
function(a=1,b=0.1,x0=0,eps =1e-006,maxit=100,pr=1)
# Solves exp(-a*x)=b*x; x0=starting point; eps=relative accuracy;
# maxit=maximum number of iterations; pr=0 no intermediate printout
{
    dump("nLINEXP","c:\\StatBook\\nLINEXP.r")
    for(iter in 1:maxit) 
    {
		e0<-exp(-a*x0) # compute the exponent
		x1<-x0+(e0-b*x0)/(a*e0+b) # Newton iteration
		rel<-abs(x1-x0)/(1+abs(x0)) # relative difference for convergence
		if(rel < eps) break
		x0<-x1 # set the previous iteration as the current one
		if(pr) print(c(iter,x0,e0-b*x0,rel))
    }
 x1
 }
