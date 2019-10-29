bufprob <-
function(n=50,ro.true=1.5,ss=4)
{
	dump("bufprob","c:\\StatBook\\bufprob.r")
	set.seed(ss)
#simulate n throws
	nc=runif(n);ang=pi*runif(n)
	y0=-ro.true/2*sin(ang)+nc;y1=ro.true/2*sin(ang)+nc
	prop=mean(y0<0 | y0>1 | y1<0 | y1>1)			
	if(prop<2/pi) 
	{
		ro.ML=pi*prop/2
	}
	else
	{
		ro.ML=1
		for(it in 1:10)
		{
			num=2/pi*acos(1/ro.ML)+2*ro.ML/pi*(1-sqrt(1-1/ro.ML^2))-prop
			den=2/pi*(ro.ML-sqrt(ro.ML^2-1))/ro.ML
			ro.ML.new=ro.ML-num/den
			if(abs(ro.ML.new-ro.ML)<.0001) break
			#print(c(it,ro.ML,num))	
			ro.ML=ro.ML.new
		}		
	}
return(c(prop,ro.ML))
}
