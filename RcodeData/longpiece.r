longpiece <-
function(nExp=100000)
{
	dump("longpiece","c:\\StatBook\\longpiece.r")
	X=runif(nExp)
	X.long=X #initialization
	X.long[X<0.5]=1-X[X<0.5] # if X is short take the other part
	X.short=1-X.long # by definition
	
	X.long=pmax(X,1-X)
    X.short=pmin(X,1-X)
	
	pr=mean(X.long>2*X.short) # proportion of experiments
	pr
}
