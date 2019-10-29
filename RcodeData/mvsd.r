mvsd <-
function(x,p)
{
	dump("mvsd","c:\\StatBook\\mvsd.r")
    # x is the vector of values
    # p is the vector of probabilities
    n<-length(x) # recover size
    mux<-sum(x*p) # mean
    ex2<-sum(x^2*p) #E(X^2)
    s2x<-ex2-mux^2 # alternative variance
    sdx<-sqrt(s2x) # SD
    return(c(mux,s2x,sdx)) # return the triple
 }
