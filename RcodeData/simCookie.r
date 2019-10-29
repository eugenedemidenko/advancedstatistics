simCookie <-
function(n=3,m=4,nSim=100000)
{
dump("simCookie","c:\\StatBook\\simCookie.r")
pr=0 #initialize counter
for(isim in 1:nSim) #simulations
{
    x=runif(n,min=0,max=m) # location of raisins in the dough
    for(i in 1:m) # loop over cookies
    {
        mi=length(x[x<i & x>i-1]) #number of cookies
        if(mi==n) #all cookies are here
        {
            pr=pr+1 # counter
            break
        }
    } 
}
pr/nSim #return the propprtion
}
