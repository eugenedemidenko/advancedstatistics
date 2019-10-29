multB <-
function(x=c(1,5,3),p1=.3,p2=.5,nSim=100000)
    {
       dump("multB","c:\\StatBook\\multB.r")
       p3=1-p1-p2
       n=sum(x)
       X=matrix(0,nrow=nSim,ncol=3)
       ik=1:3
       for(i in 1:n) # the loop over Bernoulli trials
       {
          for(isim in 1:nSim) # the loop over simulations
          {
             icat=sample(x=1:3,size=1,replace=T,prob=c(p1,p2,p3))
             X[isim,icat]=X[isim,icat]+1
          }
       }
       prSim=mean(X[,1]==x[1] & X[,2]==x[2] & X[,3]==x[3])
       prTheor=factorial(n)/factorial(x[1])/factorial(x[2])/factorial(x[3])
       prTheor=prTheor*p1^x[1]*p2^x[2]*p3^x[3]
       cat("Simulated prob =",prSim,"\nTheoretical prob =",prTheor,"\n")
    }
