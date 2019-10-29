varq <-
function(n,alpha,eps=1e-05,maxit=100)
    {
    n1=n-1
    q1=qchisq(alpha/2,df=n1)
    q2=qchisq(1-alpha/2,df=n1)
    M=matrix(ncol=2,nrow=2)
    m=rep(NA,2)
    for(it in 1:maxit)
    {
       m[1]=n1*(log(q2)-log(q1))-(q2-q1)
       m[2]=pchisq(q1,df=n1)-pchisq(q2,df=n1)+(1-alpha)
       M[1,1]=n1/q1-1
       M[1,2]=-n1/q2+1
       M[2,1]=-dchisq(q1,df=n1)
       M[2,2]=dchisq(q2,df=n1)
       delta=solve(M)%*%m
       if(max(abs(delta))<eps)break
       q1=q1+delta[1]
       q2=q2+delta[2]
    }
    return(c(q1,q2))
    }
