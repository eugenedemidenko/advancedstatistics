block.inv <-
function(m=5,k=3)
{
dump("block.inv","c:\\StatBook\\block.inv.r")
M=matrix(rnorm(m^2),ncol=m,nrow=m)
M=t(M)%*%M #we need a symmetric matrix
iM=solve(M)
iM11=iM[1:k,1:k]
print(iM11)
E=M[(k+1):m,(k+1):m]-t(M[1:k,(k+1):m])%*%solve(M[1:k,1:k])%*%M[1:k,(k+1):m]
F=solve(M[1:k,1:k])%*%M[1:k,(k+1):m]
iM11.form=solve(M[1:k,1:k])+F%*%solve(E)%*%t(F)
print(iM11.form)
}
