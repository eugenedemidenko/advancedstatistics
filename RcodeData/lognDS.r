lognDS <-
function(nSim=10000000,mu=2,sigma=1)
{
dump("lognDS","c:\\StatBook\\lognDS.r")
X=exp(rnorm(nSim,mean=mu,sd=sigma))
A=exp(mu+sigma^2/2)
cat("Theoretical mean=",A," variance=",A^2*(exp(sigma^2)-1))
emp.mean=mean(X);var.emp=var(X)
cat("\nEmpirical mean=",emp.mean," variance=",var.emp)
}
