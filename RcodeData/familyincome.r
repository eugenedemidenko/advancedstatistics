familyincome <-
function()
{
dump("familyincome","c:\\StatBook\\familyincome.r")
Y=scan("c:\\StatBook\\familyincome.txt")
m=sum(Y<15)
n=length(Y)
pr10=m/n
m1=qbinom(.025,size=n,prob=1/10)
m2=qbinom(.975,size=n,prob=1/10)
cat("Proportion < $15K =",pr10,", #families < $15K = m =",m,"\nm1 = ",m1," m2 =",m2,"\n")
}
