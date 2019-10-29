stucost <-
function()
{
dump("stucost","c:\\StatBook\\stucost.r")
#set.seed(73)
#nf=45;ns=23
#fr=round(exp(rnorm(nf,mean=log(21),sd=.3)))
#so=round(exp(rnorm(ns,mean=log(25),sd=.3)))
#da=as.data.frame(cbind(c(fr,so),c(rep("FRE",nf),rep("SOF",ns))))
#names(da)=c("Cost","Student")
#write.csv(da,"c:\\StatBook\\stucost.csv",row.names=F)

da=read.csv("c:\\StatBook\\stucost.csv")
X=da$Cost[da$Student=="FRE"] #freshman cost
nf=length(X)
Y=da$Cost[da$Student=="SOF"] #sofmore cost
ns=length(Y)

stc=c(rep("F",nf),rep("S",ns))
print("Original data, mean comparison:")
Z=c(X-mean(X),Y-mean(Y))
Z=Z/sd(Z);n=nf+ns
stcz=stc[order(Z)]
Z=Z[order(Z)]
par(mfrow=c(1,2))
plot(qnorm((1:n-.5)/n),Z,xlim=c(-3,3),ylim=c(-3,3),type="n",xlab="Theoretical quantile",ylab="Empirical quantile",main="q-q norm for original data")
text(qnorm((1:n-.5)/n),Z,stcz,cex=1.25)
segments(-4,-4,4,4,col=2)
print(t.test(X,Y,var.equal=T))

print("log data, median comparison:")
lX=log(X);lY=log(Y)
Z=c(lX-mean(lX),lY-mean(lY))
Z=Z/sd(Z);n=nf+ns
stcz=stc[order(Z)]
Z=Z[order(Z)]
plot(qnorm((1:n-.5)/n),Z,xlim=c(-3,3),ylim=c(-3,3),type="n",xlab="Theoretical quantile",ylab="Empirical quantile",main="q-q norm for log data")
text(qnorm((1:n-.5)/n),Z,stcz,cex=1.25)
segments(-4,-4,4,4,col=2)
print(t.test(lX,lY,var.equal=T))
print(t.test(lX,lY))
}
