consIR <-
function()
{
dump("consIR","c:\\StatBook\\consIR.r")
#set.seed(3)
#N=11;n=326;sigma=5
#mu=round(runif(N,min=13,max=100))
#nu=round(runif(n=n,min=-10,max=10))
#y=matrix(ncol=n,nrow=N)
#for(i in 1:N)
#for(j in 1:n)
#y[i,j]=round(mu[i]+nu[j]+rnorm(1,sd=sigma))
#y[y>100]=100;y[y<0]=0
#nam=LETTERS[1:N]
#radio=rep(nam,n)
#custid=rep(1:n,each=N)
#score=as.vector(y)
#write.csv(cbind(custid,radio,score),"c:\\StatBook\\consIR.csv",row.names=F)
d=read.csv("c:\\StatBook\\consIR.csv",header=T)
print(d)
N=11;n=326
y=matrix(d[,3],nrow=N,ncol=n)
nam=unique(d[,2])

par(mfrow=c(1,2),mar=c(4,4,2,1))

plot(1,1,xlim=c(1,N),type="n",ylim=c(0,100),axes=F,xlab="",ylab="")
axis(side=1,1:N,nam)
axis(side=2,seq(from=0,to=100,by=10))
mtext(side=1,"Internet radio brands",cex=1.5,line=2.75)
mtext(side=2,"Consumer scores",cex=1.5,line=2.75)
mtext(side=3,"Testing Internet radio quality",cex=1.5,line=0,font=2)
w=.1
for(i in 1:N)
{
	mi=mean(y[i,]);sdi=sd(y[i,])
	points(i,mi,pch=16,cex=2)
	segments(i,mi-sdi,i,mi+sdi)
	segments(i-w,mi-sdi,i+w,mi-sdi)
	segments(i-w,mi+sdi,i+w,mi+sdi)
}
# part(a)
y.bar=mean(y);yi=rowMeans(y);yj=colMeans(y) #means
Smin=0;S0=0
for(i in 1:N)
for(j in 1:n)
{
	Smin=Smin+(y[i,j]+y.bar-yi[i]-yj[j])^2
	S0=S0+(y[i,j]+y.bar-yj[j])^2
}
Fobs=(S0-Smin)/(N-1)/(Smin/(N*n-N-(n-1)))
pv=pf(Fobs,df1=N-1,df2=N*n-N-(n-1),lower.tail=F)
print(pv)

for(i in 1:N)	y[i,]=y[i,]-mean(y[i,])
matplot(1:n,t(y),type="l",lty=1,col=1,xlim=c(1,n),ylim=c(-30,30),axes=F,xlab="",ylab="")	
segments(-1,0,n,0,lwd=3,col="white")
axis(side=1,c(1,50,100,150,200,250,300,n))
axis(side=2,seq(from=-30,to=30,by=10))
mtext(side=2,"Deviations from the means",cex=1.5,line=2.75)
mtext(side=1,"Consumer id",cex=1.5,line=2.75)
mtext(side=3,"Testing consumer homogeneity",cex=1.5,line=0,font=2)
# part(b)
S0=0
for(i in 1:N)
for(j in 1:n)
	S0=S0+(y[i,j]-yi[i])^2

Fobs=(S0-Smin)/(n-1)/(Smin/(N*n-N-(n-1)))
pv=pf(Fobs,df1=n-1,df2=N*n-N-(n-1),lower.tail=F)
print(pv)
}
