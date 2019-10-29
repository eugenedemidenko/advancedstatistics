benfordFT <-
function(job=1,nSim=10000,lambda=.0001,n0=10)
{
dump("benfordFT","c:\\StatBook\\benfordFT.r")
load("c:\\StatBook\\sales.Rdata")
head(sales)
y=sales$Val;z=sales$Insp
z=z[!is.na(y)]
y=y[!is.na(y)]
xok=y[z=="ok"]
xfraud=y[z=="fraud"]
orok=floor(log10(xok))
dok=floor(xok/10^orok)	
orfraud=floor(log10(xfraud))
dofraud=floor(xfraud/10^orfraud)	
xun=y[z=="unkn"]
orunk=floor(log10(xun))
dounk=floor(xun/10^orunk)	

prdok=prdfraud=prdunkn=rep(1,9)
for(i in 1:9)	
{
	prdok[i]=mean(dok==i)
	prdfraud[i]=mean(dofraud==i)
	prdunkn[i]=mean(dounk==i)
}
bpr=log10(1+1/(1:9))
par(mfrow=c(1,1),mar=c(3.5,3.5,1,1))
matplot(1:9,cbind(bpr,prdok,prdfraud,prdunkn),type="l",lwd=3,col=1,ylim=c(0,.5),axes=F,xlab="",ylab="")
mtext(side=1,"First significant digits",cex=1.5,line=2.4)
mtext(side=2,"Probability",cex=1.5,line=2.25)
axis(side=1,1:9)
axis(side=2,seq(from=0,to=.5,by=.05),srt=90)
print(cbind(1:9,bpr,prdok,prdfraud,prdunkn))
legend(7,.5,c("Benford","OK","Frudulent","Unknown"),lty=1:4,lwd=3,cex=1.5,bg=gray(.9))
}
