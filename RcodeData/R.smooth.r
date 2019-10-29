R.smooth <-
function()
{
dump("R.smooth","c:\\StatBook\\R.smooth.r")
print(date())
rd=scan("c:\\StatBook\\R.pgm",what="")
nr=as.numeric(rd[2]);nc=as.numeric(rd[3])
rm=matrix(as.numeric(rd[5:length(rd)]),ncol=nc,nrow=nr)
rm=rm[,seq(from=nc,to=1,by=-1)]
par(mfrow=c(1,3),mar=c(1,1,2,1))
image(1:nr,1:nc,rm,col=gray((0:255)/255),axes=F,xlab="",ylab="")
title("The original 'R' image in the pgm format")
tit2=c("Kernel smoothed, hx=hy=3","Kernel smoothed, hx=hy=10")
k=1
for(h in c(3,10))
{
	hx=h;hy=h
	NR=nr;NC=nc	
	x=seq(from=1,to=nr,length=NR);y=seq(from=1,to=nc,length=NC)
	i=1:nr;j=1:nc
	M1=matrix(dnorm((rep(x,nr)-rep(i,each=NR))/hx)/nr,nrow=NR,ncol=nr)
	M2=matrix(dnorm((rep(y,nc)-rep(j,each=NC))/hy)/nc,nrow=NC,ncol=nc)
	Kxy=M1%*%rm%*%t(M2)
	image(x,y,Kxy,col=gray((0:255)/255),xlab="",ylab="",axes=F)
	title(tit2[k])
	k=k+1
}	

}
