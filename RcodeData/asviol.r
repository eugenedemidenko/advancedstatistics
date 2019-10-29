asviol <-
function () 
{
dump("asviol","c:\\StatBook\\asviol.r")
#CDF statistical comparison
sv=scan("c:\\StatBook\\Vermont.txt",what="")
sv=as.numeric(sv);sv=sv[!is.na(sv)]
sc=scan("c:\\StatBook\\Connecticut.txt",what="")
sc=as.numeric(sc);sc=sc[!is.na(sc)]
svL=log10(sv/1000);scL=log10(sc/1000)
saly=c(20,30,40,60,100,150,250);Lsaly=log10(saly)
ns=length(saly)
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(1,1,xlim=c(-1,1),ylim=range(Lsaly),type="n",axes=F,xlab="",ylab="")
mtext(side=2,"Salary, $1000",cex=1.5,line=2.5)
title("Asymmetric salary violin plot")
text(-.5,log10(150),"Vermont",font=2);text(.5,log10(150),"Connecticut",font=2)
axis(side=2,Lsaly,labels=saly,srt=90)
dv=density(svL,from=Lsaly[1],to=Lsaly[ns],bw=.03)
nd=length(dv$x)
dc=density(scL,from=Lsaly[1],to=Lsaly[ns],bw=.03)
maxD=max(c(dv$y,dc$y))
segments(0,Lsaly[1],0,Lsaly[ns],lwd=3)
segments(rep(0,length(svL))-.05,svL,rep(0,length(svL)),svL,col=3)
segments(rep(0,length(scL))+.05,scL,rep(0,length(scL)),scL,col=2)
#polygon(x=c(dv[1],dv$
lines(-dv$y/maxD,dv$x,col=3,lwd=3);lines(dc$y/maxD,dc$x,col=2,lwd=3)

}
