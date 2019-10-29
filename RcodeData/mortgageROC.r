mortgageROC <-
function()
{
dump("mortgageROC","c:\\StatBook\\mortgageROC.r")
da=read.csv("c:\\StatBook\\mortgageROC.csv",header=T)
FamilyIncome=log10(da$FamilyIncome)
Default=da$Default
Y=FamilyIncome[Default=="yes"]
X=FamilyIncome[Default=="no"]
par(mfrow=c(1,2),mar=c(4,4,4,1))

salval=c(15,50,70,100,300)
Lxlab=log10(salval);Lr=range(Lxlab)
muY=mean(Y);sdY=sd(Y)
muX=mean(X);sdX=sd(X)
x=seq(from=Lr[1],to=Lr[2],length=200)
dY=dnorm(x,mean=muY,sd=sdY);dX=dnorm(x,mean=muX,sd=sdX)
matplot(x,cbind(dY,dX),xlim=Lr,ylim=c(0,3),lty=1:2,type="l",col=1,lwd=3,xlab="",ylab="",axes=F)
rug(Y,lty=1,ticksize=.075);rug(X,lty=2,ticksize=.05)
mtext(side=1,"Family income, thousand dollars",cex=1.4,line=2.75)
mtext(side=2,"Density distribution",cex=1.4,line=2.75)
mtext(side=3,"Family income for defaulters and nondefaulters",cex=1.3,font=2,line=2.75)
axis(side=1,Lxlab,labels=as.character(salval))
axis(side=2,seq(from=0,to=3,by=.5))
legend(log10(60),3,c("Defaulters","Nondefaulters"),lty=1:2,lwd=3,bg="gray93",cex=1.5)
adi=abs(dX-dY)
x.int=x[adi==min(adi)];d.int=dnorm(x.int,mean=muY,sd=sdY)
points(x.int,d.int,cex=2)
segments(x.int,-1,x.int,d.int,col="gray93")


n=length(Default)
ind=rep(0,n)
ind[Default=="yes"]=1
XY=FamilyIncome
ind=ind[order(XY)];XY=XY[order(XY)]
sens=comp.spec=rep(0,n)
AUC=0
for(i in 1:n)
{
	sens[i]=sum(XY<XY[i] & ind==1)/sum(ind==1)
	comp.spec[i]=sum(XY<XY[i] & ind==0)/sum(ind==0)
	if(i>1) AUC=AUC+sens[i]*(comp.spec[i]-comp.spec[i-1])
}

plot(comp.spec,sens,type="s",axes=F,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",lwd=2)
F1=pnorm(x,mean=muY,sd=sdY);F2=pnorm(x,mean=muX,sd=sdX)
lines(F2,F1,lwd=3)
segments(0,0,1,1);segments(0,1,1,0,lty=2)
mtext(side=1,"False positive (1-specificity)",cex=1.4,line=2.75)
mtext(side=2,"Sensitivity",cex=1.4,line=2.75)
Lspec=pnorm(log10(salval),mean=muX,sd=sdX)
axis(side=3,Lspec,labels=as.character(salval))
axis(side=1,seq(from=0,to=1,by=.1));axis(side=2,seq(from=0,to=1,by=.1))
mtext("ROC curve for identification of mortgage defaulters",line=2.75,font=2,cex=1.3)
text(.55,.15,paste("AUC=",round(100*AUC,1),"%",sep=""),font=3,cex=1.75)
term1=muX*sdY^2-muY*sdX^2
undsq=term1^2-(sdY^2-sdX^2)*(sdY^2*muX^2-sdX^2*muY^2+sdX^2*sdY^2*log(sdX^2/sdY^2))
u.opt=(term1-sqrt(undsq))/(sdY^2-sdX^2)
Fx.opt=pnorm((u.opt-muX)/sdX);Fy.opt=pnorm((u.opt-muY)/sdY)
points(Fx.opt,Fy.opt,cex=2);segments(Fx.opt,-1,Fx.opt,1.2,col="gray50")
print(paste("Min tot error threshold=",round(10^u.opt)))
x=c(0,.5);lines(x,Fy.opt+(x-Fx.opt),col="gray50")

u.opt=(sdX*muY+sdY*muX)/(sdX+sdY)
Fx.opt=pnorm((u.opt-muX)/sdX);Fy.opt=pnorm((u.opt-muY)/sdY)
points(Fx.opt,Fy.opt,pch=2,cex=2);segments(Fx.opt,-1,Fx.opt,1.2,col="gray50")
print(paste("Min sens=spec threshold=",round(10^u.opt)))
legend(.5,.8,c("Minimum total error","Sensitivity=specificity"),pch=1:2,cex=1.25,bg="gray90")

AUC.th=pnorm((muX-muY)/sqrt(sdX^2+sdY^2))
paste("Binormal AUC=",round(100*AUC.th,1),"%",sep="")
}
