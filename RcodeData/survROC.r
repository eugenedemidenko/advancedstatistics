survROC <-
function()
{
dump("survROC","c:\\StatBook\\survROC.r")
da=read.csv("c:\\StatBook\\survcanc.csv",header=T)
ind=da[,2];st=da[,1] #0=conventional, 1=new
st1=st[ind==0];st2=st[ind==1]
n1=length(st1);n2=length(st2)
st2=st2[order(st2)]
par(mfrow=c(1,2),mar=c(3.5,3.5,3,1),cex.main=2)
cl="gray70"
plot(st2,seq(from=n2,to=1,by=-1)/n2,type="s",lwd=2,col=cl,xlab="",ylab="")
mtext(side=1,"Time to death, month",cex=1.4,line=2.5)
mtext(side=2,"Proportion alive",cex=1.4,line=2.5)
title("Survival curves")

st1=st1[order(st1)]
lines(st1,seq(from=n1,to=1,by=-1)/n1,type="s",lwd=2)
legend("topright",c(paste("Conventional chemotherapy, n =",n1),paste("New cancer drug, n =",n2)),lty=1,lwd=2,col=c(1,cl),bg="gray90",cex=1.4)

ind=ind[order(st)]
st=st[order(st)]
n=n1+n2
sens=spec=rep(0,n)
AUC=0
for(i in 1:n)
{	
	sens[i]=sum(st>st[i] & ind==1)/sum(ind==1) # live longer from new drug group
	spec[i]=sum(st<=st[i] & ind==0)/sum(ind==0) # live longer from conventional drug group
	if(i>1) AUC=AUC+(spec[i]-spec[i-1])*sens[i]
}
plot(1-spec,sens,type="s",lwd=3,xlab="",ylab="")
mtext(side=1,"False positive rate (1-specificity)",cex=1.4,line=2.5)
mtext(side=2,"Sensitivity",cex=1.4,line=2.5)
title("ROC curve")
text(.6,.2,paste("AUC=",round(AUC*100),"%",sep=""),font=3,cex=2)


#bootstrap simulations
N=100000# number of cancer patients
st1.sim=st2.sim=rep(0,N)
for(i in 1:N)
{
	st1.sim[i]=sample(x=st1,size=1,prob=rep(1/n1,n1)) # patient receives conventional chemotherapy
	st2.sim[i]=sample(x=st2,size=1,prob=rep(1/n2,n2)) # similar patient receives new drug therapy
}
cat("Percent patients who improve survival by taking new drug =",100*mean(st1.sim<st2.sim))

}
