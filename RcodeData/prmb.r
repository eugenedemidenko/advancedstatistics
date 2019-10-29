prmb <-
function(r0=2,mu1=3,mu2=5,s1=1,s2=4)
{
dump("prmb","c:\\StatBook\\prmb.r")
par(mfrow=c(1,1),mar=c(3.75,3.5,1,1))
alpha=seq(from=0,to=1,length=100)
mu=alpha*mu1+(1-alpha)*mu2
pr=pnorm((r0-mu)/sqrt(alpha^2*s1^2+(1-alpha)^2*s2^2))
plot(pr,mu,type="l",ylim=c(r0,max(mu)),xlim=c(.1,.25),lwd=3,main="",xlab="",ylab="")
mtext(side=1,paste("Probability of obtaining a return less than the guaranteed rate =",r0),cex=1.5,line=2.5)
mtext(side=2,"Expected annual return, %",cex=1.5,line=2.25)
aopt=s2^2*(mu1-r0)/(mu1*s2^2+mu2*s1^2-(s1^2+s2^2)*r0)
muopt=aopt*mu1+(1-aopt)*mu2
a=c(0,round(aopt,2),.5,1)
mua=a*mu1+(1-a)*mu2
pra=pnorm((r0-mua)/sqrt(a^2*s1^2+(1-a)^2*s2^2))
points(pra,mua,cex=1.5)
text(pra+.002,mua,paste("a=",a,sep=""),font=5,adj=0,cex=1.75)
segments(-1,r0,10,r0,lty=2)
text(.18,r0+.1,"The lowest guaranteed return",cex=1.5)
cat("The safest portfolio: mean return =",round(muopt,2),", probability =",round(pra[2],2),"\n")
}
