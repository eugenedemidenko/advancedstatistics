frlJL <-
function()
{
dump("frlJL","c:\\StatBook\\frlJL.r")
freqlet=matrix(c("a","8.167","b","1.492","c","2.782","d","4.253","e","12.702","f","2.228","g","2.015","h","6.094","i","6.966","j","0.153","k","0.772","l","4.025","m","2.406","n","6.749","o","7.507","p","1.929","q","0.095","r","5.987","s","6.327","t","9.056","u","2.758","v","0.978","w","2.360","x","0.150","y","1.974","z","0.074"),ncol=2,byrow=T)
p0=as.numeric(freqlet[,2])/100
m=length(p0)
ch=scan("c:\\StatBook\\Jack_London_Call_of_the_Wild.char",what="")
ch=tolower(ch)
frJL=rep(0,m)
for(i in 1:m) frJL[i]=length(ch[ch==freqlet[i,1]])
n=sum(frJL)
p.hat=frJL/n
PW=n*sum((p.hat-p0)^2/p0)
print(paste("Obs. test stat=",PW,",p-value=",pchisq(PW,df=m-1,lower.tail=F)))

chT=scan("c:\\StatBook\\Mark_Twain_The_Adventures_of_Tom_Sawyer_f1.txt.char",what="")
chT=tolower(chT)
frT=rep(0,m)
for(i in 1:m) frT[i]=length(chT[chT==freqlet[i,1]])
nY=sum(frT)
pY.hat=frT/nY
PW=sum((p.hat-pY.hat)^2/(p.hat/n+pY.hat/nY))
pv=pchisq(PW,df=m-1,lower.tail=F)
print(paste("Obs. test stat = ",PW,",p-value =" ,pv))
ph=cbind(p.hat,pY.hat)
plot(1:m,p.hat,type="h",lwd=4,ylim=c(0,.125),axes=F,xlab="",ylab="")
lines(1:m+.2,pY.hat,type="h",col="gray80",lwd=4)
axis(side=1,1:m,labels=freqlet[,1])
axis(side=2,seq(from=0,to=0.125,by=.025))
mtext(side=1,"English letter, lowercase",cex=1.5,line=2.75)
mtext(side=2,"Probability",cex=1.5,line=2.5)
legend("topright",c("Jack London","Mark Twain"),lty=1,lwd=4,col=c(1,"gray80"),cex=1.4,bg="gray95")
mtext(side=3,paste("Distribution of letters in Call of the Wild and The Adventures of Tom Sawyer\nThe p-value difference =",signif(pv,3)),cex=1.5,line=1)
text(m/2,.125,paste("nX = ",n,", nY = ",nY),cex=1.25,font=3)
}
