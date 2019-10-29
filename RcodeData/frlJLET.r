frlJLET <-
function()
{
    dump("frlJLET","c:\\StatBook\\frlJLET.r")
    freqlet=matrix(c("a","8.167","b","1.492","c","2.782","d","4.253","e","12.702","f","2.228","g","2.015","h","6.094","i","6.966","j","0.153","k","0.772","l","4.025","m","2.406","n","6.749","o","7.507","p","1.929","q","0.095","r","5.987","s","6.327",     "t","9.056","u","2.758","v","0.978","w","2.360","x","0.150","y","1.974","z","0.074"),ncol=2,byrow=T)
    numfr=as.numeric(freqlet[,2])
    n=length(numfr)
    ch=scan("c:\\StatBook\\Jack_London_Call_of_the_Wild_The_f1.char",what="")
    ch=tolower(ch)
    par(mfrow=c(1,1),mar=c(4.25,4.26,0,0),cex.lab=1.3)
    plot(1:n,numfr,lwd=4,type="h",axes=F,ylim=c(0,15),xlab="English letters",ylab="Probability, %")
    axis(side=2,seq(from=0,to=15,by=3),srt=90)
    axis(side=1,at=1:n,labels=freqlet[,1])
    frJL=rep(0,n)
    for(i in 1:n) frJL[i]=length(ch[ch==freqlet[i,1]])
    frJL=frJL/sum(frJL)*100
    segments(1:n+.2,rep(0,n),1:n+.2,frJL,lwd=3,col="gray80")
    legend(10,15,c("Typical English text","Call of the Wild"),col=c(1,"gray80"),lty=1,lwd=4,cex=1.5)
}
