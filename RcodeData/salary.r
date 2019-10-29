salary <-
function () 
{
dump("salary","c:\\StatBook\\salary.r")
#CDF statistical comparison
sv=scan("c:\\StatBook\\Vermont.txt",what="")
sv=as.numeric(sv);sv=sv[!is.na(sv)]
sc=scan("c:\\StatBook\\Connecticut.txt",what="")
sc=as.numeric(sc);sc=sc[!is.na(sc)]
par(mfrow=c(1,1),mar=c(4,4,1,1))
sv=sv[order(sv)];sc=sc[order(sc)]
nsc=length(sc);nsv=length(sv)
Fv=seq(from=0,to=1,length=nsc)
aa=abs(Fv-.5);mcon=mean(sc[aa==min(aa)])
plot(sc,Fv,type="n",xlab="Annual salary, $",ylab="CDF, probability",lty=1,col=2:3,lwd=3)
lines(sc,Fv,type="s",col=2,lwd=3)
segments(sc,rep(-1,nsc),sc,rep(.07,nsc),col=2)
#	title("Comparison of annual salaries in Vermont and Connecticut\nacross the board")
Fv=seq(from=0,to=1,length=nsv)
lines(sv,Fv,type="s",col=3,lwd=3)
aa=abs(Fv-.5);mven=mean(sv[aa==min(aa)])
legend(100000,.3,c("Connecticut (median = $50.9K)","Vermont (median = $40.7K)"),lty=1,col=2:3,cex=1.3,lwd=3,bg="gray95")
segments(sv,rep(-1,nsc),sv,rep(.008,nsc),col=3)
segments(-1,.5,100000,.5);text(105000,.5,"Median",adj=0)
}
