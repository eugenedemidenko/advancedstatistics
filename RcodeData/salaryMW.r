salaryMW <-
function(dvis=.1)
{
dump("salaryMW","c:\\StatBook\\salaryMW.r")
dat=read.csv("c:\\StatBook\\salaryMW_paired.csv")
salaryM=dat[,1];salaryW=dat[,2];salL=c(salaryM,salaryW)
n=nrow(dat)
par(mfrow=c(1,2),mar=c(2,3.5,3,.1))
boxplot(list(salaryM,salaryW),col="gray90",names=c("Man","Woman"),ylab="",ylim=c(40,130))
mtext(side=2,"Salary, thousands of dollars",cex=1.25,line=2.25)
segments(rep(1-dvis,n),salaryM,rep(1+dvis,n),salaryM) #actual data, men
segments(1-2*dvis,mean(salaryM),1+2*dvis,mean(salaryM),col=2,lwd=3) #mean
segments(rep(2-dvis,n),salaryW,rep(2+dvis,n),salaryW) #actual data, women
segments(2-2*dvis,mean(salaryW),2+2*dvis,mean(salaryW),col=2,lwd=3) #mean
varCOM=(var(salaryM)+var(salaryW))*(n-1)/(2*n-2)
text(1.5,1.52,paste("SE = ",round(sd(varCOM)/sqrt(2*n),4),sep=""),cex=1.25)

#Unpaired t-test
print("Unpaired t-test men vs. women:")
print(t.test(salaryM,salaryW,var.equal=T))
pv=t.test(salaryM,salaryW,var.equal=T)$p.value
title(paste("Unpaired t-test, p-value = ",round(pv,4),"\nSD = ",round(sqrt(varCOM),1),sep=""))

#Paired t-test
print("Paired t-test men vs. women:")
print(t.test(salaryM,salaryW,paired=T))
dif=salaryM-salaryW
boxplot(dif,ylab="",col="gray90",ylim=c(-45,45))
mtext(side=2,"Difference",cex=1.25,line=2.25)
segments(rep(1-dvis,n),dif,rep(1+dvis,n),dif)
segments(1-2*dvis,mean(dif),1+2*dvis,mean(dif),col=2,lwd=3) #mean
pv=t.test(salaryM,salaryW,paired=T)$p.value
title(paste("Paired t-test, p-value = ",round(pv,6),"\nSD = ",round(sd(dif),1),sep=""))

}
