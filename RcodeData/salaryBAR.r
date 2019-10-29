salaryBAR <-
function()
{
dump("salaryBAR","c:\\StatBook\\salaryBAR.r")
sv=scan("c:\\StatBook\\Vermont.txt",what="")
sv=as.numeric(sv)/1000;sv=sv[!is.na(sv)];nv=length(sv)
sc=scan("c:\\StatBook\\Connecticut.txt",what="")
sc=as.numeric(sc)/1000;sc=sc[!is.na(sc)];nc=length(sc)
par(mfrow=c(1,2),mar=c(2,4,2,.1))
boxplot(list(sv,sc),names=c("Vermont","Connecticut"),ylab="Salary, $1000")
title("Standard boxplot")

boxplot(list(log10(sv),log10(sc)),xlim=c(0.5,2.5),names=c("Vermont","Connecticut"),yaxt="n",ylim=log10(c(20,250)),ylab="Salary, $1000")
title("log10 scale boxplot")
ysal=c(20,30,50,100,150,250)
axis(side=2,log10(ysal),labels=as.character(ysal),srt=90)
segments(rep(.9,nv),log10(sv),rep(1.1,nv),log10(sv))
segments(rep(1.9,nc),log10(sc),rep(2.1,nc),log10(sc))
}
