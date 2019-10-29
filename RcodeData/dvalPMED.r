dvalPMED <-
function()
{
dump("dvalPMED","c:\\StatBook\\dvalPMED.r")
da=read.csv("c:\\StatBook\\dvalPMED.csv")
o=lm(bloodpres~drug+smoking+BMI+man+age+famhist+exer,data=da)
so=summary(o)
ta=so$coefficients
print(summary(o))
vd=2*so$sigma^2+ta[,2]^2
bvalue=pt(abs(ta[2,1])/sqrt(vd[2]),df=o$df.residual)
cat("B-value of the drug alone = ",round(100*bvalue),"%",sep="")
COV=vcov(o);cc=c(0,1,-1,0,0,0,0,2)
varde=t(cc)%*%COV%*%cc
num=sum(cc*ta[,1])
bvalue3=pt(-num/sqrt(2*so$sigma^2+varde),df=o$df.residual)
cat("\nB-value of the drug + quitting smoke + increasing exersize = ",round(100*bvalue3),"%\n",sep="")
}
