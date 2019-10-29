lungsm <-
function()
{
dump("lungsm","c:\\StatBook\\lungsm.r")
#logistic regression for the 2x2 table
yx11=cbind(rep(1,648),rep(1,648))
yx10=cbind(rep(1,122),rep(0,122))
yx01=cbind(rep(0,3852),rep(1,3852))
yx00=cbind(rep(0,12048),rep(0,12048))
yx=rbind(yx11,yx10,yx01,yx00)
o=glm(yx[,1]~yx[,2],family=binomial)
print("logistic regression output")
print(summary(o))
a=coef(o);cat("OR from logistic regression =",exp(a[2]),"\n")
#binomial regression for the 2x2 table
suc=c(648,122);fail=c(3852,12048)
x=c(1,0)
o=glm(cbind(suc,fail)~x,family=binomial)
print("binomial regression output")
print(summary(o))
a=coef(o);cat("OR from binomial regression =",exp(a[2]),"\n")

}
