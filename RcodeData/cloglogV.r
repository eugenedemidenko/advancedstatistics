cloglogV <-
function()
{
dump("cloglogV","c:\\StatBook\\cloglogV.r")
da0=read.csv("c:\\StatBook\\Traffic.Viol.csv",header=T)
da0$trviol[da0$trviol>0]=1

o=glm(trviol~marriage+gender+age+income,data=da0,family=binomial(link="cloglog"))
print(summary(o))
}
