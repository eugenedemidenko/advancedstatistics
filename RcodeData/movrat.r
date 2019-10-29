movrat <-
function(wid=.1)
{
dump("movrat","c:\\StatBook\\movrat.r")
#set.seed(5)
#n=500;m=350
#ageM=round(runif(n,min=28,max=77))
#m=rep(1,n)
#ageW=round(runif(n,min=18,max=46))
#w=rep(0,n)
#age=c(ageM,ageW)
#sex=c(m,w)
#score=round(40+3*sex+age*.1+rnorm(n+m,sd=30))
#age=age[score>1 & score<=100]
#sex=sex[score>1 & score<=100]
#score=score[score>1 & score<=100]
#da=data.frame(cbind(sex,score,age))
#write.csv(da,"c:\\StatBook\\movrat.csv",row.names=F)
da=read.csv("c:\\StatBook\\movrat.csv",header=T)
sex=da[,1];score=da[,2];age=da[,3]
print(paste("Mean age among men and women =",round(mean(age[sex==1])),", ", round(mean(age[sex==0]))))
par(mfrow=c(1,2),mar=c(3,4,3,1))
o=lm(score~sex)
print(summary(o))
pv=round(summary(o)$coefficients[2,4],3)
boxplot(list(score[sex==1],score[sex==0]),names=c("Men","Women"),main=paste("Unadjusted t-test p-value =",pv),ylab="Movie score")
nW=length(score[sex==0]);nM=length(score[sex==1])

o=lm(score~sex+age)
print(summary(o))
a=coef(o)

scage=score-a[3]*age
pv=round(summary(o)$coefficients[2,4],3)
boxplot(list(scage[sex==1],scage[sex==0]),names=c("Men","Women"),main=paste("Age adjusted p-value =",pv),ylab="Movie score")
print(t.test(scage[sex==1],scage[sex==0]))



}
