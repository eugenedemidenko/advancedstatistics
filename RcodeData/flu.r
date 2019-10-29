flu <-
function()
{
dump("flu","c:\\StatBook\\flu.r")
vd=c(41,NA,52,34,27,31,42,
20,34,28,18,NA,11,18,
30,28,18,22,15,21,30,
9,16,21,15,20,13,23,
29,19,28,25,23,NA,23,
39,38,41,28,57,35,41,
59,26,72,19,43,32,44,
41,35,49,NA,43,25,37)

d=matrix(vd,nrow=8,byrow=T)
N=nrow(d);nc=ncol(d)
print(d)
par(mfrow=c(1,1),mar=c(4,4,1,1))
boxplot(list(d[1,],d[2,],d[3,],d[4,],d[5,],d[6,],d[7,],d[8,]),names=c("<10","10-20","20-30","30-40","40-50","50-60","60-70",">70"))
mtext(side=1,"Age groups",cex=1.25,line=2.75)
mtext(side=2,"Flu incidence per thousand",cex=1.25,line=2.75)
for(i in 1:N) 
for(j in 1:nc)
	points(i,d[i,j],cex=1.25)
	

for(i in 1:N) 
	points(i,mean(d[i,],na.rm=T),pch=16,cex=1.25)
	
#part(a)
gros.av=mean(vd,na.rm=T)
S0=sum((vd-gros.av)^2,na.rm=T)
Smin=0;ni=rep(0,N)
for(i in 1:N) 
{
	ni[i]=sum(!is.na(d[i,]))
	Smin=Smin+var(d[i,],na.rm=T)*(ni[i]-1)
}
Fobs=(S0-Smin)/Smin*(sum(ni)-N)/(N-1)
pvalue=pf(Fobs,df1=N-1,df2=sum(ni)-N,lower.tail=F)
text(1,70,paste("All rates are the same: P-value =",signif(pvalue,2)),cex=1.5,adj=0)

tvd=as.vector(t(d))
y=tvd[!is.na(tvd)]
sumni=length(y)
X=matrix(0,ncol=N,nrow=sumni)
k=1
for(i in 1:N)
{
	ni=sum(!is.na(d[i,]))
	X[k:(k+ni-1),i]=1
	k=k+ni 	
}
o=lm(y~X)
print(summary(o))
#part(b)
vac=NULL
zer=c(0,0,0,1,1,1,1)

for(i in 1:N)
{
	ni=sum(!is.na(d[i,]))
	vac=c(vac,zer[!is.na(d[i,])])
}

o=lm(y~X+vac-1)
so=summary(o)
print(so)
pvVac=summary(o)$coefficients[9,4]

ta=so$coefficients
vd=2*so$sigma^2+ta[9,2]^2
bvalue=pt(abs(ta[9,1])/sqrt(vd),df=o$df.residual)
cat("B-value vaccination = ",round(100*bvalue),"%\n",sep="")
text(1,65,paste("Vaccination has no effect: P-value = ",round(pvVac,4),"\nB-value vaccination = ",round(100*bvalue),"%",sep=""),cex=1.5,adj=0)
}
