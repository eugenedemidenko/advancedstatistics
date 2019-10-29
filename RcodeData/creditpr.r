creditpr <-
function()
{
dump("creditpr","c:\\StatBook\\creditpr.r")
da=read.csv("c:\\StatBook\\creditpr.csv")
p1=da$Paycheck[da$Failed==1];n1=length(p1)
m1=da$Months[da$Failed==1]
p0=da$Paycheck[da$Failed==0];n0=length(p0)
m0=da$Months[da$Failed==0]
mux=c(mean(p1),mean(m1))
muy=c(mean(p0),mean(m0))
Omega=matrix(NA,2,2)
Omega[1,1]=(var(p1)*(n1-1)+var(p0)*(n0-1))/(n1+n0-2)
Omega[1,2]=Omega[2,1]=(var(p1,m1)*(n1-1)+var(p0,m0)*(n0-1))/(n1+n0-2)
Omega[2,2]=(var(m1)*(n1-1)+var(m0)*(n0-1))/(n1+n0-2)
s=(mux+muy)/2;d=solve(Omega)%*%(muy-mux)
U=da[,2:3]
n=nrow(U)


UU=U-rep(1,n)%*%t(s)

u=UU[,1]*d[1]+UU[,2]*d[2]
uu=seq(from=min(u),to=max(u),length=1000)
sens=fp=rep(0,1000)
auc=0
for(i in 1:1000)
{
	sens[i]=sum(u<uu[i] & da$Failed==1)/sum(da$Failed==1)
	fp[i]=sum(u<uu[i] & da$Failed==0)/sum(da$Failed==0)	
}
plot(fp,sens,type="s")
for(i in 2:1000) auc=auc+(fp[i]-fp[i-1])*sens[i]
md=t(mux-muy)%*%solve(Omega)%*%(mux-muy)
AUC.th=pnorm(sqrt(md)/sqrt(2))
c(AUC.th,auc)
}
