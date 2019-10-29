twocphCI <-
function(alpha=.05)
{
dump("twocphCI","c:\\StatBook\\twocphCI.r")
da=read.csv("c:\\StatBook\\twocph.csv",header=T,stringsAsFactors=F)
min=as.vector(da[,1]);Vt=as.vector(da[,2])

VtM=function(V0,alpha,beta,x) # Two-compartment function
{
	e1=exp(-alpha*x);e2=exp(-beta*x)
	Vt.ret=V0*(1-e1)*e2
	dVdV0=(1-e1)*e2
	dVda=V0*e1*e2*x
	dVdb=-Vt.ret*x
	attr(Vt.ret,"gradient")=cbind(dVdV0,dVda,dVdb)
	return(Vt.ret)
}

uni3=function(a,y,x,V0.0,beta.0,RSS,alpha=.05) # LR for uniroot
{
	n=length(y)
	onls3=nls(y~V0*(1-exp(-a*x))*exp(-beta*x),start=list(V0=V0.0,beta=beta.0))			
	funi=n*log(sum(summary(onls3)$residuals^2))-n*log(RSS)-qchisq(1-alpha,df=1)
	funi
}
	
unNE.prof=function(a,y,x,f.hat,V0.0,beta.0,alpha=.05) # Score for uniroot
{
	onls3=nls(y~V0*(1-exp(-a*x))*exp(-beta*x),start=list(V0=V0.0,beta=beta.0))			
	aNLS=coef(onls3)		
	s2=summary(onls3)$sigma^2
	fr3=VtM(V0=aNLS[1],alpha=a,beta=aNLS[2],x=x)
	J=attr(fr3,"grad")				
	iJtJ=solve(t(J)%*%J)		
	Jd=t(J)%*%(f.hat-aNLS[1]*(1-exp(-a*x))*exp(-aNLS[2]*x))		
	f0=t(Jd)%*%iJtJ%*%Jd/s2-qchisq(1-alpha,df=1)
	return(f0)				
}

Zcrit=qnorm(1-alpha/2)
ci3=as.data.frame(matrix(ncol=2,nrow=3),row.names=c("Wald","LR","Score"))
names(ci3)=c("Lower","Upper")
onls=nls(Vt~VtM(V0,alpha,beta,x=min),start=c(V0=530,alpha=.1,beta=.01))
SE2=summary(onls)$coefficients[2,2]
aNLS=as.vector(coef(onls))
f.hat=VtM(V0=aNLS[1],alpha=aNLS[2],beta=aNLS[3],x=min)
ci3[1,1]=aNLS[2]-Zcrit*SE2;ci3[1,2]=aNLS[2]+Zcrit*SE2				

# LR
RSS=sum(summary(onls)$residuals^2)
ci3[2,1]=uniroot(f=uni3,y=Vt,x=min,V0.0=aNLS[1],beta.0=aNLS[3],RSS=RSS,alpha=alpha,interval=c(aNLS[2]-4*SE2,aNLS[2]))$root
ci3[2,2]=uniroot(f=uni3,y=Vt,x=min,V0.0=aNLS[1],beta.0=aNLS[3],RSS=RSS,alpha=alpha,interval=c(aNLS[2]+SE2,aNLS[2]+4*SE2))$root
				
#Score			
ci3[3,1]=uniroot(f=unNE.prof,y=Vt,x=min,f.hat=f.hat,V0.0=aNLS[1],beta.0=aNLS[3],alpha=alpha,interval=c(aNLS[2]-4*SE2,aNLS[2]))$root
ci3[3,2]=uniroot(f=unNE.prof,y=Vt,x=min,f.hat=f.hat,V0.0=aNLS[1],beta.0=aNLS[3],alpha=alpha,interval=c(aNLS[2],aNLS[2]+4*SE2))$root				
ci3				

}
