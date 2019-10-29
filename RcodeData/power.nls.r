power.nls <-
function(job=1,n=20,sigma=.2,nSim=500,alpha=.05)
{
dump(c("power.nls","power.nls_1.out"),"c:\\StatBook\\power.nls.r")
fr=function(a1,a2,a3,x) # regression function
{
	et=exp(-a3*x)
	va=a1-a2*et
	n=length(x)
	d1=rep(1,n);d2=-et;d3=a2*x*et
	attr(va,"grad")=cbind(d1,d2,d3)
	return(va)
}
geninvese=function(M) #generalized sym. matrix inverse
{
	egM=eigen(M,sym=T)
	ev=egM$values
	m=length(ev)
	ev0=rep(0,m)
	ev0[abs(ev)>10^-10]=1/ev[abs(ev)>10^-10]
	giM=egM$vectors%*%diag(ev0,m,m)%*%t(egM$vectors)
	giM
}

a1.true=10;a2.true=2;a3.true=.01 #true values
nMAX=300;ti=seq(from=1,to=nMAX,length=n)

if(job==1) # Comparison of Wald, lkelihood-ratio and score/NE tests for a3
{
	print(date())
	n.a3=41 # the number of power points
	powW=powLR=powNE=Fail=rep(NA,n.a3)
	a3=seq(from=.01-.008,to=.01+.012,length=n.a3)
	pW=pLR=pNE=rep(NA,nSim)
	for(i in 1:n.a3)
	{
		for(isim in 1:nSim)
		{
			y=fr(a1=a1.true,a2=a2.true,a3=a3[i],x=ti)+rnorm(n=n,mean=0,sd=sigma)
			onls=try(nls(y~fr(a1,a2,a3,x=ti),start=c(a1=a1.true,a2=a2.true,a3=a3[i])),silent=T)
			if(attr(onls,"class")!="try-error") 
			{
				coft=summary(onls)$coefficients
				aNLS=coef(onls)				
				pW[isim]=2*pnorm(-abs(aNLS[3]-a3.true)/coft[3,2])				
			}
			onlsLR=try(nls(y~fr(a1,a2,a3=a3.true,x=ti),start=c(a1=a1.true,a2=a2.true)),silent=T)
			if(attr(onlsLR,"class")!="try-error" & attr(onls,"class")!="try-error")
			{
				frv=fr(a1=aNLS[1],a2=aNLS[2],a3=aNLS[3],x=ti)								
				aR=coef(onlsLR);s21=summary(onls)$sigma^2;s21R=summary(onlsLR)$sigma^2				
				LHS=n*log(s21R/s21)
				pLR[isim]=1-pchisq(LHS,df=1)								
				
				frR=fr(aR[1],aR[2],a3=a3.true,x=ti)
				J=attr(frR,"grad")
				iJtJ=geninvese(t(J)%*%J)		
				Jd=t(J)%*%(frv-aR[1]+aR[2]*exp(-a3.true*ti))		
				pNE[isim]=1-pchisq(t(Jd)%*%iJtJ%*%Jd/s21R,df=1)

			}
		}			
		pW=pW[!is.na(pW)];pLR=pLR[!is.na(pLR)];pNE=pNE[!is.na(pNE)]
		powW[i]=mean(pW<alpha);powLR[i]=mean(pLR<alpha);powNE[i]=mean(pNE<alpha)
		Fail[i]=1-length(pLR)/nSim
	}
	print(date())
	#power.nls_1.out=power.nls(job=1,nSim=5000)
	return(cbind(a3,powW,powLR,powNE,Fail))		
}
if(job==1.1)
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	matplot(power.nls_1.out[,1],power.nls_1.out[,2:4],type="l",lwd=3,col=1,lty=1:3,ylim=c(0,1),xlab="",ylab="")
	#mtext(side=1,expression(plain("Alternative values of the rate parameter"),alpha[3]),cex=1.4,line=2.75)
	mtext(side=1,expression(alpha[3]),cex=2,line=2.75)
	mtext(side=2,"Probability, power function",cex=1.4,line=2.75)
	segments(-100,alpha,100,alpha,col=2)
	text(.01-.0005,.6,"The null value",font=3,cex=1.5,srt=90)
	text(.005,alpha+.05,paste("a =",alpha),font=5,cex=2)
	segments(a3.true,-1,a3.true,1,col="gray80")
	legend(.002,1,c("Wald","LR","Score"),lty=1:3,lwd=3,bg="gray90",cex=1.75)	
}

}
power.nls_1.out <-
structure(c(0.002, 0.0025, 0.003, 0.0035, 0.004, 0.0045, 0.005, 
0.0055, 0.006, 0.0065, 0.007, 0.0075, 0.008, 0.0085, 0.009, 0.0095, 
0.01, 0.0105, 0.011, 0.0115, 0.012, 0.0125, 0.013, 0.0135, 0.014, 
0.0145, 0.015, 0.0155, 0.016, 0.0165, 0.017, 0.0175, 0.018, 0.0185, 
0.019, 0.0195, 0.02, 0.0205, 0.021, 0.0215, 0.022, 0.293183520599251, 
0.392313588087156, 0.483467290670465, 0.539873608185375, 0.545409081816363, 
0.535, 0.514, 0.4654, 0.406, 0.3397, 0.2748, 0.2245, 0.1783, 
0.1335, 0.097, 0.0756, 0.0661, 0.0606, 0.0743, 0.0828, 0.1038, 
0.1395, 0.1793, 0.2122, 0.2655, 0.3112, 0.3633, 0.4281, 0.4821, 
0.5281, 0.595, 0.6337, 0.675, 0.7068, 0.752, 0.7864, 0.809, 0.8368, 
0.8565, 0.872, 0.8952, 0.147116104868914, 0.227855031205518, 
0.311934072642181, 0.380078242551911, 0.396579315863173, 0.392, 
0.367, 0.3338, 0.2801, 0.224, 0.1853, 0.1425, 0.1076, 0.0755, 
0.0582, 0.0469, 0.0432, 0.0445, 0.0609, 0.0758, 0.0976, 0.1381, 
0.1763, 0.2086, 0.2677, 0.3163, 0.3727, 0.4318, 0.491, 0.5409, 
0.6056, 0.6441, 0.6881, 0.7213, 0.7682, 0.8049, 0.8268, 0.852, 
0.8682, 0.885, 0.9052, 0.128689138576779, 0.206284900908792, 
0.297690507681351, 0.36543284180961, 0.391978395679136, 0.3876, 
0.3712, 0.3364, 0.2834, 0.2282, 0.1867, 0.1465, 0.1109, 0.0818, 
0.0626, 0.0496, 0.0481, 0.0488, 0.0653, 0.0771, 0.1027, 0.1397, 
0.1787, 0.2148, 0.2684, 0.3174, 0.3715, 0.4361, 0.491, 0.5389, 
0.6102, 0.6495, 0.6904, 0.7259, 0.7697, 0.8006, 0.8261, 0.8535, 
0.8714, 0.888, 0.9075, 0.3325, 0.0867, 0.0171, 0.00309999999999999, 
0.000199999999999978, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0), .Dim = c(41L, 5L), .Dimnames = list(NULL, c("a3", "powW", 
"powLR", "powNE", "Fail")))
