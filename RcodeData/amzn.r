amzn <-
function(maxlag=8)
{
dump("amzn","c:\\StatBook\\amzn.r")
d=read.csv("c:\\StatBook\\AMZN_weekly.csv")
dat=as.character(d[,1])
y=d$Adj.Close
n=length(y);ti=1:n
par(mfrow=c(1,1),mar=c(5,3,1,0))
plot(ti,y,type="l",ylim=c(300,2000),axes=F,xlab="",ylab="",lwd=3)
nix=seq(from=1,to=n,by=4)
axis(side=1,at=nix,labels=dat[nix],las=2)
axis(side=2,seq(from=300,to=2000,by=200))
my=mean(y)
segments(-100,my,n,my,col=3,lwd=3)

legend(0,1800,c("Actual stock price","First-order autoregression","Constant model"),lty=1,col=c(1,1,3),lwd=c(3,1,3),cex=1.5,bg="gray90")
yy=y
y=yy[(maxlag+1):n] #stock to predict
X=matrix(ncol=maxlag,nrow=n-maxlag)
for(j in 1:maxlag) 
X[,j]=yy[(maxlag-j+1):(n-j)] # maxlag past values
o8=lm(y~X) #autoregression model with maxlag predictors
print("Autoregression model with 8 lags:")
print(summary(o8))
R2=summary(o8)$r.squared
lines((maxlag+1):n,o8$fitted.values)

o1=lm(y~X[,1])
print(summary(o1))
R20=summary(o1)$r.squared


R2C=(R2-R20)/(1-R20)
tit=paste("Amazon.com weekly stock prices predicted by the autoregression of the first order\ncoefficient of determination =",round(R2,3),"\ncontrast coefficient of determination =",round(R2C,3))
mtext(side=3,tit,cex=1.25,line=-2.5)
}
