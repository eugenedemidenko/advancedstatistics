pidistr1010 <-
function()
{
dump("pidistr1010","c:\\StatBook\\pidistr1010.r")
pi1m=read.csv("c:\\StatBook\\pi100000digits.csv",col.names=F)[,1]
n=length(pi1m)
pr1010=as.data.frame(matrix(,ncol=10,nrow=10),row.names=as.character(0:9))
names(pr1010)=as.character(0:9)
piM1=pi1m[1:(n-1)];piM2=pi1m[2:n]
for(i in 1:10)
for(j in 1:10)
	pr1010[i,j]=mean(piM1==(i-1) & piM2==(j-1))
pr1010
}
