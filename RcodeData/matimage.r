matimage <-
function()
{
dump("matimage","c:\\StatBook\\matimage.r")
M=matrix(round(runif(6,min=0,max=255)),ncol=2,nrow=3)
print("Matrix M:")
print(M)
image(1:3,1:2,M,axes=F,xlab="",ylab="",col=gray(0:255/255))
axis(side=1,1:3)
axis(side=2,1:2)
for(i in 1:3)
for(j in 1:2)
text(i,j,M[i,j],cex=2,col=2)
}
