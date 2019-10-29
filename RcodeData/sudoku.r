sudoku <-
function(job=1,setrand=3)
{
dump("sudoku","c:\\StatBook\\sudoku.r")
set.seed(setrand)
test.sudoku=function(sud)
{
	
	for(i in 1:9) if(length(unique(sud[i,]))<9 | length(unique(sud[,i]))<9) return(0)
	for(i in 1:3) 
	for(j in 1:3) 
	if(length(unique(as.vector(sud[(1+(i-1)):(3*i),(1+(j-1)):(3*j)])))<9) return(0)
	return(1)
}
display.sudoku=function(sud)
{
	plot(0:9,0:9,xlab="",ylab="",axes=F,type="n")
	polygon(x=c(3,6,6,3,3),y=c(0,0,3,3,0),col="grey90")
	polygon(x=c(0,3,3,0,0),y=c(3,3,6,6,3),col="grey90")
	polygon(x=c(6,9,9,6,6),y=c(3,3,6,6,3),col="grey90")
	polygon(x=c(3,6,6,3,3),y=c(6,6,9,9,6),col="grey90")
	for(i in 0:9) {segments(i,0,i,9);segments(0,i,9,i)}
	for(i in 1:9) for(j in 1:9) text(i-.5,j-.5,sud[i,j],cex=2)
}
# mother sudoku
sud=matrix(c(6,2,5,1,8,4,7,9,3,8,7,9,6,2,3,4,5,1,3,1,4,7,5,9,6,2,8,2,6,7,9,1,8,3,4,5,4,8,3,2,7,5,9,1,6,9,5,1,3,4,6,2,8,7,5,3,2,4,6,1,8,7,9,1,4,6,8,9,7,5,3,2,7,9,8,5,3,2,1,6,4),ncol=9,nrow=9)
if(job==1)
{
	par(mfrow=c(1,1),mar=c(1,1,1,1))
	display.sudoku(sud)
	print(test.sudoku(sud))
}
if(job==1.1) #test if a random complete Sudoku can be expressed as a permutation of any mother Sudoku, sud
{
	sudR=matrix(c(2,9,4, 5,1,8, 6,7,3, 6,8,1, 7,3,2, 5,9,4, 3,5,7, 4,9,6, 8,2,1, 1,2,6, 3,8,9, 4,5,7, 8,7,9, 2,5,4,1,3,6, 5,4,3, 1,6,7, 9,8,2, 9,3,5, 6,7,1, 2,4,8, 4,6,8, 9,2,3, 7,1,5, 7,1,2, 8,4,5, 3,6,9),ncol=9,nrow=9)
	par(mfrow=c(1,1),mar=c(1,1,1,1))
	display.sudoku(sudR)
	print(test.sudoku(sudR))	
}
if(job==2)
{
	par(mfrow=c(1,2),mar=c(1,1,2,2))
	display.sudoku(sud)
	title("Mother Sudoku")
	i9=sample(x=1:9,size=9,replace=F,prob=rep(1/9,9))
	sudR=sud
	for(i in 1:9)
	sudR[sud==i]=i9[i]
	print(i9)
	display.sudoku(sudR)
	title("Daughter Sudoku")
}
if(job==3)
{
	par(mfrow=c(1,2),mar=c(1,1,2,2))
	i9=sample(x=1:9,size=9,replace=F,prob=rep(1/9,9))
	sudR=sud
	for(i in 1:9)
	sudR[sud==i]=i9[i]
	
	n.blank=30
	row.blank=rep(1:9,times=9);col.blank=rep(1:9,each=9)
	ij.rand=sample(1:81,size=n.blank,replace=F,prob=rep(1/81,81))
	for(i in 1:n.blank) sudR[row.blank[ij.rand[i]],col.blank[ij.rand[i]]]=NA
	display.sudoku(sudR)
	print(sum(is.na(sudR)))
	title(paste("Easy Sudoku, n empty=",n.blank,sep=""))
	
	i9=sample(x=1:9,size=9,replace=F,prob=rep(1/9,9))
	sudR=sud
	for(i in 1:9)
	sudR[sud==i]=i9[i]
	n.blank=50
	row.blank=rep(1:9,times=9);col.blank=rep(1:9,each=9)
	ij.rand=sample(1:81,size=n.blank,replace=F,prob=rep(1/81,81))
	for(i in 1:n.blank) sudR[row.blank[ij.rand[i]],col.blank[ij.rand[i]]]=NA
	display.sudoku(sudR)
	title(paste("Difficult Sudoku, n empty=",n.blank,sep=""))
}
if(job==3.1)
{
	par(mfrow=c(1,1),mar=c(1,1,2,2))
	i9=sample(x=1:9,size=9,replace=F,prob=rep(1/9,9))
	sudR=sud
	for(i in 1:9)
	sudR[sud==i]=i9[i]
	
	n.blank=30
	row.blank=rep(1:9,times=9);col.blank=rep(1:9,each=9)
	ij.rand=sample(1:81,size=n.blank,replace=F,prob=rep(1/81,81))
	for(i in 1:n.blank) sudR[row.blank[ij.rand[i]],col.blank[ij.rand[i]]]=NA
	display.sudoku(sudR)
}
if(job==4)
{
	par(mfrow=c(1,2),mar=c(1,1,2,2))
	i9=sample(x=1:9,size=9,replace=F,prob=rep(1/9,9))
	sudR=sud
	for(i in 1:9)
	sudR[sud==i]=i9[i]
	display.sudoku(sudR)
	title("Complete random sudoku")
	
	n.blank=30
	row.blank=rep(1:9,times=9);col.blank=rep(1:9,each=9)
	ij.rand=sample(1:81,size=n.blank,replace=F,prob=rep(1/81,81))
	for(i in 1:n.blank) sudR[row.blank[ij.rand[i]],col.blank[ij.rand[i]]]=NA
	display.sudoku(sudR)
	title(paste("Easy puzzle random sudoku, n empty=",n.blank,sep=""))

	# solution
	Digmat=matrix(nrow=n.blank,ncol=9)
	i9=1:9
	for(ib in 1:n.blank)
	{
		i=row.blank[ij.rand[ib]];j=col.blank[ij.rand[ib]]
		points(i-.5,j-.5,)
		sqi=3*floor((i-1)/3)+1;sqj=3*floor((j-1)/3)+1
		for(i1 in 0:2) for(i2 in 0:2) points(sqi+i1-.5,sqj+i2-.5,col=3,pch=4)
		print(c(sqi,sqj))
		digi=c(sudR[i,],sudR[,j],as.vector(sudR[sqi:(sqi+2),sqj:(sqj+2)]))
		digi=unique(digi[!is.na(digi)])
		k=0
		for(j in 1:9) 
		if(length(digi[digi==j])==0) 
		{
			k=k+1
			Digmat[ib,k]=j
		}			
		#return(Digmat)
	}
	return(Digmat)
	
}
}
