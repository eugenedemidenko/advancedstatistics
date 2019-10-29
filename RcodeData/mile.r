mile <-
function()
{
dump("mile","c:\\StatBook\\mile.r")
n=2*10*52*5 #number of times go to work over 10 years
ex.dist=5+1387/5280 #exact distance to work
ml=rep(NA,n) #allocate an array of odometer miles  
for(i in 1:n)
{
	ex.trav=ex.dist+runif(1) #exact distance covered by car 
	ml[i]=floor(ex.trav) #miles shown on the odometer
}
cat("Exact distance to work =",ex.dist,"miles\n")
cat("Estimated distance to work over ten years =",mean(ml),"miles\n")
}
