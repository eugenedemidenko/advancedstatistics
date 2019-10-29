birthdaysim <-
function(job=1,n.people=23,Nexp=100000)
{
dump("birthdaysim","c:\\statbook\\birthdaysim.r")
ii=1:365
if(job==1)
{
prob<-0 # intitialization
for(iexp in 1:Nexp)
{
	birthday<-ceiling(runif(n.people)*365) # generate birthdays


	uniq.birthday<-unique(birthday) # different birthdays
	if(length(uniq.birthday)<n.people) prob<-prob+1
      # if the number of different birthdays<n.people then at least two people have the same birthday

}
}
if(job==2)
{
prob<-0 # intitialization
i365=1:365

pdf=.9-0.2*sin(2*pi/365*ii)
pdf=cumsum(pdf)/sum(pdf)
plot(i365,pdf,type="l")
birthday=rep(NA,n.people)

for(iexp in 1:Nexp)
{
	for(j in 1:n.people)
	{
		cdj=runif(1)
		aj=abs(cdj-pdf)
		birthday[j]=i365[aj==min(aj)]
	}
	uniq.birthday<-unique(birthday) # different birthdays
	if(length(uniq.birthday)<n.people) prob<-prob+1
      # if the number of different birthdays<n.people then at least two people have the same birthday

}
}



prob<-prob/Nexp
return(prob)
}
