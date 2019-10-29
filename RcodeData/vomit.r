vomit <-
function(job = 1)
{
	dump("vomit", "c:\\StatBook\\vomit.r")
	
	td <- matrix(scan("c:\\StatBook\\emesis.txt"), nrow = 2)
	dose <- td[1,  ]
	hour <- td[2,  ]
	Ldose <- log10(dose) # log10 dose values
	Lhour <- log10(hour) # log10 time to vomiting values
	n <- length(dose)
	
	if(job == 1) {
		crit <- c(43, 87, 104, 105, 106, 108, 67, 52, 60) # Criticality accidents
		gamma <- c(79, 92, 98, 101, 107, 76, 83, 84, 91, 19, 61, 48, 31, 40, 24) # Gamma accidents
		cv <- c(crit, gamma)
		ii <- 1:n
		k <- 0
		for(i in 1:n)
			if(length(cv[cv == i]) == 0) {
				k <- k + 1
				ii[k] <- i
			}
		ii <- ii[1:k]
		par(mfrow = c(1, 1), mar = c(4, 4, 1, 1))
		plot(dose, hour, xlim = c(0, 10), ylim = c(0, 20), type = "n", xlab = "", ylab = "", axes = T)
		for(i in 0:10)	segments(i, -1, i, 21, col="gray80")
		for(i in 0:10)	segments(-1, i * 2.5, 11, i * 2.5, col="gray80")
		
		points(dose[ii], hour[ii], pch = 1, cex = 2)
		points(dose[gamma], hour[gamma], pch = 2, cex = 2)
		points(dose[crit], hour[crit], pch = 3, cex = 2)
		mtext(side = 1, "Radiation dose received, Gy", cex = 1.5, line = 2.5)
		mtext(side = 2, "Hours to vomiting", cex = 1.5, line = 2.5)
		segments(2, -1, 2, 30, lwd=3)
		text(2.2, 12, "Medical cut-off, 2 Gy", adj = 0, font = 2, srt = 90, cex = 1.25)
		
		legend(3.25,20, c("Chernobyl (USSR), 1986 (n = 84)", "Gamma accidents-REAC/TS, 1945-2001 (n = 15)","Criticality accidents-REAC/TS, 1945-2001 (n = 9)"), pch = 1:3, cex = 1.5,bg="gray92")
	}
	if(job == 2) {
		trh <- 2 # Cut-off radiation
		par(mfrow = c(1, 1), mar = c(4, 4, 4, 1))
		tem <- seq(from = -1, to = 2, length = 1000)
		X0 <- Lhour[dose < trh]
		m0 <- mean(X0) # average time to vomiting for < trh 
		n0 <- length(X0)
		sd0 <- sqrt(var(X0)) # sd time to vomiting for < trh 
		X1 <- Lhour[dose >= trh]
		m1 <- mean(X1) # average time to vomiting for > trh 
		n1 <- length(X1)
		sd1 <- sqrt(var(X1)) # sd time to vomiting for > trh 
		
		k <- 0
		Ltrh <- log10(trh)
		Ldose <- Ldose[order(Lhour)]
		ee <- sens <- falsp <- Lhour <- Lhour[order(Lhour)] 
		AROC <- 0
		for(xtrh in Lhour) {
			k <- k + 1
			xLtrh <- Lhour[k]
			sens[k] <- length(ee[(Lhour < xLtrh) & (Ldose > Ltrh)])/length(ee[Ldose > Ltrh])
			falsp[k] <- length(ee[(Lhour < xLtrh) & (Ldose < Ltrh)])/length(ee[Ldose < Ltrh])
			if(k>1) AROC=AROC+sens[k]*(falsp[k]-falsp[k-1])			
		}
		plot(falsp, sens, type = "s", lwd=2, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), axes = F)
		axis(side = 1, at = seq(from = 0, to = 1, by = 0.2))
		segments(0,0,1,1,col="gray50")
		mtext(side = 3, "Time to vomiting, hours", line = 2.75, cex = 1.5)
		text(.7,.2,paste("AUC = ",round(AROC*100,1),"%",sep=""),cex=2)
		cXL <- c(1,2,2.5,3,3.5,4,5,8,24)		
		cp=pnorm(log10(cXL),mean=m0,sd=sd0)
		axis(side = 3, at = cp, labels = as.character(cXL), cex = 1.1)
		mtext(side = 2, "Sensitivity", line = 2.75, cex = 1.5)
		axis(side = 2, at = seq(from = 0, to = 1, by = 0.2), srt = 90)
		mtext(side = 1, "False positive (1-Specificity)", line = 2.5, cex = 1.5)
		Ht <- pnorm((tem - m1)/sd1)
		Pt <- pnorm((tem - m0)/sd0)
		lines(Pt, Ht, lwd = 5)		
		
		muX=mean(X0);sdX=sd(X0)
		muY=mean(X1);sdY=sd(X1)
		term1=muX*sdY^2-muY*sdX^2
		undsq=term1^2-(sdY^2-sdX^2)*(sdY^2*muX^2-sdX^2*muY^2+sdX^2*sdY^2*log(sdX^2/sdY^2))
		u.opt=(term1-sqrt(undsq))/(sdY^2-sdX^2)
		Fx.opt=pnorm((u.opt-muX)/sdX);Fy.opt=pnorm((u.opt-muY)/sdY)
		points(Fx.opt,Fy.opt,cex=2);segments(Fx.opt,-1,Fx.opt,1.2,col="gray50")
		x=c(0,.5);lines(x,Fy.opt+(x-Fx.opt),col="gray50")
		uh=10^u.opt
		text(Fx.opt+.01,1,paste(round(uh,2),"h"),cex=1.5,font=4,adj=0)
		cat("Time threshold=",round(uh,2),"\nfalse positive rate=",round(Fx.opt,2),"\nsensitivity=",round(Fy.opt,2),sep="")		
		
	}		
}
