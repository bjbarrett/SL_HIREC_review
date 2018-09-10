####Rogers infinite states, frequencies
w0 <- 1
b <- 3
c <- 1
U <- 0.05
s <- 0.8
timesteps <- 100
#wI <- w0 + b - c

q <- p <-  rep(0,timesteps + 1 )
u <- wS <- wI  <- wPOP <-  rep(0,timesteps )

phat <- (1-U*s*b/c)/(1-U)
q[1] <- 0
p[1] <- phat
u[1] <- 0
#u[sample(1:100, 5)] <- 1
#ul5 <- u
u <- ul5
for (t in 1:timesteps){
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	q[t+1] <- (1-u[t])*( (1-p[t])*s + p[t]*q[t]) + u[t]*0
	p[t+1] <- p[t]*wS[t]/(p[t]*wS[t] + (1-p[t])*wI)
	#u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}

plot(seq(1:101), p, pch=19, ylim=c(0.9, 1) )
points(seq(1:100), u, col="red" , pch=17 )

pdf("SLHIRECfig1_exp.pdf" , width=8.5 , height=6)

par(mfrow = c(4, 1) )
par(cex = 0.4)
par(oma = c(1,1,0.2,0.2) )
par(mar = c(2,4.2,0.1,0.1) )

plot( p ~ c(1:(timesteps + 1) ) , ylim=c(-0.05,1.09) , pch=19 , col="white" , ylab="frequency of social learners (p)" , cex.lab=1.4)
lines( (1:(timesteps +1)) , p , col="red", lty=1, lwd=2)
#abline(h=mean(p) , lty=2 , lw=1)
#title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c,"; w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)
points(  seq(1:timesteps) , (u-1) , pch=17)
text(seq(1:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 legend("topleft", legend="a) u=0.05", cex=1.5 , bty="n")
plot( q ~ c(1:(timesteps + 1) ) , ylim=c(-0.05,1.09) , pch=19 , col="white" , ylab="frequency of adaptive behavior" , cex.lab=1.4)
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=2)
#abline(h=mean(q) , lty=2 , lw=1)
points(  seq(1:timesteps) , (u-1) , pch=17)
text(seq(1:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 legend("topleft", legend="b) u=0.05", cex=1.5 , bty="n")


w0 <- 1
b <- 3
c <- 1
U <- 0.25
s <- 0.8
timesteps <- 100
#wI <- w0 + b - c


q <- p <-  rep(0,timesteps + 1 )
u <- wS <- wI  <- wPOP <-  rep(0,timesteps )

phat <- (1-U*s*b/c)/(1-U)
q[1] <- 0
p[1] <- phat
u[1] <- 0
#u[sample(1:100, 25)] <- 1
#ul25 <- u
u <- ul25
for (t in 1:timesteps){
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	q[t+1] <- (1-u[t])*( (1-p[t])*s + p[t]*q[t]) + u[t]*0
	p[t+1] <- p[t]*wS[t]/(p[t]*wS[t] + (1-p[t])*wI)
	#u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}

#par(mar = c(2,4,3,0.1) )
plot( p ~ c(1:(timesteps + 1) ) , ylim=c(-0.05,1.09) , pch=19 , col="white" , ylab="frequency of social learners (p)" , cex.lab=1.4)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey95")
lines( (1:(timesteps +1)) , p , col="red", lty=1, lwd=2)
#abline(h=mean(p) , lty=2 , lw=1)
#title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c,"; w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)
points(  seq(1:timesteps) , (u-1) , pch=17)
text(seq(1:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 legend("topleft", legend="c) u=0.25", cex=1.5 , bty="n")
 #par(mar = c(2,4,0.1,0.1) )
plot( q ~ c(1:(timesteps + 1) ) , ylim=c(-0.05,1.09) , pch=19 , col="white" , ylab="frequency of adaptive behavior" , cex.lab=1.4)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey95")
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=2)
#abline(h=mean(q) , lty=2 , lw=1)
points(  seq(1:timesteps) , (u-1) , pch=17)
text(seq(1:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 legend("topleft", legend="d) u=0.25", cex=1.5 , bty="n")

dev.off()


##########################
par(mfrow = c(4, 1) )
par(cex = 0.4)
par(oma = c(4,2,1,1) )
par(mar = c(2,4,0,0.1) )

u <- ul5
plot( p ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1.09) , pch=19 , col="white" , ylab="frequency of social learners (p)" , cex.lab=1.5)
lines( (1:(timesteps +1)) , p , col="red", lty=1, lwd=2)
#abline(h=mean(p) , lty=2 , lw=1)
#title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c,"; w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)
points(  seq(1:timesteps) , (u-1) , pch=17)
text(seq(1:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 legend("topleft", legend="a) u=0.05", cex=1.5 , bty="n")
plot( q ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1.09) , pch=19 , col="white" , ylab="frequency of adaptive behavior" , cex.lab=1.5)
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=2)
#abline(h=mean(q) , lty=2 , lw=1)
points(  seq(1:timesteps) , (u-1) , pch=17)
text(seq(1:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 legend("topleft", legend="b) u=0.05", cex=1.5 , bty="n")

 u <- ul25
 plot( p ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1.09) , pch=19 , col="white" , ylab="frequency of social learners (p)" , cex.lab=1.5)
lines( (1:(timesteps +1)) , p , col="red", lty=1, lwd=2)
#abline(h=mean(p) , lty=2 , lw=1)
#title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c,"; w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)
points(  seq(1:timesteps) , (u-1) , pch=17)
text(seq(1:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 legend("topleft", legend="c) u=0.25", cex=1.5 , bty="n")
 
plot( q ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1.09) , pch=19 , col="white" , ylab="frequency of adaptive behavior" , cex.lab=1.5)
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=2)
#abline(h=mean(q) , lty=2 , lw=1)
points(  seq(1:timesteps) , (u-1) , pch=17)
text(seq(1:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 legend("topleft", legend="d) u=0.25", cex=1.5 , bty="n")


####Rogers infinite states, frequencies
w0 <- 1
b <- 3
c <- 1
U <- 0.1
s <- 0.8
timesteps <- 100
#wI <- w0 + b - c

q <- p <- u <-  rep(0,timesteps + 1 )
wS <- wI  <- wPOP <-  rep(0,timesteps )

phat <- (1-U*s*b/c)/(1-U)
q[1] <- 0
p[1] <- phat
u[1] <- 0
u[3] <- u[9] <- u[32] <- u[53]<- u[64]<- u[66]<- u[70]<- u[73] <- u[81] <- u[99] <- 1

for (t in 1:timesteps){
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	wPOP[t] <- wI[t]*(1-p[t]) + wS[t]*p[t]
	q[t+1] <- (1-u[t])*( (1-p[t])*s + p[t]*q[t]) + u[t]*0
	p[t+1] <- p[t]*wS[t]/(p[t]*wS[t] + (1-p[t])*wI)
	#u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}

par(mfrow = c(3, 1) )
par(cex = 0.4)
par(oma = c(4,2,1,1) )
par(mar = c(2,4,0,0.1) )

plot( p ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1) , pch=19 , col="white" , ylab="frequency of social learners (p)" , cex.lab=1.5)
lines( (1:(timesteps +1)) , p , col="red", lty=1, lwd=2)
#abline(h=mean(p) , lty=2 , lw=1)
#title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c,"; w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)
points(  seq(0:timesteps) , (u-1) , pch=17)
text(seq(0:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 
plot( q ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1) , pch=19 , col="white" , ylab="frequency of adaptive behavior" , cex.lab=1.5)
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=2)
#abline(h=mean(q) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)
text(seq(0:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)

plot( wS ~ c(1:(timesteps) ) , ylim=c(0.9,b+w0) , pch=19 , col="white" , ylab="phenotypic fitness" , cex.lab=1.5)
lines( c(1:timesteps) , wS , col="orange", lty=1, lwd=2)
#abline(h=gm_mean(wS) , lty=2 , lw=1)
#points(  seq(0:timesteps) , (u-1) , pch=17)
lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=2)
lines( c(1:timesteps ) , wPOP , col="black", lty=1, lwd=2)
#abline(h=gm_mean(wI) , lty=2 , lw=1)
legend("topleft", legend=c("mean SL fitness (wS)", "mean IL fitness (wI)", "mean population fitness"),
       col=c("orange", "green" , "black"), lty=1, cex=1.2 , lw=2 , bty="n")
points(  seq(0:timesteps) , (u) , pch=17)
text(seq(0:timesteps) , (u), labels = seq(0:timesteps), pos = 1)

#plot( wI ~ c(1:timesteps ) , ylim=c(0,b+w0) , pch=19 , col="white" , ylab="fitness of individual learners (wI)" ,  xlab="time (# generations)" , cex.lab=1.3)
#lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=1)
#abline(h=gm_mean(wI) , lty=2 , lw=1)
#points(  seq(0:timesteps) , (u-1) , pch=17)


####Rogers infinite states, frequencies
w0 <- 1
b <- 3
c <- 1
U <- 0.1
s <- 0.8
timesteps <- 100
#wI <- w0 + b - c

q <- p <- u <-  rep(0,timesteps + 1 )
wS <- wI  <- wPOP <-  rep(0,timesteps )

phat <- (1-U*s*b/c)/(1-U)
q[1] <- 0
p[1] <- phat
u[1] <- 0
u[3] <- u[9] <- u[32] <- u[53]<- u[64]<- u[66]<- u[70]<- u[73] <- u[81] <- u[99] <- 1

for (t in 1:timesteps){
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	wPOP[t] <- wI[t]*(1-p[t]) + wS[t]*p[t]
	q[t+1] <- (1-u[t])*( (1-p[t])*s + p[t]*q[t]) + u[t]*0
	p[t+1] <- p[t]*wS[t]/(p[t]*wS[t] + (1-p[t])*wI)
	#u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}

par(mfrow = c(3, 1) )
par(cex = 0.4)
par(oma = c(4,2,1,1) )
par(mar = c(2,4,0,0.1) )

plot( p ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1) , pch=19 , col="white" , ylab="frequency of social learners (p)" , cex.lab=1.5)
lines( (1:(timesteps +1)) , p , col="red", lty=1, lwd=2)
#abline(h=mean(p) , lty=2 , lw=1)
#title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c,"; w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)
points(  seq(0:timesteps) , (u-1) , pch=17)
text(seq(0:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 
plot( q ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1) , pch=19 , col="white" , ylab="frequency of adaptive behavior" , cex.lab=1.5)
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=2)
#abline(h=mean(q) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)
text(seq(0:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)



####Rogers infinite states, frequencies
w0 <- 1
b <- 3
c <- 1
U <- 0.1
s <- 0.8
timesteps <- 100
#wI <- w0 + b - c

q <- p <- u <-  rep(0,timesteps + 1 )
wS <- wI  <- wPOP <-  rep(0,timesteps )

phat <- (1-U*s*b/c)/(1-U)
q[1] <- 0
p[1] <- phat
u[1] <- 0
u[3] <- u[9] <- u[32] <- u[53]<- u[64]<- u[66]<- u[70]<- u[73] <- u[81] <- u[99] <- 1

for (t in 1:timesteps){
	wI[t] <- w0 + s*b - c #just doing it in loop for vectorization and graphing ease
	wS[t] <- w0 + b*q[t] #fitness of social learners
	wPOP[t] <- wI[t]*(1-p[t]) + wS[t]*p[t]
	q[t+1] <- (1-u[t])*( (1-p[t])*s + p[t]*q[t]) + u[t]*0
	p[t+1] <- p[t]*wS[t]/(p[t]*wS[t] + (1-p[t])*wI)
	#u[t+1] <- rbinom(n=1,prob=U,size=1) #sample environment changing in next gen, if 0 all SL behav maladaptive
}

par(mfrow = c(3, 1) )
par(cex = 0.4)
par(oma = c(4,2,1,1) )
par(mar = c(2,4,0,0.1) )

plot( p ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1) , pch=19 , col="white" , ylab="frequency of social learners (p)" , cex.lab=1.5)
lines( (1:(timesteps +1)) , p , col="red", lty=1, lwd=2)
#abline(h=mean(p) , lty=2 , lw=1)
#title(paste("u=",U,"; s=",s,"; b=",b,"; c=",c,"; w0=",w0, sep=" ") , outer=TRUE , cex.main=2 , line=-0.5)
points(  seq(0:timesteps) , (u-1) , pch=17)
text(seq(0:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)
 
plot( q ~ c(1:(timesteps + 1) ) , ylim=c(-0.02,1) , pch=19 , col="white" , ylab="frequency of adaptive behavior" , cex.lab=1.5)
lines( (1:(timesteps +1)) , q , col="blue", lty=1, lwd=2)
#abline(h=mean(q) , lty=2 , lw=1)
points(  seq(0:timesteps) , (u-1) , pch=17)
text(seq(0:timesteps) , (u-1), labels = seq(0:timesteps), pos = 1)

plot( wS ~ c(1:(timesteps) ) , ylim=c(0.9,b+w0) , pch=19 , col="white" , ylab="phenotypic fitness" , cex.lab=1.5)
lines( c(1:timesteps) , wS , col="orange", lty=1, lwd=2)
#abline(h=gm_mean(wS) , lty=2 , lw=1)
#points(  seq(0:timesteps) , (u-1) , pch=17)
lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=2)
lines( c(1:timesteps ) , wPOP , col="black", lty=1, lwd=2)
#abline(h=gm_mean(wI) , lty=2 , lw=1)
legend("topleft", legend=c("mean SL fitness (wS)", "mean IL fitness (wI)", "mean population fitness"),
       col=c("orange", "green" , "black"), lty=1, cex=1.2 , lw=2 , bty="n")
points(  seq(0:timesteps) , (u) , pch=17)
text(seq(0:timesteps) , (u), labels = seq(0:timesteps), pos = 1)

#plot( wI ~ c(1:timesteps ) , ylim=c(0,b+w0) , pch=19 , col="white" , ylab="fitness of individual learners (wI)" ,  xlab="time (# generations)" , cex.lab=1.3)
#lines( c(1:timesteps ) , wI , col="green", lty=1, lwd=1)
#abline(h=gm_mean(wI) , lty=2 , lw=1)
#points(  seq(0:timesteps) , (u-1) , pch=17)