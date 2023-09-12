# EXAMPLE 1: RANDOM DISTRIBUTION, SUB-COUNT

setwd("c:/eli/teaching/IPEE101/lecture2")
	
# Illustrate sampling and population

	range <- 100

# generate population

	N <- 100
	Pop.x <- runif(N, -100/2, 100/2)
	Pop.y <- runif(N, -100/2, 100/2)
	Pop <- data.frame(x=Pop.x, y=Pop.y)

# generate sampling squares

	n.quads <- (range*2)^2
	n.Squares <- 5
	side <- 10

	Square.x <- sample(c(-40,-20,0,20,40))
	Square.y <- sample(c(-40,-20,0,20,40))
	Square.xy <- data.frame(Square.x, Square.y)
	
	dx <- rep(c(-side/2, -side/2, side/2, side/2, NA), n.Squares)
	dy <- rep(c(-side/2, side/2, side/2, -side/2, NA), n.Squares)

	Squares.Xs <- rep(Square.x, each=n.Squares) + dx
	Squares.Ys <- rep(Square.y, each=n.Squares) + dy
	
	Squares.poly <-data.frame(x=Squares.Xs, y=Squares.Ys)
	
# create simulated count data
	
	require(splancs)
	Counts <- vector(len=0)
	WhichIn <- data.frame(x=Counts, y=Counts)
	for(i in 1:n.Squares-1)
	{	WhichIn <- rbind(WhichIn, pip(Pop, Squares.poly[1:4+n.Squares*i,]))
		Counts <- c(Counts, sum(inout(Pop, Squares.poly[1:4+n.Squares*i,])))
	}
	CountData <- data.frame(Square = LETTERS[1:n.Squares], 
							X = Square.xy[,1],
							Y = Square.xy[,2],
							Counts, 
							Area = side^2)
		

# Save ell the good stuff!

	save(Pop, range, N,  file="Model1.robj")
	save(Squares.poly, Square.xy, n.Squares, side, file="Model1Squares.robj")
	save(WhichIn, CountData, file="Model1Count.robj")
	
# load Model 1 
	rm(list=ls())
	load("Model1.robj")
	load("Model1Squares.robj")
	load("Model1Count.robj")
	
# plot world

pdf("Model1A.pdf")
	par(mar=c(4,4,2,2))
	plot(0,0,type="n", xlim=c(-range,range)/2, ylim=c(-range,range)/2,
			xlab="", ylab="", asp=1)
	points(Pop, pch=19, col=rgb(.5,.5,.5,.5), cex=1.5)
dev.off()

pdf("Model1B.pdf")
	par(mar=c(4,4,2,2))
	plot(0,0,type="n", xlim=c(-range,range)/2, ylim=c(-range,range)/2,
			xlab="", ylab="", asp=1)
		points(Pop, pch=19, col=rgb(.5,.5,.5,.5), cex=1.5)
	polygon(Squares.poly, bor=2, lwd=1.5)
	text(Square.xy[,1], Square.xy[,2], LETTERS[1:n.Squares], cex=2, col="lightblue")
dev.off()

pdf("Model1C.pdf")
	par(mar=c(4,4,2,2))
	plot(0,0,type="n", xlim=c(-range,range)/2, ylim=c(-range,range)/2,
			xlab="", ylab="", asp=1)
		points(Pop, pch=19, col=rgb(.5,.5,.5,.5), cex=1.5)
	polygon(Squares.poly, bor=2, lwd=1.5)
	text(Square.xy[,1], Square.xy[,2], LETTERS[1:n.Squares], cex=2, col="lightblue")
	points(WhichIn, pch=4, col="darkgreen", cex=3, lwd=2)
dev.off()

# obtain estimate
	
	require(xtable)
	xtable(CountData, digits=0, include.rownames=FALSE)
	

	
#  DO ANOTHER ONE, WITH THE SAME POPULATION

# generate sampling squares

	n.quads <- (range*2)^2
	n.Squares <- 5
	side <- 10

	Square.x <- sample(c(-40,-20,0,20,40))
	Square.y <- sample(c(-40,-20,0,20,40))
	Square.xy <- data.frame(Square.x, Square.y)
	
	dx <- rep(c(-side/2, -side/2, side/2, side/2, NA), n.Squares)
	dy <- rep(c(-side/2, side/2, side/2, -side/2, NA), n.Squares)

	Squares.Xs <- rep(Square.x, each=n.Squares) + dx
	Squares.Ys <- rep(Square.y, each=n.Squares) + dy
	
	Squares.poly <-data.frame(x=Squares.Xs, y=Squares.Ys)
	
# create simulated count data
	
	require(splancs)
	Counts <- vector(len=0)
	WhichIn <- data.frame(x=Counts, y=Counts)
	for(i in 1:n.Squares-1)
	{	WhichIn <- rbind(WhichIn, pip(Pop, Squares.poly[1:4+n.Squares*i,]))
		Counts <- c(Counts, sum(inout(Pop, Squares.poly[1:4+n.Squares*i,])))
	}
	CountData <- data.frame(Square = LETTERS[1:n.Squares], 
							X = Square.xy[,1],
							Y = Square.xy[,2],
							Counts, 
							Area = side^2)
							
	save(Squares.poly, Square.xy, n.Squares, side, file="Model1BSquaresTake2.robj")
	save(WhichIn, CountData, file="Model1CountTake2.robj")
	
# load Model 1 
	rm(list=ls())
	load("Model1.robj")
	load("Model1BSquaresTake2.robj")
	load("Model1CountTake2.robj")
	
pdf("Model1CTake2.pdf")
	par(mar=c(4,4,2,2))
	plot(0,0,type="n", xlim=c(-range,range)/2, ylim=c(-range,range)/2,
			xlab="", ylab="", asp=1)
		points(Pop, pch=19, col=rgb(.5,.5,.5,.5), cex=1.5)
	polygon(Squares.poly, bor=2, lwd=1.5)
	text(Square.xy[,1], Square.xy[,2], LETTERS[1:n.Squares], cex=2, col="lightblue")
	points(WhichIn, pch=4, col="darkgreen", cex=3, lwd=2)
dev.off()

require(xtable)
xtable(CountData, digits=0, include.rownames=FALSE)
	
# Create function to generate counts 
load("CountSimulation.robj")
Count.Sims <- vector(len=0)
	for(i in 1:1000)
		Count.Sims <- c(Count.Sims, CountSimulation())

pdf("CountSims.pdf", height=4, width=8)
	par(mar=c(4,4,1,2))
	hist(Count.Sims, col="grey", bor="darkgrey", xlab="", main="")
	abline(v=median(Count.Sims), col=2, lwd=3, lty=2)
	abline(v=quantile(Count.Sims, c(.025, .975)), col=2, lwd=2, lty=3)
dev.off()


# Illustate count squares


for(lambda in c(1,4,10))
{
pdf(paste("Poisson",lambda,".pdf",sep=""), width=10, height=5)
	M <- rbind(c(1:3,10,10,10), c(4:6,10,10,10), c(7:9,10,10,10))
	layout(M)
	
	n <- rpois(9, lambda=lambda)
	par(mar=c(0,0,0,0))
	for(i in 1:9)
	{
		plot(runif(n[i],0,1), runif(n[i],0,1), 
			pch=21, bg="green", col="darkgreen", cex=3, lwd=2,
			xlim=c(0,1), ylim=c(0,1),
			xaxt="n", yaxt="n", xlab="n", ylab="", main="", asp=1)
		text(0,.85, n[i], col="navyblue", cex=3, font=3, pos=4)
	}
	par(mar=c(4,6,0,0), cex.lab=2, cex.axis=1.25)
			hist(rpois(1000,lambda), breaks=-.5:1000.5, 
					main="", xlab="Count",
					xlim=c(-.5,max(6.5,lambda*2.5+.5)), col="grey", bor="grey20")
	dev.off()
}

Text <- function(text="A", location="topright", cex=3)
	legend(location, title=text, legend=NA, cex=cex, pch=NA, pt.cex=0, bty="n")

	
pdf("PoissonPlot.pdf", width = 12, height = 4)
	par(mar=c(4,6,2,2), cex.lab=2, cex.axis=1.25, mfrow=c(1,3))
	xmax <- c(6,14,25)+.5
	for(lambda in c(1,4,10))
	{
			rp <- rpois(1000,lambda)
			hist(rp, breaks=-.5:1000.5, main="",
					xlab="",
					xlim=c(-.5,xmax[match(lambda, c(1,4,10))]), 	
					col="grey", bor="grey40")
			abline(v = c(mean(rp), mean(rp)-sqrt(sd(rp)), mean(rp)+sqrt(sd(rp))), col=2, lwd=2, lty=c(1,3,3))
			Text(as.expression(bquote(lambda == .(lambda))))
			Text(paste("E(X) = ",round(mean(rp),1),"\n Var(X) = ",round(sd(rp)^2,1),"\n SD(X) = ",round(sd(rp),1),sep=""), 
					location="bottomright", cex=1.5)
	}
dev.off()


SE.predicted <- function(a=.01, D=.01) D/a

library(gplots)
colors <- rich.colors(10)

########################
# Plot Error Theory
########################

pdf("SDExpected.pdf", width=8, height=5)
	par(cex.lab=1.25, mar=c(4.5,4.5,2,2))
	curve(sqrt(D/x), xaxt="n", ylab="Expected standard deviation", xlab="Total area surveyed", ylim=c(0,10))
	axis(1, at=c(0,.25,.5,.75,1), labels=paste(c(0,.25,.5,.75,1), "A"), cex=1.5)
	for(D in 1:10)
		curve(sqrt(D/x), add=TRUE, col=colors[D], lwd="2")
	legend("topright", col=colors, lty=1, lwd=2, legend=1:10, title="Density", ncol=2)
dev.off()

