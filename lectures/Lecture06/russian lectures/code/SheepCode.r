# EXAMPLE 1: RANDOM DISTRIBUTION, SUB-COUNT
rm(list=ls())
setwd("c:/eli/teaching/IPEE101/lecture1_PoissonProcess")

SheepCount <- read.csv("./data/SheepCount.csv")

X <- SheepCount$X
Y <- SheepCount$Y

pdf("./example/SheepData.pdf", width=8)
plot(X,Y, asp=1, xlab="x (m)", ylab="y (m)",pch=21, bty="n", bg="antiquewhite", col="darkgreen", lwd=2, cex=1.5)
dev.off()

Pop <- data.frame(X,Y)

# Get range

	range.X <- range(X)
	range.Y <- range(Y)
	
# Get area
	
	A <- diff(range(X)) * diff(range(Y))
	D <- length(X)/A
	
	plot(X,Y, asp=1)
	
# Subsample squares!

	n.squares <- 20
	side <- 3

	CountSheep <- function(n.squares, area.square)
	{
		side <- sqrt(area.square)
		
		Square.x <- runif(n.squares, min(X) + side, max(X)-side)
		Square.y <- runif(n.squares, min(Y) + side, max(Y)-side)
		
		Square.xy <- data.frame(Square.x, Square.y)
		
		
		dx <- rep(c(-side/2, -side/2, side/2, side/2, NA), n.squares)
		dy <- rep(c(-side/2, side/2, side/2, -side/2, NA), n.squares)

		Squares.Xs <- rep(Square.x, each=n.squares) + dx
		Squares.Ys <- rep(Square.y, each=n.squares) + dy
		
		Squares.poly <- data.frame(x=Squares.Xs, y=Squares.Ys)
		
	# create simulated count data
		
		require(splancs)
		Counts <- vector(len=0)
		WhichIn <- data.frame(x=Counts, y=Counts)
		for(i in 1:n.squares-1)
		{	WhichIn <- rbind(WhichIn, pip(Pop, Squares.poly[1:4+n.squares*i,]))
			Counts <- c(Counts, sum(inout(Pop, Squares.poly[1:4+n.squares*i,])))
		}
		
		return(data.frame(Site = 1:n.squares, X=Square.x, Y=Square.y, Counts, Area = area.square))
		
		par(mfrow=c(1,2))
		plot(X,Y, asp=1)
		points(Square.xy, pch=4, col=2, cex=2, lwd=2)
		hist(Counts, col="grey")
				
		print(paste("Estimate", round(sum(Counts)/(n.squares*side^2) * A,1),
				"; True Value", length(X), 
				"; Mean", mean(Counts), "and Variance",round(var(Counts),2)))
	}

	write.csv(CountSheep(20,4), "./data/SheepCountA.csv", row.names=FALSE)
	
	SheepCount.20.10 <- CountSheep(20, 10)
	write.csv(SheepCount.20.10, "./data/SheepCount.20.10.csv", row.names=FALSE)
	
	SheepCount.20.40 <- CountSheep(20, 40)
	write.csv(SheepCount.20.40, "./data/SheepCount.20.40.csv", row.names=FALSE)
	
	SheepCount.20.100 <- CountSheep(20, 100)
	write.csv(SheepCount.20.100, "./data/SheepCount.20.100.csv", row.names=FALSE)
	
	SheepCount.50.10 <- CountSheep(50, 10)
  write.csv(SheepCount.50.10, "./data/SheepCount.50.10.csv", row.names=FALSE)
	
  SheepCount.10.50 <- CountSheep(10, 50)
  write.csv(SheepCount.10.50, "./data/SheepCount.10.50.csv", row.names=FALSE)
  
  
	par(mfrow=c(1,2))
	plot(X,Y, asp=1)
	points(Square.xy, pch=4, col=2, cex=2, lwd=2)
	
	sum(Counts)/(n.Squares*side^2) * A
	hist(Counts, col="grey")
	mean(Counts)
	var(Counts)