CountSimulation <- function(range=100, N=100, side=10, n.Squares=5)
{
	
	# generate population

		Pop.x <- runif(N, -100/2, 100/2)
		Pop.y <- runif(N, -100/2, 100/2)
		Pop <- data.frame(x=Pop.x, y=Pop.y)

	# generate sampling squares

		n.quads <- (range*2)^2
		
		Square.x <- sample(c(-40,-20,0,20,40))
		Square.y <- sample(c(-40,-20,0,20,40))
		Square.xy <- data.frame(Square.x, Square.y)
		
		dx <- rep(c(-side/2, -side/2, side/2, side/2, NA), n.Squares)
		dy <- rep(c(-side/2, side/2, side/2, -side/2, NA), n.Squares)

		Squares.Xs <- rep(Square.x, each=n.Squares) + dx
		Squares.Ys <- rep(Square.y, each=n.Squares) + dy
		
		Squares.poly <-data.frame(x=Squares.Xs, y=Squares.Ys)
		
		n.quads <- (range*2)^2
	
		Square.x <- sample(seq(-range/2 + side, range/2 - side, length=n.Squares))
		Square.y <- sample(seq(-range/2 + side, range/2 - side, length=n.Squares))
		Square.xy <- data.frame(Square.x, Square.y)
		
		dx <- rep(c(-side/2, -side/2, side/2, side/2, NA), n.Squares)
		dy <- rep(c(-side/2, side/2, side/2, -side/2, NA), n.Squares)

		Squares.Xs <- rep(Square.x, each=n.Squares) + dx
		Squares.Ys <- rep(Square.y, each=n.Squares) + dy
		
		Squares.poly <-data.frame(x=Squares.Xs, y=Squares.Ys)
		
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
		Area <- range^2
		D.hat <- sum(CountData$Counts)/sum(CountData$Area)
		D.hat * Area
}
save(CountSimulation, file="CountSimulation.robj")