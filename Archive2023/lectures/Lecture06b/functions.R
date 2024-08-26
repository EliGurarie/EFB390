
setupPop <-  function(N = 300,  
                      leftright = 0,
                      updown = 0){
    
    if(leftright < 0)
    {alpha1 = 1; beta1 = -(1+leftright)} else {
        alpha1= 1+leftright; beta1 = 1}
    
    if(updown < 0){
        alpha2 = 1; beta2 = -(1+updown)} else {
            alpha2 = 1+updown; beta2 = 1}
    
    x <- rbeta(N, alpha1, beta1) * 100
    y <- rbeta(N, alpha2, beta2) * 100
    
    Z <- x + (0 + 1i) * y
    return(Z)
}


plotPopulation <- function(Z){
    plot(0,0, xlim = c(0,100),ylim = c(0,100), 
         type = "n", asp = 1, bty = "n", 
         xlab = "", ylab = "")    
    rect(0,0,100,100, col = NA, lwd = 3, border = "darkgrey")
    points(Z, pch = 21, bg = "antiquewhite", col = "darkorange", cex = 2)        
}

countSamples <- function(Z, k = 10, size = 10) {
    
    Z.samples <- runif(k, size/2, 100-size/2) + 1i * runif(k, size/2, 100-size/2)
    xy.samples <- data.frame(x.min = Re(Z.samples)-size/2,
                             x.max = Re(Z.samples)+size/2,
                             y.min = Im(Z.samples)-size/2,
                             y.max = Im(Z.samples)+size/2)
    
    Z.which <- apply(xy.samples, 1, function(z){
        which(Re(Z) > z["x.min"] & Re(Z) < z["x.max"] & Im(Z) > z["y.min"] & Im(Z) < z["y.max"])
    })
    
    Z.counted <- Z[unlist(Z.which)]
    counts <- sapply(Z.which, length)
    
    d.hat <- sum(counts)/(k * size^2)
    list(Z = Z, Z.samples = Z.samples, Z.counted = Z.counted, 
         xy.samples = xy.samples, size = size, a_s = k*size^2, 
         counts = counts, N.hat = d.hat * 100^2)
}

plotSampling <- function(sim){
    with(sim$xy.samples, rect(x.min,y.min,x.max,y.max, col = rgb(0,0,0,.2), bor = "darkgrey"))
    points(sim$Z.counted, pch=19, col = rgb(0,0,0,.3))
}