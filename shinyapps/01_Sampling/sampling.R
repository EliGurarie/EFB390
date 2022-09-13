require(magrittr)
require(shiny)
require(shinyjs)
library(shinyWidgets)


#alpha <-10
#hist(rbeta(1e5, alpha, 1), breaks = 20)


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

plotPopulation <- function(Z){
    par(mfrow = c(1,2), mgp = c(1.5,.25,0), tck = 0.01)
    plot(0,0, xlim = c(0,100),ylim = c(0,100), 
             type = "n", asp = 1, bty = "n", 
             xlab = "", ylab = "")    
    rect(0,0,100,100, col = NA, lwd = 3, border = "darkgrey")
    points(Z, pch = 21, bg = "antiquewhite", col = "darkorange")        
}

plotSampling <- function(sim){
    with(sim$xy.samples, rect(x.min,y.min,x.max,y.max, col = rgb(0,0,0,.2), bor = "darkgrey"))
    points(sim$Z.counted, pch=19, col = rgb(0,0,0,.3))
}

ui <- fluidPage(
    
    useShinyjs(),
   # setBackgroundColor("MidnightBlue"),
    tags$style(
        "*, div {
      font-family: Consolas;
    }"
    ),
    
    h1("EFB 370:  Quadrat Sampler 2000", style = "color: DarkRed"),
    
    sidebarPanel(
        
        h2("Population setup: "),
        
        numericInput("N", "Total number of individuals", 
                     value = 300, min = 1, step = 1),
        
        sliderInput(inputId = "leftright",
                    label = "Left-Right skew",
                    min = -5, max = 5, value = 0.1),
        sliderInput(inputId = "updown",
                    label = "Up-Down skew",
                    min = -5, max = 5, value = 0.1),
        
        h2("Sampling: "),
        
        numericInput("k", "Number of samples", 
                     value = 10, min = 1, step = 1),

        sliderInput(inputId = "size",
                    label = "Size of quadrat square (side)",
                    min = 1, max = 20, value = 10),

        actionButton("setup", "Set up population"),
        actionButton("sample", "Perform sampling")
        ),
    
    mainPanel(
        plotOutput("plots", height = '500px', width = '1000px'),
        h3("Final Counts:", style = "color: darkblue"),
        verbatimTextOutput("finalcounts"),
        tags$head(tags$style("#finalcounts{
                                 font-size: 20px;
                                 font-color: yellow;
                                 }"))
        )
    
    #fluidRow( 
    #    column(width = 7, style='padding:10px', plotOutput("plots"))),
    #textOutput("finalcounts")
)


# countSamples <- function(N = 300, k = 10, gradient = 1, size = 10)

server <- function(input, output) {
    settingup <- eventReactive(input$setup,
                         setupPop(N = input$N, 
                                  leftright = input$leftright,
                                  updown = input$updown))

    sampling <- eventReactive(input$sample,
                             countSamples(Z = Z, k = input$k, size = input$size))
    
    output$plots <- renderPlot({
        par(mfrow = c(1,2), mgp = c(1.5,.25,0), tck = 0.01)
        
        Z <- settingup()
        plotPopulation(Z)
        
        mysim <- sampling()
        plotSampling(mysim)
        hist(mysim$counts, breaks = seq(-.5, max(mysim$counts)+.5, .5)+.25, 
             xlab = "sample counts", cex.lab = 1.25, main = "Histogram of sample counts")
    })
    
    output$finalcounts <- renderPrint({
         cat(" A - total area:              ", "100 x 100",
             "\n a_s - sampled area:          ", sampling()$a_s,
             "\n c_s - sample count:          ", sum(sampling()$counts),
             "\n N_hat - estimated abundance: ", round(sampling()$N.hat, 2))},
        width = "200"
    )
}


shinyApp(ui=ui, server=server)
