# app.R
library(shiny)

SeaLions <- read.csv("SeaLions.csv", stringsAsFactors = FALSE)

Lbar <- mean(SeaLions$Length)
Wbar <- mean(SeaLions$Weight)
Lseq <- range(SeaLions$Length)
Wrng <- range(SeaLions$Weight)

Lbar_f <- mean(SeaLions$Length[SeaLions$Sex == "F"])
Lbar_m <- mean(SeaLions$Length[SeaLions$Sex == "M"])
Wbar_f <- mean(SeaLions$Weight[SeaLions$Sex == "F"])
Wbar_m <- mean(SeaLions$Weight[SeaLions$Sex == "M"])

logLik_manual <- function(y, mu) {
    n  <- length(y)
    s2 <- sum((y - mu)^2) / n          # profiled MLE of sigma^2
    -n/2 * (log(2*pi) + log(s2) + 1)
}

ui <- fluidPage(
    tags$head(tags$meta(name = "viewport",
                        content = "width=device-width, initial-scale=1")),
    tags$style(HTML("
    .fitbox {font-size:18px; font-weight:bold; text-align:center;
             padding:8px; border:1px solid #ccc; border-radius:6px; margin-top:8px;}
    .shiny-input-container {width:100% !important;}
    h5 {margin-bottom:2px; font-weight:bold;}
  ")),
    
    titlePanel("Sea Lion Weight ~ Length"),
    
    plotOutput("plot", height = "300px"),
    div(class = "fitbox", htmlOutput("fit")),
    
    radioButtons("model", "Model:", c("pooled", "sex-specific"), inline = TRUE),
    
    conditionalPanel("input.model == 'pooled'",
                     sliderInput("b0", "mean weight (kg)",
                                 round(Wrng[1]), round(Wrng[2]), round(Wbar), step = 0.5),
                     sliderInput("b1", "length effect (kg/cm)", 0, 3, 1, step = 0.02)
    ),
    conditionalPanel("input.model == 'sex-specific'",
                     h5("Female (purple)"),
                     sliderInput("b0f", "mean weight (kg)",
                                 round(Wrng[1]), round(Wrng[2]), round(Wbar_f), step = 0.5),
                     sliderInput("b1f", "length effect (kg/cm)", 0, 3, 1, step = 0.02),
                     h5("Male (orange)"),
                     sliderInput("b0m", "mean weight (kg)",
                                 round(Wrng[1]), round(Wrng[2]), round(Wbar_m), step = 0.5),
                     sliderInput("b1m", "length effect (kg/cm)", 0, 3, 1, step = 0.02)
    )
)

server <- function(input, output) {
    
    mu <- reactive({
        if (input$model == "pooled") {
            input$b0 + input$b1 * (SeaLions$Length - Lbar)
        } else {
            ifelse(SeaLions$Sex == "F",
                   input$b0f + input$b1f * (SeaLions$Length - Lbar_f),
                   input$b0m + input$b1m * (SeaLions$Length - Lbar_m))
        }
    })
    
    output$plot <- renderPlot({
        par(mar = c(4, 4, 1, 1))
        cols <- ifelse(SeaLions$Sex == "F", "purple", "orange")
        plot(Weight ~ Length, data = SeaLions, pch = 19, cex = 0.6,
             col = if (input$model == "pooled") "grey60" else adjustcolor(cols, 0.4),
             xlab = "Length (cm)", ylab = "Weight (kg)")
        
        drawfit <- function(b0, b1, Lc, col) {
            lines(Lseq, b0 + b1 * (Lseq - Lc), col = col, lwd = 3)
            points(Lc, b0, pch = 19, cex = 2.2, col = col)
        }
        
        if (input$model == "pooled") {
            drawfit(input$b0, input$b1, Lbar, "black")
        } else {
            drawfit(input$b0f, input$b1f, Lbar_f, "purple")
            drawfit(input$b0m, input$b1m, Lbar_m, "orange")
        }
    })

    output$fit <- renderText({
        ll <- logLik_manual(SeaLions$Weight, mu())
        k  <- if (input$model == "pooled") 3 else 5
        HTML(sprintf("logLikelihood = %.1f<br>AIC = %.1f", ll, -2*ll + 2*k))
    })
}

shinyApp(ui, server)