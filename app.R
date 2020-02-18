#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

xsim = matrix(rnorm(100), 50, 2 )
ysim = rep(c(-1, 1), c(25, 25))
xsim[ysim == 1, ] = xsim[ysim == 1,] + 2
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("SVM"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("cost",
                        "Cost:",
                        min = .01,
                        max = 20,
                        value = 200)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #cost <- seq(0.01, 10, length.out = input$cost+1)
        library(e1071)
        # plot(xsim, col =as.factor(ysim), pch = 19,
        #      xlab = "x1", ylab = "x2")
        
        dat = data.frame(xsim, y = as.factor(ysim))
        svmfit = svm(y ~ ., data = dat, kernel = "linear", cost=input$cost, scale = FALSE)
        
        make.grid = function(x, n = 75) {
            grange = apply(x, 2, range)
            x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
            x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
            expand.grid(X1 = x1, X2 = x2)
        }
        
        xgrid = make.grid(xsim)
        
        ygrid = predict(svmfit, xgrid)
        
        beta = drop(t(svmfit$coefs)%*%xsim[svmfit$index,])
        beta0 = svmfit$rho
        
        
        
        plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], type = "n")
        points(xsim, col = ysim + 3, pch = 19)
        points(xsim[svmfit$index,], pch = 1, cex = 3)
        abline(beta0 / beta[2], -beta[1] / beta[2])
        abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
        abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)
        

        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
