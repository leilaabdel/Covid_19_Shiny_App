## Load deSolve package
library(deSolve)
library(shiny)


ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Flattening the Curve"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    sliderInput("beta" , "Level of Social Distancing" ,  min=0.01, max = 3 , value=0.5, step = 0.001),
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("linePlot")
  )
  
  
)

## Create an SIR function
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    
    return(list(c(dS, dI, dR)))
  })
}




### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init       <- c(S = 1-1e-6, I = 1e-6, R = 0.0)
## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = 1.4247, gamma = 0.23)
## Time frame
times      <- seq(0, 70, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)


# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  output$linePlot <- renderPlot({
    
    ### Set parameters
    ## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
    init       <- c(S = 1-1e-6, I = 1e-6, R = 0.0)
    ## beta: infection parameter; gamma: recovery parameter
    parameters <- c(beta = 3 - input$beta, gamma = 0.14286)
    ## Time frame
    times <- seq(0, 150, by = 1)
    
    ## Solve using ode (General Solver for Ordinary Differential Equations)
    out <- ode(y = init, times = times, func = sir, parms = parameters)
    ## change to data frame
    out <- as.data.frame(out)
    ## Delete time variable
    out$time <- NULL
    ## Show data
    
    ## Plot
    matplot(x = times, y = out*100, type = "l",
            xlab = "Days", ylab = "Percentage of Population (%)", main = "Susceptible, Recovered, and Infected",
            lwd = 1, lty = 1, bty = "l", col= c("blue","red","green"))
    
    ## Add legend
    legend(120, 80, c("Susceptible", "Infected", "Recovered"), pch = 1, col = c("blue","red","green"), bty = "n")


  })
}

shinyApp(ui, server)