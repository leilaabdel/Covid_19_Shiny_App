library(shiny)

# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("When do we Flatten the Curve?"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    sliderInput("dayOfMaxGrowth" , "Select the Day since March 1st with the Predicted Max Growth Rate:" , min=1, max = 360 , value=30, step = 1),
    sliderInput("carryingCapacity" , "Select the Predicted Max Number of People Infected:" , min=70000, max= 214000000, value= 100000 , step=1000),
    sliderInput("daysSinceMarchFirst" , "Select the Days Since March 1st:" , min=0 , max=360 , value = 25 , step=1 )
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("linePlot")
  )


)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  output$linePlot <- renderPlot({
    
    a <- input$carryingCapacity/65 - 1
    b <- log(a)/input$dayOfMaxGrowth
    x <- seq(from = 0, to = input$daysSinceMarchFirst, by = 1)
    y <- input$carryingCapacity/(1+ a*exp(-b * x))
    plot(x,y ,pch=19, cex=.5, main="Number of Reported U.S. Cases over Time" , ylab="Reported Cases"
         , xlab = "Days since March 1st", type = "b", col="red")
  })
}

shinyApp(ui, server)