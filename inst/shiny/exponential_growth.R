#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(WILD3810)
ui <- fluidPage(theme = shinytheme("spacelab"),
  titlePanel("Exponential population growth"),

  sidebarLayout(
    sidebarPanel(
    sliderInput("N_0", "Initial abundance",
                min = 0, max=250, value=10),
    sliderInput("r", "r",
                min = -0.5, max=0.5, value=0.1),
    sliderInput("nYears", "Number of years",
                min = 2, max=30, value=20),
    checkboxInput("log", "Display log(N)?", value = FALSE),
    checkboxInput("pop2", "Multiple populations?", value = FALSE),
    sliderInput("N_02", "Initial abundance, population 2",
                min = 0, max=250, value=10),
    sliderInput("r2", "r, population 2",
                min = -0.5, max=0.5, value=0.1)
  ),
  mainPanel(
    plotOutput("plot")#,
  )
)
)


# Define server logic ----
server <- function(input, output) {
  # N <- reactive({
  #   N_0 * exp(r*nYears)
  # })

  output$plot <- reactivePlot(function(){
    if(!input$pop2){
      N <- data.frame(Year = seq(from = 1, to = input$nYears),
                      N = c(input$N_0, input$N_0 * exp(input$r * seq(2:input$nYears))))


      if(input$log) {
        N$N <- log(N$N)

        ggplot(N, aes(x = Year, y = N)) + geom_path() + geom_point(size = 5, color = "white") +
          geom_point() + scale_y_continuous("log(N)")
      }else{
        ggplot(N, aes(x = Year, y = N)) + geom_path() + geom_point(size = 5, color = "white") +
          geom_point()
      }
    }else{
      N <- data.frame(Year = seq(from = 1, to = input$nYears),
                      N = c(input$N_0, input$N_0 * exp(input$r * seq(2:input$nYears)),
                            input$N_02, input$N_02 * exp(input$r2 * seq(2:input$nYears))),
                      Population = rep(c("1","2"), each = input$nYears))


      if(input$log) {
        N$N <- log(N$N)

        ggplot(N, aes(x = Year, y = N, color = Population)) + geom_path() + geom_point(size = 5, color = "white") +
          geom_point() + scale_y_continuous("log(N)")
      }else{
        ggplot(N, aes(x = Year, y = N, color = Population)) + geom_path() + geom_point(size = 5, color = "white") +
          geom_point()
      }
    }

  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
