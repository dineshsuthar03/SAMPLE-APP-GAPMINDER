
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(tidyverse)
library(gapminder)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Year vs Mean Life Expectancy of all continents"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          inputPanel( 
            checkboxInput("linear", label = "Add trend line?", value = FALSE) 
          ) 
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({ 
      
      thePlot = gapminder %>%
        filter(year > 1960) %>%
        group_by(continent, year) %>%
        summarise(meanLife = mean(lifeExp)) %>%
        ggplot(aes(x = year, y = meanLife, group = continent, colour = continent)) +
        geom_line()
      
      if(input$linear){ 
        thePlot = thePlot + geom_smooth(method = "lm") 
      } 
      
      print(thePlot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
