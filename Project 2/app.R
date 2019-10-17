
## shiny app and ui for project 2

library(shiny)
library(shinydashboard)

ui <- fluidPage(
  
  #application title
  titlePanel("Urban Ministries Durham"),
  
  #sidebar ladout with input/output defns
  #sidebarLayout(
    
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Number of Individuals Served", 
                           sidebarPanel(
                             
                             sliderInput(inputId = "yr", label =("Year Range"), min = 2001, max = 2019, value = c(2001, 2018),
                                         step=2,ticks=TRUE, sep="")
                             
                             
                                       ),
                           mainpanel(
                             
                             
                             
                           )
                          ),
                  
                  tabPanel("New Clients by Month", verbatimTextOutput("summary")
                           
                           
                           
                           
                           ),
                  
                  tabPanel("Clients Coming and Going", tableOutput("table"))
      )
      
    )
    
  #)
  
  
)

server <- function(input, output) {
  
  
}


shinyApp(ui = ui, server = server)
