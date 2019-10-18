
## shiny app and ui for project 2

library(shiny)
library(shinydashboard)

setwd("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 2")

source("helper_functions.R")


ui <- fluidPage(
  
  #application title
  titlePanel("Urban Ministries Durham"),
  
  #sidebar ladout with input/output defns
  #sidebarLayout(
    
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Number of Individuals Served", 
                           sidebarPanel(
                             
                             sliderInput(inputId = "idyr", label =("Year Range"), min = 2001, max = 2019, value = c(2001, 2018),
                                         step=2,ticks=TRUE, sep=""),
                             
                             # tell users that if fewer than 6 years are selected, months will be displayed for increased detail
                             textOutput("yearrange.tot.ids")
                             
                                       ),
                           mainPanel(
                           
                             plotOutput("numserved")
                             
                           )
                          ),
                  
                  tabPanel("Clients Coming and Going by Month", 
                           
                           sidebarPanel(
                             
                             #select year range
                             sliderInput(inputId = "yr", label =("Year Range"), min = 2001, max = 2019, value = c(2001, 2018),
                                         step=2,ticks=TRUE, sep=""),
                             
                             #select first or last visit
                             radioButtons("visit", label = h3("Visit Type"),
                                          choices = list("First Visit" = "first", "Last Recorded" = "last"), 
                                          selected = "first"),
                             
                             #text output to accompany selection
                             textOutput("last")
                             
                             
                           ),
                           mainPanel(
                             
                             plotOutput("arr.go.plot")
                             
                           )
                           
                           ),
                  
                  
                  tabPanel("Number of Services per Client", 
                  # users can select all services, or services of a certain type, and a year range
                  #select year range
                  sliderInput(inputId = "yr", label =("Year Range"), min = 2001, max = 2019, value = c(2001, 2018),
                              step=2,ticks=TRUE, sep=""),
                  
                  checkboxGroupInput("servs", label =("Type of Services"), 
                                     choices = list("Food" = "food", "Clothing" = "clothes", "Bus Tickets" = "bustix",
                                                    "Hygiene Kits"="hyg", "School Kits"="school","Financial Support","fin"),
                                     selected = "clothes"),
                  
                  textOutput("servtyp") # explaining that food service is collapsed for pounds
                  
                           ),
                  
                  tabPanel("Ranges of Dates"
                           
                           
                           )
                  
                  
                  
                 )
      
           )
  
  
      )

server <- function(input, output) {
  
  output$numserved <- renderPlot({
    
    tot.ids.my(input$idyr[1],input$idyr[2])
    
  })
  
  output$yearrange.tot.ids <- renderText({
    
    range.id.yr <- as.numeric(input$idyr[2]) - as.numeric(input$idyr[1])
    idyr.range.text(range.id.yr)
    
  })
  
}

shinyApp(ui = ui, server = server)







