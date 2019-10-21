## shiny app and ui for project 2

library(shiny)
library(shinydashboard)

#setwd("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 2")

source("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 2\\helper_functions.R")


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
                           
                           radioButtons(inputId = "points.typ", label = "Sum display",
                                        choices = list("Year" = "year", "Year and Month" = "both"), 
                                        selected = "year")
                           
                         ),
                         mainPanel(
                           # plot of number of individuals served each year or month
                           plotOutput("numserved"),
                           
                           # describe change in number of individuals
                           textOutput("numserved.txt")
                           
                         )
                ),
                
                tabPanel("Clients Coming and Going by Month", 
                         
                         sidebarPanel(
                           
                           #select year range
                           sliderInput(inputId = "yr.arr.go", label =("Year Range"), min = 2001, max = 2019, value = c(2001, 2018),
                                       step=2,ticks=TRUE, sep=""),
                           
                           #select first or last visit
                           radioButtons("visit.arr.go", label = h3("Visit Type"),
                                        choices = list("First Visit" = "first", "Last Recorded" = "last"), 
                                        selected = "first")
                           
                           
                           
                         ),
                         mainPanel(
                           
                           plotOutput("arr.go.plot"),
                           textOutput("arr.go.txt")
                           
                         )
                         
                ),
                
                
                tabPanel("Ranges of Dates"
                         
                         
                )
                
                
                
    )
    
  )
  
  
)

server <- function(input, output) {
  
  output$numserved <- renderPlot({
    
    tot.ids.my(yrlow=input$idyr[1],yrhi=input$idyr[2],points.typ=input$points.typ)
    
  })
  
  output$numserved.txt <- renderText({
    
    numserved_txt(yrlow=input$idyr[1],yrhi=input$idyr[2])
    
  })
  
  output$arr.go.plot  <- renderPlot({
    
    
    arr.go.plot(visit=input$visit.arr.go, yrlow=input$yr.arr.go[1], yrhi=input$yr.arr.go[2])
    
  })
  
  output$arr.go.txt <- renderText({
    
    arr_go_text(visit=input$visit.arr.go, yrlow=input$yr.arr.go[1], yrhi=input$yr.arr.go[2])
    
  })
  
  
}

shinyApp(ui = ui, server = server)

