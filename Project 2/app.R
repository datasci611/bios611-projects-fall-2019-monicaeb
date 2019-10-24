## shiny app and ui for project 2

library(shiny)
library(shinydashboard)

#setwd("C:\\Users\\Monica Borges\\OneDrive - University of North Carolina at Chapel Hill\\BIOS 611\\GitHub Resources\\bios611-projects-fall-2019-monicaeb\\Project 2")

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
                           
                           radioButtons(inputId = "points.typ", label = "Sum display",
                                        choices = list("Year" = "year", "Year and Month" = "both"), 
                                        selected = "year"),
                           
                           textOutput("choose1")
                           
                         ),
                         mainPanel(
                           # plot of number of individuals served each year or month
                           plotOutput("numserved",click = "plot_click1"),
                           
                           # describe change in number of individuals
                           textOutput("numserved.txt"),
                           
                           textOutput("p1text"), # instructions for clicking on plot
                           textOutput("xyinfo1") # number of subjects
                           
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
                                        selected = "first"),
                           
                           textOutput("choose2")
                           
                           
                           
                         ),
                         mainPanel(
                           
                           plotOutput("arr.go.plot",click="plot_click2"),
                           
                           textOutput("arr.go.txt"), # describe plot
                           textOutput("bptext"), # instructions for clicking on plot
                           textOutput("xyinfo2") # number of subjects 
                         )
                         
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
  
  output$p1text <- renderText({ #instruct users to click on points they are interested in
    
    "Click on any point to see the number of individuals represented by it."
    
  })
  
  output$bptext <- renderText({ #explain points on box plot and instruct users to click on points they are interested in
    
    bptext_fun(yrlow=input$yr.arr.go[1], yrhi=input$yr.arr.go[2])
    
  })
  
  
  output$xyinfo1 <- renderText({ # output values from clicking on th eplot
    
    paste0("N = ", round(as.numeric(input$plot_click1$y)))
    
  })
  
  output$xyinfo2 <- renderText({ # output values from clicking on the plot
    
    paste0("N = ", round(as.numeric(input$plot_click2$y)))
    
  })
  
  output$choose1 <- renderText({
    
    "Select a range of years and frequency of counts from above. You may select total counts by year, or by year and month. "
    
  })
  
  output$choose2 <- renderText({
    
    "Select a range of years from above. If only one year is selected, you will see a bar plot for that year."
    
  })
  
  
}

shinyApp(ui = ui, server = server)

