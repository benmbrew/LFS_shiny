
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyapps)
#shinyapps::deployApp('/home/benbrew/Desktop/lfs_shiny')

shinyUI(fluidPage(
  
  # Application title
  titlePanel("LFS"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      uiOutput("test")

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel('Cancer Counts',
      plotOutput("plot1"),
      tableOutput('table1')),
      tabPanel('Mean Age of Onset',
               plotOutput('plot2'),
               tableOutput('table2'))
      )
    )
  )
))



