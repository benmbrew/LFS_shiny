
# shinyapps::deployApp()
#library(shiny)
library(shinyapps)
source('read_in.R')

shinyUI(pageWithSidebar(
  
  
  # Application title
  headerPanel("LFS"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    conditionalPanel(
      condition = "input.tabs == 'Model Results'",
      selectInput('models',
                  'Models',
                  choices = c('Logit' = 'perf_logit', 
                              'Random Forest' = 'perf_rf',
                              'SVM' = 'perf_svm',
                              'Ridge'= 'perf_ridge',
                              'Lasso' = 'perf_lasso',
                              'All Models'),
                  selected = 'All Models')
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'Cancer Counts'",
      selectInput('cancer', 
                  'Cancer',
                  c(unique(sort(as.character(cancer$cancer))), 'All cancers'),
                  selected = 'All cancers')
    ),
    conditionalPanel(
      condition = "input.tabs == 'Mean Age of Onset'",
      selectInput('cancer', 
                  'Cancer',
                  c(unique(sort(as.character(cancer$cancer))), 'All cancers'),
                  selected = 'All cancers')
    )
    #uiOutput("test2"),
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(id = 'tabs',
                tabPanel('Model Results',
                         plotOutput('plot3')),
                tabPanel('Cancer Counts',
                         plotOutput("plot1"),
                         tableOutput('table1')),
                tabPanel('Mean Age of Onset',
                         plotOutput('plot2'),
                         tableOutput('table2'))
    )
  )
  
))





library(shiny)

#load('lfs_models.RData')
#source('read_in.R')


shinyServer(function(input, output) {
  
 
  # SUBSET CANCER TO JUST THE USER'S INPUT
  this_cancer <- reactive({
    if(input$cancer == 'All cancers'){
      cancer
    }else{
      cancer[cancer$cancer == input$cancer,]
      
    }
  })
  
  #  BARPLOT
  output$plot1 <- renderPlot({
    
    if(input$cancer == 'All cancers'){
      temp <- this_cancer()
      temp <- temp %>%
        group_by(tp53)%>%
        summarise(counts = sum(counts))
      bp <- barplot(temp$counts, names.arg = temp$tp53,
                    col = adjustcolor('lightblue', alpha.f = 0.4))
      
    }else{
      temp <- this_cancer()
      bp <- barplot(temp$counts,
                    names.arg = temp$tp53,
                    col = adjustcolor('lightblue', alpha.f = 0.4))
    }
    
  })
  
  # TABLE
  output$table1 <- renderTable({
    
    if(input$cancer == 'All cancers'){
      temp <- this_cancer()
      temp <- temp %>%
        group_by(tp53)%>%
        summarise(counts = sum(counts))
      temp
    }else{
      temp <- this_cancer() #() must be used with reactive objects.
      temp <- temp[, c('tp53', 'counts')]
      temp
    }
    
  })
  
  
  
})




