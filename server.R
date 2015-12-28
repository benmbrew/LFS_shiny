
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

# Source the read in
source('read_in.R')

shinyServer(function(input, output) {
  
  # DYNAMIC USER INTERFACE FOR SELECTING CANCER
  output$test <- renderUI(
    selectInput('cancer', 
                'Cancer',
                c(unique(sort(as.character(cancer$cancer))), 'All cancers'),
                selected = 'All cancers')
    )
  
  
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
  
  output$plot2 <- renderPlot({
    
    if(input$cancer == 'All cancers'){
      temp <- this_cancer()
      temp <- temp %>%
        group_by(tp53)%>%
        summarise(age_of_onset = mean(age_of_onset, na.rm = T))
      bp <- barplot(temp$age_of_onset, names.arg = temp$tp53,
                    col = adjustcolor('lightblue', alpha.f = 0.4))
      
    }else{
      temp <- this_cancer()
      bp <- barplot(temp$age_of_onset,
                    names.arg = temp$tp53,
                    col = adjustcolor('lightblue', alpha.f = 0.4))
    }
    
  })
  
  # TABLE
  output$table2 <- renderTable({
    
    if(input$cancer == 'All cancers'){
      temp <- this_cancer()
      temp <- temp %>%
        group_by(tp53)%>%
        summarise(age_of_onset = mean(age_of_onset))
      temp
    }else{
      temp <- this_cancer() #() must be used with reactive objects.
      temp <- temp[, c('tp53', 'age_of_onset')]
      temp
    }
    
  })
  
  
  

})


