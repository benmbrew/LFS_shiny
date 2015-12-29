

library(shiny)

load('lfs_models.RData')


shinyServer(function(input, output) {
    
  # DYNAMIC USER INTERFACE FOR SELECTING CANCER
#   output$test <- renderUI(
#     selectInput('cancer', 
#                 'Cancer',
#                 c(unique(sort(as.character(cancer$cancer))), 'All cancers'),
#                 selected = 'All cancers')
#     )
#   output$test2 <- renderUI(
#     selectInput('models',
#                 'Models',
#                 choices = c('Logit' = 'perf_logit', 
#                             'Random Forest' = 'perf_rf',
#                             'SVM' = 'perf_svm',
#                             'Ridge'= 'perf_ridge',
#                             'Lasso' = 'perf_lasso',
#                             'All Models'),
#                 selected = 'All Models')
#   )
#   
  
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
  
  output$plot3 <- renderPlot({
  
    if(input$models == 'All Models'){
      plot(perf_logit, col = 'lightblue')
      plot(perf_rf, add = TRUE, col = 'red')
      plot(perf_svm, add = TRUE, col = 'green')
      plot(perf_lasso, add = TRUE, col = 'blue')
      plot(perf_ridge, add = TRUE, col = 'purple')
      abline(a = 0, b = 1, lty = 3)
      
      
      legend('bottomright',
             lty = 1,
             legend = c(paste(auc_logit,'logit'), 
                        paste(auc_rf,'RF'), 
                        paste(auc_svm, 'SVM'), 
                        paste(auc_ridge, 'ridge'),
                        paste(auc_lasso, 'lasso')),
             col = c('lightblue', 'red', 'green', 'blue', 'purple'),
             title = 'AUC',
             bty = 'n')
    }
    if(input$models == 'perf_logit'){
  plot(perf_logit, col = 'lightblue', lwd = 2)
  legend('bottomright',
         lty = 1,
         legend = c(paste(auc_logit,'logit')),
                    title = 'AUC', col = 'lightblue')
    }
  if(input$models == 'perf_rf'){
      plot(perf_rf, col = 'red',lwd = 2)
      legend('bottomright',
             lty = 1,
             legend = c(paste(auc_rf,'RF')),
             title = 'AUC', col = 'red')
  }
  if(input$models == 'perf_svm'){
    plot(perf_svm, col = 'green', lwd = 2)
    legend('bottomright',
           lty = 1,
           legend = c(paste(auc_svm,'SVM')),
           title = 'AUC', col = 'green')
  }
  if(input$models == 'perf_lasso'){
    plot(perf_lasso, col = 'blue', lwd = 2)
    legend('bottomright',
           lty = 1,
           legend = c(paste(auc_lasso,'Lasso')),
           title = 'AUC', col = 'blue')
  }
  if(input$models == 'perf_ridge'){
    plot(perf_ridge, col = 'purple', lwd = 2)
    legend('bottomright',
           lty = 1,
           legend = c(paste(auc_ridge,'Ridge')),
           title = 'AUC', col = 'purple')
  }
    
    
  })
  
})




