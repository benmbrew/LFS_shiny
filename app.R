
# shinyapps::deployApp()
#library(shiny)
library(shiny)
library(ggplot2)
library(DT)

# source script to get data
source('read_in.R')

# UI
ui <- fluidPage(
  titlePanel("LFS"),
  sidebarLayout(
    sidebarPanel(
      selectInput("clin_group", 
                  "Group by variable",
                  choices = clin_choices_group,
                  multiple = TRUE),
      uiOutput("clin_summarise")
    ),
    
    mainPanel(
      tableOutput("clin_tab"),
      plotOutput("clin_plot"),
      textOutput('no_data_text'),
      br(), br()
      
    )
  )
)


# SERVER
server <- function(input, output) {
  
  
  output$clin_summarise <- renderUI({
    
    first_variable <- input$clin_group
    if(is.null(first_variable)) {
      NULL
    } else {
      grouped_var <- input$clin_group
      clin_choices_sum <- clin_choices_sum[!grepl(grouped_var, clin_choices_sum)]
      selectInput('clin_summarise',
                  'Choose a second variable to summarise',
                  choices = clin_choices_sum)
    }
    
  })
  
  output$clin_tab <- renderDataTable({
    
    group_var <- input$clin_group
    summarise_var <- input$clin_summarise
    
    get_these_vars <- c(group_var, summarise_var)
    
    if(is.null(group_var) & is.null(summarise_var)) {
      NULL
    } else {
      if(!is.null(group_var) & is.null(summarise_var)) {
        clin_sub <- clin[, colnames(clin) %in% get_these_vars]
        if(length(group_var) == 1){
          clin_sub <- clin_sub[!is.na(clin_sub)]
          clin_sub <- as.data.frame(clin_sub)
          colnames(clin_sub) <- group_var
        } else {
          clin_sub <- clin_sub[complete.cases(clin_sub),]
          colnames(clin_sub) <- group_var
        }
        the_tible <- clin_sub %>% group_by_at(group_var) %>% summarise(Counts = n())
      } else if(!is.null(group_var) & !is.null(summarise_var)) {
        clin_sub <- clin[, colnames(clin) %in% get_these_vars]
        clin_sub <- clin_sub[complete.cases(clin_sub),]
        the_tible <- clin_sub %>% group_by_at(group_var) %>% summarise_all(funs(mean(. , na.rm = T)))
      }
      DT::datatable(the_tible)
    }
    
  })
  
  output$no_data_text <- renderText({
    
    if(is.null(input$clin_group)){
      paste0('Please select a variable to group by')
    } else {
      NULL
    }
    
  })
  
  output$clin_plot <- renderPlot({
   
    
  })
  
  
}

shinyApp(ui = ui, server = server)