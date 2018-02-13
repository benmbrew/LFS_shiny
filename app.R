library(shiny)
library(googleVis)
library(DT)
library(RColorBrewer)
library(networkD3)
options(gvis.plot.tag = 'chart')
library(shinyBS)
library(shinyLP)
library(ggplot2)
library(shinythemes)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(memisc)

source('read_in.R')
source('functions.R')

# add tab for raw data
# summary stats second 
# add in more group by variabl

ui <- dashboardPage(skin = 'red',
                    
                    
                    dashboardHeader(
                      title = "LFS database at SickKids",
                      titleWidth = 300
                    ),
                    
                    dashboardSidebar(width = 300,
                                     
                                     sidebarMenu(
                                       menuItem('lfs_database',
                                                icon = icon('users'),
                                                tabName = 'lfs_database'),
                                       menuItem('placeholder_1',
                                                icon = icon('address-book-o'),
                                                tabName = 'place_holder_@'),
                                       menuItem("About",
                                                icon = icon('folder-open'),
                                                tabName = "about"))),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tabItems(
                        tabItem(tabName = "lfs_database",
                                h2('Explore Sickkids database'),
                                helpText('example text'),
                                fluidRow(column(12,
                                               strong('Examine by'))),
                                fluidRow(column(4, 
                                                checkboxInput('cancer_status',
                                                              'Cancer type',
                                                              value = NULL)),
                                         column(4,
                                                checkboxInput('p53_status',
                                                              'tp53 status',
                                                              value = NULL)),
                                         column(4,
                                                checkboxInput('gender',
                                                              'Gender',
                                                              value = NULL))),
                                fluidRow(column(4,
                                                uiOutput('cancer_filter')),
                                         column(4,
                                                uiOutput('p53_filter')),
                                         column(4,
                                                uiOutput('gender_filter'))),
                                
                                
                                tabsetPanel(
                                  tabPanel('Table',
                                           fluidRow(column(12,
                                                           textOutput('lfs_text'),
                                                           DT::dataTableOutput('lfs_table')
                                           ))),
                                  tabPanel('Plot', 
                                           checkboxInput('show_labels',
                                                         'Show values on charts?',
                                                         TRUE),
                                           plotOutput('bar_plot')))),
                        tabItem(
                          tabName = 'about',
                          fluidPage(
                            fluidRow(
                              div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                              h4('Built in partnership with ',
                                 a(href = 'http://databrew.cc',
                                   target='_blank', 'Databrew'),
                                 align = 'center'),
                              p('Empowering research and analysis through collaborative data science.', align = 'center'),
                              div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                                                 icon = icon("envelope", lib = "font-awesome")),
                                    href="mailto:info@databrew.cc",
                                    align = 'center')), 
                              style = 'text-align:center;'
                            )
                          )
                        )
                        
                      )))




# Define server 
server <- function(input, output) {
  # gender filter
  output$gender_filter <- renderUI({
    if(input$gender){
      gender_types <- unique(clin$gender)
      gender_types <- gender_types[!is.na(gender_types)]
      # gender_types <- c('All', gender_types)
      selectInput('gender_filter',
                  'Filter',
                  choices = gender_types,
                  selected = NULL,
                  multiple = TRUE)
    }else{
      NULL
    }
  })
  
  output$p53_filter <- renderUI({
    if(input$p53_status){
      p53_types <- unique(clin$p53_germline)
      p53_types <- p53_types[!is.na(p53_types)]
      # p53_types <- c('All', p53_types)
      selectInput('p53_filter',
                  'Filter',
                  choices = p53_types,
                  selected = NULL,
                  multiple = TRUE)
    } else {
      NULL
    }
  })
  
  output$cancer_filter <- renderUI({
    if(input$cancer_status){
      cancer_types <- unique(clin$cancer_diagnosis_diagnoses)
      cancer__types <- cancer_types[!is.na(cancer_types)]
      # cancer_types <- c('All', cancer_types)
      selectInput('cancer_filter',
                  'Filter',
                  choices = cancer_types,
                  selected = NULL,
                  multiple = TRUE)
    } else {
      NULL
    }
  })
  
  get_data <- reactive({
    
    # set default to NULL
    gender <- p53_status <- cancer_status <- NULL
    
    if(input$gender){
      gender <- 'gender'
    } 
    if(input$p53_status){
      p53_status <- 'p53_germline'
    } 
    if(input$cancer_status){
      cancer_status <- 'cancer_diagnosis_diagnoses'
    }
    # group_by_cols <- c(input$gender, input$p53_status, input$cancer_status)
    group_by_cols <- c(gender, p53_status, cancer_status)
    
    if(is.null(group_by_cols)) {
      return(NULL)
    } else {
      x  <- clin %>% 
        group_by_at(group_by_cols) %>%  
        summarise(`Mean age diagnosis` = round(mean(age_diagnosis, na.rm = T), 2),
                  `Mean age of sample collection` = round(mean(age_sample_collection, na.rm = T), 2))
      
      if(input$gender & !is.null(input$gender_filter)) {
        x <- x %>% filter(gender %in% input$gender_filter)
      }
      
      if(input$p53_status & !is.null(input$p53_filter)) {
        x <- x %>% filter(p53_germline %in% input$p53_filter)
      }
      
      if(input$cancer_status & !is.null(input$cancer_filter)) {
        x <- x %>% filter(cancer_diagnosis_diagnoses %in% input$cancer_filter)
      }
    }
    
    return(x)
  })
  
  
  # get data table
  output$lfs_table <- renderDataTable({
    x <- get_data()
    if(!is.null(x)){
      # colnames(x) <- c('Gender','Mean age of onset', 'Means age of sample collection', 'Mean current age')
      out <- DT::datatable(x, caption = "Clinical data")
      return(out)
    } else {
      DT::datatable(data_frame(' ' = 'Please choose at least one variable to group by'), rownames = FALSE, options = list(dom = 't'))
    }
  })
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

