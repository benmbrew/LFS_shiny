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
                                                checkboxInput('gender',
                                                              'Gender',
                                                              value = TRUE)),
                                         column(4,
                                                checkboxInput('p53_status',
                                                              'tp53 status')),
                                         column(4,
                                                checkboxInput('cancer_status',
                                                              'Cancer type'))),
                                fluidRow(column(4,
                                                uiOutput('gender_filter')),
                                         column(4,
                                                uiOutput('p53_filter')),
                                         column(4,
                                                uiOutput('cancer_filter'))),
                                
                                
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
      gender_types <- c('All', gender_types)
      selectInput('gender_filter',
                  'Filter',
                  choices = gender_types,
                  selected = 'All',
                  multiple = TRUE)
    }
  })
  
  output$p53_filter <- renderUI({
    if(input$p53_status){
      p53_types <- unique(clin$p53_germline)
      p53_types <- p53_types[!is.na(p53_types)]
      p53_types <- c('All', p53_types)
      selectInput('p53_filter',
                  'Filter',
                  choices = p53_types ,
                  multiple = TRUE)
    }
  })
  
  output$cancer_filter <- renderUI({
    if(input$cancer_status){
      cancer_types <- unique(clin$cancer_diagnosis_diagnoses)
      cancer__types <- cancer_types[!is.na(cancer_types)]
      cancer_types <- c('All', cancer_types)
      selectInput('cancer_filter',
                  'Filter',
                  choices = cancer_types ,
                  multiple = TRUE)
    }
  
  })
  
  get_data <- reactive({
    
    x <- clin
    
    if(is.null(input$gender)){
      return(NULL)
    } else if(input$gender & is.null(input$gender_filter)){
        x <- x %>% group_by(gender) %>% filter(!is.na(gender)) %>%
          summarise(mean_age_diagnosis = round(mean(age_diagnosis, na.rm = T), 2),
                    mean_age_sample_collection = round(mean(age_sample_collection, na.rm = T), 2),
                    mean_current_age = round(mean(age, na.rm = T), 2))
      } else if(input$gender & (input$gender_filter == 'All')) {
        x <- x %>% group_by(gender) %>% filter(!is.na(gender)) %>%
          summarise(mean_age_diagnosis = round(mean(age_diagnosis, na.rm = T), 2),
                    mean_age_sample_collection = round(mean(age_sample_collection, na.rm = T), 2),
                    mean_current_age = round(mean(age, na.rm = T), 2))
      } else if(input$gender & !is.null(input$gender_filter)) {
        x <- x %>% group_by(gender) %>% filter(!is.na(gender)) %>%
          summarise(mean_age_diagnosis = round(mean(age_diagnosis, na.rm = T), 2),
                    mean_age_sample_collection = round(mean(age_sample_collection, na.rm = T), 2),
                    mean_current_age = round(mean(age, na.rm = T), 2))
        x <- x %>% filter(gender %in% input$gender_filter)
      }
    
  })

  output$lfs_table <- renderDataTable({
    x <- get_data()
    if(!is.null(x)){
      colnames(x) <- c('Gender','Mean age of onset', 'Means age of sample collection', 'Mean current age')
      out <- DT::datatable(x)
      return(out)
    } else {
      return(NULL)
    }
  })
  
  
  
  
  
 
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

