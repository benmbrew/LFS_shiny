library(shiny)
library(DT)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(ggthemes)

source('read_in_clin.R')
# source('read_in_methyl.R')

source('functions.R')

# add tab for raw data
# summary stats second 
# add in more group 

ui <- dashboardPage(skin = 'red',
                    
                    
                    dashboardHeader(disable = TRUE,
                      title = "LFS database at SickKids",
                      titleWidth = 300
                    ),
                    
                    dashboardSidebar(width = 175,
                                     
                                     sidebarMenu(
                                       menuItem('Search data',
                                                icon = icon('database'),
                                                tabName = 'search_data'),
                                       menuItem('Summary statistics',
                                                icon = icon('table'),
                                                tabName = 'stats_data'),
                                       menuItem("Visualizations",
                                                icon = icon('eye'),
                                                tabName = "viz"))),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      
                      tabItems(
                        
                        tabItem(tabName = "search_data",
                                h2('Search samples'),
                                fluidRow(
                                  column(4,
                                         selectInput('id_search',
                                                     'Malkin ID',
                                                     choices = unique(clin$blood_dna_malkin_lab),
                                                     selected = NULL,
                                                     multiple = TRUE)),
                                  column(4,
                                         sliderInput('age_search',
                                                     'Select an age range',
                                                      min = 0,
                                                      max = 90, 
                                                      value = c(0, 90))),
                                  
                                  column(4,
                                         selectInput('cancer_search',
                                                     'Cancer type',
                                                     choices = cancer_names,
                                                     selected = NULL,
                                                     multiple = TRUE))
                                  
                                ),
                                
                                fluidRow(
                                  column(4,
                                         selectInput('methyl_search',
                                                     'Methylation data',
                                                     choices = unique(clin$methyl_status),
                                                     selected = NULL,
                                                     multiple = TRUE)),
                                  column(4,
                                         selectInput('image_search',
                                                     'Imaging data',
                                                     choices = unique(clin$image_status),
                                                     selected = NULL,
                                                     multiple = TRUE)),
                                  
                                  column(4,
                                         selectInput('seq_search',
                                                     'Sequencing data',
                                                     choices = unique(clin$seq_status),
                                                     selected = NULL,
                                                     multiple = TRUE))
                                  
                                ),
                                
                                
                                tabsetPanel(
                                  tabPanel('Table', 
                                           h5(strong(textOutput('search_table_text'))),
                                           fluidRow(column(12,
                                                           DT::dataTableOutput('search_table'))))
                
                                  )
                                
                                ),
                        
                        
                        tabItem(tabName = "stats_data",
                                h2('Explore Sickkids database'),
                                helpText('example text'),
                                fluidRow(column(12,
                                                strong('Examine by'))),
                                fluidRow(column(2, 
                                                checkboxInput('cancer_status',
                                                              'Cancer type',
                                                              value = NULL)),
                                         column(2,
                                                checkboxInput('p53_status',
                                                              'tp53 status',
                                                              value = NULL)),
                                         column(2,
                                                checkboxInput('gender',
                                                              'Gender',
                                                              value = NULL)),
                                         column(2,
                                                checkboxInput('methyl_status',
                                                              'Methylation samples',
                                                              value = NULL)),
                                         column(2,
                                                checkboxInput('image_status',
                                                              'Imaging samples',
                                                              value = NULL)),
                                         column(2,
                                                checkboxInput('seq_status',
                                                              'Sequencing samples',
                                                              value = NULL))
                                         ),
                                fluidRow(column(2,
                                                uiOutput('cancer_filter')),
                                         column(2,
                                                uiOutput('p53_filter')),
                                         column(2,
                                                uiOutput('gender_filter')),
                                         column(2,
                                                uiOutput('methyl_filter')),
                                         column(2,
                                                uiOutput('image_filter')),
                                         column(2,
                                                uiOutput('seq_filter'))
                                         ),
                                
                                
                                tabsetPanel(
                                  tabPanel('Plot', 
                                           plotOutput('stats_plot')))),
                        
                        tabItem(
                          tabName = 'viz',
                          fluidRow(column(6,
                                          selectInput('p53_subset',
                                                      'Choose LFS status',
                                                      choices = c('All', 'MUT', 'WT'),
                                                      selected = 'All')),
                                   column(6, 
                                          selectInput('cancer_subset',
                                                      'Choose cancer status',
                                                      choices = cancer_names,
                                                      selected = 'ACC'))),
                          fluidRow(column(6,
                                          plotOutput('onset_dist')),
                                   column(6,
                                          plotOutput('bar_plot_gender')))
                        
                        )
                        
                      )))




# Define server 
server <- function(input, output) {
  
  #----------------------
  # ui filters
  
  
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
  
  # methyl filter
  output$methyl_filter <- renderUI({
    if(input$methyl_status){
      methyl_types <- Hmisc::capitalize(unique(clin$methyl_status))
      methyl_types <- methyl_types[!is.na(methyl_types)]
      # gender_types <- c('All', gender_types)
      selectInput('methyl_filter',
                  'Filter',
                  choices = methyl_types,
                  selected = NULL,
                  multiple = TRUE)
    }else{
      NULL
    }
  })
  
  
  # image filter
  output$image_filter <- renderUI({
    if(input$image_status){
      image_types <- Hmisc::capitalize(unique(clin$image_status))
      image_types <- image_types[!is.na(image_types)]
      # gender_types <- c('All', gender_types)
      selectInput('image_filter',
                  'Filter',
                  choices = image_types,
                  selected = NULL,
                  multiple = TRUE)
    }else{
      NULL
    }
  })
  
  # seq filter
  output$seq_filter <- renderUI({
    if(input$seq_status){
      clin$seq_status[is.na(clin$seq_status)] <- 'No'
      seq_types <- Hmisc::capitalize(unique(clin$seq_status))
      seq_types <- seq_types[!is.na(seq_types)]
      # gender_types <- c('All', gender_types)
      selectInput('seq_filter',
                  'Filter',
                  choices = seq_types,
                  selected = NULL,
                  multiple = TRUE)
    }else{
      NULL
    }
  })
  
  
  
  
  #----------------------
  # reactive objects
  
  get_data_search <- reactive({
    
    id_search <- input$id_search
    age_search <- input$age_search
    cancer_search <- input$cancer_search
    methyl_search <- input$methyl_search
    image_search <- input$image_search
    seq_search <- input$seq_search
    
      x <- clin
      
      # ids 
      if(!is.null(id_search)){
        x <- x %>% filter(blood_dna_malkin_lab %in% id_search)
      }
      
      # age
      if(!is.null(age_search)){
        
        x <- x %>% filter(age_sample_collection >= age_search[1] &
                            age_sample_collection <= age_search[2])
      }
      
      # cancer
      if(!is.null(cancer_search)){
        x <- x %>% filter(cancer_diagnosis_diagnoses %in% cancer_search)
      }
      
      # methyl
      if(!is.null(methyl_search)){
        x <- x %>% filter(methyl_status %in% methyl_search)
      }
      
      # image
      if(!is.null(image_search)){
        x <- x %>% filter(image_status %in% image_search)
      }
      
      # seqeunce
      if(!is.null(seq_search)){
        x <- x %>% filter(seq_status %in% seq_search)
      }
      
      return(x)
      
      
  
   
    return(x)
  })
  
  
  get_data_stats <- reactive({
    
    # set default to NULL
    gender <- p53_status <- cancer_status <- 
      methyl_status <- image_status <- seq_status <- NULL
    
    if(input$gender){
      gender <- 'gender'
    } 
    if(input$p53_status){
      p53_status <- 'p53_germline'
    } 
    if(input$cancer_status){
      cancer_status <- 'cancer_diagnosis_diagnoses'
    }
    
    if(input$methyl_status){
      methyl_status <- 'methyl_status'
    }
    
    if(input$image_status){
      image_status <- 'image_status'
    }
    
    if(input$seq_status){
      seq_status <- 'seq_status'
    }
    
    # group_by_cols <- c(input$gender, input$p53_status, input$cancer_status)
    group_by_cols <- c(gender, p53_status, cancer_status, methyl_status, image_status, seq_status)
    
    if(is.null(group_by_cols)) {
      return(NULL)
    } else {
      x  <- clin %>% 
        group_by_at(group_by_cols) %>%  
        summarise(`Mean age diagnosis` = round(mean(age_diagnosis, na.rm = T), 2),
                  `Mean age of sample collection` = round(mean(age_sample_collection, na.rm = T), 2),
                  Counts = n())
      
      if(input$gender & !is.null(input$gender_filter)) {
        x <- x %>% filter(gender %in% input$gender_filter)
      }
      
      if(input$p53_status & !is.null(input$p53_filter)) {
        x <- x %>% filter(p53_germline %in% input$p53_filter)
      }
      
      if(input$cancer_status & !is.null(input$cancer_filter)) {
        x <- x %>% filter(cancer_diagnosis_diagnoses %in% input$cancer_filter)
      }
      
      if(input$methyl_status & !is.null(input$methyl_filter)) {
        x <- x %>% filter(methyl_status %in% input$methyl_status)
      }
      
      if(input$image_status & !is.null(input$image_filter)) {
        x <- x %>% filter(image_status %in% input$image_status)
      }
      
      if(input$seq_status & !is.null(input$seq_filter)) {
        x <- x %>% filter(seq_status %in% input$seq_status)
      }
      
    }
    
    return(x)
  })
  
  #----------------------
  # table text 
  output$search_table_text <- renderText({
    
    x <- get_data_search()
    
    if(is.null(x)) {
      NULL
    } else {
      n_rows <- nrow(x)
      paste0(n_rows, ' samples for your selection')
    }
    
  })
  
  

  #----------------------
  # tables and plots
  
  output$search_table <- renderDataTable({
    x <- get_data_search()
    if(!is.null(x)){
      x[is.na(x)] <- "NA"
      
      # colnames(x) <- c('Gender','Mean age of onset', 'Means age of sample collection', 'Mean current age')
      out <- prettify(x, 
                      download_options = TRUE)
      return(out)
    } else {
      DT::datatable(data_frame(' ' = 'No data available for those specifications'), rownames = FALSE, options = list(dom = 't'))
    }
  })
  
  
  # 
  # get data table
  output$stats_table <- renderDataTable({
    x <- get_data_stats()
    if(!is.null(x)){
      x[is.na(x)] <- 'NA'
      # colnames(x) <- c('Gender','Mean age of onset', 'Means age of sample collection', 'Mean current age')
      out <- prettify(x)
      return(out)
    } else {
      DT::datatable(data_frame(' ' = 'Please choose at least one variable to group by'), rownames = FALSE, options = list(dom = 't'))
    }
  })
  
  
  
  
#   # get data table
#   output$clin_table <- renderDataTable({
#     x <- get_clin_table()
#     if(!is.null(x)){
#       # colnames(x) <- c('Gender','Mean age of onset', 'Means age of sample collection', 'Mean current age')
#       out <- prettify(x)
#       return(out)
#     } else {
#       DT::datatable(data_frame(' ' = 'Please by'), rownames = FALSE, options = list(dom = 't'))
#     }
#   })
# 
  # onset_dist, onset_age_cor, bar_plot,
  # and p53_subset, cancer_subset
  output$onset_dist <- renderPlot({
    p53_subset <- 'All'
    cancer_subset <- 'ACC'
    p53_subset <- input$p53_subset
    cancer_subset <- input$cancer_subset
    
    x <- clin
    # subset data based on inputs 
    if(p53_subset != 'All') {
      x <- x[x$p53_germline == p53_subset,]
    }
    
    x <- x[x$cancer_diagnosis_diagnoses == cancer_subset,]
    
    plot_title <- paste0('Showing ',p53_subset, ' patients with ', cancer_subset, ' cancer status' )
    # plot distribution of age of onset 
   ggplot(x, aes(x=age_diagnosis)) + 
      geom_histogram(aes(y=..count..),      # Histogram with density instead of count on y-axis
                     binwidth=1,
                     colour="black", fill="blue", alpha = 0.4) +
      labs(title = plot_title, x = 'Age of diagnosis', y = 'Counts') +
      theme_pander(base_size = 16, base_family = 'Ubuntu')    
    
  })
  
  # onset_dist, onset_age_cor, bar_plot,
  # and p53_subset, cancer_subset
  output$bar_plot_gender <- renderPlot({
    p53_subset <- 'All'
    cancer_subset <- 'ACC'
    p53_subset <- input$p53_subset
    cancer_subset <- input$cancer_subset
    
    x <- clin
    # subset data based on inputs 
    if(p53_subset != 'All') {
      x <- x[x$p53_germline == p53_subset,]
    }
    
    x <- x[x$cancer_diagnosis_diagnoses == cancer_subset,]
    
    plot_title <- paste0('Showing ',p53_subset, ' patients (by gender) with ', cancer_subset, ' cancer status' )
    
    # add in p53 and cancer group based on inputs
    
    x <- x[!is.na(x$gender),]
    
    x <- x %>%
      group_by(gender) %>%
      summarise(`mean_age_onset` = mean(age_diagnosis, na.rm = T))
    
    # plot distribution of age of onset 
    ggplot(x, aes(x = gender, y = mean_age_onset)) + 
      geom_bar(stat = 'identity', position = 'dodge', colour = 'black', fill = 'orange', alpha = 0.6) +
      labs(title = plot_title, x = 'Gender', y = 'Mean age of onset') +
      theme_pander(base_size = 16, base_family = 'Ubuntu')    
    

  })
  
}




# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

