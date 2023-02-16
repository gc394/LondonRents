library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(plotly)

ui <- fluidPage(
  
  shinytheme(theme = 'journal'),
  
  titlePanel("London Rents"),
  
  sidebarLayout(
    
    sidebarPanel(
    
      pickerInput(
        inputId = "borough",
        label =  "Select Boroughs:", 
        choices = NULL,
        selected = NULL,
        multiple = T,
        options = list(actionsBox = T)),

      br(),

        pickerInput(
        inputId = 'financial_year',
        label = 'Select Financial Years:',
        choices = NULL,
        selected = NULL,
        multiple = T,
        options = list(actionsBox = T)
      )

        ),
    
    mainPanel(
      
      fluidRow(
        
        tabBox(
          
          id = 'tabs',
          
          tabPanel(title = 'Indexed Time Series', 
                   fluidRow(
                     column(width = 1, 
                            box(plotlyOutput(outputId = 'indexed_time_series', 
                                             height = '500px', 
                                             width = '800px')) %>%
                              withSpinner())
                   ), 
                   width = 60),
          
          tabPanel(title = 'Bar Chart Comparison', 
                   fluidRow(
                     column(width = 1, 
                            box(plotlyOutput(outputId = 'indexed_bar_chart', 
                                             height = '500px', 
                                             width = '800px')) %>%
                              withSpinner())
                   ), 
                   width = 60)
          
        )
    
      ),
      
      )
    
    )
  
  )


server <- function(input, output, session) {
  
  # read and wrangle data
  
  rents = readxl::read_xlsx(
    path = 'local-authority-rents-borough.xlsx', 
    sheet = "Local Authority Rents"
    )
  
  rents = rents %>% 
    # New Code identifies London boroughs
    tidyr::drop_na('New Code') %>%
    dplyr::select(-'Code', -'New Code') %>%
    janitor::clean_names() %>%
    dplyr::mutate(across(-area, ~round(as.numeric(.x), 2))) %>%
    tidyr::pivot_longer(
      cols = -area, 
      names_to = 'financial_year', 
      values_to = 'index'
      ) %>%
    dplyr::mutate(
      financial_year = gsub(pattern = 'x', replacement = '', x = financial_year)
      ) %>%
    dplyr::rename(
      'Index' = 'index',
      'Financial Year' = 'financial_year',
      'Borough' = 'area'
    )
  
  # updates
  
  updatePickerInput(
    session = session, 
    inputId = 'borough', 
    choices = sort(unique(rents$Borough)), 
    selected = sort(unique(rents$Borough))[1],
    options = list(actionsBox = T))
  
  observeEvent(input$tabs, {
    
    if (input$tabs == 'Indexed Time Series'){  

      updatePickerInput(
        session = session, 
        inputId = 'financial_year', 
        choices = sort(unique(rents$`Financial Year`)), 
        selected = sort(unique(rents$`Financial Year`)),
        options = list(actionsBox = T))
      
    } else if (input$tabs == 'Bar Chart Comparison'){
        
      updatePickerInput(
        session = session, 
        inputId = 'financial_year', 
        choices = sort(unique(rents$`Financial Year`)), 
        selected = sort(unique(rents$`Financial Year`))[1],
        options = list(maxOptions = 1))
      
      }
    })

  # outputs
  
  # set data as subset of boroughs and fys
  
 data = reactive({
    rents <- rents %>% 
      dplyr::filter(Borough %in% input$borough &
                      `Financial Year` %in% input$financial_year)
  })

  output$indexed_time_series <- renderPlotly({
    
    alpha = 1 - (length(input$borough)^2)/1200
    
    gg = ggplot2::ggplot(
      data = data(), 
      mapping = aes(
        x = `Financial Year`, 
        y = Index, 
        col = Borough)
    ) +
      geom_point(alpha = alpha) +
      # geom_smooth(
      #   mapping = aes(
      #     x = `Financial Year`, 
      #     y = Index),
      #   method = lm) +
      # ggthemes::theme_economist(horizontal = F) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
      scale_x_discrete(breaks = function(x){x[c(TRUE, FALSE)]})
    
    
    plotly::ggplotly(gg,  tooltip = c('Borough', 'Financial Year', 'Index'))
    
    })
  
  output$indexed_bar_chart = renderPlotly({
    
    gg = ggplot2::ggplot(
      data = data(), 
      mapping = aes(
        x = Borough, 
        y = Index, 
        fill = Borough)) +
        geom_bar(stat = 'Identity') +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      
    plotly::ggplotly(gg,  tooltip = c('Borough', 'Financial Year', 'Index'))
    
  })
  
}

shinyApp(ui, server)