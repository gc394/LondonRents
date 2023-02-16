library(dplyr)
library(tidyr)
library(magrittr)
library(readxl)
library(janitor)
library(ggplot2)
library(plotly)
library(shiny)
library(tidymodels)
# https://data.london.gov.uk/dataset/local-authority-average-rents
path = 'local-authority-rents-borough.xlsx'
df = readxl::read_xlsx(
  path = path)%>%
  dplyr::rename(
    'Index' = 'index',
    'Financial Year' = 'financial_year',
    'Borough' = 'area'
  )

df

readxl::excel_sheets(path = path)

rents = readxl::read_xlsx(
  path = path, 
  sheet = "Local Authority Rents"
  ) %>% 
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
    )

rents$financial_year = map_chr(.x = rents$financial_year, .f = function(x){ return(clean(x))})
library(openxlsx)
openxlsx::write.xlsx(df,'local-authority-rents-borough.xlsx')

openxlsx::write

clean = function(x){
  
  if(!grepl(pattern = '[0-9]{4}_[0-9]{2}$', x = x)){
    
    x = gsub(pattern = '_[0-9]{2}', replacement = '_', x = x)
    
  }
  
  return(x)
  
}


clean()

gg = ggplot2::ggplot(
  data = rents, 
  mapping = aes(
    x = financial_year, 
    y = index, 
    col = area)
  ) +
  geom_point()

plotly::ggplotly(gg,  tooltip = c('financial_year', 'index', 'area'))

