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

gg = ggplot2::ggplot(
  data = rents, 
  mapping = aes(
    x = financial_year, 
    y = index, 
    col = area)
  ) +
  geom_point()

plotly::ggplotly(gg,  tooltip = c('financial_year', 'index', 'area'))

