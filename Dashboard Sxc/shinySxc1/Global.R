library(shiny)
library(shinydashboard)
library(dplyr)
library(tidytext)
library(tidyr)
library(DT)
library(ggplot2)
library(wordcloud2)
library(readxl)
library(ggimage)
library(ggthemes)
library(leaflet)
library(sf)
 
load("mapa.RData")
data <- read_excel("data.xlsx")

data$Image <- rep(c('http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-01.jpg',
                   'http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-03.jpg',
                   "http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-05.jpg",
                   "https://mujeres360.org/wp-content/uploads/2021/06/ODS-9-1024x1024.jpg",
                   "http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-04.jpg"),6)


