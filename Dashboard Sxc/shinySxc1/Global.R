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
 

load("coordenadasComunas.RData")
data <- read_excel("data.xlsx")
data$Lugar_geografico <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(data$Lugar_geografico))

map_comunas1 <- data %>% 
              group_by(Lugar_geografico) %>% 
                summarise(Beneficiarios = sum(Beneficiarios))
map_comunas1 <- rename(map_comunas1, "Comuna" = "Lugar_geografico")

map_comunas1 <- merge(map_comunas1, map_comunas, by = "Comuna")
data$Image <- rep(c('http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-01.jpg',
                   'http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-03.jpg',
                   "http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-05.jpg",
                   "https://mujeres360.org/wp-content/uploads/2021/06/ODS-9-1024x1024.jpg",
                   "http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-04.jpg"),6)

