library(readr)
library(tidyverse)
library(esquisse)

data <- read_delim("Priorizatec/data.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
data$entrega <- as.numeric(data$entrega)  
data$comconpers <- as.numeric(data$comconpers)  
data$motivación <- as.numeric(data$motivación) 

data$diagnostico[which(data$diagnostico==1)] <- 2
data$diagnostico[which(data$diagnostico==0.5)] <- 1

# data$diagnostico[which(data$diagnostico==2)] <- "2 Pruebas"
# data$diagnostico[which(data$diagnostico==1)] <- "1 Prueba"
# data$diagnostico[which(data$diagnostico==0)] <- "Ninguna"


data$diagnostico <- as.factor(data$diagnostico)

df <- data %>% filter(!is.na(comconpers)) %>% filter(!is.na(entrega))
# esquisser(df)

library(plotly)

plot <- ggplot(df) +
  aes(x = entrega, y = comconpers, colour = `motivación`, size = diagnostico) +
  geom_point() +
  scale_color_distiller(palette = "RdBu") +
  labs(x = "Tareas", y = "Conectividad", color = "Motivación", size = "Diagnóstico") +
  theme_minimal()

plot

priori <- ggplotly(plot)

htmlwidgets::saveWidget(
  priori, "priorizaTec.html", libdir = "lib",
  title = "Priorización de Recursos Tecnológicos",
  selfcontained = FALSE
)



# Clusters


cl <- kmeans(df[,c(2:5)], 2, iter.max = 1000, nstart = 10)

plot(df[,c(2,3)], col = cl$cluster)
plot(df[,c(2,4)], col = cl$cluster)
plot(df[,c(4,5)], col = cl$cluster)


points(cl$centers, col = 1:2, pch = 8, cex = 2)
