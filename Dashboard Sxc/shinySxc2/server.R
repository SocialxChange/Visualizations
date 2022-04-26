server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    
    output$oneBox3 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo[data$Nombre_organizacion=="Org 1"]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox3 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios[data$Nombre_organizacion=="Org 1"]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox3 <- renderInfoBox({
        infoBox(
            "Duración proyectos", paste0(round(mean(data$Duracion_proyecto[data$Nombre_organizacion=="Org 1"]), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox4 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales[data$Nombre_organizacion=="Org 1"]), 1), " (HH)"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox4 <- renderInfoBox({
        colorx <- "green"
        if (mean(data$Costos_totales[data$Nombre_organizacion=="Org 1"]) > as.integer(quantile(data$Costos_totales, prob=c(0.6)))){
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data$Costos_totales[data$Nombre_organizacion=="Org 1"]), 1)), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    output$approvalBox4 <- renderInfoBox({
        infoBox(
            "Lugar geográfico", paste( unlist(data$Lugar_geografico[data$Nombre_organizacion=="Org 1"]), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
            color = "teal", fill = TRUE
        )
    })
    
    
    
    
    output$oneBox5 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox5 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox5 <- renderInfoBox({
        infoBox(
            "Duración proyectos", paste0(round(mean(data$Duracion_proyecto), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox6 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales), 1), " (HH)"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox6 <- renderInfoBox({
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data$Costos_totales), 1)), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    
    
    
    
    
    output$oneBox7 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo[data$Nombre_proyecto==input$projects]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox7 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios[data$Nombre_proyecto==input$projects]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox7 <- renderInfoBox({
        infoBox(
            "Duración proyecto", paste0(round(mean(data$Duracion_proyecto[data$Nombre_proyecto==input$projects]), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox8 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales[data$Nombre_proyecto==input$projects]), 1), " (HH)"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox8 <- renderInfoBox({
        colorx <- "green"
        if (mean(data$Costos_totales[data$Nombre_proyecto==input$projects]) > as.integer(quantile(data$Costos_totales, prob=c(0.6)))){
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data$Costos_totales[data$Nombre_proyecto==input$projects]), 1)), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    output$approvalBox8 <- renderInfoBox({
        infoBox(
            "Lugar geográfico", paste( unlist(data$Lugar_geografico[data$Nombre_proyecto==input$projects]), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
            color = "teal", fill = TRUE
        )
    })
    
    
    output$oneBox9 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox9 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox9 <- renderInfoBox({
        infoBox(
            "Duración proyectos", paste0(round(mean(data$Duracion_proyecto), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox10 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales), 1), " (HH)"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox10 <- renderInfoBox({
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data$Costos_totales), 1)), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    
    
    
    
    output$oneBox11 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo[data$Nombre_organizacion=="Org 1"]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox11 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios[data$Nombre_organizacion=="Org 1"]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox11 <- renderInfoBox({
        infoBox(
            "Duración proyectos", paste0(round(mean(data$Duracion_proyecto[data$Nombre_organizacion=="Org 1"]), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox12 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales[data$Nombre_organizacion=="Org 1"]), 1), " (HH)"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox12 <- renderInfoBox({
        colorx <- "green"
        if (mean(data$Costos_totales[data$Nombre_organizacion=="Org 1"]) > as.integer(quantile(data$Costos_totales, prob=c(0.6)))){
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data$Costos_totales[data$Nombre_organizacion=="Org 1"]), 1)), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    output$approvalBox12 <- renderInfoBox({
        infoBox(
            "Lugar geográfico", paste( unlist(data$Lugar_geografico[data$Nombre_organizacion=="Org 1"]), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
            color = "teal", fill = TRUE
        )
    })
    
    
    
    
    
    
    
    
    
    output$grafico1 <- renderPlot({
        ggplot(mpg,aes(x=cty,y=..density..),xlim(0,40))+
            geom_histogram(color="steelblue", fill="blue")+
            geom_density(fill="salmon",
                         alpha=0.4)+
            labs(x = "Eje x", y = "Eje y")
    })
    
    output$grafico2 <- renderPlot({
        df<-data.frame(
            Categorias=c("Categoría 1","Categoría 2","Categoría 3"),
            porcentaje=c(25,60,15))
        
        ggplot(df,aes(x=2,y=porcentaje,
                      fill=Categorias))+
            geom_bar(stat="identity")+
            coord_polar(theta = "y")+
            xlim(0.5,2.5)+theme_void()+
            geom_text(aes(label=porcentaje),
                      position = position_stack(vjust=0.5))
    })
    
    output$grafico3 <- renderWordcloud2({
        ##HFALTA HACER UN UNIQUE DE LAS MISIONES ANTES DE CREAR LA VARIABLE WORD_FREQ
        word_freq <- data %>%
            unnest_tokens(output = word,
                          input = Mision_organizacion,
                          token = "words",
                          format = "text") %>%
            #anti_join(stop_words, by = c("word" = "texto")) %>%
            count(word) # Cantidad de palabras
        word_freq <- word_freq[order(word_freq$n, decreasing = T), ]
        word_freq <- word_freq[(15:100),]
        listax <- data.frame(word = c("las", "para", "través", "así", "es", "lo", "otro", "hay", "sabe", "se", "sí", "sin", "sino"))
        word_freq <- anti_join(word_freq, listax)
        wordcloud2(data = word_freq, size = 0.5, color='random-dark' , backgroundColor="white")
    })
    
    
    
    
    
    output$mytable <- renderDataTable({
        datatable(unique(data[,c("Nombre_organizacion","Mision_organizacion")]), 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  options = list(
                      # dom = 't',
                      # deferRender = TRUE,
                      searching = TRUE,
                      autoWidth = TRUE,
                      # scrollCollapse = TRUE,
                      rownames = FALSE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = "500px",
                      fixedHeader = TRUE,
                      class = 'cell-border stripe',
                      fixedColumns = list(
                          leftColumns = 1,
                          heightMatch = 'none'
                      )
                  ),
                  rownames = F
        )
    })
    
    output$mytable1 <- renderDataTable({
        
        datatable(select(filter(data, Nombre_organizacion == "Org 1"), -c("Nombre_organizacion","Mision_organizacion")), 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  options = list(
                      # dom = 't',
                      # deferRender = TRUE,
                      searching = TRUE,
                      autoWidth = TRUE,
                      # scrollCollapse = TRUE,
                      rownames = FALSE,
                      scroller = TRUE,
                      scrollX = TRUE,
                      scrollY = "500px",
                      fixedHeader = TRUE,
                      class = 'cell-border stripe',
                      fixedColumns = list(
                          leftColumns = 1,
                          heightMatch = 'none'
                      )
                  ),
                  rownames = F
        )
    })
    
    
    
    
    output$texto1 <- renderText({
        x <- paste("Promedio organización")#input$projects)
        x
    })
    output$texto2 <- renderText({
        "Promedio todas las organizaciones"
    })
    output$texto3 <- renderText({
        x <- paste(input$projects)
        x
    })
    output$texto4 <- renderText({
        x <- paste("Promedio organización")#input$projects)
        x
    })
    output$texto5 <- renderText({
        "Promedio todas las organizaciones"
    })
    
    
    output$titulo1 <- renderText({
        ""
    })
    output$titulo2 <- renderText({
        "Proyectos de la organización"
    })
    output$titulo3 <- renderText({
        "Estadísticas promedio de la organización"
    })
    output$titulo4 <- renderText({
        "Estadísticas de cada proyecto"
    })
}




#data1 <- as.integer(runif(min=0, max=200, n=1000))

#quantile(data1)
#Sin parámetros quantile nos retorna los cuartiles, que podemos verificar invocandola con el parámetro prob pasandole un vector con los puntos de cada cuartil

#aux1 <- as.integer(quantile(data$Costos_totales, prob=c(0.2)))
#quantile(data, prob=seq(0, 1, 1/4))