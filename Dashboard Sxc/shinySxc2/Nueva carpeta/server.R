server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    
    output$oneBox <- renderInfoBox({
        infoBox(
            "Público objetivo", paste0(round(mean(data$Publico_objetivo), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox <- renderInfoBox({
        infoBox(
            "N° usuarios directos", paste0(round(mean(data$N_usuarios_directos), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox <- renderInfoBox({
        infoBox(
            "Indicador X", paste0("Valor X"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox2 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales), 1), " (HH)"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox2 <- renderInfoBox({
        colorx <- "green"
        if (input$slider>50) {
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ ", input$slider), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    
    
    
    
    output$oneBox3 <- renderInfoBox({
        infoBox(
            "Público objetivo", paste0(round(mean(data$Publico_objetivo[data$Nombre_organizacion==input$projects]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox3 <- renderInfoBox({
        infoBox(
            "N° usuarios directos", paste0(round(mean(data$N_usuarios_directos[data$Nombre_organizacion==input$projects]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox3 <- renderInfoBox({
        infoBox(
            "Indicador X", paste0("Valor x"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox4 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales[data$Nombre_organizacion==input$projects]), 1), " (HH)"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox4 <- renderInfoBox({
        colorx <- "green"
        if (mean(data$Costos_totales[data$Nombre_organizacion==input$projects]) > as.integer(quantile(data$Costos_totales, prob=c(0.6)))){
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data$Costos_totales[data$Nombre_organizacion==input$projects]), 1)), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    output$approvalBox4 <- renderInfoBox({
        infoBox(
            "Lugar geográfico", paste( unlist(data$Lugar_geografico[data$Nombre_organizacion==input$projects]), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
            color = "teal", fill = TRUE
        )
    })
    
    
    
    
    output$oneBox5 <- renderInfoBox({
        infoBox(
            "Público objetivo", paste0(round(mean(data$Publico_objetivo), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox5 <- renderInfoBox({
        infoBox(
            "N° usuarios directos", paste0(round(mean(data$N_usuarios_directos), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox5 <- renderInfoBox({
        infoBox(
            "Indicador X", paste0("Valor X"), icon = icon("fas fa-hourglass"),
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
                          leftColumns = 2,
                          heightMatch = 'none'
                      )
                  )
        )
    })
    
    output$mytable1 <- renderDataTable({
        
        datatable(select(filter(data, Nombre_organizacion == input$projects), -c("Nombre_organizacion","Mision_organizacion")), 
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
                          leftColumns = 2,
                          heightMatch = 'none'
                      )
                  )
        )
    })
    
    
    
    
    output$texto1 <- renderText({
        x <- paste("Promedios:", input$projects)
        x
    })
    output$texto2 <- renderText({
        "Promedio todas las organizaciones"
    })

}




#data1 <- as.integer(runif(min=0, max=200, n=1000))

#quantile(data1)
#Sin parámetros quantile nos retorna los cuartiles, que podemos verificar invocandola con el parámetro prob pasandole un vector con los puntos de cada cuartil

#aux1 <- as.integer(quantile(data$Costos_totales, prob=c(0.2)))
#quantile(data, prob=seq(0, 1, 1/4))