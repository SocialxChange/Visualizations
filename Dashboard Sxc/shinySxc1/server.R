server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    
    output$oneBox <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox <- renderInfoBox({
        infoBox(
            "Duracion proyectos", paste0(round(mean(data$Duracion_proyecto), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox2 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales), 1), " Personas"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox2 <- renderInfoBox({
        colorx <- "green"
        if (60>50) {
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ ", 600), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    
    
    
    
    output$oneBox3 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo[data$Nombre_organizacion==input$projects]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox3 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios[data$Nombre_organizacion==input$projects]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox3 <- renderInfoBox({
        infoBox(
            "Duracion proyectos", paste0(round(mean(data$Duracion_proyecto[data$Nombre_organizacion==input$projects]), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox4 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales[data$Nombre_organizacion==input$projects]), 1), " Personas"), icon = icon("fas fa-address-card"),
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
            "Lugar geográfico", paste( unlist(unique(data$Lugar_geografico[data$Nombre_organizacion==input$projects])), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
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
            "Duracion proyectos", paste0(round(mean(data$Duracion_proyecto), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox6 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales), 1), " Personas"), icon = icon("fas fa-address-card"),
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
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo[data$Nombre_proyecto==input$projects1]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox7 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios[data$Nombre_proyecto==input$projects1]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox7 <- renderInfoBox({
        infoBox(
            "Duración proyecto", paste0(round(mean(data$Duracion_proyecto[data$Nombre_proyecto==input$projects1]), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox8 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales[data$Nombre_proyecto==input$projects1]), 1), " Personas"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox8 <- renderInfoBox({
        colorx <- "green"
        if (mean(data$Costos_totales[data$Nombre_proyecto==input$projects1]) > as.integer(quantile(data$Costos_totales, prob=c(0.6)))){
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data$Costos_totales[data$Nombre_proyecto==input$projects1]), 1)), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    output$approvalBox8 <- renderInfoBox({
        infoBox(
            "Lugar geográfico", paste( unlist(unique(data$Lugar_geografico[data$Nombre_proyecto==input$projects1])), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
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
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales), 1), " Personas"), icon = icon("fas fa-address-card"),
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
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo[data$Nombre_organizacion==as.character(unique(select(filter(data, Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox11 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios[data$Nombre_organizacion==as.character(unique(select(filter(data, Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox11 <- renderInfoBox({
        infoBox(
            "Duración proyectos", paste0(round(mean(data$Duracion_proyecto[data$Nombre_organizacion==as.character(unique(select(filter(data, Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1), " Semanas"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox12 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data$RRHH_totales[data$Nombre_organizacion==as.character(unique(select(filter(data, Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1), " Personas"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox12 <- renderInfoBox({
        colorx <- "green"
        if (mean(data$Costos_totales[data$Nombre_organizacion==as.character(unique(select(filter(data, Nombre_proyecto==input$projects1), "Nombre_organizacion")))]) > as.integer(quantile(data$Costos_totales, prob=c(0.6)))){
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data$Costos_totales[data$Nombre_organizacion==as.character(unique(select(filter(data, Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1)), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    output$approvalBox12 <- renderInfoBox({
        infoBox(
            "Lugar geográfico", paste( unlist(unique(data$Lugar_geografico[data$Nombre_organizacion==as.character(unique(select(filter(data, Nombre_proyecto==input$projects1), "Nombre_organizacion")))])), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
            color = "teal", fill = TRUE
        )
    })
    
    
    
    
    
    
    
    output$oneBox <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data$Poblacion_objetivo), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data$Beneficiarios), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    
    
    
    
    
    
    output$metrica1 <- renderInfoBox({
        infoBox(
            "Beneficiarios por población", round( (sum(data$Beneficiarios)/sum(data$Poblacion_objetivo)), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$metrica2 <- renderInfoBox({
        infoBox(
            "Beneficiarios por RRHH", round( (sum(data$Beneficiarios)/sum(data$RRHH_totales)), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$metrica3 <- renderInfoBox({
        infoBox(
            "Beneficiarios por proyecto", round( (sum(data$Beneficiarios)/length(unique(data$Nombre_proyecto))), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    
    
    
    output$metrica4 <- renderInfoBox({
        infoBox(
            "Presupuesto por proyecto", round( (sum(data$Costos_totales)/length(unique(data$Nombre_proyecto))), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    output$metrica5 <- renderInfoBox({
        infoBox(
            "Presupuesto por beneficiario", round( (sum(data$Costos_totales)/sum(data$Beneficiarios)), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    output$metrica6 <- renderInfoBox({
        infoBox(
            "Presupuesto por población", round( (sum(data$Costos_totales)/sum(data$Poblacion_objetivo)), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    
    
    output$metrica7 <- renderInfoBox({
        infoBox(
            "Presupuesto por RRHH", round( (sum(data$Costos_totales)/sum(data$RRHH_totales)), 1), icon = icon("far fa-dollar-sign"),
            color = "blue", fill = TRUE
        )
    })
    output$metrica8 <- renderInfoBox({
        infoBox(
            "Población por RRHH", round( (sum(data$Poblacion_objetivo)/sum(data$RRHH_totales)), 1), icon = icon("fas fa-users"),
            color = "blue", fill = TRUE
        )
    })
    
    
    
    data1 <- reactive({
        data1 <- filter(data, Nombre_organizacion == input$projects)
    })
    
    output$metrica9 <- renderInfoBox({
        infoBox(
            "Beneficiarios por población", round( (sum(data1()$Beneficiarios)/sum(data1()$Poblacion_objetivo)), 2), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$metrica10 <- renderInfoBox({
        infoBox(
            "Beneficiarios por RRHH", round( (sum(data1()$Beneficiarios)/sum(data1()$RRHH_totales)), 2), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$metrica11 <- renderInfoBox({
        infoBox(
            "Beneficiarios por proyecto", round( (sum(data1()$Beneficiarios)/length(unique(data1()$Nombre_proyecto))), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    
    
    output$metrica12 <- renderInfoBox({
        infoBox(
            "Presupuesto por proyecto", round( (sum(data1()$Costos_totales)/length(unique(data1()$Nombre_proyecto))), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    output$metrica13 <- renderInfoBox({
        infoBox(
            "Presupuesto por beneficiario", round( (sum(data1()$Costos_totales)/sum(data1()$Beneficiarios)), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    output$metrica14 <- renderInfoBox({
        infoBox(
            "Presupuesto por población", round( (sum(data1()$Costos_totales)/sum(data1()$Poblacion_objetivo)), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    
    
    output$metrica15 <- renderInfoBox({
        infoBox(
            "Presupuesto por RRHH", round( (sum(data1()$Costos_totales)/sum(data1()$RRHH_totales)), 1), icon = icon("far fa-dollar-sign"),
            color = "blue", fill = TRUE
        )
    })
    output$metrica16 <- renderInfoBox({
        infoBox(
            "Población por RRHH", round( (sum(data1()$Poblacion_objetivo)/sum(data1()$RRHH_totales)), 1), icon = icon("fas fa-users"),
            color = "blue", fill = TRUE
        )
    })
    
    
    
    data2 <- reactive({
        data2 <- filter(data, Nombre_proyecto == input$projects1)
    })
    
    output$metrica17 <- renderInfoBox({
        infoBox(
            "Beneficiarios por población", round( (sum(data2()$Beneficiarios)/sum(data2()$Poblacion_objetivo)), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$metrica18 <- renderInfoBox({
        infoBox(
            "Beneficiarios por RRHH", round( (sum(data2()$Beneficiarios)/sum(data2()$RRHH_totales)), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$metrica19 <- renderInfoBox({
        infoBox(
            "Beneficiarios por proyecto", round( (sum(data2()$Beneficiarios)/length(unique(data2()$Nombre_proyecto))), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    
    
    output$metrica20 <- renderInfoBox({
        infoBox(
            "Presupuesto por proyecto", round( (sum(data2()$Costos_totales)/length(unique(data2()$Nombre_proyecto))), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    output$metrica21 <- renderInfoBox({
        infoBox(
            "Presupuesto por beneficiario", round( (sum(data2()$Costos_totales)/sum(data2()$Beneficiarios)), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    output$metrica22 <- renderInfoBox({
        infoBox(
            "Presupuesto por población", round( (sum(data2()$Costos_totales)/sum(data2()$Poblacion_objetivo)), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    
    
    output$metrica23 <- renderInfoBox({
        infoBox(
            "Presupuesto por RRHH", round( (sum(data2()$Costos_totales)/sum(data2()$RRHH_totales)), 1), icon = icon("far fa-dollar-sign"),
            color = "blue", fill = TRUE
        )
    })
    output$metrica24 <- renderInfoBox({
        infoBox(
            "Población por RRHH", round( (sum(data2()$Poblacion_objetivo)/sum(data2()$RRHH_totales)), 1), icon = icon("fas fa-users"),
            color = "blue", fill = TRUE
        )
    })
    
    
    
    
    
    
    
    
    output$grafico1 <- renderPlot({
        aux <- filter(data, Año==max(Año))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Poblacion_objetivo))+
            geom_col(aes(fill = Nombre_organizacion), width = 0.7) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Poblacion_objetivo)) + geom_hline(yintercept = sum(aux$Poblacion_objetivo)) +
            annotate("text", x=2022.4, y=mean(aux$Poblacion_objetivo), label=paste("Promedio:",mean(aux$Poblacion_objetivo)), color="red") +
            labs(x = "Año" , y = "Población objetivo" , fill = "Nombre organizacion" , title = "Población objetivo") + theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_text(x=2022.4, y=mean(aux$Poblacion_objetivo), label=paste("Promedio:",mean(aux$Poblacion_objetivo))) + 
            geom_text(x=2022.4, y=sum(aux$Poblacion_objetivo), label=paste("Total:",sum(aux$Poblacion_objetivo))) + 
            scale_x_continuous(limits = c(2021.6, 2022.4) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Poblacion_objetivo)))
        p
    })
    
    output$grafico2 <- renderPlot({
        aux <- filter(data, Año==max(Año))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Beneficiarios))+
            geom_col(aes(fill = Nombre_organizacion), width = 0.7) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Beneficiarios)) + geom_hline(yintercept = sum(aux$Beneficiarios)) +
            annotate("text", x=2022.4, y=mean(aux$Beneficiarios), label=paste("Promedio:",mean(aux$Beneficiarios)), color="red") +
            labs(x = "Año" , y = "Beneficiarios" , fill = "Nombre organización") +
            geom_text(x=2022.4, y=mean(aux$Beneficiarios), label=paste("Promedio:",mean(aux$Beneficiarios))) + 
            geom_text(x=2022.4, y=sum(aux$Beneficiarios), label=paste("Total:",sum(aux$Beneficiarios))) + 
            scale_x_continuous(limits = c(2021.6, 2022.4) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Poblacion_objetivo)))
        p
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
    
    output$grafico4 <- renderPlot({
        #Gráfico por años (General)
        aux <- data %>%
            group_by(Año) %>%
            summarise(Costos_totales = sum(Costos_totales))
        r <- ggplot(aux, aes(x = Año, y = Costos_totales)) +
            geom_point(size = 4, colour = 4) + geom_line(size = 2)+
            geom_hline(yintercept = mean(aux$Costos_totales), linetype = "dotdash", colour = "red")  +
            annotate("text", x= 2020.2, y=(mean(aux$Costos_totales)+1000), label=paste("Promedio:",round(mean(aux$Costos_totales)),1), color="red")+
            labs(x = "Año" , y = "Costos totales")
        r 
    })
    
    output$grafico5 <- renderPlot({
        #Gráfico por años (Organizaciones)
        aux <- data %>%
            group_by(Año, Nombre_organizacion) %>%
            summarise(Costos_totales = sum(Costos_totales))
        r <- ggplot(aux, aes(x = Año, y = Costos_totales, color = Nombre_organizacion)) +
            geom_point(size = 4, colour = 4) + geom_line(size = 2)+
            geom_hline(yintercept = mean(aux$Costos_totales), linetype = "dotdash", colour = "red")  +
            annotate("text", x= 2020.2, y=(mean(aux$Costos_totales)+1000), label=paste("Promedio:",round(mean(aux$Costos_totales)),1), color="red")+
            labs(x = "Año" , y = "Costos totales" , color = "Nombre organización")
        r 
    })
    
    output$grafico6 <- renderPlot({
        #Gráfico por años (Proyectos)
        aux <- data %>%
            group_by(Año, Nombre_proyecto) %>%
            summarise(Costos_totales = sum(Costos_totales))
        r <- ggplot(aux, aes(x = Año, y = Costos_totales, color = Nombre_proyecto)) +
            geom_point(size = 4, colour = 4) + geom_line(size = 2)+
            geom_hline(yintercept = mean(aux$Costos_totales), linetype = "dotdash", colour = "red")  +
            annotate("text", x= 2020.2, y=(mean(aux$Costos_totales)+1000), label=paste("Promedio:",round(mean(aux$Costos_totales)),1), color="red")+
            labs(x = "Año" , y = "Costos totales" , color = "Nombre organización")
        r
    })
    
#    output$grafico7 <- renderPlot({   
#        aux <- data
#        aux$ODS <- factor(aux$ODS, levels=c("Fin de la pobreza","Salud y bienestar","Educación de calidad","Igualdad de género", "Industria, innovación e infraestructura"))
#        ggplot(aux, aes(x = ODS, y = Poblacion_objetivo, fill=Nombre_organizacion)) +
#            geom_bar(stat = "identity") +
#            theme(legend.position = "top",
#                  axis.title.x = element_blank(),
#                  axis.text.x = element_blank())
#    })

    output$grafico8 <- renderPlot({ 
        df2 <- data
        df2$Image <- rep(c('http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-01.jpg',
                           'http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-03.jpg',
                           "http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-05.jpg",
                           "https://mujeres360.org/wp-content/uploads/2021/06/ODS-9-1024x1024.jpg",
                           "http://www.un.org/sustainabledevelopment/es/wp-content/uploads/sites/3/2016/01/S_SDG_Icons-01-04.jpg"),6)
        df2$ODS <- factor(df2$ODS, levels=c("Industria, innovación e infraestructura", "Igualdad de género","Educación de calidad", "Salud y bienestar", "Fin de la pobreza" ))
        m <- ggplot(df2, aes(x = ODS, y = Poblacion_objetivo, fill=Nombre_organizacion)) +
            geom_bar(stat = "identity") + coord_flip()+
            theme(axis.text.y = element_blank())+
            labs(x = "ODS" , y = "Población objetivo" , fill = "Nombre organización")
        m + geom_image(aes(x = ODS, image = Image), y = 0,  # add geom_image layer
                       size = 0.108, hjust = 2,
                       inherit.aes = FALSE)
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
                          leftColumns = 1,
                          heightMatch = 'none'
                      )
                  ),
                  rownames = F
        )
    })
    
    
    output$mytable2 <- renderDataTable({
        
        datatable(select(data, -c("Nombre_organizacion","Mision_organizacion")), 
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
    
    
    
    

    output$texto01 <- renderText({
        x <- paste(input$projects1)
        x
    })
    output$texto02 <- renderText({
        x <- paste("Promedio", unique(select(filter(data, Nombre_proyecto==input$projects1), "Nombre_organizacion")))#input$projects)
        x
    })
    output$texto03 <- renderText({
        "Promedio todas las organizaciones"
    })
    
    

    output$titulo01 <- renderText({
        "Proyectos de la organización"
    })

    output$titulo02 <- renderText({
        "Estadísticas de cada proyecto"
    })
    
    
    
    
    
    output$texto1 <- renderText({
        x <- paste("Promedios:", input$projects)
        x
    })
    output$texto2 <- renderText({
        "Promedio todas las organizaciones"
    })
    output$texto3 <- renderText({
        "Estadísticas promedio de todas las organizaciones"
    })
    output$texto4 <- renderText({
        "Organizaciones registradas"
    })
    output$texto5 <- renderText({
        "Estadísticas promedio de cada organización"
    })
    output$texto6 <- renderText({
        x <- paste("Proyectos de la organización:", input$projects)
        x
    })
    output$texto7 <- renderText({
        x <- "Métricas"
        x
    })
    output$texto8 <- renderText({
        x <- paste("Métricas:", input$projects)
        x
    })
    output$texto9 <- renderText({
        x <- paste("Métricas:", input$projects1)
        x
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
}



#data1 <- as.integer(runif(min=0, max=200, n=1000))

#quantile(data1)
#Sin parámetros quantile nos retorna los cuartiles, que podemos verificar invocandola con el parámetro prob pasandole un vector con los puntos de cada cuartil

#aux1 <- as.integer(quantile(data$Costos_totales, prob=c(0.2)))
#quantile(data, prob=seq(0, 1, 1/4))