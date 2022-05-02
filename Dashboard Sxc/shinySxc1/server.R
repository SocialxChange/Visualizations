server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    
    data0 <- reactive({
        data0 <- filter(data, Año == input$periodo)
    })
    
    
    output$oneBox7 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data0()$Poblacion_objetivo[data0()$Nombre_proyecto==input$projects1]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox7 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data0()$Beneficiarios[data0()$Nombre_proyecto==input$projects1]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox7 <- renderInfoBox({
        infoBox(
            "Duración proyecto", paste0(round(mean(data0()$Duracion_proyecto[data0()$Nombre_proyecto==input$projects1]), 1), " Meses"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox8 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data0()$RRHH_totales[data0()$Nombre_proyecto==input$projects1]), 1), " Personas"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox8 <- renderInfoBox({
        colorx <- "green"
        if (mean(data0()$Costos_totales[data0()$Nombre_proyecto==input$projects1]) > as.integer(quantile(data0()$Costos_totales, prob=c(0.6)))){
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data0()$Costos_totales[data0()$Nombre_proyecto==input$projects1]), 1)), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    output$approvalBox8 <- renderInfoBox({
        infoBox(
            "Lugar geográfico", paste( unlist(unique(data0()$Lugar_geografico[data0()$Nombre_proyecto==input$projects1])), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
            color = "teal", fill = TRUE
        )
    })
    
    
    output$oneBox9 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data0()$Poblacion_objetivo), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox9 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data0()$Beneficiarios), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox9 <- renderInfoBox({
        infoBox(
            "Duración proyectos", paste0(round(mean(data0()$Duracion_proyecto), 1), " Meses"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox10 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data0()$RRHH_totales), 1), " Personas"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox10 <- renderInfoBox({
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data0()$Costos_totales), 1)), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    
    
    
    
    output$oneBox11 <- renderInfoBox({
        infoBox(
            "Población objetivo", paste0(round(mean(data0()$Poblacion_objetivo[data0()$Nombre_organizacion==as.character(unique(select(filter(data0(), Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1), " Personas"), icon = icon("fas fa-users"),
            color = "aqua", fill = TRUE
        )
    })
    output$progressBox11 <- renderInfoBox({
        infoBox(
            "N° beneficiarios", paste0(round(mean(data0()$Beneficiarios[data0()$Nombre_organizacion==as.character(unique(select(filter(data0(), Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1), " Personas"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$approvalBox11 <- renderInfoBox({
        infoBox(
            "Duración proyectos", paste0(round(mean(data0()$Duracion_proyecto[data0()$Nombre_organizacion==as.character(unique(select(filter(data0(), Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1), " Meses"), icon = icon("fas fa-hourglass"),
            color = "yellow", fill = TRUE
        )
    })
    
    output$oneBox12 <- renderInfoBox({
        infoBox(
            "RRHH utilizado", paste0(round(mean(data0()$RRHH_totales[data0()$Nombre_organizacion==as.character(unique(select(filter(data0(), Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1), " Personas"), icon = icon("fas fa-address-card"),
            color = "blue", fill = TRUE
        )
    })
    output$progressBox12 <- renderInfoBox({
        colorx <- "green"
        if (mean(data0()$Costos_totales[data0()$Nombre_organizacion==as.character(unique(select(filter(data0(), Nombre_proyecto==input$projects1), "Nombre_organizacion")))]) > as.integer(quantile(data0()$Costos_totales, prob=c(0.6)))){
            colorx <- "red"
        }
        infoBox(
            "Costos", paste0("M$ " ,round(mean(data0()$Costos_totales[data0()$Nombre_organizacion==as.character(unique(select(filter(data0(), Nombre_proyecto==input$projects1), "Nombre_organizacion")))]), 1)), icon = icon("far fa-dollar-sign"),
            color = colorx, fill = TRUE
        )
    })
    output$approvalBox12 <- renderInfoBox({
        infoBox(
            "Lugar geográfico", paste( unlist(unique(data0()$Lugar_geografico[data0()$Nombre_organizacion==as.character(unique(select(filter(data0(), Nombre_proyecto==input$projects1), "Nombre_organizacion")))])), collapse= ', ') , icon = icon("fas fa-map-marker-alt"),
            color = "teal", fill = TRUE
        )
    })
    
    
    
    
    
    
    
    
    output$metrica1 <- renderInfoBox({
        infoBox(
            "Beneficiarios por población", paste0((round( (sum(data0()$Beneficiarios)/sum(data0()$Poblacion_objetivo)), 3))*100, "%"), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$metrica2 <- renderInfoBox({
        infoBox(
            "Beneficiarios por RRHH", round( (sum(data0()$Beneficiarios)/sum(data0()$RRHH_totales)), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    output$metrica3 <- renderInfoBox({
        infoBox(
            "Beneficiarios por proyecto", round( (sum(data0()$Beneficiarios)/length(unique(data0()$Nombre_proyecto))), 1), icon = icon("fas fa-user-check"),
            color = "purple", fill = TRUE
        )
    })
    
    
    
    output$metrica4 <- renderInfoBox({
        infoBox(
            "Presupuesto por proyecto", round( (sum(data0()$Costos_totales)/length(unique(data0()$Nombre_proyecto))), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    output$metrica5 <- renderInfoBox({
        infoBox(
            "Presupuesto por beneficiario", round( (sum(data0()$Costos_totales)/sum(data0()$Beneficiarios)), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    output$metrica6 <- renderInfoBox({
        infoBox(
            "Presupuesto por población", round( (sum(data0()$Costos_totales)/sum(data0()$Poblacion_objetivo)), 1), icon = icon("far fa-dollar-sign"),
            color = "green", fill = TRUE
        )
    })
    
    
    output$metrica7 <- renderInfoBox({
        infoBox(
            "Presupuesto por RRHH", round( (sum(data0()$Costos_totales)/sum(data0()$RRHH_totales)), 1), icon = icon("far fa-dollar-sign"),
            color = "blue", fill = TRUE
        )
    })
    output$metrica8 <- renderInfoBox({
        infoBox(
            "Población por RRHH", round( (sum(data0()$Poblacion_objetivo)/sum(data0()$RRHH_totales)), 1), icon = icon("fas fa-users"),
            color = "blue", fill = TRUE
        )
    })
    
    
    
    
    
    data1 <- reactive({
        if (input$projects=="Todos") {
            data1 <- filter(data, Año==input$periodo)
        } else {
            data1 <- filter(data, Nombre_organizacion == input$projects & Año == input$periodo)
        }
    })
    
    
    
    output$metrica9 <- renderInfoBox({
        infoBox(
            "Beneficiarios por población", paste0((round( (sum(data1()$Beneficiarios)/sum(data1()$Poblacion_objetivo)), 3))*100, "%"), icon = icon("fas fa-user-check"),
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
        data2 <- filter(data, Nombre_proyecto == input$projects1 & Año == input$periodo)
    })
    
    output$metrica17 <- renderInfoBox({
        infoBox(
            "Beneficiarios por población", paste0((round( (sum(data2()$Beneficiarios)/sum(data2()$Poblacion_objetivo)), 3))*100, "%"), icon = icon("fas fa-user-check"),
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
        aux <- filter(data, Año==input$periodo)
        aux <- aux %>% 
                group_by(Nombre_organizacion, Año) %>% 
                summarise(Poblacion_objetivo = sum(Poblacion_objetivo))
        aux <- aux[order(aux$Poblacion_objetivo, decreasing = F), ]
        aux$Poblacion_objetivo1 <- NA
        aux$Poblacion_objetivo1[1] <- aux$Poblacion_objetivo[1]
        for (i in (2:nrow(aux))) {
            aux$Poblacion_objetivo1[i] <- aux$Poblacion_objetivo[i] - aux$Poblacion_objetivo[i-1]
        }
        aux$Nombre_organizacion <- paste0(aux$Nombre_organizacion, ": ", aux$Poblacion_objetivo)
        aux$Nombre_organizacion <- factor(aux$Nombre_organizacion, levels=rev(aux$Nombre_organizacion))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Poblacion_objetivo1))+
            geom_col(aes(fill = Nombre_organizacion), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Poblacion_objetivo)) + 
            labs(x = "Año" , y = element_blank() , fill = "Nombre organización" , title = "Población objetivo (Personas)") + 
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$Poblacion_objetivo), label=paste("Promedio:",round(mean(aux$Poblacion_objetivo),2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Poblacion_objetivo1)*0.98), label=paste("Suma total:",sum(aux$Poblacion_objetivo)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Poblacion_objetivo1))) +
            theme_igray()
        p
    })
    
    output$grafico2 <- renderPlot({
        aux <- filter(data, Año==input$periodo)
        aux <- aux %>% 
            group_by(Nombre_organizacion, Año) %>% 
            summarise(Beneficiarios = sum(Beneficiarios),
                      Poblacion_objetivo = sum(Poblacion_objetivo))
        aux <- aux[order(aux$Poblacion_objetivo, decreasing = F), ]
        aux$Poblacion_objetivo1 <- NA
        aux$Poblacion_objetivo1[1] <- aux$Poblacion_objetivo[1]
        for (i in (2:nrow(aux))) {
            aux$Poblacion_objetivo1[i] <- aux$Poblacion_objetivo[i] - aux$Poblacion_objetivo[i-1]
        }
        aux <- aux[order(aux$Beneficiarios, decreasing = F), ]
        aux$Beneficiarios1 <- NA
        aux$Beneficiarios1[1] <- aux$Beneficiarios[1]
        for (i in (2:nrow(aux))) {
            aux$Beneficiarios1[i] <- aux$Beneficiarios[i] - aux$Beneficiarios[i-1]
        }
        aux$Nombre_organizacion <- paste0(aux$Nombre_organizacion, ": ", aux$Beneficiarios)
        aux$Nombre_organizacion <- factor(aux$Nombre_organizacion, levels=rev(aux$Nombre_organizacion))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Beneficiarios1))+
            geom_col(aes(fill = Nombre_organizacion), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Beneficiarios)) +
            labs(x = "Año" , y = element_blank() , fill = "Nombre organización", title = "Beneficiarios (Personas)") +
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$Beneficiarios), label=paste("Promedio:", round(mean(aux$Beneficiarios), 2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Poblacion_objetivo1)*0.98), label=paste("Suma total:",sum(aux$Beneficiarios)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Poblacion_objetivo1))) + 
            theme_igray()
        p
    })
    
    output$grafico2_1 <- renderPlot({
        aux <- filter(data, Año==input$periodo)
        aux <- aux %>% 
            group_by(Nombre_organizacion, Año) %>% 
            summarise(RRHH_totales = sum(RRHH_totales),
                      Poblacion_objetivo = sum(Poblacion_objetivo))
        aux <- aux[order(aux$Poblacion_objetivo, decreasing = F), ]
        aux$Poblacion_objetivo1 <- NA
        aux$Poblacion_objetivo1[1] <- aux$Poblacion_objetivo[1]
        for (i in (2:nrow(aux))) {
            aux$Poblacion_objetivo1[i] <- aux$Poblacion_objetivo[i] - aux$Poblacion_objetivo[i-1]
        }
        aux <- aux[order(aux$RRHH_totales, decreasing = F), ]
        aux$RRHH_totales1 <- NA
        aux$RRHH_totales1[1] <- aux$RRHH_totales[1]
        for (i in (2:nrow(aux))) {
            aux$RRHH_totales1[i] <- aux$RRHH_totales[i] - aux$RRHH_totales[i-1]
        }
        aux$Nombre_organizacion <- paste0(aux$Nombre_organizacion, ": ", aux$RRHH_totales)
        aux$Nombre_organizacion <- factor(aux$Nombre_organizacion, levels=rev(aux$Nombre_organizacion))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = RRHH_totales1))+
            geom_col(aes(fill = Nombre_organizacion), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$RRHH_totales)) +
            labs(x = "Año" , y = element_blank() , fill = "Nombre organización", title = "Recursos humanos (Personas)") +
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$RRHH_totales), label=paste("Promedio:", round(mean(aux$RRHH_totales), 2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Poblacion_objetivo1)*0.98), label=paste("Suma total:",sum(aux$RRHH_totales)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Poblacion_objetivo1))) + 
            theme_igray()
        p
    })
    
    output$grafico2_2 <- renderPlot({
        aux <- filter(data, Año==input$periodo)
        aux <- aux %>% 
            group_by(Nombre_organizacion, Año) %>% 
            summarise(Costos_totales = sum(Costos_totales))
        aux <- aux[order(aux$Costos_totales, decreasing = F), ]
        aux$Costos_totales1 <- NA
        aux$Costos_totales1[1] <- aux$Costos_totales[1]
        for (i in (2:nrow(aux))) {
            aux$Costos_totales1[i] <- aux$Costos_totales[i] - aux$Costos_totales[i-1]
        }
        aux$Nombre_organizacion <- paste0(aux$Nombre_organizacion, ": ", aux$Costos_totales)
        aux$Nombre_organizacion <- factor(aux$Nombre_organizacion, levels=rev(aux$Nombre_organizacion))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Costos_totales1))+
            geom_col(aes(fill = Nombre_organizacion), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Costos_totales)) + 
            labs(x = "Año" , y = element_blank() , fill = "Nombre organización" , title = "Costos (M$)") + 
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$Costos_totales), label=paste("Promedio:",round(mean(aux$Costos_totales),2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Costos_totales1)*0.98), label=paste("Suma total:",sum(aux$Costos_totales)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Costos_totales1))) +
            theme_solarized()
        p
    })
    
    output$grafico2_3 <- renderPlot({
        aux <- filter(data, Año==input$periodo)
        aux <- aux %>% 
            group_by(Nombre_organizacion, Año) %>% 
            summarise(Duracion_proyecto = sum(Duracion_proyecto))
        aux <- aux[order(aux$Duracion_proyecto, decreasing = F), ]
        aux$Duracion_proyecto1 <- NA
        aux$Duracion_proyecto1[1] <- aux$Duracion_proyecto[1]
        for (i in (2:nrow(aux))) {
            aux$Duracion_proyecto1[i] <- aux$Duracion_proyecto[i] - aux$Duracion_proyecto[i-1]
        }
        aux$Nombre_organizacion <- paste0(aux$Nombre_organizacion, ": ", aux$Duracion_proyecto)
        aux$Nombre_organizacion <- factor(aux$Nombre_organizacion, levels=rev(aux$Nombre_organizacion))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Duracion_proyecto1))+
            geom_col(aes(fill = Nombre_organizacion), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Duracion_proyecto)) + 
            labs(x = "Año" , y = element_blank() , fill = "Nombre organización" , title = "Duración proyectos (Meses)") + 
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$Duracion_proyecto), label=paste("Promedio:",round(mean(aux$Duracion_proyecto),2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Duracion_proyecto1)*0.98), label=paste("Suma total:",sum(aux$Duracion_proyecto)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Duracion_proyecto1)))+
            theme_solarized()
        p
    })
    
    
    
    
    output$grafico2_4 <- renderPlot({
        aux <- data1()
        aux <- aux[order(aux$Poblacion_objetivo, decreasing = F), ]
        aux$Poblacion_objetivo1 <- NA
        aux$Poblacion_objetivo1[1] <- aux$Poblacion_objetivo[1]
        for (i in (2:nrow(aux))) {
            aux$Poblacion_objetivo1[i] <- aux$Poblacion_objetivo[i] - aux$Poblacion_objetivo[i-1]
        }
        aux$Nombre_proyecto <- paste0(aux$Nombre_proyecto, ": ", aux$Poblacion_objetivo)
        aux$Nombre_proyecto <- factor(aux$Nombre_proyecto, levels=rev(aux$Nombre_proyecto))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Poblacion_objetivo1))+
            geom_col(aes(fill = Nombre_proyecto), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Poblacion_objetivo)) + 
            labs(x = "Año" , y = element_blank() , fill = "Nombre proyecto" , title = "Población objetivo (Personas)") + 
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$Poblacion_objetivo), label=paste("Promedio:",round(mean(aux$Poblacion_objetivo),2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Poblacion_objetivo1)*0.98), label=paste("Suma total:",sum(aux$Poblacion_objetivo)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Poblacion_objetivo1))) +
            theme_igray()
        p
        
        
    })
    
    output$grafico2_5 <- renderPlot({
        aux <- data1()
        aux <- aux[order(aux$Poblacion_objetivo, decreasing = F), ]
        aux$Poblacion_objetivo1 <- NA
        aux$Poblacion_objetivo1[1] <- aux$Poblacion_objetivo[1]
        for (i in (2:nrow(aux))) {
            aux$Poblacion_objetivo1[i] <- aux$Poblacion_objetivo[i] - aux$Poblacion_objetivo[i-1]
        }
        aux <- aux[order(aux$Beneficiarios, decreasing = F), ]
        aux$Beneficiarios1 <- NA
        aux$Beneficiarios1[1] <- aux$Beneficiarios[1]
        for (i in (2:nrow(aux))) {
            aux$Beneficiarios1[i] <- aux$Beneficiarios[i] - aux$Beneficiarios[i-1]
        }
        aux$Nombre_proyecto <- paste0(aux$Nombre_proyecto, ": ", aux$Beneficiarios)
        aux$Nombre_proyecto <- factor(aux$Nombre_proyecto, levels=rev(aux$Nombre_proyecto))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Beneficiarios1))+
            geom_col(aes(fill = Nombre_proyecto), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Beneficiarios)) +
            labs(x = "Año" , y = element_blank() , fill = "Nombre proyecto", title = "Beneficiarios (Personas)") +
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$Beneficiarios), label=paste("Promedio:", round(mean(aux$Beneficiarios), 2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Poblacion_objetivo1)*0.98), label=paste("Suma total:",sum(aux$Beneficiarios)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Poblacion_objetivo1))) + 
            theme_igray()
        p
    })
    
    output$grafico2_6 <- renderPlot({
        aux <- data1()
        aux <- aux[order(aux$Poblacion_objetivo, decreasing = F), ]
        aux$Poblacion_objetivo1 <- NA
        aux$Poblacion_objetivo1[1] <- aux$Poblacion_objetivo[1]
        for (i in (2:nrow(aux))) {
            aux$Poblacion_objetivo1[i] <- aux$Poblacion_objetivo[i] - aux$Poblacion_objetivo[i-1]
        }
        aux <- aux[order(aux$RRHH_totales, decreasing = F), ]
        aux$RRHH_totales1 <- NA
        aux$RRHH_totales1[1] <- aux$RRHH_totales[1]
        for (i in (2:nrow(aux))) {
            aux$RRHH_totales1[i] <- aux$RRHH_totales[i] - aux$RRHH_totales[i-1]
        }
        aux$Nombre_proyecto <- paste0(aux$Nombre_proyecto, ": ", aux$RRHH_totales)
        aux$Nombre_proyecto <- factor(aux$Nombre_proyecto, levels=rev(aux$Nombre_proyecto))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = RRHH_totales1))+
            geom_col(aes(fill = Nombre_proyecto), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$RRHH_totales)) +
            labs(x = "Año" , y = element_blank() , fill = "Nombre proyecto", title = "Recursos humanos (Personas)") +
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$RRHH_totales), label=paste("Promedio:", round(mean(aux$RRHH_totales), 2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Poblacion_objetivo1)*0.98), label=paste("Suma total:",sum(aux$RRHH_totales)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Poblacion_objetivo1))) + 
            theme_igray()
        p
    })
    
    output$grafico2_7 <- renderPlot({
        aux <- data1()
        aux <- aux[order(aux$Costos_totales, decreasing = F), ]
        aux$Costos_totales1 <- NA
        aux$Costos_totales1[1] <- aux$Costos_totales[1]
        for (i in (2:nrow(aux))) {
            aux$Costos_totales1[i] <- aux$Costos_totales[i] - aux$Costos_totales[i-1]
        }
        aux$Nombre_proyecto <- paste0(aux$Nombre_proyecto, ": ", aux$Costos_totales)
        aux$Nombre_proyecto <- factor(aux$Nombre_proyecto, levels=rev(aux$Nombre_proyecto))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Costos_totales1))+
            geom_col(aes(fill = Nombre_proyecto), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Costos_totales)) + 
            labs(x = "Año" , y = element_blank() , fill = "Nombre proyecto" , title = "Costos (M$)") + 
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$Costos_totales), label=paste("Promedio:",round(mean(aux$Costos_totales),2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Costos_totales1)*0.98), label=paste("Suma total:",sum(aux$Costos_totales)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Costos_totales1))) +
            theme_solarized()
        p
    })
    
    output$grafico2_8 <- renderPlot({
        aux <- data1()
        aux <- aux[order(aux$Duracion_proyecto, decreasing = F), ]
        aux$Duracion_proyecto1 <- NA
        aux$Duracion_proyecto1[1] <- aux$Duracion_proyecto[1]
        for (i in (2:nrow(aux))) {
            aux$Duracion_proyecto1[i] <- aux$Duracion_proyecto[i] - aux$Duracion_proyecto[i-1]
        }
        aux$Nombre_proyecto <- paste0(aux$Nombre_proyecto, ": ", aux$Duracion_proyecto)
        aux$Nombre_proyecto <- factor(aux$Nombre_proyecto, levels=rev(aux$Nombre_proyecto))
        p <- ggplot(aux, aes(x = as.numeric(Año), y = Duracion_proyecto1))+
            geom_col(aes(fill = Nombre_proyecto), width = 0.4) + coord_flip() + 
            geom_hline(yintercept = mean(aux$Duracion_proyecto)) + 
            labs(x = "Año" , y = element_blank() , fill = "Nombre proyecto" , title = "Duración proyectos (Meses)") + 
            theme(plot.title = element_text(size = 16, family = "serif", hjust = 0.5)) + 
            geom_label(x=(max(aux$Año) + 0.3), y=mean(aux$Duracion_proyecto), label=paste("Promedio:",round(mean(aux$Duracion_proyecto),2)), colour = "black", fill = "grey") + 
            geom_label(x=(max(aux$Año) - 0.3), y=(sum(aux$Duracion_proyecto1)*0.98), label=paste("Suma total:",sum(aux$Duracion_proyecto)), colour = "navy", fill = "lightblue") + 
            scale_x_continuous(limits = c((max(aux$Año) - 0.4), (max(aux$Año) + 0.4)) ,breaks = unique(aux$Año)) + 
            scale_y_continuous(limits = c(0, sum(aux$Duracion_proyecto1)))+
            theme_solarized()
        p
    })
    
    
    
    output$grafico3 <- renderWordcloud2({
        ##HFALTA HACER UN UNIQUE DE LAS MISIONES ANTES DE CREAR LA VARIABLE WORD_FREQ
        word_freq <- data0() %>%
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
            geom_point(size = 2, colour = 4) + 
            geom_area(fill = 3,
                      alpha = 0.5,
                      color = 1,   
                      lwd = 0.5,   
                      linetype = 1) +
            geom_label(aes(label = Costos_totales)) +
            scale_x_continuous(breaks = unique(aux$Año))  + 
            scale_y_continuous(limits = c(0, (max(aux$Costos_totales) * 1.2))) + 
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
            geom_point(size = 4, colour = 4) + 
            geom_line(size = 2)+
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
            geom_point(size = 4, colour = 4) + 
            geom_line(size = 2)+
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
        df2 <- data0()
        df2$ODS <- factor(df2$ODS, levels=c("Industria, innovación e infraestructura", "Igualdad de género","Educación de calidad", "Salud y bienestar", "Fin de la pobreza" ))
        m <- ggplot(df2, aes(x = ODS, y = Beneficiarios, fill=Nombre_organizacion)) +
            geom_bar(stat = "identity") + coord_flip()+
            theme(axis.text.y = element_blank())+
            labs(x = "ODS" , y = "Beneficiarios" , fill = "Nombre organización")
        m + geom_image(aes(x = ODS, image = Image), y = 0,  # add geom_image layer
                       size = 0.108, hjust = 2,
                       inherit.aes = FALSE)
    }) 
    
    output$grafico9 <- renderPlot({ 
        df2 <- data1()
        df2$ODS <- factor(df2$ODS, levels=c("Industria, innovación e infraestructura", "Igualdad de género","Educación de calidad", "Salud y bienestar", "Fin de la pobreza" ))
        m <- ggplot(df2, aes(x = ODS, y = Beneficiarios, fill=Nombre_organizacion)) +
            geom_bar(stat = "identity") + coord_flip()+
            theme(axis.text.y = element_blank())+
            labs(x = "ODS" , y = "Beneficiarios" , fill = "Nombre organización")
        m + geom_image(aes(x = ODS, image = Image), y = 0,  # add geom_image layer
                       size = 0.108, hjust = 2,
                       inherit.aes = FALSE)
    }) 
    
    
    
    output$grafico10 <- renderPlot({ 
        data1 <- data0() %>%
            group_by(Año) %>%
            summarise(Porcentaje_avance_actividades = mean(Porcentaje_avance_actividades))
        
        data1$Estado <- "Completado" 
        data2 <- data1
        data2$Porcentaje_avance_actividades <- 100-as.double(data1$Porcentaje_avance_actividades)
        data2$Estado <- "Pendiente"
        data1 <- rbind(data1, data2)
        hsize <- 1.5
        data1$Estado <- factor(data1$Estado, levels=c( "Pendiente" ,"Completado" ))
        data1 <- data1 %>% 
            mutate(x = hsize)
        ggplot(data = data1, aes(x = hsize, y = Porcentaje_avance_actividades, fill = Estado)) +
            geom_col() +
            coord_polar(theta = "y") +
            xlim(c(0.2, hsize + 0.5)) + 
            labs(x = element_blank(), y = element_blank(), title = "Porcentaje avance actividades") +
            geom_label(aes(label=paste(Porcentaje_avance_actividades, "%")), position = position_stack(vjust = 0.5), show.legend = F) +
            theme( axis.text = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.background = element_rect(fill = "white"),
                   plot.title = element_text(size = 16, family = "serif", hjust = 0.5))
    }) 
    
    
    
    output$grafico11 <- renderPlot({ 
        data1 <- data1() %>%
            group_by(Año) %>%
            summarise(Porcentaje_avance_actividades = mean(Porcentaje_avance_actividades))
        
        data1$Estado <- "Completado" 
        data2 <- data1
        data2$Porcentaje_avance_actividades <- 100-as.double(data1$Porcentaje_avance_actividades)
        data2$Estado <- "Pendiente"
        data1 <- rbind(data1, data2)
        hsize <- 1.5
        data1$Estado <- factor(data1$Estado, levels=c( "Pendiente" ,"Completado" ))
        data1 <- data1 %>% 
            mutate(x = hsize)
        ggplot(data = data1, aes(x = hsize, y = Porcentaje_avance_actividades, fill = Estado)) +
            geom_col() +
            coord_polar(theta = "y") +
            xlim(c(0.2, hsize + 0.5)) + 
            labs(x = element_blank(), y = element_blank(), title = "Porcentaje avance actividades") +
            geom_label(aes(label=paste(Porcentaje_avance_actividades, "%")), position = position_stack(vjust = 0.5), show.legend = F) +
            theme( axis.text = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.background = element_rect(fill = "white"),
                   plot.title = element_text(size = 16, family = "serif", hjust = 0.5))
    }) 
    
    
    
    output$grafico12 <- renderPlot({ 
        data1 <- data2() %>%
            group_by(Año) %>%
            summarise(Porcentaje_avance_actividades = mean(Porcentaje_avance_actividades))
        
        data1$Estado <- "Completado" 
        data2 <- data1
        data2$Porcentaje_avance_actividades <- 100-as.double(data1$Porcentaje_avance_actividades)
        data2$Estado <- "Pendiente"
        data1 <- rbind(data1, data2)
        hsize <- 1.5
        data1$Estado <- factor(data1$Estado, levels=c( "Pendiente" ,"Completado" ))
        data1 <- data1 %>% 
            mutate(x = hsize)
        ggplot(data = data1, aes(x = hsize, y = Porcentaje_avance_actividades, fill = Estado)) +
            geom_col() +
            coord_polar(theta = "y") +
            xlim(c(0.2, hsize + 0.5)) + 
            labs(x = element_blank(), y = element_blank(), title = "Porcentaje avance actividades") +
            geom_label(aes(label=paste(Porcentaje_avance_actividades, "%")), position = position_stack(vjust = 0.5), show.legend = F) +
            theme( axis.text = element_blank(),
                   axis.ticks.y = element_blank(),
                   panel.background = element_rect(fill = "white"),
                   plot.title = element_text(size = 16, family = "serif", hjust = 0.5))
    }) 
    
    
    
    user_dataTO <- reactive({
        leaflet() %>% setView(lng=-69.64029, lat=-39.44606 , zoom=4) %>%
            # Opcion para anadir imagenes o mapas de fondo (tiles)
            addProviderTiles(provider = providers$CartoDB.Positron) %>%
            # Funcion para agregar poligonos
            addPolygons(data=map_regiones1,
                        color="black",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 1,
                        fillOpacity = 0)%>%
            # Funcion para agregar poligonos
            addPolygons(data=map_comunas1,
                        color = ~pal_num_comunas_pn(map_comunas1$N_comunas_pn), 
                        weight = 0.9,
                        smoothFactor = 0.9,
                        opacity = 0.7,
                        fillOpacity = 0.4,
                        highlightOptions = highlightOptions(color = "black", weight = 1, bringToFront = TRUE),
                        label = ~labels_num_comunas_pn, labelOptions = labelOptions(direction = "auto")) %>%
            addLegend(position = "bottomright", pal = pal_num_comunas_pn, values = map_comunas1$N_comunas_pn, 
                      title = "Número de beneficiarios por comuna")
        
        
        


    })
    
    
    output$mapa1 <- renderLeaflet({
        user_dataTO()
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$mytable <- renderDataTable({
        datatable(unique(data0()[,c("Nombre_organizacion","Mision_organizacion")]), 
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
        
        datatable(select(data1(), -c("Nombre_organizacion","Mision_organizacion")), 
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
        
        datatable(select(data0(), -c("Nombre_organizacion","Mision_organizacion")), 
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