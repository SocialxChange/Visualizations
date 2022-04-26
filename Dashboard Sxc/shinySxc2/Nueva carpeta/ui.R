ui <- dashboardPage(
    dashboardHeader(title = "Dashboard SxC"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Datos generales", tabName = "organizacion", icon = icon("fas fa-paste")),
            menuItem("Organizaciones", tabName = "proyectos", icon = icon("fa-solid fa-building"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "organizacion",
                    h2("Estadísticas promedio de todas las organizaciones"),
                    fluidRow(
                        infoBoxOutput("oneBox"),
                        infoBoxOutput("progressBox"),
                        infoBoxOutput("approvalBox")
                    ),
                    fluidRow(
                        column(width = 2),
                        infoBoxOutput("oneBox2"),
                        infoBoxOutput("progressBox2")
                    ),
                    
                    fluidRow(
                        column(width = 3),
                        box(
                            title = "Slider de ejemplo para los costos",
                            sliderInput("slider", "Numero (Las infobox pueden tener color de alerta):", 1, 100, 50)
                        )
                    ),
                    
                    fluidRow(width =  12,
                             
                             column(width =  12, 
                                    box(width = 12,
                                        title = "Gráfico 1: Barras", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                        plotOutput('grafico1', width = "100%", height = "400px")
                                        
                                    )
                             ),
                             
                             column(width =  12, 
                                    box(width = 12,
                                        title = "Gráfico 2: Circular", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                        plotOutput('grafico2', width = "100%", height = "400px")
                                        
                                    )
                             ),
                             
                             column(width =  12, 
                                    box(width = 12,
                                        title = "Gráfico 3: Nube de palabras", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                        wordcloud2Output('grafico3', width = "100%", height = "500px")
                                        
                                    )
                             ),
                             dataTableOutput("mytable")
                             
                    )
            ),
            tabItem(tabName = "proyectos",
                    h2("Estadísticas promedio de cada organización"),
                    selectInput("projects", "Organización:",
                                unique(as.character(data$Nombre_organizacion))),
                    fluidRow(column(width = 6,  textOutput("texto1")),
                             tags$style(type="text/css", "#texto1 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                             column(width = 6, textOutput("texto2")),
                             tags$style(type="text/css", "#texto2 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}")),
                    fluidRow(
                        infoBoxOutput("oneBox3", width = 6),
                        infoBoxOutput("oneBox5", width = 6),
                        
                        infoBoxOutput("progressBox3", width = 6),
                        infoBoxOutput("progressBox5", width = 6),
                        
                        infoBoxOutput("approvalBox3", width = 6),
                        infoBoxOutput("approvalBox5", width = 6),
                        
                        infoBoxOutput("oneBox4", width = 6),
                        infoBoxOutput("oneBox6", width = 6),
                        
                        infoBoxOutput("progressBox4", width = 6),
                        infoBoxOutput("progressBox6", width = 6),
                        
                        infoBoxOutput("approvalBox4", width = 6)
                    ),
                    fluidRow(
                        dataTableOutput("mytable1")
                    )
            )
        )
    )
)