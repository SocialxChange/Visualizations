ui <- dashboardPage(
    dashboardHeader(title = "Perfil estándar"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Organización", tabName = "organizacion", icon = icon("fa-solid fa-building")),
            menuItem("Proyectos", tabName = "proyectos", icon = icon("fas fa-tasks"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "organizacion",
                    fluidRow(column(width = 12, textOutput("titulo3"), br()),
                             tags$style(type="text/css", "#titulo3 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
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
                    br(),
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
                             
                   #          fluidRow(column(width = 4,br(),  textOutput("titulo1")),
                   #                   tags$style(type="text/css", "#titulo1 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                    #         ),
                    #         
                    #         column(width = 12,
                    #                dataTableOutput("mytable"))
                             
                    )
            ),
            tabItem(tabName = "proyectos",
                    fluidRow(column(width = 12, textOutput("titulo4")),
                             tags$style(type="text/css", "#titulo4 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
                    selectInput("projects", "Proyecto:",
                                unique(as.character(data$Nombre_proyecto[data$Nombre_organizacion=="Org 1"]))),
                    fluidRow(column(width = 4,br(),  textOutput("texto3")),
                             tags$style(type="text/css", "#texto3 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                             column(width = 4,br(), textOutput("texto4")),
                             tags$style(type="text/css", "#texto4 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                             column(width = 4, textOutput("texto5")),
                             tags$style(type="text/css", "#texto5 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}")),
                    fluidRow(br()),
                    fluidRow(
                        infoBoxOutput("oneBox7", width = 4),
                        infoBoxOutput("oneBox11", width = 4),
                        infoBoxOutput("oneBox9", width = 4),
                        
                        
                        infoBoxOutput("progressBox7", width = 4),
                        infoBoxOutput("progressBox11", width = 4),
                        infoBoxOutput("progressBox9", width = 4),
                        
                        
                        infoBoxOutput("approvalBox7", width = 4),
                        infoBoxOutput("approvalBox11", width = 4),
                        infoBoxOutput("approvalBox9", width = 4),
                        
                        
                        infoBoxOutput("oneBox8", width = 4),
                        infoBoxOutput("oneBox12", width = 4),
                        infoBoxOutput("oneBox10", width = 4),
                        
                        
                        infoBoxOutput("progressBox8", width = 4),
                        infoBoxOutput("progressBox12", width = 4),
                        infoBoxOutput("progressBox10", width = 4),
                        
                        
                        infoBoxOutput("approvalBox8", width = 4),
                        infoBoxOutput("approvalBox12", width = 4)
                        
                    ),
                    fluidRow(column(width = 12, br(), br(), br(), br(), br(),  textOutput("titulo2")),
                             tags$style(type="text/css", "#titulo2 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                             ),
                    fluidRow(
                        column(width = 12,
                               dataTableOutput("mytable1"))
                    )
            )
        )
    )
)