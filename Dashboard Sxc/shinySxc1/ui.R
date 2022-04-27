ui <- dashboardPage(
    dashboardHeader(title = "Perfil Administrador"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Datos generales", tabName = "general", icon = icon("fas fa-paste")),
            menuItem("Organizaciones", tabName = "organizaciones", icon = icon("fa-solid fa-building")),
            menuItem("Proyectos", tabName = "proyectos", icon = icon("fas fa-tasks"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "general",
                    fluidRow(column(width = 12,  textOutput("texto3"), br()),
                             tags$style(type="text/css", "#texto3 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
                    fluidRow(
                        box(width = 12,
                            title = "Indicadores", collapsible = T, collapsed = F, status = "primary", solidHeader = TRUE,
                            plotOutput('grafico1', width = "100%", height = "200px"), 
                            plotOutput('grafico2', width = "100%", height = "200px"),
                            plotOutput('grafico2_1', width = "100%", height = "200px"),
                            br(),
                            plotOutput('grafico2_2', width = "100%", height = "200px"),
                            plotOutput('grafico2_3', width = "100%", height = "200px"),
                            
                        )
                    ),
                    
                    fluidRow(column(width = 12, br(),br(),  textOutput("texto7"), br()),
                             tags$style(type="text/css", "#texto7 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
                    fluidRow(
                        infoBoxOutput("metrica1", width = 4),
                        infoBoxOutput("metrica2", width = 4),
                        infoBoxOutput("metrica3", width = 4),
                        
                        infoBoxOutput("metrica4", width = 4),
                        infoBoxOutput("metrica5", width = 4),
                        infoBoxOutput("metrica6", width = 4),
                    ),
                    fluidRow(
                        column(width = 2),
                        infoBoxOutput("metrica7", width = 4),
                        infoBoxOutput("metrica8", width = 4),
                    ),
                    
                    fluidRow(
                        column(width = 3),
                    ),
                    
                    fluidRow(width =  12,
                             
                             column(width =  12, 
                                    box(width = 12,
                                        title = "Costos anuales", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                        plotOutput('grafico4', width = "100%", height = "400px"), 
                                        
                                    )
                             ),
                             
                             column(width =  12, 
                                    box(width = 12,
                                        title = "ODS y tematicas", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                        plotOutput('grafico8', width = "100%", height = "600px"), 
                                        #img(src='imagenOds.png', align = "right")
                                    )
                             ),
                             
                             column(width =  12, 
                                    box(width = 12,
                                        title = "Misión de organizaciones", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                        wordcloud2Output('grafico3', width = "100%", height = "500px")
                                        
                                    )
                             ),
                             fluidRow(column(width = 12, br(), textOutput("texto4")),
                                      tags$style(type="text/css", "#texto4 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
                             column(width = 12,
                             dataTableOutput("mytable"))
                             
                    )
            ),
            tabItem(tabName = "organizaciones",
                    fluidRow(column(width = 12,  textOutput("texto5")),
                             tags$style(type="text/css", "#texto5 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"),
                            ),
                    fluidRow(
                        box(width = 12,
                            title = "Indicadores", collapsible = T, collapsed = F, status = "primary", solidHeader = TRUE,
                            plotOutput('grafico2_4', width = "100%", height = "220px"), 
                            plotOutput('grafico2_5', width = "100%", height = "220px"),
                            plotOutput('grafico2_6', width = "100%", height = "220px"),
                            br(),
                            plotOutput('grafico2_7', width = "100%", height = "220px"),
                            plotOutput('grafico2_8', width = "100%", height = "220px"),
                            
                        )
                        
                    ),
                    
                    fluidRow(column(width = 12, br(),br(),  textOutput("texto8"), br()),
                             tags$style(type="text/css", "#texto8 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"),
                             selectInput("projects", "Organización:",
                                         unique(as.character(data$Nombre_organizacion)))),
                    fluidRow(
                        infoBoxOutput("metrica9", width = 4),
                        infoBoxOutput("metrica10", width = 4),
                        infoBoxOutput("metrica11", width = 4),
                        
                        infoBoxOutput("metrica12", width = 4),
                        infoBoxOutput("metrica13", width = 4),
                        infoBoxOutput("metrica14", width = 4),
                    ),
                    fluidRow(
                        column(width = 2),
                        infoBoxOutput("metrica15", width = 4),
                        infoBoxOutput("metrica16", width = 4),
                    ),
                    
                    fluidRow(
                        column(width =  12, 
                               box(width = 12,
                                   title = "Costos anuales", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                   plotOutput('grafico5', width = "100%", height = "400px"), 
                               )
                        ),
                    ),
                    fluidRow(column(width = 12, br(),  textOutput("texto6")),
                             tags$style(type="text/css", "#texto6 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
                    fluidRow(
                        column(width = 12,
                               dataTableOutput("mytable1"))
                    )
            ),
            tabItem(tabName = "proyectos",
                    fluidRow(column(width = 12, textOutput("titulo02")),
                             tags$style(type="text/css", "#titulo02 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
                    selectInput("projects1", "Proyecto:",
                                unique(as.character(data$Nombre_proyecto))),
                    fluidRow(column(width = 4,br(),  textOutput("texto01")),
                             tags$style(type="text/css", "#texto01 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                             column(width = 4,br(), textOutput("texto02")),
                             tags$style(type="text/css", "#texto02 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}"),
                             column(width = 4, textOutput("texto03")),
                             tags$style(type="text/css", "#texto03 { height: 50px; width: 100%; text-align:center; font-size: 25px; display: block;}")),
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
                    
                    fluidRow(column(width = 12, br(),br(),  textOutput("texto9"), br()),
                             tags$style(type="text/css", "#texto9 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")),
                    fluidRow(
                        infoBoxOutput("metrica17", width = 4),
                        infoBoxOutput("metrica18", width = 4),
                        infoBoxOutput("metrica19", width = 4),
                        
                        infoBoxOutput("metrica20", width = 4),
                        infoBoxOutput("metrica21", width = 4),
                        infoBoxOutput("metrica22", width = 4),
                    ),
                    fluidRow(
                        column(width = 2),
                        infoBoxOutput("metrica23", width = 4),
                        infoBoxOutput("metrica24", width = 4),
                    ),
                    
                    fluidRow(
                        column(width =  12, 
                               box(width = 12,
                                   title = "Costos anuales", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                   plotOutput('grafico6', width = "100%", height = "400px"), 
                               )
                        ),
                    ),
                    fluidRow(column(width = 12, br(), br(), br(), br(), br(),  textOutput("titulo2")),
                             tags$style(type="text/css", "#titulo01 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                    ),
                    fluidRow(
                        column(width = 12,
                               dataTableOutput("mytable2"))
                    )
            )
        )
    )
)