ui <- dashboardPage(
    dashboardHeader(title = "Perfil Estándar"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Organizaciones", tabName = "organizacion", icon = icon("fa-solid fa-building")),
            menuItem("Proyectos", tabName = "proyectos", icon = icon("fas fa-tasks")),
            selectInput("periodo", "Año:",
                        rev(unique(as.character(data$Año))),
                        selected = max(data$Año))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "organizacion",
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
                             tags$style(type="text/css", "#texto8 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}")
                             ),
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
                               ),
                               box(width = 12,
                                   title = "ODS y tematicas", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                   plotOutput('grafico9', width = "100%", height = "600px"), 
                               ), 
                               box(width = 12,
                                   title = "Porcentaje avance actividades", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                   plotOutput('grafico11', width = "100%", height = "400px"), 
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
                                unique(as.character(data$Nombre_proyecto[data$Nombre_organizacion=="Org 1"]))),
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
                               ),
                               box(width = 12,
                                   title = "Porcentaje avance actividades", collapsible = T, collapsed = T, status = "primary", solidHeader = TRUE,
                                   plotOutput('grafico12', width = "100%", height = "400px"), 
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