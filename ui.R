################################################################################
# UI
################################################################################

shinyUI <- secure_app(dashboardPage(
  dashboardHeader(title = "Dares"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Demandes d'activité partielle", tabName = "ReqDAP", icon = icon("cog")),
                menuItem("Demandes d'indemnisation", tabName = "ReqDI", icon = icon("cog"))
    )
  ),
  dashboardBody(
    
    tabItems(
      # Requêteur DAP
      tabItem(tabName = "ReqDAP",
              h1("Demandes d'activité partielle par secteur d'activité"),
              tags$br(),
              fluidRow(
                column(1,
                       uiOutput("m0rdap")),
                column(3,
                       uiOutput("m1rdap")),
                column(1,
                       uiOutput("m2rdap"))
                
              ),
              fluidRow(
                column(1,
                       uiOutput("m3rdap")),
                column(3,
                       uiOutput("m4rdap"))
              ),
              dataTableOutput('dataReqDAP'),
              fluidRow(
                column(7,
                       dataTableOutput('TotReqDAP', width = "100%"))
              ),
              tags$br(),
              fluidRow(
                column(3,
                       downloadButton("downloadDAP", "Télécharger les données"))
              ),
              tags$br(),
              h2("Représentation graphique"),
              fluidRow(
                column(3,
                       uiOutput("m5rdap"))
              ),
              fluidRow(
                column(3,
                       bsButton("DAPviz", "Visualiser ma sélection", icon = icon("bar-chart-o")))
              ),
              tags$br(),
              plotlyOutput('plotDAP', height = "100%", width = "100%")
              
      ),
      
      # Requêteur DI
      tabItem(tabName = "ReqDI",
              h1("Demandes d'indemnisation par secteur d'activité"),
              tags$br(),
              fluidRow(
                column(1,
                       uiOutput("m0rdi")),
                column(3,
                       uiOutput("m1rdi")),
                column(1,
                       uiOutput("m2rdi"))
              ),
              fluidRow(
                column(1,
                       uiOutput("m3rdi")),
                column(2,
                       uiOutput("m4rdi")),
                column(2,
                       uiOutput("m5rdi"))
              ),
              dataTableOutput('dataReqDI'),
              fluidRow(
                column(7,
                       dataTableOutput('TotReqDI', width = "100%"))
              ),
              tags$br(),
              fluidRow(
                column(3,
                       downloadButton("downloadDI", "Télécharger les données"))
              ),
              tags$br(),
              h2("Représentation graphique"),
              fluidRow(
                column(3,
                       uiOutput("m6rdi")),
                column(3,
                       uiOutput("m7rdi"))
              ),
              fluidRow(
                column(3,
                       bsButton("DIviz", "Visualiser ma sélection", icon = icon("bar-chart-o")))
              ),
              tags$br(),
              
              tags$br(),
              plotlyOutput('plotDI', height = "100%", width = "100%")
              
      )        
    )
  )
))
