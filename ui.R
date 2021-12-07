# HEADER ----
header <- dashboardHeader(
  title = "PROYECTO 1",
  tags$li(
    class = "dropdown",
    tags$a(HTML(paste("Universidad Galileo")))
  )
)


# SIDEBAR ----
sidebar <- dashboardSidebar(
  h6("JUAN CARLOS ROMERO", style = "color:gray;"),
  sidebarMenu(
    menuItem(text = "Acerca de", tabName = "acerca_de", icon = icon("home"), selected = TRUE),
    menuItem(text = "Tablas", tabName = "tablas_resumen_datos", icon = icon("table")),
    menuItem(text = "Graficas", tabName = "resumen_graficas", icon = icon("dashboard")),
    menuItem(text = "Parametros en URL", tabName = "parametros_url", icon = icon("adjust"))
  ),
  h5("FILTROS-INPUTS", style = "color:gray;"),
  numericInput("anhos", "Anhos (0 para mostrar todos)", value = 10),
  selectInput(inputId = "especies", label = "Especies (Depende del anho)", choices = NULL),
  sliderInput("meses", "Meses (0 para mostrar todos - Depende del anho) ",  min = 0, max = 15, value = 5),
  h5("UPDATE FUNCTION", style = "color:gray;"),
  actionButton("reset", "Limpiar filtros")
  
)



body <- dashboardBody(
 
  tabItems(
    tabItem(
      tabName = "acerca_de",
      fluidRow(
        h1(
          id = "titulo",
          "Evaluar  las  competencia  adquiridas  en  la  creación  de  shiny apps.  Durante  las  clases  vimos  todos  los  bloques  con  los  que  podemos  construir  pero  no  los acoplamos,  esto  es  lo  que  esta  parte  del  parcial  busca",
          align = "center"
        )
      ),
      fluidRow(
        h2("  Instrucciones"),
        p("   Usted  deberá  seleccionar  un  dataset  y  crear  un  shiny  app  que  utilice
          cada  una  de  los  conceptos listados  a  continuación,",
          align = "left"
        ),
        withTags(
          ul(
            li("Tablas",
               align = "justify"
            ),
            li("Graficas", align = "justify"),
            li("Input", align = "justify"),
            li("Layouts", align = "justify"),
            li("Interactividad", align = "justify"),
            li("Reactividad", align = "justify"),
            li("Update function", align = "justify"),
            li("Parametros en el URL", align = "justify")
          ) 
        ),
        p("   Se  deja  a  la  creatividad  de  cada  developer  que  construirá,  por  lo  que  parte 
          de  la  nota  es  la creatividad  mostrada.  Pueden  trabajar  individualmente  o  en  parejas.",
          align = "left"
        ),
        p("   La  entrega  tiene  que  ser  un  repositorio  en  Github  y  publicar  el  shiny  en shinyapps.io 
          pueden ver  la  siguiente  guia https://shiny.rstudio.com/tutorial/written-tutorial/lesson7/",
          align = "left"
        ),
        h2("  DATOS"),
        withTags(
          ul(
            li("record_id", align = "justify"),
            li("month", align = "justify"),
            li("day", align = "justify"),
            li("year", align = "justify"),
            li("plot_id", align = "justify"),
            li("species_id", align = "justify"),
            li("sex", align = "justify"),
            li("hindfoot_length", align = "justify"),
            li("weight", align = "justify")
          ) 
        ),
      )
    ),

    tabItem(
      tabName = "tablas_resumen_datos",
      fluidRow(
        column(
          width = 12,
          h4("Resumen de datos", "", align = "left"),
          DT::dataTableOutput("tabla_datos")
        )
      )
    ),

    tabItem(
      tabName = "resumen_graficas",
      fluidRow(
        h3("", align = "center"),
        box(
          width = 6,
          plotOutput("grafica_sexo")
        ),
        box(
          width = 6,
          plotOutput("grafica_anho")
        ),
        box(
          width = 12,
          title = textOutput("Tamanho vs peso"),
          plotOutput(outputId = "grafica_tamanho_peso")
        )
        
      )
    ), 
    
    tabItem(
      tabName = "parametros_url",
      
      
      fluidRow(
        box(
          width = 12,
          title = "Parametros",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          tabBox(
            width = 12,
            selectInput("grafica_id", "Seleccione la estadistica a visualizar",
                        choices = c("Resumen-estadistico-especiess","Resumen-Estadistico-peso", "Resumen-Estadistico-sexo")),
            hr(),
            tabsetPanel(
              id="params",
              type="hidden",
              tabPanel(
                title = "Resumen-estadistico-especies",
                column(
                  width = 4,
                  icon = icon("table"),
                  h4("Listado de especies unicas"),
                  DT::dataTableOutput("especies_dt")
                )
              ),
              tabPanel(
                title = "Resumen-Estadistico-peso",
                icon = icon("table"),
                column(
                  width = 5,
                  h4("Peso"),
                  DT::dataTableOutput(outputId = "peso_dt")
                )
              ),
              tabPanel(
                title = "Resumen-Estadistico-sexo",
                icon = icon("table"),
                column(
                  width = 5,
                  h4("Sexo"),
                  DT::dataTableOutput(outputId = "sexo_dt")
                )
              )
            )
          ),
          plotOutput("plot_estadistica")
        )
      )
    )
  )
) 

ui <- dashboardPage(header = header, sidebar = sidebar, body = body)
