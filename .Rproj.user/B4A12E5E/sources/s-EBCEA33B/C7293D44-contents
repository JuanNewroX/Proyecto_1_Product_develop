server <- function(input, output, session) {
  
  datos <- reactive({
    df <- read.csv("inputdata/surveys.csv")
    df1 <- read.csv("inputdata/species.csv")
    
    df %>% left_join(df1)
  })
  
  
  observeEvent(input$reset, {
    updateNumericInput(
      session = session, inputId = "anhos", 
      min = datos() %>% select(year) %>% distinct() %>% pull() %>% min(),
      max = datos() %>% select(year) %>% distinct() %>% pull() %>% max(),
      value = 0
    )
    
    dftmp <- datos()
    
    updateSelectInput(
      session = session, inputId = "especies",
      choices = c("TODOS", unique(dftmp %>% select(species) %>% distinct()) , selected  = "TODOS" )
    )
    
    updateSliderInput(
      session = session, inputId = "meses", 
      min = 0,
      max = dftmp %>% select(month) %>% distinct() %>% pull() %>% max(),
      value = 0
    )
  })
  
  datos_filtrados <- reactive({
    datos_analisis <- datos()
    if(is.na(input$anhos) | input$anhos == 0){
      dftmp <- datos_analisis
    }
    else{
      dftmp <- datos_analisis  %>% filter(year == input$anhos)
    }
    if(input$especies != "TODOS"){
      dftmp <- dftmp %>% filter(species == input$especies) 
    }
    if(!is.na(input$meses) & input$meses != 0){
      dftmp <- dftmp %>% filter(month == input$meses) 
    }
    
    dftmp
  })
 
  
  observeEvent( datos(), {
    updateNumericInput(
      session = session, inputId = "anhos", 
      min = datos() %>% select(year) %>% distinct() %>% pull() %>% min(),
      max = datos() %>% select(year) %>% distinct() %>% pull() %>% max(),
      value = 0
    )
  })
  
  observe({
    if(is.na(input$anhos) | input$anhos == 0){
      dftmp <- datos()
    }
    else{
      dftmp <- datos() %>% filter(year == input$anhos)
    }
    updateSelectInput(
      session = session, inputId = "especies",
      choices = c("TODOS", unique(dftmp %>% select(species) %>% distinct()))
    )
  })
  
  observe({
    if(is.na(input$anhos) | input$anhos == 0){
      dftmp <- datos()
    }
    else{
      dftmp <- datos()  %>% filter(year == input$anhos)
    }
    
    updateSliderInput(
      session = session, inputId = "meses", 
      min = 0,
      max = dftmp %>% select(month) %>% distinct() %>% pull() %>% max(),
      value = 0
    )
  })
  
  observeEvent(input$grafica_id,{
    updateTabsetPanel(session, "params", selected = input$grafica_id)
  })
  
  output$tabla_datos <- DT::renderDataTable({
    datos_filtrados() %>% 
      DT::datatable(filter = 'top', selection = list(
        mode = "single",
        target = "row"
      )) %>% formatString(columns = "hindfoot_length", suffix = " mm")
  })
  
  output$grafica_sexo <- renderPlot({
    datos_filtrados() %>% ggplot(aes(x=sex))+
      theme_minimal() +
      geom_bar() +
      ggtitle("Sexo")
  })
  
  
  output$grafica_anho <- renderPlot({
    
    if(input$anhos == 0){
      plot <- datos_filtrados() %>% ggplot(aes(x=year))+
        theme_minimal() +
        geom_bar() +
        ggtitle("Encuestas por anio")
    }
    else{
      plot <- datos_filtrados() %>% mutate(
        fecha = ifelse(month < 10, paste0(year , "-0", month), paste0(year , "-", month)))%>% ggplot(aes(x=fecha))+
        theme_minimal() +
        geom_bar() +
        ggtitle("Encuestas por mes")
    }
    plot
    
  })
  
  
  output$grafica_tamanho_peso <- renderPlot({
    datos_filtrados() %>% ggplot(aes(x=hindfoot_length, y=weight, group=species))+
      theme_minimal() +
      geom_point(aes(color=species)) +
      ggtitle("Tamanho vs peso")
  })
  
  
  output$especies_dt <- DT::renderDataTable({
      datos_filtrados() %>% select(species) %>% unique()
  })
  
  output$peso_dt <- DT::renderDataTable({
    datos_filtrados() %>% select(weight) %>% summary() %>% as.data.frame()
  })
  
  output$sexo_dt <- DT::renderDataTable({
    datos_filtrados() %>% select(sex) %>% group_by(sex) %>% mutate(
      cantidad = n()
    ) %>%  dplyr::select(sex, cantidad) %>% unique()
  })
  
  
  sample_plot <- reactive({
    switch (input$grafica_id,
            "Resumen-estadistico-especies" = datos_filtrados() %>% dplyr::select("Parametro" = species) ,
            "Resumen-Estadistico-peso" = datos_filtrados() %>% dplyr::select("Parametro" =weight) ,
            "Resumen-Estadistico-sexo" = datos_filtrados() %>% dplyr::select("Parametro" = sex)
    )
  })
  
  output$plot_estadistica <- renderPlot({
    ggplot(sample_plot(), aes(x = Parametro)) +
      geom_bar()
  })
}

