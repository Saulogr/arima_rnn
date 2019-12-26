
shinyServer(function(input, output) {
    
    # Selecionado o banco de dados de forma reativa
    datasetInput <- reactive({
        arquivo = input$ts
        if (is.null(arquivo)){
            return(NULL) 
        } else {
            read.csv(input$ts$datapath, header = input$header,
                     sep = input$delim)
        }
    })
    
    # Selecionado a série temporal que será usada de forma dinâmica
    serieTemporal = reactive({
        if (is.null(datasetInput())){
            return(NULL) 
        } else {
            # Extraindo o dia inicial
            dia.in =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                       "\\3", input$intervalo[1]))
            # Extraindo o mes inicial
            mes.in =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                       "\\2", input$intervalo[1]))
            # Extraindo o ano inicial
            ano.in =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                          "\\1", input$intervalo[1]))
            # Extraindo o dia final
            dia.out = as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                          "\\3", input$intervalo[2]))
            # Extraindo o mes inicial
            mes.out =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                          "\\2", input$intervalo[2]))
            # Extraindo o ano final
            ano.out =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                          "\\1", input$intervalo[2]))
            ts(select(datasetInput(), input$SelectedColumn), 
               start = c(ano.in, mes.in, dia.in),
               end = c(ano.out, mes.out, dia.out),
               frequency = input$period)
        }
    })
    
    # Resumindo a série temporal
    output$resumo.ts = renderTable({
      Month <-  factor(cycle(serieTemporal()),
                       levels = 1:12, labels = month.abb)
      data.frame(tapply(serieTemporal(),
                        list(year = floor(time(serieTemporal())),
                             month = Month), c))
      
    })
    # Ajustando o ARIMA para a série
    arima.ts = reactive({
        auto.arima(serieTemporal())
    })
    
    # Gerando a UI de forma dinâmica
    output$ColumnSelector <- renderUI({
        selectInput("SelectedColumn",
                    "Em qual coluna está a série", 
                    choices = colnames(datasetInput()))
    })
    
    # Plotando o o gráfico da série temporal
    output$plotts = renderPlot({
        
        if (is.null(datasetInput())){
            return(NULL) 
        } else {
          # Extraindo o ano inicial
          
          ggplot(serieTemporal(), aes(x, y))+
            geom_line(color = "#E7B800", size = 1)+
            theme_minimal()+
            theme(panel.border = element_rect(fill = NA),
                  axis.title = element_text(face = "bold",
                                           size = 12))+
            labs(x = "Período",
                 y = "Total")
              
            
        }
            
    })
  
    # Plotando um summary da série temporal
    output$arima.ts = renderPrint({
        
        if (is.null(datasetInput())){
            return(NULL) 
        } else {
            arima.ts()
            }
    })

})
