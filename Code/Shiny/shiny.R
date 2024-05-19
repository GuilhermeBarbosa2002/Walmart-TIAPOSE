library(shiny)
library(shinythemes)
library(lubridate)
library(DT)
library(plotly)
library(shinyWidgets)

# Carrega o arquivo CSV
walmart_data <- read.csv("walmart.csv")
selected = ""

# Convertendo a coluna Date para o formato correto
walmart_data$Date <- as.Date(walmart_data$Date)

start_date <- as.Date("2012-03-02")
end_date <- as.Date("2012-11-09")
date_sequence <- seq(from = start_date, to = end_date, by = "4 weeks")

inverse_index <- 9:0

# Define a UI para a aplicação
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Análise de Vendas do Walmart"),
  tabsetPanel(id = 'opcoes',
    tabPanel("Univariado",
             sidebarLayout(
               sidebarPanel(
                 sliderTextInput("selected_dates_uni", "Selecione um intervalo de datas:",
                                 choices = date_sequence,
                                 grid = FALSE),
                 selectInput("package", "Pacote:",
                             choices = c("rminer" = "rminer", "forecast" = "forecast")),
                 uiOutput("model_selector_uni"),
                 selectInput("objetivo_uni", "Escolha o objetivo:",
                             choices = c("Uniobjetivo", "Multiobjetivo")),
                 uiOutput("objetivo_selector"),
                 selectInput("otimizacao_uni", "Modelo de Otimização:",
                             choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo","RBGA","RBGA.BIN","Tabu")),
                 uiOutput("otimizacao_selector"),
                 actionButton("predict_button_uni", "Predict")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Previsões",
                            fluidRow(
                              column(12,
                                     DTOutput("predictions_table_uni")
                              )
                            ),
                            fluidRow(
                              column(12,
                                     plotOutput("selected_plot_uni")
                              )
                            )
                   ),
                   tabPanel("Otimização",
                            fluidRow(
                              column(4, tableOutput("hired_workers_table_uni"))
                              
                            ),
                            fluidRow(
                              
                              column(4, tableOutput("product_orders_table_uni"))
                              
                            ),
                            fluidRow(
                              
                              column(4, tableOutput("sales_table_uni"))
                            ),
                            fluidRow(
                              column(12, textOutput("monthly_profit_output_uni"))
                            )
                   )
                 )
               )
             )
    ),
    tabPanel("Multivariado",
             sidebarLayout(
               sidebarPanel(
                 sliderTextInput("selected_dates_multi", "Selecione um intervalo de datas:",
                                 choices = date_sequence,
                                 grid = FALSE),
                 selectInput("variable_type", "Tipo de variaveis:",
                             choices = c("Endogenas" = "Endogenas", "Exogenas" = "Exogenas")),
                 uiOutput("type_selector"),
                 selectInput("objetivo_multi", "Escolha o objetivo:",
                             choices = c("Uniobjetivo", "Multiobjetivo")),
                 uiOutput("objetivo_selector"),
                 selectInput("otimizacao_multi", "Modelo de Otimização:",
                             choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo","RBGA","RBGA.BIN","Tabu")),
                 uiOutput("otimizacao_multi_selector"),
                 actionButton("predict_button_multi", "Predict")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Previsões",
                            fluidRow(
                              column(12,
                                     DTOutput("predictions_table_multi")
                              )
                            ),
                            fluidRow(
                              column(12,
                                     plotOutput("selected_plot_multi")
                              )
                            )
                   ),
                   tabPanel("Otimização",
                            fluidRow(
                              column(4, tableOutput("hired_workers_table_multi"))
                              
                            ),
                            fluidRow(
                              
                              column(4, tableOutput("product_orders_table_multi"))
                              
                            ),
                            fluidRow(
                              
                              column(4, tableOutput("sales_table_multi"))
                            ),
                            fluidRow(
                              column(12, textOutput("monthly_profit_output_multi"))
                            )
                   )
                 )
               )
             )
    ),
        tabPanel("Dataset",
             fluidRow(
               column(12,
                      DTOutput("data_table")
               )
             )
    ),
    tabPanel("Vendas totais por departamento",
             fluidRow(
               column(6, plotlyOutput("gauge_WSdep1")),
               column(6, plotlyOutput("gauge_WSdep2")),
               column(6, plotlyOutput("gauge_WSdep3")),
               column(6, plotlyOutput("gauge_WSdep4"))
             )
    ),
    tabPanel("Correlação de Spearman",
             fluidRow(
               column(12,
                      plotlyOutput("spearman_correlation_plot")
               )
             )
    ),
    
    
        tabPanel("Melhor Modelo",
             sidebarLayout(
               sidebarPanel(
                 sliderTextInput("selected_dates_best_model", "Selecione um intervalo de datas:",
                                 choices = date_sequence,
                                 grid = FALSE),
                 actionButton("predict_button_best_model", "Predict")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("previsão",
                            fluidRow(
                              column(12,
                                     DTOutput("predictions_table_best_model")
                              )
                            ),
                            fluidRow(
                              column(12,
                                     plotOutput("selected_plot_best_model")
                              )
                            )
                  
                 ),
                 tabPanel("otimização",
                          fluidRow(
                            column(4, tableOutput("hired_workers_table_best_model"))
                          ),
                          fluidRow(
                            column(4, tableOutput("product_orders_table_best_model"))
                          ),
                          fluidRow(
                            
                            column(4, tableOutput("sales_table_best_model"))
                          ),
                          fluidRow(
                            column(12, textOutput("monthly_profit_output_best_model"))
                          )
                          
                 )
               )
             )
             ))
  ))

    
  
  


# Define a lógica do servidor
server <- function(input, output, session) {
  output$model_selector_uni <- renderUI({
    if (input$package == "rminer") {
      selectInput("model_uni", "Modelo de Previsão:",
                  choices = c("Random Forest", "mlpe", "xgboost", "lm", "mars", "ksvm"))
    } else if (input$package == "forecast") {
      selectInput("model_uni", "Modelo de Previsão:",
                  choices = c("Holtwinters", "Arima", "NN", "ETS"))
    }
    
  })
  output$type_selector <- renderUI({
    if (input$variable_type == "Endogenas") {
      tagList(
        checkboxGroupInput("endogenous_vars", "Selecione as variáveis endógenas:",
                           choices = c("WSdep1", "WSdep2", "WSdep3","WSdep4", "IsHoliday", "Week"),
                           selected = c("WSdep1", "IsHoliday")),
        selectInput("model_multi", "Modelo de Previsão:",
                    choices = c("AUTOVAR", "ARIMAX", "MLPE"))
      )
    } else if (input$variable_type == "Exogenas") {
      selectInput("model_multi", "Modelo de Previsão:",
                  choices = c("ARIMAX"))
    }
  })
  
  
  observeEvent(input$objetivo_uni, {
    if (input$objetivo_uni == "Uniobjetivo") {
      updateSelectInput(session, "otimizacao_uni",
                        choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo", "RBGA", "RBGA.BIN", "Tabu"))
    } else if (input$objetivo_uni == "Multiobjetivo") {
      updateSelectInput(session, "otimizacao_uni",
                        choices = c("NGSA-II", "a definir"))
    }
  })
  
  observeEvent(input$objetivo_multi, {
    if (input$objetivo_multi == "Uniobjetivo") {
      updateSelectInput(session, "otimizacao_multi",
                        choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo", "RBGA", "RBGA.BIN", "Tabu"))
    } else if (input$objetivo_multi == "Multiobjetivo") {
      updateSelectInput(session, "otimizacao_multi",
                        choices = c("NGSA-II", "a definir"))
    }
  })
  
  
  predictions_uni <- reactiveVal(data.frame()) 
  predictions_multi <- reactiveVal(data.frame())  
  
  
  
  
  
  
  
  
  ############################### BEST MODEL PREVISÃO ###################################################
  
  observeEvent(input$predict_button_best_model, {
    source("Models4Shiny_2.R")
    print("BEST MODEL")
    
    
    selected_date <- as.Date(input$selected_dates_best_model)
    selected_position <- which(date_sequence == selected_date)
    selected_inverse_index <- inverse_index[selected_position]
    model <- input$model_uni
    objective <- input$objetivo_uni
    otimization_uni <- input$otimizacao_uni
    
    d1 <- walmart_data[,"WSdep1"]  
    d2 <- walmart_data[,"WSdep2"]  
    d3 <- walmart_data[,"WSdep3"]  
    d4 <- walmart_data[,"WSdep4"]  
    

    Pred1 <- Univariado_Rminer(departamento = d1, nomedepartamento = "Departamento 1", modelo = "ksvm", D = selected_inverse_index)
    Pred2 <- Univariado_Rminer(departamento = d2, nomedepartamento = "Departamento 2", modelo = "lm", D = selected_inverse_index)
    Pred3 <- Univariado_Rminer(departamento = d3, nomedepartamento = "Departamento 3", modelo = "mars", D = selected_inverse_index)
    Pred4 <- Univariado_Rminer(departamento = d4, nomedepartamento = "Departamento 4", modelo = "ksvm", D = selected_inverse_index)
    
      
    # Update the predictions reactive value
    predictions_uni(data.frame(
      Time = 1:length(Pred1),
      Department1 = Pred1,
      Department2 = Pred2,
      Department3 = Pred3,
      Department4 = Pred4
    ))
    
    DataFrame=data.frame(Pred1,Pred2,Pred3,Pred4)
    
    
    # if(objetivo_uni == "Uniobjetivo"){
    #   
    #   
    # }  
    # else if(objetivo_uni == 'Multiobjetivo'){
    #   
    #   optimization_results <- mul(df = DataFrame, algoritmo = otimization_uni)
    # }
    # 
    optimization_results <- Uniobjetivo(df = DataFrame, algoritmo = "RBGA")
    
    
    
    # Update UI with optimization results
    output$hired_workers_table_best_model <<- renderTable(optimization_results$hired_workers)
    output$product_orders_table_best_model <<- renderTable(optimization_results$product_orders)
    output$sales_table_best_model <<- renderTable(optimization_results$sales)
    output$monthly_profit_output_best_model <<- renderText({
      paste("Monthly Profit: ", round(optimization_results$monthly_profit, 2))
    })
    
    output$predictions_table_best_model<- renderDT({
      predictions_rounded <- data.frame(lapply(predictions_uni(), function(x) {
        if (is.numeric(x)) return(round(x, 2))
        return(x)
      }))
      
      datatable(predictions_rounded, 
                selection = 'single',
                options = list(
                  pageLength = 10,     
                  searching = FALSE,   
                  paging = FALSE,      
                  info = FALSE,        
                  ordering = FALSE    
                ),
                rownames = FALSE       
      )
    })
    
    output$selected_plot_best_model <- renderPlot({
      req(input$predictions_table_best_model_rows_selected)
      sel_row <- input$predictions_table_best_model_rows_selected
      if (length(sel_row) == 0) return()
      
      selected_data <- predictions_uni()[sel_row, ]
      
      barplot(as.numeric(selected_data[-1]), 
              names.arg = colnames(selected_data)[-1], 
              ylab = "Value",
              col = "darkblue")
    })
    
    
    
    
  }) 
  
  
  
  
  
  
  
 
 
  
  
  
  
  
  
  ############################### UNIVARIADO ###################################################
  
  observeEvent(input$predict_button_uni, {
    source("Models4Shiny_2.R")
    print("MULTIVARIADO")
    
    
    selected_date <- as.Date(input$selected_dates_uni)
    selected_position <- which(date_sequence == selected_date)
    selected_inverse_index <- inverse_index[selected_position]
    model <- input$model_uni
    objective <- input$objetivo_uni
    otimization_uni <- input$otimizacao_uni
    
    d1 <- walmart_data[,"WSdep1"]  
    d2 <- walmart_data[,"WSdep2"]  
    d3 <- walmart_data[,"WSdep3"]  
    d4 <- walmart_data[,"WSdep4"]  
    
    
      if (model %in% c("Arima", "Holtwinters", "NN", "ETS")) {
        print(paste("MODELO: ", model))
        Pred1 <- Univariado_Forecast(departamento = d1, nomedepartamento = "Departamento 1", modelo = model, D = selected_inverse_index)
        Pred2 <- Univariado_Forecast(departamento = d2, nomedepartamento = "Departamento 2", modelo = model, D = selected_inverse_index)
        Pred3 <- Univariado_Forecast(departamento = d3, nomedepartamento = "Departamento 3", modelo = model, D = selected_inverse_index)
        Pred4 <- Univariado_Forecast(departamento = d4, nomedepartamento = "Departamento 4", modelo = model, D = selected_inverse_index)
        
      }
      
      if (model %in% c("Random Forest", "mlpe", "xgboost", "lm", "mars", "ksvm")) {
        Pred1 <- Univariado_Rminer(departamento = d1, nomedepartamento = "Departamento 1", modelo = model, D = selected_inverse_index)
        Pred2 <- Univariado_Rminer(departamento = d2, nomedepartamento = "Departamento 2", modelo = model, D = selected_inverse_index)
        Pred3 <- Univariado_Rminer(departamento = d3, nomedepartamento = "Departamento 3", modelo = model, D = selected_inverse_index)
        Pred4 <- Univariado_Rminer(departamento = d4, nomedepartamento = "Departamento 4", modelo = model, D = selected_inverse_index)
      }
      
    
      # Update the predictions reactive value
      predictions_uni(data.frame(
        Time = 1:length(Pred1),
        Department1 = Pred1,
        Department2 = Pred2,
        Department3 = Pred3,
        Department4 = Pred4
      ))
      
      DataFrame=data.frame(Pred1,Pred2,Pred3,Pred4)
    

    # if(objetivo_uni == "Uniobjetivo"){
    #   
    #   
    # }  
    # else if(objetivo_uni == 'Multiobjetivo'){
    #   
    #   optimization_results <- mul(df = DataFrame, algoritmo = otimization_uni)
    # }
    # 
      optimization_results <- Uniobjetivo(df = DataFrame, algoritmo = otimization_uni)
  
      
    
    # Update UI with optimization results
    output$hired_workers_table_uni <<- renderTable(optimization_results$hired_workers)
    output$product_orders_table_uni <<- renderTable(optimization_results$product_orders)
    output$sales_table_uni <<- renderTable(optimization_results$sales)
    output$monthly_profit_output_uni <<- renderText({
      paste("Monthly Profit: ", round(optimization_results$monthly_profit, 2))
    })
    
    output$predictions_table_uni <- renderDT({
      predictions_rounded <- data.frame(lapply(predictions_uni(), function(x) {
        if (is.numeric(x)) return(round(x, 2))
        return(x)
      }))
      
      datatable(predictions_rounded, 
                selection = 'single',
                options = list(
                  pageLength = 10,     
                  searching = FALSE,   
                  paging = FALSE,      
                  info = FALSE,        
                  ordering = FALSE    
                ),
                rownames = FALSE       
      )
    })
  
  output$selected_plot_uni <- renderPlot({
    req(input$predictions_table_uni_rows_selected)
    sel_row <- input$predictions_table_uni_rows_selected
    if (length(sel_row) == 0) return()
    
    selected_data <- predictions_uni()[sel_row, ]
    
    barplot(as.numeric(selected_data[-1]), 
            names.arg = colnames(selected_data)[-1], 
            ylab = "Value",
            col = "darkblue")
  })
  }) 
  
  ############################### MULTIVARIADO ###################################################
  
  observeEvent(input$predict_button_multi, {
    source("Models4Shiny_2.R")
    print("MULTIVARIADO")
    
    
    selected_date <- as.Date(input$selected_dates_multi)
    selected_position <- which(date_sequence == selected_date)
    selected_inverse_index <- inverse_index[selected_position]
    model <- input$model_multi
    objective <- input$objetivo_multi
    otimization_uni <- input$otimizacao_multi
    
    d1 <- walmart_data[,"WSdep1"]  
    d2 <- walmart_data[,"WSdep2"]  
    d3 <- walmart_data[,"WSdep3"]  
    d4 <- walmart_data[,"WSdep4"]  
    
    
    if (model %in% c("ARIMAX", "AUTOVAR", "MLPE")) {
      selected_vars <- input$endogenous_vars
      
      reorder_vars <- function(vars, primary) {
        if (!primary %in% vars) {
          c(primary, vars)
        } else {
          c(primary, vars[vars != primary])
        }
      }
      
      selected_vars_d1 <- reorder_vars(selected_vars, "WSdep1")
      selected_vars_d2 <- reorder_vars(selected_vars, "WSdep2")
      selected_vars_d3 <- reorder_vars(selected_vars, "WSdep3")
      selected_vars_d4 <- reorder_vars(selected_vars, "WSdep4")
      
      
      Pred1 <- Multivariado(departamento = d1, nomedepartamento = "Departamento 1", modelo = model, D = selected_inverse_index, variaveis = selected_vars_d1)
      Pred2 <- Multivariado(departamento = d2, nomedepartamento = "Departamento 2", modelo = model, D = selected_inverse_index, variaveis = selected_vars_d2)
      Pred3 <- Multivariado(departamento = d3, nomedepartamento = "Departamento 3", modelo = model, D = selected_inverse_index, variaveis = selected_vars_d3)
      Pred4 <- Multivariado(departamento = d4, nomedepartamento = "Departamento 4", modelo = model, D = selected_inverse_index, variaveis = selected_vars_d4)
    }
    
    # if (model %in% c("ARIMAX")) {
    #   Pred1 <- Multi_Exogen(departamento = d1, nomedepartamento = "Departamento 1", modelo = model, D = selected_inverse_index)
    #   Pred2 <- Multi_Exogen(departamento = d2, nomedepartamento = "Departamento 2", modelo = model, D = selected_inverse_index)
    #   Pred3 <- Multi_Exogen(departamento = d3, nomedepartamento = "Departamento 3", modelo = model, D = selected_inverse_index)
    #   Pred4 <- Multi_Exogen(departamento = d4, nomedepartamento = "Departamento 4", modelo = model, D = selected_inverse_index)
    # }
    
    # Update the predictions reactive value
    predictions_multi(data.frame(
      Time = 1:length(Pred1),
      Department1 = Pred1,
      Department2 = Pred2,
      Department3 = Pred3,
      Department4 = Pred4
    ))
    
    DataFrame_multi=data.frame(Pred1,Pred2,Pred3,Pred4)
    
    
    # if(objetivo_uni == "Uniobjetivo"){
    #   
    #   
    # }  
    # else if(objetivo_uni == 'Multiobjetivo'){
    #   
    #   optimization_results <- mul(df = DataFrame, algoritmo = otimization_uni)
    # }
    # 
    optimization_results_multi <- Uniobjetivo(df = DataFrame_multi, algoritmo = otimization_uni)
    
    
    # Update UI with optimization results
    output$hired_workers_table_multi <<- renderTable(optimization_results_multi$hired_workers)
    output$product_orders_table_multi <<- renderTable(optimization_results_multi$product_orders)
    output$sales_table_multi <<- renderTable(optimization_results_multi$sales)
    output$monthly_profit_output_multi <<- renderText({
      paste("Monthly Profit: ", round(optimization_results_multi$monthly_profit, 2))
    })
    
    output$predictions_table_multi <- renderDT({
      predictions_rounded_multi <- data.frame(lapply(predictions_multi(), function(x) {
        if (is.numeric(x)) return(round(x, 2))
        return(x)
      }))
      
      datatable(predictions_rounded_multi, 
                selection = 'single',
                options = list(
                  pageLength = 10,     
                  searching = FALSE,   
                  paging = FALSE,      
                  info = FALSE,        
                  ordering = FALSE    
                ),
                rownames = FALSE       
      )
    })
    
    output$selected_plot_multi <- renderPlot({
      req(input$predictions_table_multi_rows_selected)
      sel_row <- input$predictions_table_multi_rows_selected
      if (length(sel_row) == 0) return()
      
      selected_data <- predictions_multi()[sel_row, ]
      
      barplot(as.numeric(selected_data[-1]), 
              names.arg = colnames(selected_data)[-1], 
              ylab = "Value",
              col = "darkblue")
    })
    
  })    
  
  # Função para calcular a correlação de Spearman e renderizar o gráfico
  output$spearman_correlation_plot <- renderPlotly({
    spearman_correlation <- cor(walmart_data[, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")], method = "spearman")
    plot_ly(x = colnames(spearman_correlation), y = colnames(spearman_correlation), z = as.matrix(spearman_correlation), type = "heatmap") %>%
      layout(title = "Correlação de Spearman entre departamentos")
  })
  
  
  
  output$data_table <- renderDT({
    datatable(walmart_data, options = list(lengthMenu = c(5, 10, 15), pageLength = 5))
  })
  
  mean_values <- reactive({
    mean_WSdep1 <- mean(walmart_data$WSdep1, na.rm = TRUE)
    mean_WSdep2 <- mean(walmart_data$WSdep2, na.rm = TRUE)
    mean_WSdep3 <- mean(walmart_data$WSdep3, na.rm = TRUE)
    mean_WSdep4 <- mean(walmart_data$WSdep4, na.rm = TRUE)
    
    data.frame(Department = c("WSdep1", "WSdep2", "WSdep3", "WSdep4"),
               Mean = c(mean_WSdep1, mean_WSdep2, mean_WSdep3, mean_WSdep4))
  })
  
  output$gauge_WSdep1 <- renderPlotly({
    render_gauge_plot("WSdep1", mean_values())
  })
  
  output$gauge_WSdep2 <- renderPlotly({
    render_gauge_plot("WSdep2", mean_values())
  })
  
  output$gauge_WSdep3 <- renderPlotly({
    render_gauge_plot("WSdep3", mean_values())
  })
  
  output$gauge_WSdep4 <- renderPlotly({
    render_gauge_plot("WSdep4", mean_values())
  })


  render_gauge_plot <- function(department, mean_values) {
    mean_value <- mean_values[mean_values$Department == department, "Mean"]
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = mean_value,
      title = list(text = department),
      gauge = list(
        axis = list(range = list(NULL, max(mean_values$Mean))),
        bar = list(color = "darkblue"),
        bordercolor = "gray",
        bgcolor = "white",
        steps = list(
          list(range = c(0, max(mean_values$Mean) / 3), color = "lightgray"),
          list(range = c(max(mean_values$Mean) / 3, 2 * max(mean_values$Mean) / 3), color = "gray"),
          list(range = c(2 * max(mean_values$Mean) / 3, max(mean_values$Mean)), color = "darkgray")
        )
      )
    )
  }
}

# Cria a aplicação Shiny
shinyApp(ui = ui, server = server)