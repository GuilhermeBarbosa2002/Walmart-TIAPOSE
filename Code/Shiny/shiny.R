suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(shinythemes)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(DT)))
suppressMessages(suppressWarnings(library(plotly)))
suppressMessages(suppressWarnings(library(shinyWidgets)))
suppressMessages(suppressWarnings(source("Functions_Otimization.R")))

# Carrega o arquivo CSV
walmart_data <- read.csv("walmart.csv")
selected = ""

# Convertendo a coluna Date para o formato correto
walmart_data$Date <- as.Date(walmart_data$Date)

start_date <- as.Date("2012-03-01")
end_date <- as.Date("2012-11-07")
date_sequence <- seq(from = start_date, to = end_date, by = "4 weeks")

inverse_index <- 9:0
optimization_results <- list(hired_workers  = matrix(rep(0, 12), ncol = 4, nrow = 3),
                             product_orders = matrix(rep(0, 16), ncol = 4, nrow = 4),
                             sales          = matrix(rep(0, 16), ncol = 4, nrow = 4),
                             monthly_profit = 0,
                             convergence_curve = NA,
                             nsga_results = list(pareto.optimal = c(TRUE, TRUE),
                                                 value = matrix(c(0,0,0,0), nrow = 2, ncol = 2),
                                                 par = matrix(rep(0, 56), ncol = 28)))
DataFrame <- data.frame(matrix(rep(0,16), nrow = 4, ncol = 4))

optimization_results_multi <- list(hired_workers  = matrix(rep(0, 12), ncol = 4, nrow = 3),
                                   product_orders = matrix(rep(0, 16), ncol = 4, nrow = 4),
                                   sales          = matrix(rep(0, 16), ncol = 4, nrow = 4),
                                   monthly_profit = 0,
                                   convergence_curve = NA,
                                   nsga_results = list(pareto.optimal = c(TRUE, TRUE),
                                                       value = matrix(c(0,0,0,0), nrow = 2, ncol = 2),
                                                       par = matrix(rep(0, 56), ncol = 28)))
DataFrame_multi <- data.frame(matrix(rep(0,16), nrow = 4, ncol = 4))

# Define a UI para a aplicação
# Define a UI para a aplicação
# Define a UI para a aplicação
# Define a UI para a aplicação
ui <- navbarPage(
  title = "Análise de Vendas do Walmart",
  theme = shinytheme("flatly"),
  
  # Tab "Melhor Modelo"
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
                 tabPanel("Previsões",
                          fluidRow(
                            column(12,
                                   DTOutput("predictions_table_best_model")
                            )
                          ),
                          fluidRow(
                            column(12,
                                   plotOutput("selected_plot_best_model")
                            )
                          ),
                          fluidRow(
                            column(12, div(style = "text-align: center;", textOutput("rmse_best_model"))),
                            column(12, div(style = "text-align: center;", textOutput("nmae_best_model"))),
                            column(12, div(style = "text-align: center;", textOutput("r2_best_model")))
                          )
                 ),
                 tabPanel("Otimização",
                          fluidRow(
                            column(6, 
                                   div(style = "text-align: center;",
                                       h4("Trabalhadores a Contratar"),
                                       tableOutput("hired_workers_table_best")
                                   ),
                                   column(6,
                                          div(style = "text-align: center;",
                                              textOutput("total_number_workers_output_best"),
                                              textOutput("total_cost_workers_output_best")
                                          )
                                   )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       h4("Stock"),
                                       tableOutput("stock_best")
                                   ),
                                   column(6,
                                          div(style = "text-align: center;",
                                              textOutput("total_cost_stock_output_best")
                                          )
                                   )
                            )
                          ),
                          fluidRow(
                            column(6, 
                                   div(style = "text-align: center;",
                                       h4("Encomendas a Realizar"),
                                       tableOutput("product_orders_table_best")
                                   ),
                                   column(6,
                                          div(style = "text-align: center;",
                                              textOutput("total_number_orders_output_best"),
                                              textOutput("total_cost_orders_output_best")
                                          )
                                   )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       h4("Vendas"),
                                       tableOutput("sales_best")
                                   )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       textOutput("total_sales_output_best"),
                                   ),
                                   br(), br(),br(),
                            )
                          ),
                          fluidRow(
                            column(12, align = "center",
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("monthly_profit_output_best")
                                   ),
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("monthly_effort_output_best")
                                   ),
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("total_cost_output_best")
                                   )
                            )
                          )
                 )
               )
             )
           )
  ),
  
  # Tab "Univariado"
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
                           choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo","RBGA","RBGA.BIN","Tabu","NSGA II")),
               uiOutput("otimizacao_selector"),
               actionButton("predict_button_uni", "Predict")
             ),
             mainPanel(
               tabsetPanel(id = "uni_tabs",
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
                          ),
                          fluidRow(
                            column(12, div(style = "text-align: center;", textOutput("rmse_uni"))),
                            column(12, div(style = "text-align: center;", textOutput("nmae_uni"))),
                            column(12, div(style = "text-align: center;", textOutput("r2_uni")))
                          )
                 ),
                 tabPanel("Otimização",
                          fluidRow(
                            conditionalPanel(
                              condition = "input.otimizacao_uni == 'NGSA-II'",
                              column(6,
                                   numericInput("pareto_numeric",
                                                label=paste("Pareto front point"), 
                                                min=1,
                                                max=1,
                                                step=1,
                                                value=1
                                   )
                              )
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.otimizacao_uni == 'NGSA-II'",
                              column(12,
                                   plotOutput("pareto_curve")
                              )
                            )
                          ),
                          fluidRow(
                            column(6, 
                                   div(style = "text-align: center;",
                                       h4("Trabalhadores a Contratar"),
                                       tableOutput("hired_workers_table_uni")
                                   ),
                              column(6,
                                    div(style = "text-align: center;",
                                        textOutput("total_number_workers_output_uni"),
                                        textOutput("total_cost_workers_output_uni")
                                    )
                              
                              )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       h4("Stock"),
                                       tableOutput("stock_uni")
                                   ),
                                 column(6,
                                        div(style = "text-align: center;",
                                            textOutput("total_cost_stock_output_uni")
                                        )
                                 )
                            )
                          ),
                          fluidRow(
                            column(6, 
                                   div(style = "text-align: center;",
                                       h4("Encomendas a Realizar"),
                                       tableOutput("product_orders_table_uni")
                                   ),
                                   column(6,
                                          div(style = "text-align: center;",
                                              textOutput("total_number_orders_output_uni"),
                                              textOutput("total_cost_orders_output_uni")
                                          )
                                   )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       h4("Vendas"),
                                       tableOutput("sales_uni")
                                   )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       textOutput("total_sales_output_uni"),
                                   ),
                                   br(), br(),br(),
                            )
                          ),
                          fluidRow(
                            column(12, align = "center",
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("monthly_profit_output_uni")
                                   ),
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("monthly_effort_output_uni")
                                   ),
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("total_cost_output_uni")
                                   )
                            )
                          )
                 )
               )
             )
           )
  ),
  
  # Tab "Multivariado"
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
                           choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo","RBGA","RBGA.BIN","Tabu","NSGA II")),
               uiOutput("otimizacao_multi_selector"),
               actionButton("predict_button_multi", "Predict")
             ),
             mainPanel(
               tabsetPanel(id = "multi_tabs",
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
                          ),
                          fluidRow(
                            column(12, div(style = "text-align: center;", textOutput("rmse_multi"))),
                            column(12, div(style = "text-align: center;", textOutput("nmae_multi"))),
                            column(12, div(style = "text-align: center;", textOutput("r2_multi")))
                          )
                 ),
                 tabPanel("Otimização",
                          fluidRow(
                            conditionalPanel(
                              condition = "input.otimizacao_multi == 'NGSA-II'",
                              column(6,
                                     numericInput("pareto_numeric_multi",
                                                  label=paste("Pareto front point"), 
                                                  min=1,
                                                  max=1,
                                                  step=1,
                                                  value=1
                                     )
                              )
                            )
                          ),
                          fluidRow(
                            conditionalPanel(
                              condition = "input.otimizacao_multi == 'NGSA-II'",
                              column(12,
                                     plotOutput("pareto_curve_multi")
                              )
                            )
                          ),
                          fluidRow(
                            column(6, 
                                   div(style = "text-align: center;",
                                       h4("Trabalhadores a Contratar"),
                                       tableOutput("hired_workers_table_multi")
                                   ),
                                   column(6,
                                          div(style = "text-align: center;",
                                              textOutput("total_number_workers_output_multi"),
                                              textOutput("total_cost_workers_output_multi")
                                          )
                                   )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       h4("Stock"),
                                       tableOutput("stock_multi")
                                   ),
                                   column(6,
                                          div(style = "text-align: center;",
                                              textOutput("total_cost_stock_output_multi")
                                          )
                                   )
                            )
                          ),
                          fluidRow(
                            column(6, 
                                   div(style = "text-align: center;",
                                       h4("Encomendas a Realizar"),
                                       tableOutput("product_orders_table_multi")
                                   ),
                                   column(6,
                                          div(style = "text-align: center;",
                                              textOutput("total_number_orders_output_multi"),
                                              textOutput("total_cost_orders_output_multi")
                                          )
                                   )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       h4("Vendas"),
                                       tableOutput("sales_multi")
                                   )
                            ),
                            column(6,
                                   div(style = "text-align: center;",
                                       textOutput("total_sales_output_multi"),
                                   ),
                                   br(), br(),br(),
                            )
                          ),
                          fluidRow(
                            column(12, align = "center",
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("monthly_profit_output_multi")
                                   ),
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("monthly_effort_output_multi")
                                   ),
                                   div(style = "margin-bottom: 20px;", 
                                       textOutput("total_cost_output_multi")
                                   )
                            )
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
  )
)





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
                        choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo", "RBGA", "RBGA.BIN", "Tabu","NGSA-II"))
    }
  })
  
  observeEvent(input$objetivo_multi, {
    if (input$objetivo_multi == "Uniobjetivo") {
      updateSelectInput(session, "otimizacao_multi",
                        choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo", "RBGA", "RBGA.BIN", "Tabu"))
    } else if (input$objetivo_multi == "Multiobjetivo") {
      updateSelectInput(session, "otimizacao_multi",
                        choices = c("Hill Climbing", "Simulated Annealing", "Montecarlo", "RBGA", "RBGA.BIN", "Tabu","NGSA-II"))
    }
  })
  
  observeEvent(input$objetivo_uni, {
      removeTab(inputId = "uni_tabs", target = "convergence_curve_tab")
      
      # Add a new tab with the updated title
      if (input$objetivo_uni != "Multiobjetivo") {
        appendTab(inputId = "uni_tabs",
                  tabPanel("Curva de Convergência", value = "convergence_curve_tab",
                           fluidRow(
                             column(
                               12,
                               plotOutput("convergence_curve")
                             )
                           )
                  ))
      }
  })
  
  observeEvent(input$objetivo_multi, {
    removeTab(inputId = "multi_tabs", target = "convergence_curve_tab_multi")
    
    # Add a new tab with the updated title
    if (input$objetivo_multi != "Multiobjetivo") {
      appendTab(inputId = "multi_tabs",
                tabPanel("Curva de Convergência", value = "convergence_curve_tab_multi",
                         fluidRow(
                           column(
                             12,
                             plotOutput("convergence_curve_multi")
                           )
                         )
                ))
    }
  })
  
  observeEvent(input$pareto_numeric, {
    output$pareto_curve <- renderPlot({
      if(input$otimizacao_uni == "NGSA-II"){
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 2)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        
        for(i in 1:length(I)){
          if(i == 1)  plot(Pareto, xlim = c(min(Pareto[,1]), max(Pareto[,1]) * 1.1), ylim = c(0, max(Pareto[,2]) * 1.1),
                           xlab = "f1", ylab = "f2", main = "Curva de Pareto")
        }
        lines(Pareto)
        points(Pareto[input$pareto_numeric, 1], Pareto[input$pareto_numeric, 2], col = "red", pch = 19)
      }
    })
    
    output$hired_workers_table_uni <- renderTable({
      if(input$otimizacao_uni != "NGSA-II"){
        hired_workers <- round(optimization_results$hired_workers)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers <- matrix(x[1:12], ncol = 4, nrow = 3)
      }
    
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      hired_workers <- as.data.frame(hired_workers)
      
      # Defina os nomes das colunas
      colnames(hired_workers) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(hired_workers) <- c("Junior", "Normal", "Senior")
      
      # Calcula o somatório de cada coluna
      total <- colSums(hired_workers)
      
      # Adiciona a linha "Total" ao data frame
      hired_workers <- rbind(hired_workers, Total = total)
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      hired_workers
    }, digits = 0, rownames = TRUE, caption = "Tabela de Trabalhadores a Contratar")
    
    output$product_orders_table_uni <- renderTable({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
      }
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      product_orders <- as.data.frame(product_orders)
      
      # Defina os nomes das colunas
      colnames(product_orders) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(product_orders) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      # Calcula o somatório de cada coluna
      total <- colSums(product_orders)
      
      # Adiciona a linha "Total" ao data frame
      product_orders <- rbind(product_orders, Total = total)
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      product_orders
    }, digits = 0, rownames = TRUE, caption = "Tabela de Produtos a Encomendar")
    
    output$stock_uni <- renderTable({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
        
      }
      stock = calculate_stock(product_orders, sales)
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      stock <- as.data.frame(stock)
      
      # Defina os nomes das colunas
      colnames(stock) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(stock) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      stock
    }, digits = 0, rownames = TRUE, caption = "Tabela de Stock")
    
    output$sales_uni <- renderTable({
      if(input$otimizacao_uni != "NGSA-II"){
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
      }
      sales <- as.data.frame(sales)
      
      # Defina os nomes das colunas
      colnames(sales) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(sales) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Calcula o somatório de cada coluna
      total <- colSums(sales)
      
      # Adiciona a linha "Total" ao data frame
      sales <- rbind(sales, Total = total)
      
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      sales
    }, digits = 0, rownames = TRUE, caption = "Tabela de Vendas Atuais")
    
    #output$sales_table_uni <<- renderTable(optimization_results$sales)
    
    output$monthly_profit_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Profit Mensal: ", round(optimization_results$monthly_profit))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
        monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
        paste("Profit Mensal:", round(monthly_profit))
      }
    })
    
    output$total_number_workers_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Número Trabalhadores: ", round(total_number_of_workers(round(optimization_results$hired_workers))))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        paste("Número Trabalhadores:", round(total_number_of_workers(round(hired_workers))))
      }
    })
    
    output$total_number_orders_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Número Encomendas: ", round(total_number_of_orders(round(optimization_results$product_orders))))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        paste("Número Encomendas: ", round(total_number_of_orders(round(product_orders))))
      }
    })
    
    output$total_cost_workers_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Custo Trabalhadores: ", round(total_cost_workers(round(optimization_results$hired_workers))))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        paste("Custo Trabalhadores: ", round(total_cost_workers(round(hired_workers))))
      }
    })
    
    output$total_cost_orders_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Custo Encomendas: ", round(total_cost_orders(round(optimization_results$product_orders))))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        paste("Custo Encomendas: ", round(total_cost_orders(round(product_orders))))
      }
    })
    
    output$total_cost_stock_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
      }
      stock <- calculate_stock_in_usd(product_orders, sales)
      paste("Custo Stock: ", round(stock))
    })
    
    output$total_cost_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
        sales <- round(optimization_results$sales)
        hired_workers <- round(optimization_results$hired_workers)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
      }
      total = total_costs(hired_workers,product_orders, sales)
      paste("Custo Total: ", round(total))
    })
    
    output$monthly_effort_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
        hired_workers <- round(optimization_results$hired_workers)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
      }
      total_number_of_workers = total_number_of_workers(hired_workers)
      total_number_of_orders = total_number_of_orders(product_orders)
      total = total_number_of_workers + total_number_of_orders
      paste("Effort Mensal: ", round(total))
    })
    
    output$total_sales_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
      }
      sales = sales_in_usd(sales)
      paste("Vendas Totais: ", round(sales))
    })
  })
  
  observeEvent(input$pareto_numeric_multi, {
    output$pareto_curve_multi <- renderPlot({
      if(input$otimizacao_multi == "NGSA-II"){
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 2)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        
        for(i in 1:length(I)){
          if(i == 1)  plot(Pareto, xlim = c(min(Pareto[,1]), max(Pareto[,1]) * 1.1), ylim = c(0, max(Pareto[,2]) * 1.1),
                           xlab = "f1", ylab = "f2", main = "Curva de Pareto")
        }
        lines(Pareto)
        points(Pareto[input$pareto_numeric_multi, 1], Pareto[input$pareto_numeric_multi, 2], col = "red", pch = 19)
      }
    })
    
    output$hired_workers_table_multi <- renderTable({
      if(input$otimizacao_multi != "NGSA-II"){
        hired_workers <- round(optimization_results_multi$hired_workers)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers <- matrix(x[1:12], ncol = 4, nrow = 3)
      }
      
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      hired_workers <- as.data.frame(hired_workers)
      
      # Defina os nomes das colunas
      colnames(hired_workers) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(hired_workers) <- c("Junior", "Normal", "Senior")
      
      # Calcula o somatório de cada coluna
      total <- colSums(hired_workers)
      
      # Adiciona a linha "Total" ao data frame
      hired_workers <- rbind(hired_workers, Total = total)
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      hired_workers
    }, digits = 0, rownames = TRUE, caption = "Tabela de Trabalhadores a Contratar")
    
    output$product_orders_table_multi <- renderTable({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
      }
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      product_orders <- as.data.frame(product_orders)
      
      # Defina os nomes das colunas
      colnames(product_orders) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(product_orders) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      # Calcula o somatório de cada coluna
      total <- colSums(product_orders)
      
      # Adiciona a linha "Total" ao data frame
      product_orders <- rbind(product_orders, Total = total)
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      product_orders
    }, digits = 0, rownames = TRUE, caption = "Tabela de Produtos a Encomendar")
    
    output$stock_multi <- renderTable({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
        sales <- round(optimization_results_multi$sales)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
        
      }
      stock = calculate_stock(product_orders, sales)
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      stock <- as.data.frame(stock)
      
      # Defina os nomes das colunas
      colnames(stock) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(stock) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      stock
    }, digits = 0, rownames = TRUE, caption = "Tabela de Stock")
    
    output$sales_multi <- renderTable({
      if(input$otimizacao_multi != "NGSA-II"){
        sales <- round(optimization_results_multi$sales)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
      }
      sales <- as.data.frame(sales)
      
      # Defina os nomes das colunas
      colnames(sales) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(sales) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Calcula o somatório de cada coluna
      total <- colSums(sales)
      
      # Adiciona a linha "Total" ao data frame
      sales <- rbind(sales, Total = total)
      
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      sales
    }, digits = 0, rownames = TRUE, caption = "Tabela de Vendas Atuais")
    
    #output$sales_table_uni <<- renderTable(optimization_results$sales)
    
    output$monthly_profit_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Profit Mensal: ", round(optimization_results_multi$monthly_profit))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
        monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
        paste("Profit Mensal:", round(monthly_profit))
      }
    })
    
    output$total_number_workers_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Número Trabalhadores: ", round(total_number_of_workers(round(optimization_results_multi$hired_workers))))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        paste("Número Trabalhadores:", round(total_number_of_workers(round(hired_workers))))
      }
    })
    
    output$total_number_orders_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Número Encomendas: ", round(total_number_of_orders(round(optimization_results_multi$product_orders))))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        paste("Número Encomendas: ", round(total_number_of_orders(round(product_orders))))
      }
    })
    
    output$total_cost_workers_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Custo Trabalhadores: ", round(total_cost_workers(round(optimization_results_multi$hired_workers))))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        paste("Custo Trabalhadores: ", round(total_cost_workers(round(hired_workers))))
      }
    })
    
    output$total_cost_orders_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Custo Encomendas: ", round(total_cost_orders(round(optimization_results_multi$product_orders))))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        paste("Custo Encomendas: ", round(total_cost_orders(round(product_orders))))
      }
    })
    
    output$total_cost_stock_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
      }
      stock <- calculate_stock_in_usd(product_orders, sales)
      paste("Custo Stock: ", round(stock))
    })
    
    output$total_cost_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
        sales <- round(optimization_results_multi$sales)
        hired_workers <- round(optimization_results_multi$hired_workers)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
      }
      total = total_costs(hired_workers,product_orders, sales)
      paste("Custo Total: ", round(total))
    })
    
    output$monthly_effort_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
        hired_workers <- round(optimization_results_multi$hired_workers)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
      }
      total_number_of_workers = total_number_of_workers(hired_workers)
      total_number_of_orders = total_number_of_orders(product_orders)
      total = total_number_of_workers + total_number_of_orders
      paste("Effort Mensal: ", round(total))
    })
    
    output$total_sales_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        sales <- round(optimization_results_multi$sales)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[input$pareto_numeric_multi, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
      }
      sales = sales_in_usd(sales)
      paste("Vendas Totais: ", round(sales))
    })
  })
  
  predictions_uni <- reactiveVal(data.frame()) 
  predictions_multi <- reactiveVal(data.frame())  
  predictions_best <- reactiveVal(data.frame()) 
  
  
  ############################### BEST MODEL PREVISÃO ###################################################
  
  observeEvent(input$predict_button_best_model, {
    source("Models4Shiny_2.R")
    print("BEST MODEL")
    
    
    selected_date <- as.Date(input$selected_dates_best_model)
    selected_position <- which(date_sequence == selected_date)
    selected_inverse_index <- inverse_index[selected_position]
    
    
    d1 <- walmart_data[,"WSdep1"]  
    d2 <- walmart_data[,"WSdep2"]  
    d3 <- walmart_data[,"WSdep3"]  
    d4 <- walmart_data[,"WSdep4"]  
    
    df <- read.csv("walmart.csv")
    result_df = data.frame()
    
    get_real_data <- function(selected_inverse_index) {
      if (selected_inverse_index == 0) {
        return(NULL)
      }
      
      start_row <- 143 - ( selected_inverse_index - 1) * 4 + 1
      end_row <- start_row + 3
      
      result_df <<- df[start_row:end_row, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]
      colnames(result_df)[1]<<-"Department1"
      colnames(result_df)[2]<<-"Department2"
      colnames(result_df)[3]<<-"Department3"
      colnames(result_df)[4]<<-"Department4"
      
      result_df[is.na(result_df)] <<- 0
      print(result_df)
    }
    
    get_real_data(selected_inverse_index)
    
    
    Pred1 <- Univariado_Rminer(departamento = d1, nomedepartamento = "Departamento 1", modelo = "lm", D = selected_inverse_index)
    Pred2 <- Univariado_Rminer(departamento = d2, nomedepartamento = "Departamento 2", modelo = "lm", D = selected_inverse_index)
    Pred3 <- Univariado_Rminer(departamento = d3, nomedepartamento = "Departamento 3", modelo = "mars", D = selected_inverse_index)
    Pred4 <- Univariado_Rminer(departamento = d4, nomedepartamento = "Departamento 4", modelo = "lm", D = selected_inverse_index)
    
    
    # Update the predictions reactive value
    predictions_best <- data.frame(
      Department = c(1,2,3,4),
      Week1 = round(c(Pred1[1], Pred2[1], Pred3[1], Pred4[1])),
      Week2 = round(c(Pred1[2], Pred2[2], Pred3[2], Pred4[2])),
      Week3 = round(c(Pred1[3], Pred2[3], Pred3[3], Pred4[3])),
      Week4 = round(c(Pred1[4], Pred2[4], Pred3[4], Pred4[4]))
    )
    
    
    DataFrame <<- data.frame(Pred1,Pred2,Pred3,Pred4)
    
   
    optimization_results_best <- Uniobjetivo(df = DataFrame, algoritmo = "RBGA", func="eval_min")
    
    # Update UI with optimization results
    output$hired_workers_table_best <- renderTable({
     
      hired_workers <- round(optimization_results_best$hired_workers)
      
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      hired_workers <- as.data.frame(hired_workers)
      
      # Defina os nomes das colunas
      colnames(hired_workers) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(hired_workers) <- c("Junior", "Normal", "Senior")
      
      # Calcula o somatório de cada coluna
      total <- colSums(hired_workers)
      
      # Adiciona a linha "Total" ao data frame
      hired_workers <- rbind(hired_workers, Total = total)
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      hired_workers
    }, digits = 0, rownames = TRUE, caption = "Tabela de Trabalhadores a Contratar")
    
    output$product_orders_table_best <- renderTable({
      
      product_orders <- round(optimization_results_best$product_orders)
      
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      product_orders <- as.data.frame(product_orders)
      
      # Defina os nomes das colunas
      colnames(product_orders) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(product_orders) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      # Calcula o somatório de cada coluna
      total <- colSums(product_orders)
      
      # Adiciona a linha "Total" ao data frame
      product_orders <- rbind(product_orders, Total = total)
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      product_orders
    }, digits = 0, rownames = TRUE, caption = "Tabela de Produtos a Encomendar")
    
    
    output$stock_best <- renderTable({
      
      product_orders <- round(optimization_results_best$product_orders)
      sales <- round(optimization_results_best$sales)
      
      stock = calculate_stock(product_orders, sales)
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      stock <- as.data.frame(stock)
      
      # Defina os nomes das colunas
      colnames(stock) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(stock) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      stock
    }, digits = 0, rownames = TRUE, caption = "Tabela de Stock")
    
    output$sales_best <- renderTable({
      
      sales <- round(optimization_results_best$sales)
      
      sales <- as.data.frame(sales)
      
      # Defina os nomes das colunas
      colnames(sales) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(sales) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Calcula o somatório de cada coluna
      total <- colSums(sales)
      
      # Adiciona a linha "Total" ao data frame
      sales <- rbind(sales, Total = total)
      
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      sales
    }, digits = 0, rownames = TRUE, caption = "Tabela de Vendas Atuais")
    
    output$sales_table_best <<- renderTable(optimization_results_best$sales)
    output$monthly_profit_output_best <<- renderText({
      paste("Profit Mensal: ", round(optimization_results_best$monthly_profit))
    })
    
    output$total_number_workers_output_best <<- renderText({
      paste("Número Trabalhadores: ", round(total_number_of_workers(round(optimization_results_best$hired_workers))))
    })
    
    output$total_number_orders_output_best <<- renderText({
      paste("Número Encomendas: ", round(total_number_of_orders(round(optimization_results_best$product_orders))))
    })
    
    output$total_cost_workers_output_best <<- renderText({
      paste("Custo Trabalhadores: ", round(total_cost_workers(round(optimization_results_best$hired_workers))))
    })
    output$total_cost_orders_output_best <<- renderText({
      paste("Custo Encomendas: ", round(total_cost_orders(round(optimization_results_best$product_orders))))
    })
    output$total_cost_stock_output_best <<- renderText({
      product_orders <- round(optimization_results_best$product_orders)
      sales <- round(optimization_results_best$sales)
      stock = calculate_stock_in_usd(product_orders, sales)
      paste("Custo Stock: ", round(stock))
    })
    
    output$total_cost_output_best <<- renderText({
      product_orders <- round(optimization_results_best$product_orders)
      sales <- round(optimization_results_best$sales)
      hired_workers <- round(optimization_results_best$hired_workers)
      total = total_costs(hired_workers,product_orders, sales)
      paste("Custo Total: ", round(total))
    })
    
    output$monthly_effort_output_best <<- renderText({
      product_orders <- round(optimization_results_best$product_orders)
      hired_workers <- round(optimization_results_best$hired_workers)
      total_number_of_workers = total_number_of_workers(hired_workers)
      total_number_of_orders = total_number_of_orders(product_orders)
      total = total_number_of_workers + total_number_of_orders
      paste("Effort Mensal: ", round(total))
    })
    output$total_sales_output_best<<- renderText({
      sales <- round(optimization_results_best$sales)
      sales = sales_in_usd(sales)
      paste("Vendas Totais: ", round(sales))
    })
    
    output$predictions_table_best_model<- renderDT({
      
      
      datatable(predictions_best, 
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
      
      # Seleciona os dados da linha selecionada
      selected_data <- predictions_best[sel_row, 2:5]
      
      # Converte os dados selecionados para um vetor numérico
      selected_data <- as.numeric(selected_data)
      
      print(result_df)
      
      selected_values <- as.numeric(c(result_df[1, sel_row], result_df[2, sel_row], result_df[3, sel_row], result_df[4, sel_row]))
      
      MAE=round(mmetric(y=selected_values,x=selected_data,metric="MAE"),2)
      print(MAE)
      NMAE=round(mmetric(y=selected_values,x=selected_data,metric="NMAE"),2)
      print(NMAE)
      RMSE=round(mmetric(y=selected_values,x=selected_data,metric="RMSE"),2)
      print(RMSE)
      RRSE=round(mmetric(y=selected_values,x=selected_data,metric="RRSE"),2)
      print(RRSE)
      R2=round(mmetric(y=selected_values,x=selected_data,metric="R22"),2)
      print(R2)
      
      output$rmse_best_model <- renderText({ 
        if (any(selected_values != 0)) {
          paste("MAE: ", MAE)
        } else {
          ""
        }
      })
      
      output$nmae_best_model <- renderText({ 
        if (any(selected_values != 0)) {
          paste("NMAE: ", NMAE)
        } else {
          ""
        }
      })
      
      output$r2_best_model <- renderText({ 
        if (any(selected_values != 0)) {
          paste("R2: ", R2)
        } else {
          ""
        }
      })
      
      # Calcula os limites do gráfico
      y_min <- min(selected_values,selected_data)
      y_max <- max(selected_values,selected_data)
      
      
      if(all(selected_values)==0){
        y_min <- min(selected_data)
        y_max <- max(selected_data)}
      
      
      
      # Plota os dados selecionados
      plot(selected_data, 
           type = "o", 
           xlab = "Weeks", 
           ylab = "Sales", 
           xaxt = "n",
           col = "darkblue",
           ylim = c(y_min, y_max))
           
      if(all(selected_values)!=0){
        
        lines(as.numeric(selected_values), 
              type = "o", 
              col = "red")}
      
      
    
      # Adiciona os rótulos do eixo x
      axis(1, at = 1:4, labels = paste("Week", 1:4))
      
      if(all(selected_values)!=0){
      
      legend("topright", legend = c("Predictions", "Real Values"), col = c("darkblue", "red"), lty = 1, pch = 1)}
      else{legend("topright", legend = c("Predictions"), col = c("darkblue"), lty = 1, pch = 1)}
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
    
    df <- read.csv("walmart.csv")
    result_df = data.frame()
    
    get_real_data <- function(selected_inverse_index) {
      if (selected_inverse_index == 0) {
        return(NULL)
      }
      
      start_row <- 143 - ( selected_inverse_index - 1) * 4 + 1
      end_row <- start_row + 3
      
      result_df <<- df[start_row:end_row, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]
      colnames(result_df)[1]<<-"Department1"
      colnames(result_df)[2]<<-"Department2"
      colnames(result_df)[3]<<-"Department3"
      colnames(result_df)[4]<<-"Department4"
      
      result_df[is.na(result_df)] <<- 0
      print(result_df)
    }
    
    get_real_data(selected_inverse_index)
    
    
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
    predictions_uni = data.frame(
      Department = c(1,2,3,4),
      Week1 = round(c(Pred1[1], Pred2[1], Pred3[1], Pred4[1])),
      Week2 = round(c(Pred1[2], Pred2[2], Pred3[2], Pred4[2])),
      Week3 = round(c(Pred1[3], Pred2[3], Pred3[3], Pred4[3])),
      Week4 = round(c(Pred1[4], Pred2[4], Pred3[4], Pred4[4]))
    )
    
    DataFrame <<- data.frame(Pred1,Pred2,Pred3,Pred4)
    
    
    if (objective == "Uniobjetivo" && (otimization_uni=="Simulated Annealing" || otimization_uni=="RBGA.BIN" || otimization_uni == "RBGA")){
      eval = "eval_min"
    }
    if (objective == "Uniobjetivo" && (otimization_uni != "Simulated Annealing" && otimization_uni != "RBGA.BIN" && otimization_uni != "RBGA")){
      eval = "eval_max"
    }
    if (objective == "Multiobjetivo" && (otimization_uni=="Simulated Annealing" || otimization_uni=="RBGA.BIN" || otimization_uni == "RBGA")){
      eval = "eval_mix_min"
    }
    if (objective == "Multiobjetivo" && (otimization_uni != "Simulated Annealing" && otimization_uni != "RBGA.BIN" && otimization_uni != "RBGA")){
      eval = "eval_mix_max"
    }
    
    
    optimization_results <<- Uniobjetivo(df = DataFrame, algoritmo = otimization_uni, func=eval)
    
    
    
    # Update UI with optimization results
    
    output$pareto_curve <- renderPlot({
      if(input$otimizacao_uni == "NGSA-II"){
         G <- optimization_results$nsga_results
         I <- which(G$pareto.optimal)
         P <- matrix(NA, nrow = length(I), ncol = 2)
         for(i in 1:length(I)){
           P[i,1] <- -G$value[,1][I[i]]
           P[i,2] <- G$value[,2][I[i]]
         }
         
         st <- sort.int(P[,1], index.return = TRUE)
         Pareto <- P[st$ix, ]
         
         for(i in 1:length(I)){
           if(i == 1)  plot(Pareto, xlim = c(min(Pareto[,1]), max(Pareto[,1]) * 1.1), ylim = c(0, max(Pareto[,2]) * 1.1),
                            xlab = "f1", ylab = "f2", main = "Curva de Pareto")
         }
         lines(Pareto)
         points(Pareto[input$pareto_numeric, 1], Pareto[input$pareto_numeric, 2], col = "red", pch = 19)
         updateNumericInput(session, "pareto_numeric", label=paste("Pareto front point (1 to ",length(I),"):"), 
                            min=1,
                            max=length(I), 
                            step=1, 
                            value=1
         )
      }
    })
    
    output$hired_workers_table_uni <- renderTable({
      
      if(input$otimizacao_uni != "NGSA-II"){
        
        hired_workers <- round(optimization_results$hired_workers)
        
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers <- matrix(x[1:12], ncol = 4, nrow = 3)
        
      }
      
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      hired_workers <- as.data.frame(hired_workers)
      
      # Defina os nomes das colunas
      colnames(hired_workers) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(hired_workers) <- c("Junior", "Normal", "Senior")
      
      # Calcula o somatório de cada coluna
      total <- colSums(hired_workers)
      
      # Adiciona a linha "Total" ao data frame
      hired_workers <- rbind(hired_workers, Total = total)
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      hired_workers
    }, digits = 0, rownames = TRUE, caption = "Tabela de Trabalhadores a Contratar")
    
    output$product_orders_table_uni <- renderTable({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
      }
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      product_orders <- as.data.frame(product_orders)
      
      # Defina os nomes das colunas
      colnames(product_orders) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(product_orders) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      # Calcula o somatório de cada coluna
      total <- colSums(product_orders)
      
      # Adiciona a linha "Total" ao data frame
      product_orders <- rbind(product_orders, Total = total)
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      product_orders
    }, digits = 0, rownames = TRUE, caption = "Tabela de Produtos a Encomendar")
    
    
    output$stock_uni <- renderTable({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
        
      }
      stock = calculate_stock(product_orders, sales)
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      stock <- as.data.frame(stock)
      
      # Defina os nomes das colunas
      colnames(stock) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(stock) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      stock
    }, digits = 0, rownames = TRUE, caption = "Tabela de Stock")
    
    output$sales_uni <- renderTable({
      if(input$otimizacao_uni != "NGSA-II"){
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
      }
      sales <- as.data.frame(sales)
      
      # Defina os nomes das colunas
      colnames(sales) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(sales) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Calcula o somatório de cada coluna
      total <- colSums(sales)
      
      # Adiciona a linha "Total" ao data frame
      sales <- rbind(sales, Total = total)
      
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      sales
    }, digits = 0, rownames = TRUE, caption = "Tabela de Vendas Atuais")
    
    #output$sales_table_uni <<- renderTable(optimization_results$sales)
    
    output$monthly_profit_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Profit Mensal: ", round(optimization_results$monthly_profit))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
        monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
        paste("Profit Mensal:", round(monthly_profit))
      }
    })
    
    output$total_number_workers_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Número Trabalhadores: ", round(total_number_of_workers(round(optimization_results$hired_workers))))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        paste("Número Trabalhadores:", round(total_number_of_workers(round(hired_workers))))
      }
    })
    
    output$total_number_orders_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Número Encomendas: ", round(total_number_of_orders(round(optimization_results$product_orders))))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        paste("Número Encomendas: ", round(total_number_of_orders(round(product_orders))))
      }
    })
    
    output$total_cost_workers_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Custo Trabalhadores: ", round(total_cost_workers(round(optimization_results$hired_workers))))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        paste("Custo Trabalhadores: ", round(total_cost_workers(round(hired_workers))))
      }
    })
    
    output$total_cost_orders_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        paste("Custo Encomendas: ", round(total_cost_orders(round(optimization_results$product_orders))))
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        paste("Custo Encomendas: ", round(total_cost_orders(round(product_orders))))
      }
    })
    
    output$total_cost_stock_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
      }
      stock <- calculate_stock_in_usd(product_orders, sales)
      paste("Custo Stock: ", round(stock))
    })
    
    output$total_cost_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
        sales <- round(optimization_results$sales)
        hired_workers <- round(optimization_results$hired_workers)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
      }
      total = total_costs(hired_workers,product_orders, sales)
      paste("Custo Total: ", round(total))
    })
    
    output$monthly_effort_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        product_orders <- round(optimization_results$product_orders)
        hired_workers <- round(optimization_results$hired_workers)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
      }
      total_number_of_workers = total_number_of_workers(hired_workers)
      total_number_of_orders = total_number_of_orders(product_orders)
      total = total_number_of_workers + total_number_of_orders
      paste("Effort Mensal: ", round(total))
    })
    
    output$total_sales_output_uni <<- renderText({
      if(input$otimizacao_uni != "NGSA-II"){
        sales <- round(optimization_results$sales)
      }else{
        G <- optimization_results$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame, hired_workers, product_orders)
      }
      sales = sales_in_usd(sales)
      paste("Vendas Totais: ", round(sales))
    })
    # column(12, textOutput("total_cost_workers_output_uni")),
    # column(12, textOutput("total_cost_orders_output_uni")),
    # column(12, textOutput("total_cost_output_uni"))
    # total_cost_stock_output_uni
    
    output$predictions_table_uni <- renderDT({
      
      
      datatable(predictions_uni, 
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
    output$convergence_curve <- renderPlot({
      plot(optimization_results$convergence_curve, 
           type = "l", 
           ylab = "Values",
           xlab = "Iteractions")
    })
    
    output$selected_plot_uni <- renderPlot({
      req(input$predictions_table_uni_rows_selected)
      sel_row <- input$predictions_table_uni_rows_selected
      
      if (length(sel_row) == 0) return()
      
      # Seleciona os dados da linha selecionada
      selected_data <- predictions_uni[sel_row, 2:5]
      
      # Converte os dados selecionados para um vetor numérico
      selected_data <- as.numeric(selected_data)
      
      print(result_df)
      
      selected_values <- as.numeric(c(result_df[1, sel_row], result_df[2, sel_row], result_df[3, sel_row], result_df[4, sel_row]))
      
      MAE=round(mmetric(y=selected_values,x=selected_data,metric="MAE"),2)
      print(MAE)
      NMAE=round(mmetric(y=selected_values,x=selected_data,metric="NMAE"),2)
      print(NMAE)
      RMSE=round(mmetric(y=selected_values,x=selected_data,metric="RMSE"),2)
      print(RMSE)
      RRSE=round(mmetric(y=selected_values,x=selected_data,metric="RRSE"),2)
      print(RRSE)
      R2=round(mmetric(y=selected_values,x=selected_data,metric="R22"),2)
      print(R2)
      
      
      output$rmse_uni <- renderText({ 
        if (any(selected_values != 0)) {
          paste("MAE: ", NMAE)
        } else {
          ""
        }
      })
      
      output$nmae_uni <- renderText({ 
        if (any(selected_values != 0)) {
          paste("NMAE: ", NMAE)
        } else {
          ""
        }
      })
      
      output$r2_uni <- renderText({ 
        if (any(selected_values != 0)) {
          paste("R2: ", R2)
        } else {
          ""
        }
      })
      
      # Calcula os limites do gráfico
      y_min <- min(selected_values,selected_data)
      y_max <- max(selected_values,selected_data)
      
      if(all(selected_values)==0){
        y_min <- min(selected_data)
        y_max <- max(selected_data)}
      
      
      
      # Plota os dados selecionados
      plot(selected_data, 
           type = "o", 
           xlab = "Weeks", 
           ylab = "Sales", 
           xaxt = "n",
           col = "darkblue",
           ylim = c(y_min, y_max))
      
      if(all(selected_values)!=0){
        
        lines(as.numeric(selected_values), 
              type = "o", 
              col = "red")}
      
      
      
      # Adiciona os rótulos do eixo x
      axis(1, at = 1:4, labels = paste("Week", 1:4))
      
      if(all(selected_values)!=0){
        legend("topright", legend = c("Predictions", "Real Values"), col = c("darkblue", "red"), lty = 1, pch = 1)}
      else{legend("topright", legend = c("Predictions"), col = c("darkblue"), lty = 1, pch = 1)}
      
    })
  }) 
  
  ############################### MULTIVARIADO ###################################################
    
    
  observeEvent(input$predict_button_multi, {
    source("Models4Shiny_2.R")
    print("MULTIVARIADO")
    print(input$otimizacao_uni)
    selected_date <- as.Date(input$selected_dates_multi)
    selected_position <- which(date_sequence == selected_date)
    selected_inverse_index <- inverse_index[selected_position]
    model <- input$model_multi
    objective <- input$objetivo_multi
    otimization_multi <- input$otimizacao_multi
    
    d1 <- walmart_data[,"WSdep1"]  
    d2 <- walmart_data[,"WSdep2"]  
    d3 <- walmart_data[,"WSdep3"]  
    d4 <- walmart_data[,"WSdep4"]  
    
    df <- read.csv("walmart.csv")
    result_df = data.frame()
    
    get_real_data <- function(selected_inverse_index) {
      if (selected_inverse_index == 0) {
        return(NULL)
      }
      
      start_row <- 143 - ( selected_inverse_index - 1) * 4 + 1
      end_row <- start_row + 3
      
      result_df <<- df[start_row:end_row, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]
      colnames(result_df)[1]<<-"Department1"
      colnames(result_df)[2]<<-"Department2"
      colnames(result_df)[3]<<-"Department3"
      colnames(result_df)[4]<<-"Department4"
      
      result_df[is.na(result_df)] <<- 0
      print(result_df)
    }
    
    get_real_data(selected_inverse_index)
    
    
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
    
    
    # Update the predictions reactive value
    predictions_multi = data.frame(
      Department = c(1,2,3,4),
      Week1 = round(c(Pred1[1], Pred2[1], Pred3[1], Pred4[1])),
      Week2 = round(c(Pred1[2], Pred2[2], Pred3[2], Pred4[2])),
      Week3 = round(c(Pred1[3], Pred2[3], Pred3[3], Pred4[3])),
      Week4 = round(c(Pred1[4], Pred2[4], Pred3[4], Pred4[4]))
    )
    
    DataFrame_multi <<- data.frame(Pred1,Pred2,Pred3,Pred4)

    if (objective == "Uniobjetivo" && (otimization_multi=="Simulated Annealing" || otimization_multi=="RBGA.BIN" || otimization_multi == "RBGA")){
      eval = "eval_min"
    }
    if (objective == "Uniobjetivo" && (otimization_multi != "Simulated Annealing" && otimization_multi != "RBGA.BIN" && otimization_multi != "RBGA")){
      eval = "eval_max"
    }
    if (objective == "Multiobjetivo" && (otimization_multi=="Simulated Annealing" || otimization_multi=="RBGA.BIN" || otimization_multi == "RBGA")){
      eval = "eval_mix_min"
    }
    if (objective == "Multiobjetivo" && (otimization_multi != "Simulated Annealing" && otimization_multi != "RBGA.BIN" && otimization_multi != "RBGA")){
      eval = "eval_mix_max"
    }
    
    optimization_results_multi <<- Uniobjetivo(df = DataFrame_multi, algoritmo = otimization_multi, func= eval)
    
    # Update UI with optimization results
    # Update UI with optimization results
    
    output$pareto_curve_multi <- renderPlot({
      if(input$otimizacao_multi == "NGSA-II"){
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 2)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        
        for(i in 1:length(I)){
          if(i == 1)  plot(Pareto, xlim = c(min(Pareto[,1]), max(Pareto[,1]) * 1.1), ylim = c(0, max(Pareto[,2]) * 1.1),
                           xlab = "f1", ylab = "f2", main = "Curva de Pareto")
        }
        lines(Pareto)
        points(Pareto[input$pareto_numeric_multi, 1], Pareto[input$pareto_numeric_multi, 2], col = "red", pch = 19)
        updateNumericInput(session, "pareto_numeric_multi", label=paste("Pareto front point (1 to ",length(I),"):"), 
                           min=1,
                           max=length(I), 
                           step=1, 
                           value=1
        )
      }
    })
    
    output$hired_workers_table_multi <- renderTable({
      if(input$otimizacao_multi != "NGSA-II"){
        hired_workers <- round(optimization_results_multi$hired_workers)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers <- matrix(x[1:12], ncol = 4, nrow = 3)
      }
      
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      hired_workers <- as.data.frame(hired_workers)
      
      # Defina os nomes das colunas
      colnames(hired_workers) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(hired_workers) <- c("Junior", "Normal", "Senior")
      
      # Calcula o somatório de cada coluna
      total <- colSums(hired_workers)
      
      # Adiciona a linha "Total" ao data frame
      hired_workers <- rbind(hired_workers, Total = total)
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      hired_workers
    }, digits = 0, rownames = TRUE, caption = "Tabela de Trabalhadores a Contratar")
    
    output$product_orders_table_multi <- renderTable({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
      }
      
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      product_orders <- as.data.frame(product_orders)
      
      # Defina os nomes das colunas
      colnames(product_orders) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(product_orders) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      # Calcula o somatório de cada coluna
      total <- colSums(product_orders)
      
      # Adiciona a linha "Total" ao data frame
      product_orders <- rbind(product_orders, Total = total)
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      product_orders
    }, digits = 0, rownames = TRUE, caption = "Tabela de Produtos a Encomendar")
    
    
    output$stock_multi <- renderTable({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
        sales <- round(optimization_results_multi$sales)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
        
      }
      
      stock = calculate_stock(product_orders, sales)
      # Se optimization_results$hired_workers for uma matriz, converta-a em um data frame
      stock <- as.data.frame(stock)
      
      # Defina os nomes das colunas
      colnames(stock) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(stock) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      stock
    }, digits = 0, rownames = TRUE, caption = "Tabela de Stock")
    
    output$sales_multi <- renderTable({
      if(input$otimizacao_multi != "NGSA-II"){
        sales <- round(optimization_results_multi$sales)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
      }
      
      sales <- as.data.frame(sales)
      
      # Defina os nomes das colunas
      colnames(sales) <- c("Dep1","Dep2","Dep3","Dep4")
      
      # Defina os nomes das linhas
      rownames(sales) <- c("1º semana", "2º semana", "3º semana", "4º semana")
      
      # Calcula o somatório de cada coluna
      total <- colSums(sales)
      
      # Adiciona a linha "Total" ao data frame
      sales <- rbind(sales, Total = total)
      
      
      # Retorna a tabela com os nomes das colunas e das linhas alterados e valores arredondados
      sales
    }, digits = 0, rownames = TRUE, caption = "Tabela de Vendas Atuais")
    
    output$sales_table_multi <<- renderTable(optimization_results_multi$sales)
    output$monthly_profit_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Profit Mensal: ", round(optimization_results_multi$monthly_profit))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
        monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
        paste("Profit Mensal:", round(monthly_profit))
      }
    })
    output$total_number_workers_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Número Trabalhadores: ", round(total_number_of_workers(round(optimization_results_multi$hired_workers))))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        paste("Número Trabalhadores:", round(total_number_of_workers(round(hired_workers))))
      }
    })
    output$total_number_orders_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Número Encomendas: ", round(total_number_of_orders(round(optimization_results_multi$product_orders))))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        paste("Número Encomendas: ", round(total_number_of_orders(round(product_orders))))
      }
    })
    output$total_cost_workers_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Custo Trabalhadores: ", round(total_cost_workers(round(optimization_results_multi$hired_workers))))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        paste("Custo Trabalhadores: ", round(total_cost_workers(round(hired_workers))))
      }
    })
    output$total_cost_orders_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        paste("Custo Encomendas: ", round(total_cost_orders(round(optimization_results_multi$product_orders))))
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        paste("Custo Encomendas: ", round(total_cost_orders(round(product_orders))))
      }
    })
    
    output$total_cost_stock_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
        sales <- round(optimization_results_multi$sales)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
      }
      stock = calculate_stock_in_usd(product_orders, sales)
      paste("Custo Stock: ", round(stock))
    })
    
    output$total_cost_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
        sales <- round(optimization_results_multi$sales)
        hired_workers <- round(optimization_results_multi$hired_workers)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
      }
      total = total_costs(hired_workers,product_orders, sales)
      paste("Custo Total: ", round(total))
    })
    
    output$monthly_effort_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        product_orders <- round(optimization_results_multi$product_orders)
        hired_workers <- round(optimization_results_multi$hired_workers)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
      }
      total_number_of_workers = total_number_of_workers(hired_workers)
      total_number_of_orders = total_number_of_orders(product_orders)
      total = total_number_of_workers + total_number_of_orders
      paste("Effort Mensal: ", round(total))
    })
    
    output$total_sales_output_multi <<- renderText({
      if(input$otimizacao_multi != "NGSA-II"){
        sales <- round(optimization_results_multi$sales)
      }else{
        G <- optimization_results_multi$nsga_results
        I <- which(G$pareto.optimal)
        P <- matrix(NA, nrow = length(I), ncol = 3)
        for(i in 1:length(I)){
          P[i,1] <- -G$value[,1][I[i]]
          P[i,2] <- G$value[,2][I[i]]
          P[i,3] <- I[i]
        }
        
        st <- sort.int(P[,1], index.return = TRUE)
        Pareto <- P[st$ix, ]
        x <- ceiling(G$par[Pareto[1, 3],])
        hired_workers  <- matrix(x[1:12], ncol = 4, nrow = 3)
        product_orders <- matrix(x[13:28], ncol = 4, nrow = 4)
        sales          <- calculate_sales(DataFrame_multi, hired_workers, product_orders)
      }
      sales = sales_in_usd(sales)
      paste("Vendas Totais: ", round(sales))
    })
    
    output$predictions_table_multi <- renderDT({
      
      
      datatable(predictions_multi, 
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
    
    output$convergence_curve_multi <- renderPlot({
      plot(optimization_results_multi$convergence_curve, 
           type = "l", 
           ylab = "Values",
           xlab = "Iteractions")
    })
    
    output$selected_plot_multi <- renderPlot({
      req(input$predictions_table_multi_rows_selected)
      sel_row <- input$predictions_table_multi_rows_selected
      print(result_df)
      if (length(sel_row) == 0) return()
      
      selected_data <- predictions_multi[sel_row, 2:5]
      
      selected_data <- as.numeric(selected_data)
      
      selected_values <- as.numeric(c(result_df[1, sel_row], result_df[2, sel_row], result_df[3, sel_row], result_df[4, sel_row]))
      
      MAE=round(mmetric(y=selected_values,x=selected_data,metric="MAE"),2)
      print(MAE)
      NMAE=round(mmetric(y=selected_values,x=selected_data,metric="NMAE"),2)
      print(NMAE)
      RMSE=round(mmetric(y=selected_values,x=selected_data,metric="RMSE"),2)
      print(RMSE)
      RRSE=round(mmetric(y=selected_values,x=selected_data,metric="RRSE"),2)
      print(RRSE)
      R2=round(mmetric(y=selected_values,x=selected_data,metric="R22"),2)
      print(R2)
      
      
      output$nmae_multi <- renderText({ 
        if (all(selected_values != 0)) {
          paste("NMAE: ", NMAE)
        } else {
          ""
        }
      })
      
      output$rmse_multi <- renderText({ 
        if (all(selected_values != 0)) {
          paste("MAE: ", MAE)
        } else {
          ""
        }
      })
      
      
      
      output$r2_multi <- renderText({ 
        if (all(selected_values != 0)) {
          paste("R2: ", R2)
        } else {
          ""
        }
      })
      
      
      y_min <- min(selected_values,selected_data)
      y_max <- max(selected_values,selected_data)
      
      if(all(selected_values)==0){
        y_min <- min(selected_data)
        y_max <- max(selected_data)}
      
      # Plota os dados selecionados
      plot(selected_data, 
           type = "o", 
           xlab = "Weeks", 
           ylab = "Sales", 
           xaxt = "n",
           col = "darkblue",
           ylim = c(y_min, y_max))
      
      if(all(selected_values)!=0){
        
        lines(as.numeric(selected_values), 
              type = "o", 
              col = "red")}
      
      
      # Adiciona os rótulos do eixo x
      axis(1, at = 1:4, labels = paste("Week", 1:4))
      
      if(all(selected_values)!=0){
        legend("topright", legend = c("Predictions", "Real Values"), col = c("darkblue", "red"), lty = 1, pch = 1)}
      else{legend("topright", legend = c("Predictions"), col = c("darkblue"), lty = 1, pch = 1)}
      
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