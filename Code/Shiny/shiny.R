library(shiny)
library(shinythemes)
library(lubridate)
library(DT)
library(plotly)
library(shinyWidgets)

# Carrega o arquivo CSV
walmart_data <- read.csv("walmart.csv")

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
  tabsetPanel(
    tabPanel("Univariado",
             sidebarLayout(
               sidebarPanel(
                 sliderTextInput("selected_dates", "Selecione um intervalo de datas:",
                                 choices = date_sequence,
                                 grid = FALSE),
                 selectInput("package", "Escolha o pacote a ser executado:",
                             choices = c("rminer" = "rminer", "forecast" = "forecast")),
                 uiOutput("model_selector"),
                 actionButton("predict_button", "Predict")
               ),
               mainPanel(
                 textOutput("selected_date_text"),
                 fluidRow(
                   column(6, plotOutput("pred_plot1")),
                   column(6, plotOutput("pred_plot2"))
                 ),
                 fluidRow(
                   column(6, plotOutput("pred_plot3")),
                   column(6, plotOutput("pred_plot4"))
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
    )
  )
)


# Define a lógica do servidor
server <- function(input, output, session) {
  
  
  output$model_selector <- renderUI({
    if (input$package == "rminer") {
      selectInput("model", "Escolha o modelo:",
                  choices = c("rf","mlpe", "xgboost","lm","mars","ksvm"))
    } else if (input$package == "forecast") {
      selectInput("model", "Escolha o modelo:",
                  choices = c("HW","auto.arima","nnetar","ets"))
    }
  })
  
  
  
  # Função para previsão usando Growing_Window do arquivo Growing_Gonçalo.R
  observeEvent(input$predict_button, {
    source("Models4Shiny_2.R")
    
    selected_date <- as.Date(input$selected_dates)
    selected_position <- which(date_sequence == selected_date)
    selected_inverse_index <- inverse_index[selected_position]
    model = input$model
    
    
    d1=data[,"WSdep1"]  
    d2=data[,"WSdep2"]  
    d3=data[,"WSdep3"]  
    d4=data[,"WSdep4"]  
    
    
    print(model)
    if(model == "auto.arima"){
      Pred1 <- Univariado_Forecast(departamento=d1, nomedepartamento="Departamento 1", modelo="Arima", D=selected_inverse_index)
      Pred2 <- Univariado_Forecast(departamento=d2, nomedepartamento="Departamento 2", modelo="Arima", D=selected_inverse_index)
      Pred3 <- Univariado_Forecast(departamento=d3, nomedepartamento="Departamento 3", modelo="Arima", D=selected_inverse_index)
      Pred4 <- Univariado_Forecast(departamento=d4, nomedepartamento="Departamento 4", modelo="Arima", D=selected_inverse_index)
    }
    if(model == "HW"){
      Pred1 <- Univariado_Forecast(departamento=d1, nomedepartamento="Departamento 1", modelo="Holtwinters", D=selected_inverse_index)
      Pred2 <- Univariado_Forecast(departamento=d2, nomedepartamento="Departamento 2", modelo="Holtwinters", D=selected_inverse_index)
      Pred3 <- Univariado_Forecast(departamento=d3, nomedepartamento="Departamento 3", modelo="Holtwinters", D=selected_inverse_index)
      Pred4 <- Univariado_Forecast(departamento=d4, nomedepartamento="Departamento 4", modelo="Holtwinters", D=selected_inverse_index)
    }
    if(model == "nnetar"){
      Pred1 <- Univariado_Forecast(departamento=d1, nomedepartamento="Departamento 1", modelo="NN", D=selected_inverse_index)
      Pred2 <- Univariado_Forecast(departamento=d2, nomedepartamento="Departamento 2", modelo="NN", D=selected_inverse_index)
      Pred3 <- Univariado_Forecast(departamento=d3, nomedepartamento="Departamento 3", modelo="NN", D=selected_inverse_index)
      Pred4 <- Univariado_Forecast(departamento=d4, nomedepartamento="Departamento 4", modelo="NN", D=selected_inverse_index)
    }
    if(model == "ets"){
      Pred1 <- Univariado_Forecast(departamento=d1, nomedepartamento="Departamento 1", modelo="ETS", D=selected_inverse_index)
      Pred2 <- Univariado_Forecast(departamento=d2, nomedepartamento="Departamento 2", modelo="ETS", D=selected_inverse_index)
      Pred3 <- Univariado_Forecast(departamento=d3, nomedepartamento="Departamento 3", modelo="ETS", D=selected_inverse_index)
      Pred4 <- Univariado_Forecast(departamento=d4, nomedepartamento="Departamento 4", modelo="ETS", D=selected_inverse_index)
    }
    if(model == "rf"){
      Pred1 <- Univariado_Rminer(departamento=d1, nomedepartamento="Departamento 1", modelo="Random Forest", D=selected_inverse_index)
      Pred2 <- Univariado_Rminer(departamento=d2, nomedepartamento="Departamento 2", modelo="Random Forest", D=selected_inverse_index)
      Pred3 <- Univariado_Rminer(departamento=d3, nomedepartamento="Departamento 3", modelo="Random Forest", D=selected_inverse_index)
      Pred4 <- Univariado_Rminer(departamento=d4, nomedepartamento="Departamento 4", modelo="Random Forest", D=selected_inverse_index)
    }
    if(model == "mlpe"){
      Pred1 <- Univariado_Rminer(departamento=d1, nomedepartamento="Departamento 1", modelo="mlpe", D=selected_inverse_index)
      Pred2 <- Univariado_Rminer(departamento=d2, nomedepartamento="Departamento 2", modelo="mlpe", D=selected_inverse_index)
      Pred3 <- Univariado_Rminer(departamento=d3, nomedepartamento="Departamento 3", modelo="mlpe", D=selected_inverse_index)
      Pred4 <- Univariado_Rminer(departamento=d4, nomedepartamento="Departamento 4", modelo="mlpe", D=selected_inverse_index)
    }
    if(model == "xgboost"){
      Pred1 <- Univariado_Rminer(departamento=d1, nomedepartamento="Departamento 1", modelo="xgboost", D=selected_inverse_index)
      Pred2 <- Univariado_Rminer(departamento=d2, nomedepartamento="Departamento 2", modelo="xgboost", D=selected_inverse_index)
      Pred3 <- Univariado_Rminer(departamento=d3, nomedepartamento="Departamento 3", modelo="xgboost", D=selected_inverse_index)
      Pred4 <- Univariado_Rminer(departamento=d4, nomedepartamento="Departamento 4", modelo="xgboost", D=selected_inverse_index)
    }
    if(model == "lm"){
      Pred1 <- Univariado_Rminer(departamento=d1, nomedepartamento="Departamento 1", modelo="lm", D=selected_inverse_index)
      Pred2 <- Univariado_Rminer(departamento=d2, nomedepartamento="Departamento 2", modelo="lm", D=selected_inverse_index)
      Pred3 <- Univariado_Rminer(departamento=d3, nomedepartamento="Departamento 3", modelo="lm", D=selected_inverse_index)
      Pred4 <- Univariado_Rminer(departamento=d4, nomedepartamento="Departamento 4", modelo="lm", D=selected_inverse_index)
    }
    if(model == "mars"){
      Pred1 <- Univariado_Rminer(departamento=d1, nomedepartamento="Departamento 1", modelo="mars", D=selected_inverse_index)
      Pred2 <- Univariado_Rminer(departamento=d2, nomedepartamento="Departamento 2", modelo="mars", D=selected_inverse_index)
      Pred3 <- Univariado_Rminer(departamento=d3, nomedepartamento="Departamento 3", modelo="mars", D=selected_inverse_index)
      Pred4 <- Univariado_Rminer(departamento=d4, nomedepartamento="Departamento 4", modelo="mars", D=selected_inverse_index)
    }
    if(model == "ksvm"){
      Pred1 <- Univariado_Rminer(departamento=d1, nomedepartamento="Departamento 1", modelo="ksvm", D=selected_inverse_index)
      Pred2 <- Univariado_Rminer(departamento=d2, nomedepartamento="Departamento 2", modelo="ksvm", D=selected_inverse_index)
      Pred3 <- Univariado_Rminer(departamento=d3, nomedepartamento="Departamento 3", modelo="ksvm", D=selected_inverse_index)
      Pred4 <- Univariado_Rminer(departamento=d4, nomedepartamento="Departamento 4", modelo="ksvm", D=selected_inverse_index)
    }
    
   
    
    # Plot for Pred1
    output$pred_plot1 <- renderPlot({
      if (exists("Pred1")) {
        plot(Pred1, type="l", col="red", lwd=2, xlab="Time", ylab="Value", main="Forecast for Department 1")
      }
    })
    
    # Plot for Pred2
    output$pred_plot2 <- renderPlot({
      if (exists("Pred2")) {
        plot(Pred2, type="l", col="blue", lwd=2, xlab="Time", ylab="Value", main="Forecast for Department 2")
      }
    })
    
    # Plot for Pred3
    output$pred_plot3 <- renderPlot({
      if (exists("Pred3")) {
        plot(Pred3, type="l", col="green", lwd=2, xlab="Time", ylab="Value", main="Forecast for Department 3")
      }
    })
    
    # Plot for Pred4
    output$pred_plot4 <- renderPlot({
      if (exists("Pred4")) {
        plot(Pred4, type="l", col="purple", lwd=2, xlab="Time", ylab="Value", main="Forecast for Department 4")
      }
    })
    
  })
  
  # Renderiza a tabela com os dados CSV
  output$data_table <- renderDT({
    datatable(walmart_data, 
              options = list(
                lengthMenu = c(5, 10, 15),
                pageLength = 5
              ))
  })
  
  # Calcula o valor médio da soma das colunas WSdep1, WSdep2, WSdep3 e WSdep4
  mean_values <- reactive({
    mean_WSdep1 <- mean(walmart_data$WSdep1)
    mean_WSdep2 <- mean(walmart_data$WSdep2)
    mean_WSdep3 <- mean(walmart_data$WSdep3)
    mean_WSdep4 <- mean(walmart_data$WSdep4)
    
    data.frame(Dep = c("WSdep1", "WSdep2", "WSdep3", "WSdep4"),
               Mean = c(mean_WSdep1, mean_WSdep2, mean_WSdep3, mean_WSdep4))
  })
  
  # Renderiza o gráfico de velocímetro para WSdep1
  output$gauge_WSdep1 <- renderPlotly({
    render_gauge_plot("WSdep1", mean_values())
  })
  
  # Renderiza o gráfico de velocímetro para WSdep2
  output$gauge_WSdep2 <- renderPlotly({
    render_gauge_plot("WSdep2", mean_values())
  })
  
  # Renderiza o gráfico de velocímetro para WSdep3
  output$gauge_WSdep3 <- renderPlotly({
    render_gauge_plot("WSdep3", mean_values())
  })
  
  # Renderiza o gráfico de velocímetro para WSdep4
  output$gauge_WSdep4 <- renderPlotly({
    render_gauge_plot("WSdep4", mean_values())
  })
  
  # Função para renderizar os gráficos de velocímetro
  render_gauge_plot <- function(department, mean_values) {
    mean_value <- mean_values[mean_values$Dep == department, "Mean"]
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = mean_value,
      title = list(text = department),
      gauge = list(
        axis = list(range = list(NULL, max(mean_values$Mean))),
        bar = list(color = "darkblue"),
        bordercolor = "gray",
        bgcolor = "white",  # Define o fundo como branco
        steps = list(
          list(range = c(0, max(mean_values$Mean) / 3), color = "white"),
          list(range = c(max(mean_values$Mean) / 3, 2 * max(mean_values$Mean) / 3), color = "white"),
          list(range = c(2 * max(mean_values$Mean) / 3, max(mean_values$Mean)), color = "white")
        )
      )
    )
  }
  
  
}

# Cria a aplicação Shiny
shinyApp(ui = ui, server = server)
