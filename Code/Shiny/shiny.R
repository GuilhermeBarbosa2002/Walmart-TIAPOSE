library(shiny)
library(shinythemes)

# Define a UI for the application
ui <- fluidPage(
  theme = shinytheme("flatly"), # Escolha o tema "flatly"
  titlePanel("Selecione uma Semana"),
  sidebarLayout(
    sidebarPanel(
      dateInput("selected_week", "Selecione o Início da Semana:", 
                format = "yyyy-mm-dd", 
                startview = "month",
                weekstart = 1,
                language = "pt",
                autoclose = TRUE)
    ),
    mainPanel(
      h3("Semana Selecionada:", style = "color: #333333;"),
      verbatimTextOutput("selected_week_output")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Atualiza o output sempre que a data selecionada mudar
  output$selected_week_output <- renderText({
    # Extrai a semana a partir da data selecionada
    selected_week <- input$selected_week
    end_of_week <- selected_week + lubridate::weeks(1) - lubridate::days(1)
    paste("De", format(selected_week, "%d/%m/%Y"), "até", format(end_of_week, "%d/%m/%Y"))
  })
}

# Cria uma aplicação Shiny
shinyApp(ui = ui, server = server)