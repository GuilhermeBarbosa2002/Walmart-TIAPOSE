source("Functions_Otimization.R")
library(mco)
library(emoa)

# Definir as vendas da semana
actual_sales <- data.frame(
  WSdep1 = c(54480, 42221, 36267, 35283),
  WSdep2 = c(159460, 156945, 146388, 132156),
  WSdep3 = c(63584, 62888, 62768, 60279),
  WSdep4 = c(127009, 124560, 123346, 117375)
)

# Definir a função de avaliação
eval <- function(s) {
  s <- round(s)
  hired_workers = matrix(s[1:12], nrow = 3, ncol = 4)
  product_orders = matrix(s[13:28], nrow = 4, ncol = 4)
  sales = calculate_sales(actual_sales, hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
  return(-monthly_profit)
}

eval_max <- function(s) {
  s <- round(s)
  hired_workers = matrix(s[1:12], nrow = 3, ncol = 4)
  product_orders = matrix(s[13:28], nrow = 4, ncol = 4)
  sales = calculate_sales(actual_sales, hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
  return(monthly_profit)
}

F2 <- function(s) {
  s <- round(s)
  hired_workers = matrix(s[1:12], nrow = 3, ncol = 4)
  product_orders = matrix(s[13:28], nrow = 4, ncol = 4)
  monthly_effort = total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)
  return(monthly_effort)
}
# Definir a função objetivo
objective_function <- function(x) {
  x <- round(x)
  c(eval(x), F2(x))
}

###################################### LOAD DATA #####################################
dados=read.csv("previsoes.csv",header=TRUE,sep=",")

# Selecionar apenas as colunas necessárias
dados_growing <- dados[, c("Departamento1_KSVM", "Departamento2_LM", "Departamento3_MARS", "Departamento4_LM")]
# Dividir os dados em grupos de 4 linhas
grupos <- split(dados_growing, rep(1:8, each = 4))

#######################################################################################

# Definir os parâmetros da otimização
D <- 28
# Ponto de referência fixo
reference_point <- c(200000, 1000)

nsga_growing <- function(){
  nsga_values = vector(length = 8)
  # Loop through all the matrices
  for (i in 1:length(grupos)) {
    lower <- rep(0, D) # Lower bounds
    upper <- calculate_uppers(grupos[[i]]) # Upper bounds
    # Executar a otimização multiobjetivo
    G <- nsga2(fn = objective_function, idim = D, odim = 2, lower.bounds = lower, upper.bounds = upper, popsize = 200, generations = 20)
    
    # Extrair soluções ótimas de Pareto da geração final
    pareto_optimal_solutions <- G$value[which(G$pareto.optimal), ]
    
    
    # Calcular o hypervolume com o ponto de referência
    nsga_values[i] <- dominated_hypervolume(points = t(pareto_optimal_solutions), ref = reference_point)
    
    # # Exibir o resultado
    # print("Hypervolume:")
    # print(hv)
    # 
    # I <- which(G$pareto.optimal)
    # 
    # for (i in I) {
    #   x <- ceiling(G$par[i,])
    #   # cat("Hired workers and product orders:", x, "\n")
    #   # cat("Monthly profit:", eval_max(x), "\n")
    #   # cat("Monthly effort:", F2(x), "\n\n")
    # }
  }
  
  return(median(nsga_values))
}

ngsa = nsga_growing()
print(paste("NGSA - ", ngsa))

