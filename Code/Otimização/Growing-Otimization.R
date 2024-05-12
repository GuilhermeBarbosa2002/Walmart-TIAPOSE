source("functions_Otimization.R")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
source("hill.R") #  hclimbing is defined here
source("grid.R") #  gsearch is defined here

# definir as vendas da semana
actual_sales <- data.frame(
  WSdep1 = c(54480,42221,36267,35283),
  WSdep2 = c(159460,156945,146388,132156),
  WSdep3 = c(63584,62888,62768,60279),
  WSdep4 = c(127009,124560,123346,117375)
)

###################################### DEFINE PARAMETERS ##############################
D = 28
N = 1000
BEST = 0
EV = 0
curve=rep(NA,N) # vector with the convergence values


## Define the eval function
eval_min <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  
  EV <<- EV + 1
  if(monthly_profit > BEST){
    BEST <<- monthly_profit
  }
  
  if(EV <= N){
    curve[EV] <<- BEST
  }
  
  return(-monthly_profit)
}

eval_max <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  
  EV <<- EV + 1
  if(monthly_profit > BEST){
    BEST <<- monthly_profit
  }
  
  if(EV <= N){
    curve[EV] <<- BEST
  }
  
  return(monthly_profit)
}



###################################### LOAD DATA #####################################
dados=read.csv("walmart.csv",header=TRUE,sep=",")
dados_growing <- tail(dados, 32)

# Selecionar apenas as colunas necessárias
dados_growing <- dados_growing[, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]

# Dividir os dados em grupos de 4 linhas
grupos <- split(dados_growing, rep(1:8, each = 4))


###################################### MONTECARLO_SEARCH ##############################
montecarlo_growing <- function(){
  montecarlo_values = vector(length = 8)
  montecarlo_curve = vector(length = 8 * N)
  # Loop through all the matrices
  for (i in 1:length(grupos)) {
    lower <- rep(0, D) # Lower bounds
    upper <- calculate_uppers(grupos[[i]]) # Upper bounds
    MC = mcsearch(fn = eval_max, lower = lower, upper = upper, N = N, type = "max")
    montecarlo_values[i] = MC$eval
    
    # Plotar o gráfico dentro do loop
    #plot(curve, type = "l", col = "blue", xlab = "Iterations", ylab = "Evaluation Function Value", main = paste("Convergence Curve - Monte Carlo - iteracao ", i))
    
    # Armazenar os valores de convergência em montecarlo_curve
    start_index <- (i - 1) * N + 1
    end_index <- i * N
    montecarlo_curve[start_index:end_index] <- curve
    
    EV <<- 0
    BEST <<- -Inf
    curve <<- rep(NA,N) 
  }
  
  # # Calcular a média dos valores de convergência agrupados pelo índice da iteração
  # final_means <- numeric(length = N)
  # for (j in 1:N) {
  #   # Extrair os valores correspondentes ao índice da iteração j
  #   indices <- seq(j, length(montecarlo_curve), N)
  #   values <- montecarlo_curve[indices]
  #   final_means[j] <- mean(values, na.rm = TRUE)
  # }
  # 
  # # Plotar o gráfico com as médias dos valores de convergência agrupados pelo índice da iteração
  # plot(final_means, type = "l", col = "blue", xlab = "Iteration Index", ylab = "Mean Evaluation Function Value", main = "Convergence Curve - Monte Carlo")
  plot_iteration_means(montecarlo_curve, N, "Montecarlo")
  return(median(montecarlo_values))
}



plot_iteration_means <- function(curve, N, name) {
  # Calcular a média dos valores de convergência agrupados pelo índice da iteração
  final_means <- numeric(length = N)
  for (j in 1:N) {
    # Extrair os valores correspondentes ao índice da iteração j
    indices <- seq(j, length(curve), N)
    values <- curve[indices]
    final_means[j] <- mean(values, na.rm = TRUE)
  }
  # Plotar o gráfico com as médias dos valores de convergência agrupados pelo índice da iteração
  plot(final_means, type = "l", col = "blue", xlab = "Iteration Index", ylab = "Mean Evaluation Function Value", main = paste("Convergence Curve - ",name))
}


montecarlo_result <- montecarlo_growing()
print(paste("Monte Carlo - ", montecarlo_result))



