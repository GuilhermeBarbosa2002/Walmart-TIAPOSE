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

###################################### EVAL #####################################
eval <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  return(monthly_profit)
}

## Define the eval function
eval_min <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  
  # EV <<- EV + 1
  # if(monthly_profit > BEST){
  #   BEST <<- monthly_profit
  # }
  # 
  # if(EV <= N){
  #   curve[EV] <<- BEST
  # }
  # 
  return(-monthly_profit)
}

eval_max <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  
  # EV <<- EV + 1
  # if(monthly_profit > BEST){
  #   BEST <<- monthly_profit
  # }
  # 
  # if(EV <= N){
  #   curve[EV] <<- BEST
  # }
  
  return(monthly_profit)
}


###################################### LOAD DATA #####################################
dados=read.csv("walmart.csv",header=TRUE,sep=",")
dados_growing <- tail(dados, 32)

# Selecionar apenas as colunas necessárias
dados_growing <- dados_growing[, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]

# Dividir os dados em grupos de 4 linhas
grupos <- split(dados_growing, rep(1:8, each = 4))

# Dividir os dados em grupos de 4 linhas
grupos <- split(dados_growing, rep(1:8, each = 4))

###################################### DEFINE PARAMETERS ##############################
D = 28
N = 10000


###################################### MONTECARLO_SEARCH ##############################
montecarlo <- function(eval_max, lower, upper, N, type){
  MC <- mcsearch(fn = eval_max, lower = lower, upper = upper, N = N, type = type)
  # cat("\n ******** MONTECARLO ******\n")
  # cat("Melhor solução:", round(MC$sol), "Função de avaliação:", MC$eval, " (encontrado na iteração:", MC$index, ")\n")
  return(MC$eval)
}
montecarlo_growing <- function(){
  montecarlo_values=vector(length=8) 
  # Loop para percorrer todas as matrizes
  for (i in 1:length(grupos)) {
    lower <- rep(0,D) # limites inferiores
    upper <- calculate_uppers(grupos[[i]])# limites superiores
    actual_sales = grupos[[i]]
    
    montecarlo_values[i] = montecarlo(eval_max,lower,upper,N,"max")
  
  }
  
  return(median(montecarlo_values))

}


###################################### HILL_CLIMBING ##############################

# slight change of a real par under a normal u(0,0.5) function:
rchange1 <- function(par, lower, upper) { 
  new_par <- hchange(par, lower = lower, upper = upper, rnorm, mean = 0, sd = 0.25, round = FALSE)
  rounded_par <- ceiling(new_par)
  return(rounded_par)
}

hill_climbing_growing <- function(){
  hill_climbing_values=vector(length=8) 
  for (i in 1:length(grupos)) {
    lower <- rep(0,D) # limites inferiores
    upper <- calculate_uppers(grupos[[i]])# limites superiores
    actual_sales = grupos[[i]]
    #get s0 from montecarlo with one iteration
    MC <- mcsearch(fn = eval_max, lower = lower, upper = upper, N = 1, type = "max")
    s0 = MC$sol
    HC = hclimbing(par = s0, fn = eval_max, change = rchange1, lower = lower, upper = upper, type = "max",
                                             control = list(maxit = N, REPORT = 0, digits = 2, trace = TRUE))
    hill_climbing_values[i] = HC$eval
   
  }
  
  return(median(hill_climbing_values))
  
}


###################################### SIMULATED ANNEALING ##############################

simulatedAnnealing_growing <- function(){
  simulatedAnnealing_values=vector(length=8) 
  eval_values <- numeric(N)
  #Função de mudança para o Simulated Annealing
  rchange2 <- function(par) {
    new_par <- hchange(par, lower = lower, upper = upper, rnorm, mean = 0, sd = 0.5, round = FALSE)
    rounded_par <- ceiling(new_par)
    return(rounded_par)
  }
  # Definição dos parâmetros do Simulated Annealing
  CSANN <- list(maxit = N, temp = 100, trace = FALSE)
  
  for (i in 1:length(grupos)) {
    lower <- rep(0,D) # limites inferiores
    upper <- calculate_uppers(grupos[[i]])# limites superiores
    actual_sales = grupos[[i]]
    #get s0 from montecarlo with one iteration
    MC <- mcsearch(fn = eval_max, lower = lower, upper = upper, N = 1, type = "max")
    s0 = MC$sol
    # Execução do Simulated Annealing
    SA <- optim(par = s0, fn = eval_min, method = "SANN", gr = rchange2, control = CSANN)
    simulatedAnnealing_values[i] = -SA$value
  }
  
  return(median(simulatedAnnealing_values))
}


######################################### RGBA - genetic #########################################
# get 
rgba_growing <- function(){
  rgba_values=vector(length=8) 
  # Loop para percorrer todas as matrizes
  for (i in 1:length(grupos)) {
    lower <- rep(0,D) # limites inferiores
    upper <- calculate_uppers(grupos[[i]])# limites superiores
    actual_sales = grupos[[i]]
    
    montecarlo_values[i] = montecarlo(eval_max,lower,upper,N,"max")
  
  }
  
  return(median(montecarlo_values))
  popSize = 200
  iter = N/popSize
  lowers = rep(0,28)
  uppers = calculate_uppers(actual_sales)

  rga=rbga(lowers,uppers,popSize=2000,mutationChance=0.33,elitism=10,evalFunc=eval,iter=iter) 


  plot(rga)
  bindex=which.min(rga$evaluations)
  cat("best solution:",rga$population[bindex,],"evaluation function",rga$evaluations[bindex],"\n")

}

