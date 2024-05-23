source("functions_Otimization.R")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
source("hill.R") #  hclimbing is defined here
source("grid.R") #  gsearch is defined here
library(tabuSearch)
library(genalg)
library(adana)


# definir as vendas da semana
actual_sales <- data.frame(
  WSdep1 = c(54480,42221,36267,35283),
  WSdep2 = c(159460,156945,146388,132156),
  WSdep3 = c(63584,62888,62768,60279),
  WSdep4 = c(127009,124560,123346,117375)
)

###################################### DEFINE PARAMETERS ##############################
D = 28
N = 10000
Ni = 1000 # iterations to get the s0 at montecarlo
N2 = N - Ni # Iteractios to HC and SAN

# Binary
bits_workers <- 0
bits_orders  <- 0
a <- 0

F2 <- function(s){
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  monthly_effort  = total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)
  
  return(monthly_effort)
}

F1 <- function(s){
  s <- ceiling(s)
  hired_workers = matrix(s[1:12], nrow=3, ncol=4)
  product_orders = matrix(s[13:28], nrow=4, ncol=4)
  sales = calculate_sales(actual_sales, hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
  
  return(monthly_profit)
}

eval_mix_max<- function(s){
  s <- ceiling(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  
  effort=F2(s)
  
  return((0.5 * monthly_profit)-(10000*effort*0.5))
}


eval_mix_min<- function(s){
  s <- ceiling(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  
  effort=F2(s)
  
  return((-0.5 * monthly_profit)+(10000*effort*0.5))
}


###################################### LOAD DATA #####################################
dados=read.csv("previsoes.csv",header=TRUE,sep=",")

# Selecionar apenas as colunas necessárias
dados_growing <- dados[, c("Departamento1_KSVM", "Departamento2_LM", "Departamento3_MARS", "Departamento4_LM")]
# Dividir os dados em grupos de 4 linhas
grupos <- split(dados_growing, rep(1:8, each = 4))


##################################### MONTECARLO ##############################
montecarlo_growing <- function(){
  montecarlo_values = vector(length = 8)
  montecarlo_profit = vector(length = 8)
  montecarlo_effort = vector(length = 8)
  
  # Loop through all the matrices
  for (i in 1:length(grupos)) {
    lower <- rep(0, D) # Lower bounds
    upper <- calculate_uppers(grupos[[i]]) # Upper bounds
    MC    <- mcsearch(fn = eval_mix_max, lower = lower, upper = upper, N = N, type = "max")
    sol = ceiling(MC$sol)
    montecarlo_values[i] <- eval_mix_max(sol)
    montecarlo_profit[i] <- F1(sol)
    montecarlo_effort[i] <- F2(sol)
  }
  
  return(list(
    median_value = median(montecarlo_values),
    median_profit = median(montecarlo_profit),
    median_effort = median(montecarlo_effort)
  ))
}

###################################### HILL_CLIMBING ##############################

# slight change of a real par under a normal u(0,0.5) function:
rchange1 <- function(par, lower, upper) { 
  new_par <- hchange(par, lower = lower, upper = upper, rnorm, mean = 1, sd = 0.20, round = FALSE)
  rounded_par <- ceiling(new_par)
  return(rounded_par)
}

hill_climbing_growing <- function(){
  hill_climbing_values <- vector(length = 8) 
  hill_climbing_profit = vector(length = 8)
  hill_climbing_effort = vector(length = 8)
  
  
  for (i in 1:length(grupos)) {
    lower <- rep(0, D) # limites inferiores
    upper <- calculate_uppers(grupos[[i]])# limites superiores
    actual_sales <- grupos[[i]]
    
    #get s0 from montecarlo with one iteration
    MC <- mcsearch(fn = eval_mix_max, lower = lower, upper = upper, N = Ni, type = "max")
    s0 <- ceiling(MC$sol)
    HC <- hclimbing(par = s0, fn = eval_mix_max, change = rchange1, lower = lower, upper = upper, type = "max",
                    control = list(maxit = N2, REPORT = 0, digits = 2, trace = TRUE))
    sol = ceiling(MC$sol)
    
    hill_climbing_values[i] <- eval_mix_max(sol)
    hill_climbing_profit[i] <- F1(sol)
    hill_climbing_effort[i] <- F2(sol)
    
  }
  return(list(
    median_value = median(hill_climbing_values),
    median_profit = median(hill_climbing_profit),
    median_effort = median(hill_climbing_effort)
  ))
  
}

###################################### SIMULATED ANNEALING ##############################

simulatedAnnealing_growing <- function(){
  simulatedAnnealing_values <- vector(length = 8)
  simulatedAnnealing_profit = vector(length = 8)
  simulatedAnnealing_effort = vector(length = 8)
  
  #Função de mudança para o Simulated Annealing
  rchange2 <- function(par) {
    new_par     <- hchange(par, lower = lower, upper = upper, rnorm, mean = 1, sd = 0.2, round = FALSE)
    rounded_par <- ceiling(new_par)
    return(rounded_par)
  }
  
  # Definição dos parâmetros do Simulated Annealing
  CSANN <- list(maxit = N2, temp = 900, trace = FALSE)
  
  for (i in 1:length(grupos)) {
    lower <- rep(0,D) # limites inferiores
    upper <- calculate_uppers(grupos[[i]])# limites superiores
    actual_sales <- grupos[[i]]
    
    #get s0 from montecarlo with one iteration
    MC <- mcsearch(fn = eval_mix_max, lower = lower, upper = upper, N = Ni, type = "max")
    s0 <- ceiling(MC$sol)
    # Execução do Simulated Annealing
    SA <- optim(par = s0, fn = eval_mix_min, method = "SANN", gr = rchange2, control = CSANN)
    
    sol = ceiling(SA$par)
    
    simulatedAnnealing_values[i] <- eval_mix_max(sol)
    simulatedAnnealing_profit[i] <- F1(sol)
    simulatedAnnealing_effort[i] <- F2(sol)
    
  }
  
  return(list(
    median_value = median(simulatedAnnealing_values),
    median_profit = median(simulatedAnnealing_profit),
    median_effort = median(simulatedAnnealing_effort)
  ))
}

######################################### RGBA - genetic #########################################
rgba_growing <- function(){
  rgba_values <- vector(length=8) 
  rgba_value_profit = vector(length = 8)
  rgba_value_effort = vector(length = 8)
  
  popSize <- 100
  size    <- 28
  
  # Loop para percorrer todas as matrizes
  for (i in 1:length(grupos)) {
    lower <- rep(0,D) # limites inferiores
    upper <- calculate_uppers(grupos[[i]])# limites superiores
    actual_sales <- grupos[[i]]
    
    rga <- rbga(stringMin      = lower, 
                stringMax      = upper, 
                popSize        = popSize, 
                mutationChance = 1 / (size + 1), 
                elitism        = popSize * 0.2, 
                evalFunc       = eval_mix_min, 
                iter           = N/popSize)
    
    bs <- rga$population[rga$evaluations == min(rga$evaluations)]
    bs = ceiling(bs)
    rgba_values[i] <- eval_mix_min(bs)
    rgba_value_profit[i] <- F1(bs)
    rgba_value_effort[i] <- F2(bs)
  }
  
  return(list(
    rgba_values = median(rgba_values),
    rgba_value_profit = median(rgba_value_profit),
    rgba_value_effort = median(rgba_value_effort)
  ))
}


####################################### Tabu - Search ############################################
# Function to divide binary array by bits
matrix_transform <- function(solution, start, elements, dimension_start, bits){
  matrix_final <- c()
  for(i in 1:elements){
    matrix_final[i]  <- bin2int(solution[start:(start + bits - 1)])
    start <- start + bits
  }
  return(matrix_final)
}

evaltabu <- function(solution){
  hired_workers  <- matrix(matrix_transform(solution        = solution, 
                                            start           = 1, 
                                            elements        = 12,
                                            dimension_start = 1,
                                            bits            = bits_workers), nrow = 3, ncol = 4)
  
  product_orders <- matrix(matrix_transform(solution        = solution, 
                                            start           = 12 * bits_workers + 1, 
                                            elements        = 16, 
                                            dimension_start = 13, 
                                            bits            = bits_orders), nrow = 4, ncol = 4)
  
  sales          <- calculate_sales(actual_sales, hired_workers, product_orders)
  monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
  
  return(monthly_profit)
}

F2tabu <- function(solution){
  hired_workers  <- matrix(matrix_transform(solution        = solution, 
                                            start           = 1, 
                                            elements        = 12,
                                            dimension_start = 1,
                                            bits            = bits_workers), nrow = 3, ncol = 4)
  
  product_orders <- matrix(matrix_transform(solution        = solution, 
                                            start           = 12 * bits_workers + 1, 
                                            elements        = 16, 
                                            dimension_start = 13, 
                                            bits            = bits_orders), nrow = 4, ncol = 4)
  
  monthly_effort <- total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)
  return(monthly_effort) # needs to be negative because tabuSearch only maximizes
}

# Function to build Initial Config
initial_config_build <- function(config, n_bits, dimensions){
  initial_length <- length(config)
  while(length(config) - initial_length < dimensions * n_bits){
    config <- c(config, rep(0, n_bits), rep(1, n_bits))
  }
  return(config)
}

eval_mix_tabu <- function(solution){
  
  w1=0.5
  w2=w1
  
  return(w1*evaltabu(solution) - (w2*F2tabu(solution)*10000))
  
}

tabu_growing <- function(){
  tabu_values <- vector(length=8)
  tabu_value_profit = vector(length = 8)
  tabu_value_effort = vector(length = 8)
  
  for (i in 1:length(grupos)) {
    a <<- a + 1
    lower <- rep(0,D) # limites inferiores
    upper <- calculate_uppers(grupos[[i]])# limites superiores
    bits_workers <<- ceiling(max(log(upper[1:12] , 2))) # Bits for Hired Workers
    bits_orders  <<- ceiling(max(log(upper[13:28], 2))) # Bits for Product Orders
    size         <- 12 * bits_workers + 16 * bits_orders # solution size
    
    initial_config <- c() # Building Initial configuration
    initial_config <- initial_config_build(config = initial_config, n_bits = bits_workers, dimensions = 12) # Building Initial configuration for Hired Workers
    initial_config <- initial_config_build(config = initial_config, n_bits = bits_orders , dimensions = 16) # Building Initial configuration for Product Orders
    solution <- tabuSearch(size, iters = N/100, objFunc = eval_mix_tabu, config = initial_config, verbose = F)
    
    b  <- which.max(solution$eUtilityKeep) # best index
    bs  <- solution$configKeep[b,]
    
    hired_workers <- matrix_transform(solution       = bs, 
                                      start           = 1, 
                                      elements        = 12,
                                      dimension_start = 1,
                                      bits            = bits_workers)
    product_orders <- matrix_transform(solution        = bs, 
                                       start           = 12 * bits_workers + 1, 
                                       elements        = 16, 
                                       dimension_start = 13, 
                                       bits            = bits_orders)
    
    tabu_values[i] <- eval_mix_max(c(hired_workers, product_orders))
    tabu_value_profit[i] <- F1(c(hired_workers, product_orders))
    tabu_value_effort[i] <- F2(c(hired_workers, product_orders)) 
    
  }
  
  return(list(
    tabu_values = median(tabu_values),
    tabu_value_profit = median(tabu_value_profit),
    tabu_value_effort = median(tabu_value_effort)
  ))
  
}

####################################### RGBA.bin ############################################
eval_rbga <- function(solution){
  hired_workers  <- matrix(matrix_transform(solution        = solution, 
                                            start           = 1, 
                                            elements        = 12,
                                            dimension_start = 1,
                                            bits            = bits_workers), nrow = 3, ncol = 4)
  
  product_orders <- matrix(matrix_transform(solution        = solution, 
                                            start           = 12 * bits_workers + 1, 
                                            elements        = 16, 
                                            dimension_start = 13, 
                                            bits            = bits_orders), nrow = 4, ncol = 4)
  
  sales          <- calculate_sales(actual_sales, hired_workers, product_orders)
  monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
  return(-monthly_profit)
}

F2RGBA <- function(solution){
  hired_workers  <- matrix(matrix_transform(solution        = solution, 
                                            start           = 1, 
                                            elements        = 12,
                                            dimension_start = 1,
                                            bits            = bits_workers), nrow = 3, ncol = 4)
  
  product_orders <- matrix(matrix_transform(solution        = solution, 
                                            start           = 12 * bits_workers + 1, 
                                            elements        = 16, 
                                            dimension_start = 13, 
                                            bits            = bits_orders), nrow = 4, ncol = 4)
  
  monthly_effort <- total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)
  return(monthly_effort) # needs to be negative because tabuSearch only maximizes
}

eval_mix_rbga <- function(solution){
  w1=0.5
  w2=w1
  return(w1*eval_rbga(solution) + (w2*F2RGBA(solution)*10000))
  
}

rbga_bin_growing <- function(){
  rbga_bin_values <- vector(length = 8)
  rbga_bin_value_profit = vector(length = 8)
  rbga_bin_value_effort = vector(length = 8)
  
  for(i in 1:length(grupos)) {
    
    # RBGA.bin PARAMETERS
    Low            <- rep(0, D) # Lower
    Up             <- calculate_uppers(grupos[[i]]) # limites superiores 
    bits_workers   <<- ceiling(max(log(Up[1:12] , 2))) # Bits for Hired Workers
    bits_orders    <<- ceiling(max(log(Up[13:28], 2))) # Bits for Product Orders
    size           <- 12 * bits_workers + 16 * bits_orders # solution size
    mutationChance <- 1 / (size + 1)
    popsize        <- 100
    elitism        <- popsize * 0.2
    
    rga <- rbga.bin(size           = size,
                    popSize        = popsize,
                    iters          = N/popsize,
                    mutationChance = mutationChance,
                    elitism        = elitism,
                    zeroToOneRatio = 10,
                    evalFunc       = eval_mix_rbga,
                    verbose        = FALSE)
    
    
    bs <- rga$population[rga$evaluations == min(rga$evaluations)]
    
    hired_workers <- matrix_transform(solution       = bs, 
                                      start           = 1, 
                                      elements        = 12,
                                      dimension_start = 1,
                                      bits            = bits_workers)
    product_orders <- matrix_transform(solution        = bs, 
                                       start           = 12 * bits_workers + 1, 
                                       elements        = 16, 
                                       dimension_start = 13, 
                                       bits            = bits_orders)
    
    rbga_bin_values[i] <- eval_mix_max(c(hired_workers, product_orders))
    rbga_bin_value_profit[i] <- F1(c(hired_workers, product_orders))
    rbga_bin_value_effort[i] <- F2(c(hired_workers, product_orders)) 
  }
  
  return(list(
    rbga_bin_values = median(rbga_bin_values),
    rbga_bin_value_profit = median(rbga_bin_value_profit),
    rbga_bin_value_effort = median(rbga_bin_value_effort)
  ))
  
}


monte_carlo = montecarlo_growing()
print("***** MONTECARLO ********")
print(paste("total - ", monte_carlo[[1]]))
print(paste("profit - ", monte_carlo[[2]]))
print(paste("effort - ", monte_carlo[[3]]))

hill_climbing = hill_climbing_growing()
print("***** HILL CLIMBING ********")
print(paste("total - ", hill_climbing[[1]]))
print(paste("profit - ", hill_climbing[[2]]))
print(paste("effort - ", hill_climbing[[3]]))

san = simulatedAnnealing_growing()
print("***** SIMULATED ANNEALING ********")
print(paste("total - ", san[[1]]))
print(paste("profit - ", san[[2]]))
print(paste("effort - ", san[[3]]))

rgba_genetic = rgba_growing()
print("***** RBGA GENETIC ********")
print(paste("total - ", rgba_genetic[[1]]))
print(paste("profit - ", rgba_genetic[[2]]))
print(paste("effort - ", rgba_genetic[[3]]))

rgba_bin = rbga_bin_growing()
print("***** RBGA.BIN ********")
print(paste("total - ", rgba_bin[[1]]))
print(paste("profit - ", rgba_bin[[2]]))
print(paste("effort - ", rgba_bin[[3]]))

tabu = tabu_growing()
print("***** TABU ********")
print(paste("total - ", tabu[[1]]))
print(paste("profit - ", tabu[[2]]))
print(paste("effort - ", tabu[[3]]))