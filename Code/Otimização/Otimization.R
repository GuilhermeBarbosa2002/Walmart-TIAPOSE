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

F2 <- function(s){
    hired_workers = matrix(s[1:12],nrow=3,ncol=4)
    product_orders = matrix(s[13:28],nrow=4,ncol=4)
    monthly_effort  = total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)

    return(monthly_effort)
}

##################### MONTECARLO_SEARCH #################
montecarlo <- function(eval_max, lower, upper, N, type){
  
  MC <- mcsearch(fn = eval_max, lower = lower, upper = upper, N = N, type = type)
  
  best_solution <- round(MC$sol)
  
  cat("\n ******** MONTECARLO ******\n")
  cat("Melhor solução:", best_solution, "Função de avaliação:", MC$eval, " (encontrado na iteração:", MC$index, ")\n")
  
  # Plotar a curva de convergência
  plot(curve, type = "l", col = "blue", xlab = "Iterações", ylab = "Valor da Função de Avaliação", main = "Curva de Convergência - MonteCarlo")
  
  return(best_solution)
}

##################### HILL_CLIMBING #################
hill_climbing <- function(eval, lower, upper, N, type, s0, REPORT){
  
  # slight change of a real par under a normal u(0,0.5) function:
  rchange1 <- function(par, lower, upper) { 
    new_par <- hchange(par, lower = lower, upper = upper, rnorm, mean = 0, sd = 0.25, round = FALSE)
    rounded_par <- ceiling(new_par)
    return(rounded_par)
  }

  # ##with report
  # HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type=type,
  #              control=list(maxit=N,REPORT=REPORT,digits=2))
  
  ##without report
  HC = hclimbing(par = s0, fn = eval, change = rchange1, lower = lower, upper = upper, type = type,
                 control = list(maxit = N, REPORT = 0, digits = 2, trace = TRUE))
 
  cat("\n ******** HILL CLIMBING ******\n")
  cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
  
  plot(curve, type = "l", col = "blue", xlab = "Iterações", ylab = "Valor da Função de Avaliação", main = "Curva de Convergência - Hill Climbig")
  
  return(HC$sol)
}


##################### Simulated Annealing #################  
SimulatedAnnealing <- function(eval, lower, upper, s0, type){
  
  eval_values <- numeric(N)
  
  
  #Função de mudança para o Simulated Annealing
  rchange2 <- function(par) {
  new_par <- hchange(par, lower = lower, upper = upper, rnorm, mean = 0, sd = 0.5, round = FALSE)
    rounded_par <- ceiling(new_par)
    return(rounded_par)
  }
  
  cat("\n ******** Simulated Annealing ******\n")
 
  # Definição dos parâmetros do Simulated Annealing
  CSANN <- list(maxit = N, temp = 100, trace = FALSE)
  
  # Execução do Simulated Annealing
  SA <- optim(par = s0, fn = eval, method = "SANN", gr = rchange2, control = CSANN)
  
  cat("Melhor solução encontrada:", SA$par, "Valor da função de avaliação:", -SA$value, "\n")  
  
  # Plotar a curva de convergência
  plot(curve, type = "l", col = "blue", xlab = "Iterações", ylab = "Valor da Função de Avaliação", main = "Curva de Convergência - Simulated Annealing")
  
  return(SA$par)
}

##################### PARAMETERS #################
# dimension
D=28
N <- 100000 #número de pesquisas
REPORT=N/20 # report results

lower <- rep(0, D) # limites inferiores
upper <- calculate_uppers(actual_sales)# limites superiores
# define the initial solution for hill_climbing
x = c(5,6,7,4,5,6,3,4,5,2,3,4,61662,0,12985,39924,78292,0,55403,75160,56434,0,69133,62131,24182,0,37167,99708)
x1 = c(5 ,37, 13, 23, 5, 19, 1, 5, 2, 2, 2, 11, 133982, 134325, 30651, 11723, 159884, 367385, 140936, 122379, 26785, 82987, 45094, 27352, 94076, 139434, 82896, 64937)
x2 = c(8, 38, 15, 24, 7, 22, 3, 5, 4, 3, 3, 13, 134101, 134441, 30785, 11860, 159979, 367501, 141060, 122515, 26920, 83112, 45240, 27495, 94235, 139555, 83043, 65082 )


EV=0 #  initial evaluation point is zero.
BEST=-Inf # initial best is -Inf
curve=rep(NA,N) # vector with the convergence values

# 
# # #Simulated Annealing
#s <- SimulatedAnnealing(eval_min,lower,upper,x,"max")

#Montecarlo
#s <- montecarlo(eval_max,lower,upper,N,"max")

# #Hill_Climbing
s <- hill_climbing(eval_max,lower, upper, N, "max", x, REPORT)

hired_workers  <- matrix(s[1:12] , nrow = 3, ncol = 4)
product_orders <- matrix(s[13:28], nrow = 4, ncol = 4)
sales          <- calculate_sales(actual_sales, hired_workers, product_orders)
monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)

cat("Best Solution: \nHired Workers \n")
print(hired_workers) 
cat("\nProduct Orders \n")
print(product_orders) 
cat("\nSales \n")
print(sales)
cat("\nMonthly Profit:", monthly_profit,"\n")