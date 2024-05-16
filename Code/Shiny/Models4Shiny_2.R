library(forecast)
library(rminer)
library(vars)
library(lubridate)
library(genalg)
library(adana)
library(tabuSearch)
library(vars)

source("multi-utils.R")


data=read.csv("walmart.csv",header=TRUE,sep=",")
data$Week <- week(as.Date(data$Date))
K=4 
LTS=K 
fuel_price = data[,"Fuel_Price"] 
is_holiday = data[,"IsHoliday"]  
d1=data[,"WSdep1"]  
d2=data[,"WSdep2"] 
d3=data[,"WSdep3"]  
d4=data[,"WSdep4"]
week=data[,"Week"] 

bits_workers <- 0
bits_orders  <- 0

matrix_transform <- function(solution, start, elements, dimension_start, bits){
  matrix_final <- c()
  for(i in 1:elements){
    matrix_final[i]  <- bin2int(solution[start:(start + bits - 1)])
    start <- start + bits
  }
  return(matrix_final)
}

#-------------Univariado

Univariado_Forecast = function(departamento, nomedepartamento, modelo, D){
d=read.csv("walmart.csv",header=TRUE,sep=",")
d1=d[,4] 
d2=d[,5]
d3=d[,6] 
d4=d[,7] 

L=length(d1)
K = 4 
H = 4 

NTR = L - D*4   
  
TR = 1: NTR 
  


Pred = NULL

dtr = ts(departamento[TR], frequency = K)

if(modelo == "Holtwinters") {
  
  HW = HoltWinters(dtr)
  Pred=forecast(HW,h=4)$mean  
}

if(modelo == "Arima") {
  
  ARIMA = suppressWarnings(auto.arima(dtr))
  Pred=forecast(ARIMA,h=4)$mean  
}

if(modelo == "NN") {
  
  NN = nnetar(dtr,P=1,repeats=3)
  Pred=forecast(NN,h=4)$mean  
}


if(modelo == "ETS") {
  
  ETS = ets(dtr) 
  Pred=forecast(ETS,h=4)$mean  
}


return(Pred)



}

Univariado_Rminer = function(departamento, nomedepartamento, modelo, D){
d=read.csv("walmart.csv",header=TRUE,sep=",")
d1=d[,4] 
d2=d[,5]
d3=d[,6] 
d4=d[,7] 


C = CasesSeries(departamento,c(1:4))

K = 4 
H = 4 

L=nrow(C)

NTR = L - D*4   

TR = 1: NTR 



Pred = NULL




LTS=4 # length of the test set
START=length(C[TR,])+1

if(modelo == "Random Forest") {
  
  RF=fit(y~.,C[TR,],model="randomForest")
  Pred=lforecast(RF,C,start=START,horizon=LTS) 
}

if(modelo == "mlpe") {
  
  MLPE=fit(y~.,C[TR,],model="mlpe")
  Pred=lforecast(MLPE,C,start=START,horizon=LTS) 
}

if(modelo == "mars") {
  
  mars <- fit(y~., C[TR,], model = "mars")
  Pred=lforecast(mars,C,start=START,horizon=LTS) 
}


if(modelo == "ksvm") {
  
  ksvm <- fit(y~., C[TR,], model = "ksvm")
  Pred=lforecast(ksvm,C,start=START,horizon=LTS)  
}


if(modelo == "xgboost") {
  
  XG=fit(y~.,C[TR,],model="xgboost")
  Pred=lforecast(XG,C,start=START,horizon=LTS)   
}


if(modelo == "lm") {
  
  LM=fit(y~.,C[TR,],model="lm")
  Pred=lforecast(LM,C,start=START,horizon=LTS)  
}


return(Pred)


}

#-------------Multivariado
Multivariado = function(departamento, nomedepartamento, modelo, D,variaveis){
  
  data=read.csv("walmart.csv",header=TRUE,sep=",")
  data$Week <- week(as.Date(data$Date))
  K=4 
  LTS=K 
  fuel_price = data[,"Fuel_Price"] 
  is_holiday = data[,"IsHoliday"]  
  d1=data[,"WSdep1"]  
  d2=data[,"WSdep2"] 
  d3=data[,"WSdep3"]  
  d4=data[,"WSdep4"]
  week=data[,"Week"] 
  Test=K
  
  
  ts = ts(departamento,frequency = K)
  L = length(ts)
  NTR = L - D   
  
  TR = 1: NTR 
 
  variaveis_selecionadas <- data[, variaveis, drop = FALSE]
 
  cdata = cbind(variaveis_selecionadas) 
  mtr=ts(cdata[TR,],frequency=K)
    
  
  if(modelo == "AUTOVAR"){
    mvar=autoVAR(mtr,LAGMAX=4)
    Pred = forecastVAR(mvar,h=LTS)
    Pred = Pred[[1]]
  }
  if(modelo == "ARIMAX"){
    arimax=autoARIMAX(mtr, frequency=4)
    Pred=forecastARIMAX(arimax,h=LTS)
    Pred = Pred[[1]]
  }
  if(modelo == "MLPE"){
    
    VINP <- vector("list", length = length(colnames(cdata)))
    for (i in 1:length(VINP)) {
      lags <- vector("list", length(VINP))
      for (j in 1:length(VINP)) {
        if (j == i) {
          lags[[j]] <- 1:4
        } else {
          lags[[j]] <- 1
        }
      }
      VINP[[i]] <- lags
    }
    
    MNN=mfit(mtr,"mlpe",VINP)
    Pred=lforecastm(MNN,h=LTS)
    Pred=Pred[[1]]
  
  }
  # 
  # plot(Pred, type="l", col="black", lwd=2, xlab="Time", ylab="Value", main=paste("Forecast for Department ", nomedepartamento, "\n ", modelo))
  # legend("topright", legend="Pred", col="black", lwd=2)
  # 
  print(Pred)
  return(Pred)
}


#-------------Uniobjetivo

Uniobjetivo=function(df,algoritmo){
  
  source("Functions_Otimization.R")
  source("blind.R") # fsearch is defined here
  source("montecarlo.R") # mcsearch is defined here
  source("hill.R") #  hclimbing is defined here
  source("grid.R") #  gsearch is defined here
  
  # definir as vendas da semana
  actual_sales <- df
  print(actual_sales)
  
  algoritmo = algoritmo
  print(algoritmo)
  
  
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
  
  F1 <- function(s){
    s <- round(s)
    hired_workers = matrix(s[1:12], nrow=3, ncol=4)
    product_orders = matrix(s[13:28], nrow=4, ncol=4)
    sales = calculate_sales(actual_sales, hired_workers, product_orders)
    monthly_profit = sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
    
    return(-monthly_profit)
  }
  
  objective_function <- function(x) {
    x <- round(x)
    c(F1(x), F2(x))
  }
  
  
  ############# RGBA.BIN ################
  
  RBGABIN = function(){
    
    
    
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
    
    
    # global variables (can be used inside the functions):
    D              <- 28 # dimension
    Low            <- rep(0, D) # Lower
    Up             <- calculate_uppers(actual_sales) # Upper
    N              <- 100 # number of iterations
    Low            <- rep(0, D) # Lower
    Up             <- calculate_uppers(actual_sales) # Upper
    bits_workers   <<- ceiling(max(log(Up[1:12] , 2))) # Bits for Hired Workers
    bits_orders    <<- ceiling(max(log(Up[13:28], 2))) # Bits for Product Orders
    size           <- 12 * bits_workers + 16 * bits_orders # solution size
    mutationChance <- 1 / (size + 1)
    popsize        <- 200
    elitism        <- popsize * 0.2
    #ITER           <- 1
    
    rga <- rbga.bin(size           = size,
                    popSize        = popsize,
                    iters          = N, 
                    mutationChance = mutationChance,
                    elitism        = elitism, 
                    zeroToOneRatio = 10,
                    #monitorFunc    = NULL, 
                    evalFunc       = eval_rbga,
                    verbose        = FALSE)
    
    print(rga)
    
    
    bs <- rga$population[rga$evaluations == min(rga$evaluations), ]
    print(bs)
    
    
    
    
    return(bs)
  }
  
  
  ##################### MONTECARLO_SEARCH #################
  montecarlo <- function(eval_max, lower, upper, N, type){
    
    MC <- mcsearch(fn = eval_max, lower = lower, upper = upper, N = N, type = type)
    
    best_solution <- round(MC$sol)
    
    cat("\n ******** MONTECARLO ******\n")
    cat("Melhor solução:", best_solution, "Função de avaliação:", MC$eval, " (encontrado na iteração:", MC$index, ")\n")
  
    
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
    SA <- optim(par = s0, fn = eval_min, method = "SANN", gr = rchange2, control = CSANN)
    
    cat("Melhor solução encontrada:", SA$par, "Valor da função de avaliação:", -SA$value, "\n")  
    
    
    
    return(SA$par)
  }
  
  
  ###############RGBA-Genetic#####################
  RBGA = function(eval, lower, upper, N){
    
    
    rga=rbga(lower,upper,popSize=200,mutationChance=0.33,elitism=10,evalFunc=eval,iter=N)
    
    bindex=which.min(rga$evaluations)
    
   
    return(ceiling(rga$population[bindex,]))
    
  }
  
  ##################Tabu########################
  Tabu = function(){
    
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
    
    # Evaluation Function 
    F2 <- function(solution){
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
      return(-monthly_effort) # needs to be negative because tabuSearch only maximizes
    }
    
    initial_config_build <- function(config, n_bits, dimensions){
      initial_length <- length(config)
      while(length(config) - initial_length < dimensions * n_bits){
        config <- c(config, rep(0, n_bits), rep(1, n_bits))
      }
      return(config)
    }
    
    D            <- 28 # dimension
    Low          <- rep(0, 28) # Lower
    Up           <- calculate_uppers(actual_sales) # Upper
    bits_workers <<- ceiling(max(log(Up[1:12] , 2))) # Bits for Hired Workers
    bits_orders  <<- ceiling(max(log(Up[13:28], 2))) # Bits for Product Orders
    N            <- 100 # number of iterations
    size         <- 12 * bits_workers + 16 * bits_orders # solution size
    
    # Building Initial configuration
    initial_config <- c()
    
    # Building Initial configuration for Hired Workers
    initial_config <- initial_config_build(config     = initial_config, 
                              n_bits     = bits_workers, 
                              dimensions = 12)
    
    # Building Initial configuration for Product Orders
    initial_config <- initial_config_build(config     = initial_config, 
                              n_bits     = bits_orders, 
                              dimensions = 16)
    
    
    solution <- tabuSearch(size, iters = N, objFunc = evaltabu, config = initial_config, verbose = F)
    
    b  <- which.max(solution$eUtilityKeep) # best index
    bs <- solution$configKeep[b,]
    
    return(bs)
    
    
    
  }
  
  ####################### NSGA-II ###########################
  
  ##################### PARAMETERS #################
  # dimension
  D=28
  N <- 10000 #número de pesquisas
  REPORT=N/20 # report results
  
  lower <- rep(0, D) # limites inferiores
  upper <- calculate_uppers(actual_sales)# limites superiores
  # define the initial solution for hill_climbing
  
  x2 = c(8, 38, 15, 24, 7, 22, 3, 5, 4, 3, 3, 13, 134101, 134441, 30785, 11860, 159979, 367501, 141060, 122515, 26920, 83112, 45240, 27495, 94235, 139555, 83043, 65082 )
  
  
  EV=0 #  initial evaluation point is zero.
  BEST=-Inf # initial best is -Inf
  curve=rep(NA,N) # vector with the convergence values
  
  
  # # #Simulated Annealing
  if(algoritmo=="Simulated Annealing"){
    
    s <- SimulatedAnnealing(eval_min,lower,upper,x2,"max")
  }
  
  
  # RGBA genetic
  if(algoritmo=="RBGA"){
    
    s <- RBGA(eval_max,lower,upper,N=100)
  }
  
  # TabuSearch
  if(algoritmo=="Tabu"){
    
    s <- Tabu()
  }
  
  
  #Montecarlo
  
  if(algoritmo=="Montecarlo"){
    s <- montecarlo(eval_max,lower,upper,N,"max")
  }
  
  
  #RGBABIN
  
  if(algoritmo=="RBGA.BIN"){
    s <- RBGABIN()
  }
  
  
  
  #Hill_Climbing
  
  
  if(algoritmo=="Hill Climbing"){
    s <- hill_climbing(eval_max,lower, upper, N, "max", x2, REPORT)
  }
  
  if(algoritmo != "RBGA.BIN"){
    hired_workers  <- matrix(s[1:12] , nrow = 3, ncol = 4)
    product_orders <- matrix(s[13:28], nrow = 4, ncol = 4)
    sales          <- calculate_sales(actual_sales, hired_workers, product_orders)
    monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
  }
  
  
  if(algoritmo %in% c("RBGA.BIN", "Tabu")){
    
    
    
    hired_workers  <- matrix(matrix_transform(solution        = s,
                                              start           = 1, 
                                              elements        = 12,
                                              dimension_start = 1,
                                              bits            = bits_workers), nrow = 3, ncol = 4)
    
    product_orders <- matrix(matrix_transform(solution        = s, 
                                              start           = 12 * bits_workers + 1, 
                                              elements        = 16, 
                                              dimension_start = 13,
                                              bits            = bits_orders), nrow = 4, ncol = 4)
    
    sales          <- calculate_sales(actual_sales, hired_workers, product_orders)
    monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)
    
  }
  
 
  
  cat("Best Solution: \nHired Workers \n")
  print(hired_workers) 
  cat("\nProduct Orders \n")
  print(product_orders) 
  cat("\nSales \n")
  print(sales)
  cat("\nMonthly Profit:", monthly_profit,"\n")
  
  return(list(
    hired_workers = hired_workers,
    product_orders = product_orders,
    sales = sales,
    monthly_profit = monthly_profit
  ))
  
}


#-------------MultiObjetivo
#-------------BestSolution

