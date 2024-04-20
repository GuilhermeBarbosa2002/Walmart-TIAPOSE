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
eval <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  return(monthly_profit)
}


F2 <- function(s){
    hired_workers = matrix(s[1:12],nrow=3,ncol=4)
    product_orders = matrix(s[13:28],nrow=4,ncol=4)
    monthly_effort  = total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)

    return(monthly_effort)
}

##################### PARAMETERS #################
# dimension
D=28
N <- 100# número de pesquisas
REPORT=N/20 # report results

lower <- rep(0, D) # limites inferiores
upper <- calculate_uppers(actual_sales)# limites superiores
# define the initial solution for hill_climbing
x = c(5,6,7,4,5,6,3,4,5,2,3,4,61662,0,12985,39924,78292,0,55403,75160,56434,0,69133,62131,24182,0,37167,99708)
x1 = c(5 ,37, 13, 23, 5, 19, 1, 5, 2, 2, 2, 11, 133982, 134325, 30651, 11723, 159884, 367385, 140936, 122379, 26785, 82987, 45094, 27352, 94076, 139434, 82896, 64937)
x2 = c(8, 38, 15, 24, 7, 22, 3, 5, 4, 3, 3, 13, 134101, 134441, 30785, 11860, 159979, 367501, 141060, 122515, 26920, 83112, 45240, 27495, 94235, 139555, 83043, 65082 )



##################### MONTECARLO_SEARCH #################
montecarlo <- function(eval, lower, upper, N, type){
  MC <- mcsearch(fn = eval, lower = lower, upper = upper, N = N, type = type)
  cat("\n ******** MONTECARLO ******\n")
  cat("Melhor solução:", round(MC$sol), "Função de avaliação:", MC$eval, " (encontrado na iteração:", MC$index, ")\n")
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
  HC=hclimbing(par=s0,fn=eval,change=rchange1,lower=lower,upper=upper,type=type,
               control=list(maxit=N,REPORT= 0, digits=2))
  cat("\n ******** HILL CLIMBING ******\n")
  cat("best solution:",HC$sol,"evaluation function",HC$eval,"\n")
}

##################### Simulated Annealing #################  
SimulatedAnnealing <- function(eval, lower, upper, N, type){
  
  # slight change of a real par under a normal u(0,0.5) function:
  rchange2=function(par) # change for hclimbing
  { hchange(par,lower=lower,upper=upper,rnorm,mean=0,sd=0.5,round=FALSE) }
  
  cat("\n ******** Simulated Annealing ******\n")
  #cat("Simulated Annealing search D=",D,"(iters=",N,")\n")
  CSANN=list(maxit=N,temp=5,trace=TRUE)
  SA=optim(par=rep(-10.4,D),fn=eval,method="SANN",gr=rchange2,control=CSANN)
  cat("best solution:",SA$par,"evaluation function",SA$value,"\n")  
}

#Simulated Annealing
SimulatedAnnealing(eval,lower,upper,N,"max")

#Montecarlo
montecarlo(eval,lower,upper,N,"max")

#Hill_Climbing
hill_climbing(eval,lower, upper, N, "max", x, REPORT)



