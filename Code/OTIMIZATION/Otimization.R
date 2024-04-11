source("functions_Otimization.R")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here

# definir as vendas da semana
actual_sales <- data.frame(
  WSdep1 = c(54480,42221,36267,35283),
  WSdep2 = c(159460,156945,146388,132156),
  WSdep3 = c(63584,62888,62768,60279),
  WSdep4 = c(127009,124560,123346,117375)
)


x = c(5,6,7,4,5,6,3,4,5,2,3,4,61662,0,12985,39924,78292,0,55403,75160,56434,0,69133,62131,24182,0,37167,99708)


## Define the eval function
eval <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales)- total_costs(hired_workers,product_orders, sales)
  return(monthly_profit)
}


F2 <- function(x){
    hired_workers = matrix(s[1:12],nrow=3,ncol=4)
    product_orders = matrix(s[13:28],nrow=4,ncol=4)
    monthly_effort  = total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)

    return(monthly_effort)
}


# dimension
D=28

N <- 1000000 # número de pesquisas
# Pesquisa de Monte Carlo com D=2 e x em [-10.4,10.4]
lower <- rep(0, D) # limites inferiores
upper <- rep(100000, D) # limites superiores
MC_min <- mcsearch(fn = eval, lower = lower, upper = upper, N = N, type = "min")

cat("Melhor solução:", round(MC$sol), "Função de avaliação:", MC$eval, " (encontrado na iteração:", MC$index, ")\n")