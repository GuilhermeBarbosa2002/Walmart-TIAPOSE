source("functions_Otimization.R")
library(genalg)


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
  return(-monthly_profit)
}


iter=100
lowers = rep(0,28)
uppers = calculate_uppers(actual_sales)

rga=rbga(lowers,uppers,popSize=20,mutationChance=0.33,elitism=10,evalFunc=eval,iter=iter) 


plot(rga)
bindex=which.min(rga$evaluations)
cat("best solution:",rga$population[bindex,],"evaluation function",rga$evaluations[bindex],"\n")
