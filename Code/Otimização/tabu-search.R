# if needed, install this package:
# install.packages("tabuSearch")
library(tabuSearch)
library(adana)


source("C:/Users/Neon/Documents/Universidade/4º Ano/TIAPOSE/Walmart-TIAPOSE/Code/OTIMIZATION/Functions_Otimization.R")

matrix_transform <- function(solution, start, elements, dimension_start, bits){
  matrix_final <- c()
  for(i in 1:elements){
    matrix_final[i]  <- bin2int(solution[start:(start + bits - 1)])
    start <- start + bits
  }
  return(matrix_final)
}

eval <- function(solution){
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

initial_config_build <- function(config, n_bits, dimensions){
  initial_length <- length(config)
  while(length(config) - initial_length < dimensions * n_bits){
    config <- c(config, rep(0, n_bits), rep(1, n_bits))
  }
  return(config)
}

actual_sales <- data.frame(
  WSdep1 = c(54480 , 42221 , 36267 , 35283 ),
  WSdep2 = c(159460, 156945, 146388, 132156),
  WSdep3 = c(63584 , 62888 , 62768 , 60279 ),
  WSdep4 = c(127009, 124560, 123346, 117375)
)

# global variables (can be used inside the functions):
D            <- 28 # dimension
Low          <- rep(0, 28) # Lower
Up           <- calculate_uppers(actual_sales) # Upper
bits_workers <- ceiling(max(log(Up[1:12] , 2))) # Bits for Hired Workers
bits_orders  <- ceiling(max(log(Up[13:28], 2))) # Bits for Product Orders
N            <- 1000 # number of iterations
size         <- 12 * bits_workers + 16 * bits_orders # solution size

cat("Tabu Search Sphere D =", D,"( iters =", N,")\n")

# Building Initial configuration
s <- c()

# Building Initial configuration for Hired Workers
s <- initial_config_build(config     = s, 
                          n_bits     = bits_workers, 
                          dimensions = 12)

# Building Initial configuration for Product Orders
s <- initial_config_build(config     = s, 
                          n_bits     = bits_orders, 
                          dimensions = 16)

# Do the Tabu Search
s <- tabuSearch(size, iters = N, objFunc = eval, config = s, verbose = TRUE)

b  <- which.max(s$eUtilityKeep) # best index
bs <- s$configKeep[b,]

hired_workers  <- matrix(matrix_transform(solution        = bs,
                                          start           = 1, 
                                          elements        = 12,
                                          dimension_start = 1,
                                          bits            = bits_workers), nrow = 3, ncol = 4)

product_orders <- matrix(matrix_transform(solution        = bs, 
                                          start           = 12 * bits_workers + 1, 
                                          elements        = 16, 
                                          dimension_start = 13,
                                          bits            = bits_orders), nrow = 4, ncol = 4)

sales          <- calculate_sales(actual_sales, hired_workers, product_orders)
monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)

cat("Best Solution: \nHired Workers \n")
print(hired_workers) 
cat("\nProduct Orders \n")
print(product_orders) 
cat("\nSales \n")
print(sales)
cat("\nMonthly Profit:", monthly_profit,"\n")
cat("\nSolution in Binary Space\n")
print(bs)
