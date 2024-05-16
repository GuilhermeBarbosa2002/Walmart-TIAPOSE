library(genalg)

actual_sales <- data.frame(
  WSdep1 = c(54480 , 42221 , 36267 , 35283 ),
  WSdep2 = c(159460, 156945, 146388, 132156),
  WSdep3 = c(63584 , 62888 , 62768 , 60279 ),
  WSdep4 = c(127009, 124560, 123346, 117375)
)

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
  
  return(-monthly_profit)
}

monitor <- function(obj) {
  xlim = c(100000, 0) # x-axis limits
  ylim = c(100000, 0) # y-axis limits
  COL=paste("gray",80-ITER*4,sep="") # set the points color depending on ITER
  
  for(i in 1:nrow(obj$population))
    cat(obj$population[i,]," fit:",obj$evaluations[i],"\n") # show in console all individual values and fitness
  PMAX=which.max(obj$evaluations) # show which point provides the best (lowest) value
  cat("PMIN:",PMAX,"\n")
  
  plot(obj$population, xlim=xlim, ylim=ylim, xlab="x", ylab="y",pch=19,cex=0.5,col=COL) # plot all points
  #plot(obj$population, xlim=xlim, ylim=ylim, xlab="x", ylab="y",pch=19,cex=sizepop(obj$evaluations));
  text(obj$population[PMIN,1],obj$population[PMIN,2],"min") # put the label "min" near the best point
  cat("-- generation:",ITER,"(press enter)\n");readLines(n=1) # wait for user to press enter
  ITER<<-ITER+1 # global variable ITER increase (outside this function, ITER is valid)
}


# global variables (can be used inside the functions):
D              <- 28 # dimension
Low            <- rep(0, D) # Lower
Up             <- calculate_uppers(actual_sales) # Upper
bits_workers   <- ceiling(max(log(Up[1:12] , 2))) # Bits for Hired Workers
bits_orders    <- ceiling(max(log(Up[13:28], 2))) # Bits for Product Orders
N              <- 100 # number of iterations
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
                evalFunc       = eval,
                verbose        = FALSE)

plot(rga)

bs <- rga$population[rga$evaluations == min(rga$evaluations), ]


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


monthly_effort <- total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)
sales          <- calculate_sales(actual_sales, hired_workers, product_orders)
monthly_profit <- sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)

cat("Hired Workers \n")
print(hired_workers) 
cat("\nProduct Orders \n")
print(product_orders)
cat("\nSales \n")
print(sales)
cat("\nMonthly Profit:", monthly_profit,"\n")
cat("\nMonthly Effort:", monthly_effort,"\n")
cat("\nSolution in Binary Space\n")
print(bs)