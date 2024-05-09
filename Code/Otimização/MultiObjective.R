source("functions_Otimization.R")

library(mco)


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
  hired_workers = matrix(s[1:12], nrow=3, ncol=4)
  product_orders = matrix(s[13:28], nrow=4, ncol=4)
  sales = calculate_sales(actual_sales, hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers, product_orders, sales)

  return(-monthly_profit)
}

F2 <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12], nrow=3, ncol=4)
  product_orders = matrix(s[13:28], nrow=4, ncol=4)
  monthly_effort  = total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)
  return(monthly_effort)
}

##################### PARAMETERS #################
# dimension
D <- 28


lower <- rep(0, D)  # limites inferiores
upper <- calculate_uppers(actual_sales)  # limites superiores

# define a função de objetivo como uma combinação de eval e F2
objective_function <- function(x) {
  x <- round(x)
  c(eval(x), F2(x))
}

# execute a otimização multiobjetivo
G <- nsga2(fn = objective_function, idim = D, odim = 2,
           lower.bounds = lower, upper.bounds = upper,
           popsize = 200, generations = 1:1000)

# mostrar os melhores indivíduos
I <- which(G[[100]]$pareto.optimal)
for (i in I) {
  x <- round(G[[100]]$par[i,], digits = 0)
  cat("Hired workers and product orders:", x, "\n")
  cat("Monthly profit:", eval(x), "\n")
  cat("Monthly effort:", F2(x), "\n\n")
}

# create PDF com a evolução da fronteira de Pareto
#pdf(file = "nsga-eval-F2.pdf", paper = "special", height = 5, width = 5)
par(mar = c(4.0, 4.0, 0.1, 0.1))
I <- 1:100
for (i in I) {
  P <- G[[i]]$value  # objetivos f1 e f2
  # color from light gray (75) to dark (1):
  COL <- paste("gray", round(76 - i * 0.75), sep = "")
  if (i == 1) plot(P, xlim = c(0, max(P[,1]) * 1.1), ylim = c(0, max(P[,2]) * 1.1),
                   xlab = "f1", ylab = "f2", cex = 0.5, col = COL, main = "Pareto Front Evolution")
  Pareto <- P[G[[i]]$pareto.optimal, ]
  # sort Pareto according to x axis:
  points(P, type = "p", pch = 1, cex = 0.5, col = COL)
  if (is.matrix(Pareto)) {  # if Pareto has more than 1 point
    I <- sort.int(Pareto[,1], index.return = TRUE)
    Pareto <- Pareto[I$ix, ]
    lines(Pareto, type = "l", cex = 0.5, col = COL)
  }
}
dev.off()
