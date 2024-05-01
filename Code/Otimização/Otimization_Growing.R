source("functions_Otimization.R")
source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here
source("hill.R") #  hclimbing is defined here
source("grid.R") #  gsearch is defined here


##################### MONTECARLO_SEARCH #################
montecarlo <- function(eval, lower, upper, N, type){
  MC <- mcsearch(fn = eval, lower = lower, upper = upper, N = N, type = type)
  cat("\n ******** MONTECARLO ******\n")
  cat("Melhor solução:", round(MC$sol), "Função de avaliação:", MC$eval, " (encontrado na iteração:", MC$index, ")\n")
  return(MC$eval)
}

## Define the eval function
eval <- function(s){
  s <- round(s)
  hired_workers = matrix(s[1:12],nrow=3,ncol=4)
  product_orders = matrix(s[13:28],nrow=4,ncol=4)
  sales = calculate_sales(actual_sales,hired_workers, product_orders)
  monthly_profit = sales_in_usd(sales) - total_costs(hired_workers,product_orders, sales)
  return(monthly_profit)
}


# Ler o arquivo CSV
dados=read.csv("walmart.csv",header=TRUE,sep=",")
dados_growing <- tail(dados, 32)
print(dados_growing)

# Selecionar apenas as colunas necessárias
dados_growing <- dados_growing[, c("WSdep1", "WSdep2", "WSdep3", "WSdep4")]

# Dividir os dados em grupos de 4 linhas
grupos <- split(dados_growing, rep(1:8, each = 4))

# Exibir a última matriz
print("Última matriz:")
print(grupos)

# Dividir os dados em grupos de 4 linhas
grupos <- split(dados_growing, rep(1:8, each = 4))


montecarlo_values=vector(length=8) 


# Loop para percorrer todas as matrizes
for (i in 1:length(grupos)) {
  
  # dimension
  D=28
  N <- 100# número de pesquisas
  REPORT=N/20 # report results
  
  lower <- rep(0,D) # limites inferiores
  upper <- calculate_uppers(grupos[[i]])# limites superiores
  actual_sales = grupos[[i]]
  
  montecarlo_values[i] = montecarlo(eval,lower,upper,N,"max")
  
}







