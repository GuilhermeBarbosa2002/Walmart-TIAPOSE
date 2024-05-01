# calcular o custo total dos colaboradores
total_cost_workers <- function(hired_workers) {
  junior <- 6000
  normal <- 8000
  senior <- 9750
  
  colaboradores_por_tipo <- rowSums(hired_workers)
  
  total_cost <- colaboradores_por_tipo[1] * junior + colaboradores_por_tipo[2] * normal + colaboradores_por_tipo[3] * senior
  return(total_cost)
}

# calcular o numero total de trabalhadores 
total_number_of_workers <- function(hired_workers) {
  total_numbers <- rowSums(hired_workers)
  total_workers <- sum(total_numbers)
  return(total_workers)
}

# calcular o maximo de produtos por departamento que podem ser produzidos pelos os hired_workers
max_product_per_dep <- function(hired_workers) {
  junior <- 4000
  normal <- 7000
  senior <- 9500
  
  scores <- numeric(ncol(hired_workers)) # Inicializando um vetor para armazenar as pontuações
  
  for(i in 1:nrow(hired_workers)){
    for(j in 1:ncol(hired_workers)){
      if(i == 1){
        scores[j] <- scores[j] + hired_workers[i,j] * junior
      }
      if(i == 2){
        scores[j] <- scores[j] + hired_workers[i,j] * normal
      }
      if(i == 3){
        scores[j] <- scores[j] + hired_workers[i,j] * senior
      }
    }
  }
  
  return(scores)
}


#calcular o custo total de encomendas
total_cost_orders <- function(product_orders) {
  d1 <- 6
  d2 <- 8
  d3 <- 9
  d4 <- 11
  
  encomendas_departamento <- colSums(product_orders)
  
  total_cost <- encomendas_departamento[1] * d1 + encomendas_departamento[2] * d2 + encomendas_departamento[3] * d3 + encomendas_departamento[4] * d4
  return(total_cost)
}

#calcular o numero de encomendas a realizar
total_number_of_orders <- function(product_orders){
  total_nonzero <- sum(product_orders != 0)
  return(total_nonzero)
}




#calcular vendas em dolares
sales_in_usd <- function(sales){
  
  preço <- c(8, 10, 12, 16)
  preço = as.matrix(preço)
  sales_usd = 0
  
  for(i in 1:ncol(sales)){
    
    
    sales_usd <- sales_usd + sum(sales[,i] * preço[i])
    
  }
  
  return(sales_usd)
}


#stock
calculate_stock <- function(product_orders, sales){
  stock<- matrix(0, nrow = 4, ncol = 4)
  for(i in 1:ncol(product_orders)){
    for(j in 1:nrow(product_orders)){
      if( j  > 1){
        stock[j,i] = product_orders[j,i]- sales[j,i] + (stock[j-1,i])
      }
      else{
        stock[j,i] = product_orders[j,i]- sales[j,i]
      }
    }
  }
  return(stock)
}

#calcular stock em dolares

calculate_stock_in_usd <- function(product_orders,sales){
  
  stockM = calculate_stock(product_orders, sales)
  
  
  price <- c(3, 5, 6, 8)
  price = as.matrix(price)
  
  stock_usd = 0
  
  for(i in 1:ncol(stockM)){
    
    stock_usd <- stock_usd + sum(stockM[,i] * price[i])
    
  }
  
  return(stock_usd)
  
  
}

# calcular custos totais

total_costs <- function(hired_workers,product_orders, sales){
  
  cost_orders = total_cost_orders(product_orders)
  cost_workers = total_cost_workers(hired_workers)
  cost_stock_in_usd = calculate_stock_in_usd(product_orders,sales)
  total = cost_orders + cost_workers + cost_stock_in_usd
  
  return(total)
}




#ultima funcao crl
calculate_sales <- function(actual_sales, hired_workers, product_orders){
  max_support_per_dep = max_product_per_dep(hired_workers)
  sales = matrix(c(16),nrow=4,ncol=4)
  stock = matrix(c(16),nrow=4,ncol=4)
  for(j in 1:ncol(sales)){
    for(i in 1:nrow(sales)){
      valor_previsto_venda = actual_sales[i,j]  #buscar o valor da posição do actual sales
      valor_encomenda = product_orders[i,j]     #buscar o valor da posição da product orders
      valor_maximo_dep = max_support_per_dep[j] #buscar o valor maximo que pode ser produzido para o departamento (i)
      
      
      if(i > 1){ # com stock
        if(valor_previsto_venda <= valor_encomenda){
          if((valor_previsto_venda) > valor_maximo_dep){
            sales[i,j] = valor_maximo_dep
            stock[i,j] = valor_encomenda - sales[i,j] + stock[i-1,j]
          }else{
            sales[i,j] = valor_previsto_venda 
            stock[i,j] = valor_encomenda - sales[i,j] + stock[i-1,j]
          }
        }else{
          if((valor_encomenda + stock[i-1,j]) >= valor_maximo_dep) {
            
            sales[i,j] = valor_maximo_dep
            stock[i,j] = valor_encomenda - sales[i,j] + stock[i-1,j]
          }else if(valor_encomenda + stock[i-1,j] <= valor_maximo_dep){
            sales[i,j] = valor_encomenda + stock[i-1,j]
            stock[i,j] = valor_encomenda - sales[i,j] + stock[i-1,j]
          }else{
            sales[i,j] = valor_encomenda
            stock[i,j] = valor_encomenda - sales[i,j] + stock[i-1,j]
          }
        }
        
      }else{   #sem stock
        if(valor_previsto_venda <= valor_encomenda){
          if(valor_previsto_venda > valor_maximo_dep){
            sales[i,j] = valor_maximo_dep
            stock[i,j] = valor_encomenda - sales[i,j]
          }else{
            sales[i,j] = valor_previsto_venda
            stock[i,j] = valor_encomenda - sales[i,j]
          }
        }else{
          if(valor_encomenda > valor_maximo_dep){
            sales[i,j] = valor_maximo_dep
            stock[i,j] = valor_encomenda - sales[i,j]
          }else{
            sales[i,j] = valor_encomenda
            stock[i,j] = valor_encomenda - sales[i,j]
          }
         
        }
        
      }
      
      
    }
  }
  
  return(sales)
}



# Funcao otimizada
calculate_uppers <- function(actual_sales) {
  # Definir os níveis de vendas para cada tipo de funcionário
  sales_levels <- c(4000, 7000, 9500) * 4

  # Identificar os departamentos
  departments <- rep(1:4, each = 4)

  # Inicializar o vetor uppers
  uppers <- rep(0, 28)

  # Calcular os valores uppers para os primeiros 12 elementos
  actual_sales_by_dep <- colSums(actual_sales)
  for (i in 1:3) {
    for (j in 1:4) {
      index <- (i - 1) * 4 + j
      uppers[index] <- calculate_number_max_func(sales_levels[i], actual_sales_by_dep[j])
    }
  }

  # Preencher os últimos 16 elementos com os totais de vendas reais de cada departamento
  for (i in 1:4) {
    index <- 12 + (i - 1) * 4
    uppers[(index + 1):(index + 4)] <- actual_sales_by_dep[i]
  }

  # Retornar o vetor uppers
  return(uppers)
}


  # funcao manual
# calculate_uppers <- function(actual_sales){
#   junior <- 4000 * 4
#   normal <- 7000 * 4
#   senior <- 9500 * 4
# 
#   uppers = rep(0,12)
#   actual_sales_by_dep = colSums(actual_sales)
# 
# 
#  ##c(5,6,7,4,5,6,3,4,5,2,3,4,61662,0,12985,39924,78292,0,55403,75160,56434,0,69133,62131,24182,0,37167,99708)
#   uppers[1] = calculate_number_max_func(junior, actual_sales_by_dep[1])
#   uppers[2] = calculate_number_max_func(junior, actual_sales_by_dep[2])
#   uppers[3] = calculate_number_max_func(junior, actual_sales_by_dep[3])
#   uppers[4] = calculate_number_max_func(junior, actual_sales_by_dep[4])
# 
#   uppers[5] = calculate_number_max_func(normal, actual_sales_by_dep[1])
#   uppers[6] = calculate_number_max_func(normal, actual_sales_by_dep[2])
#   uppers[7] = calculate_number_max_func(normal, actual_sales_by_dep[3])
#   uppers[8] = calculate_number_max_func(normal, actual_sales_by_dep[4])
# 
#   uppers[9] = calculate_number_max_func(senior, actual_sales_by_dep[1])
#   uppers[10] = calculate_number_max_func(senior, actual_sales_by_dep[2])
#   uppers[11] = calculate_number_max_func(senior, actual_sales_by_dep[3])
#   uppers[12] = calculate_number_max_func(senior, actual_sales_by_dep[4])
# 
# 
#   uppers[13] = actual_sales_by_dep[1]
#   uppers[14] = actual_sales_by_dep[1]
#   uppers[15] = actual_sales_by_dep[1]
#   uppers[16] = actual_sales_by_dep[1]
# 
#   uppers[17] = actual_sales_by_dep[2]
#   uppers[18] = actual_sales_by_dep[2]
#   uppers[19] = actual_sales_by_dep[2]
#   uppers[20] = actual_sales_by_dep[2]
# 
#   uppers[21] = actual_sales_by_dep[3]
#   uppers[22] = actual_sales_by_dep[3]
#   uppers[23] = actual_sales_by_dep[3]
#   uppers[24] = actual_sales_by_dep[3]
# 
#   uppers[25] = actual_sales_by_dep[4]
#   uppers[26] = actual_sales_by_dep[4]
#   uppers[27] = actual_sales_by_dep[4]
#   uppers[28] = actual_sales_by_dep[4]
# 
#   return(uppers)
# }

## funcao otimizidada
# sales_levels <- list(junior = junior, normal = normal, senior = senior)
#
# for (i in 1:3) {
#   for (j in 1:4) {
#     dep_index <- (i - 1) * 4 + j
#     uppers[dep_index] <- calculate_number_max_func(sales_levels[[i]], actual_sales_by_dep[j])
#   }
# }



calculate_number_max_func <- function(prod_by_func, sales_by_dep){
  number_max_func = sales_by_dep / prod_by_func
  number_max_func <- ceiling(number_max_func) # arredonda sempre para cima (42,01 -> 43)
  return(number_max_func)
  
}

