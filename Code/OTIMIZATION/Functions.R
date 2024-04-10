# definir as vendas da semana
actual_sales <- data.frame(
  WSdep1 = c(54480,42221,36267,35283),
  WSdep2 = c(159460,156945,146388,132156),
  WSdep3 = c(63584,62888,62768,60279),
  WSdep4 = c(127009,124560,123346,117375)
)


s = c(5,6,7,4,5,6,3,4,5,2,3,4,61662,0,12985,39924,78292,0,55403,75160,56434,0,69133,62131,24182,0,37167,99708)
hired_workers =matrix(s[1:12],nrow=3,ncol=4)
product_orders =matrix(s[13:28],nrow=4,ncol=4)


# vendas por semana por departamento
sales <- data.frame(
  WSdep1 = c(54480,7182,12985,35283),
  WSdep2 = c(78292,0,55403,75160),
  WSdep3 = c(56434,0,62768,60279),
  WSdep4 = c(24182,0,37167,67000)
)

sales = as.matrix(sales)



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

F1 <- function(hired_workers,product_orders, sales){
  monthly_profit = sales_in_usd(sales)-total_costs(hired_workers,product_orders, sales)
  
  return(monthly_profit)
}

F2 <- function(hired_workers,product_orders){
  monthly_effort  = total_number_of_workers(hired_workers) + total_number_of_orders(product_orders)
  
  return(monthly_effort)
}






# Função EVAL

eval=function(x) - F1(s) 
  hired_workers =matrix(s[1:12],nrow=3,ncol=4)
  product_orders =matrix(s[13:28],nrow=4,ncol=4)




#ultima funcao crl
calculate_sales <- function(hired_workers, product_orders){
  max_support_per_dep = max_product_per_dep(hired_workers)
  sales = matrix(c(16),nrow=4,ncol=4)
  stock = matrix(c(16),nrow=4,ncol=4)
  for(i in 1:ncol(sales)){
    for(j in 1:nrow(sales)){
      valor_previsto_venda = actual_sales[i,j]  #buscar o valor da posição do actual sales
      valor_encomenda = product_orders[i,j]     #buscar o valor da posição da product orders
      valor_maximo_dep = max_support_per_dep[i] #buscar o valor maximo que pode ser produzido para o departamento (i)
      if(i > 1){ # com stock
        
    
    
      }else{   #sem stock
        if(valor_previsto_venda <= valor_encomenda){
          sales[i,j] = valor_previsto_venda
          print(valor_maximo_dep) 
          print(valor_encomenda)
          if(valor_maximo_dep <= valor_encomenda){
              stock[i,j] = valor_encomenda - valor_maximo_dep
          }
         
        }else{
          sales[i,j] = valor_encomenda
        }
            
      }
        
      #verificação final - não podemos encomendar mais do que aquilo que produzimos
      if(sales[i,j] > valor_maximo_dep){
          sales[i,j] = valor_maximo_dep # o valor da encomenda fica o máximo que podemos encomendar
      }
    }
  }
  
  return(stock)
}
