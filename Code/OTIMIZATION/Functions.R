# definir as vendas da semana
actual_sales <- data.frame(
  WSdep1 = c(54480,42221,36267,35283),
  WSdep2 = c(159460,156945,146388,132156),
  WSdep3 = c(63584,62888,62768,60279),
  WSdep4 = c(127009,124560,123346,117375)
)
# Adicionando os índices
rownames(actual_sales) <- c(139, 140, 141, 142)


# definir os Funcionarios contratados
hired_workers <- data.frame(
  WSdep1 = c(5,6,7),
  WSdep2 = c(4,5,6),
  WSdep3 = c(3,4,5),
  WSdep4 = c(2,3,4)
)

hired_workers = as.matrix(hired_workers)


# definir as encomendas de produtos
product_orders <- data.frame(
  WSdep1 = c(61662,0,12985,39924),
  WSdep2 = c(78292,0,55403,75160),
  WSdep3 = c(56434,0,69133,62131),
  WSdep4 = c(24182,0,37167,99708)
)
product_orders = as.matrix(product_orders)



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


