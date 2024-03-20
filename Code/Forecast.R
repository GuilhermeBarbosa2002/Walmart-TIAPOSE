  
  #TREINAR COM 139 E TESTAR AS ULTIMAS 4 SEMANAS - FORECAST
  
library(forecast) # access forecast functions -> HoltWinters, forecast
library(rminer) # para termos a função mpause(), apenas!

# read data:
cat("read passenger time series:")
d=read.csv("walmart.csv",header=TRUE,sep=",")

d1=d[,4] # d1 -departamento 1
d2=d[,5] # d2 departamento 2
d3=d[,6] # d1 departamento 3
d4=d[,7] # d1 departamento 4

# Definir as Variaveis

L=length(d1) # Numero de semanas que temos -> 143
K = 4 # Sazonalidade dos dados (achamos que eles se repetem de mês em mês)
H = 4 # Numero de previsões que vamos fazer (4 ultimas semanas)


Forecast <- function(departamento, numeroDepartamento){
            
  # (Os indices)
  # Para ficarmos com os dados de treino 
  NTR = L - H  # numero de dados de treino        
  
  TR = 1: NTR  # ficar com os dados de treino
  
  # Para ficarmos com os dados de teste
  TS = (NTR + 1):L
  

  # Vamos fazer o fit com os dados de treino
  #Neste caso tamos a usar o HoltWinter, mas podemos mudar para o que quisermos:
  
  #ARIMA = auto.arima(ts(departamento[TS], frequency = K))
  #Rede_Neuronal = nnetar(ts(departamento[TS], frequency = K), P=2, repeats=100, size = 50, decay = 0.1) | ATENCAO NAO TA A DAR
  

  HW = HoltWinters(ts(departamento[TR], frequency = K))
 # plot(HW)
  

  #Vamos prever as 4 semanas seguintes (que vai corresponder ao TS)
  FHW = forecast(HW,h=4)  
  
  #Vamos prever para os dados teste e vamos calcular o RMSE
  plot(FHW)
  
  #Vamos calcular o RMSE

  # Extrair os valores reais dos dados de teste
  valores_reais = departamento[TS]
  
  # Extrair as previsões dos dados de teste
  previsoes = as.numeric(FHW$mean)
  
  # Calcular o erro quadrático para cada ponto
  erros_quadraticos = (valores_reais - previsoes)^2
  
  # Calcular o RMSE
  rmse = sqrt(mean(erros_quadraticos))
  
  # Calcular o MAE
  mae = mean(abs(valores_reais - previsoes))
  
  # Calcular o NMAE
  nmae = mae / mean(valores_reais)
  
  # Calcular o RRSE
  rrse = sqrt(sum(erros_quadraticos) / sum((valores_reais - mean(valores_reais))^2))
  
  # Calcular o R2
  r2 = 1 - sum(erros_quadraticos) / sum((valores_reais - mean(valores_reais))^2)
  
  
  # Plotagem do gráfico
  plot(departamento[TS], 
       type = "l", 
       col = "blue", 
       ylim = range(c(departamento[TS], previsoes, valores_reais)),
       xlab = "Semanas", 
       ylab = "Valores",
       main = paste("Comparação entre Previsões e Valores Reais - Departamento", numeroDepartamento, "\n RMSE = ", round(rmse,2)))
  
  # Adicionando linhas para previsões e valores reais
  lines(previsoes, col = "blue")
  lines(valores_reais, col = "black")
  
  # Adicionando legenda
  legend("topright", 
         legend = c("Previsões", "Valores Reais"),
         col = c("blue", "black"), 
         lty = 1)
  
  mpause() # wait for enter

  # Mostrar métricas
  cat("RMSE para o Departamento ", numeroDepartamento, ":", rmse, "\n")
  cat("MAE para o Departamento ", numeroDepartamento, ":", mae, "\n")
  cat("NMAE para o Departamento ", numeroDepartamento, ":", nmae, "\n")
  cat("RRSE para o Departamento ", numeroDepartamento, ":", rrse, "\n")
  cat("R2 para o Departamento ", numeroDepartamento, ":", r2, "\n")
}

# Testar para todos
Forecast(d1, "1")
Forecast(d2, "2")
Forecast(d3, "3")
Forecast(d4, "4")







