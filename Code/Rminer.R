#TREINAR COM 139 E TESTAR AS ULTIMAS 4 SEMANAS - Rminer

library(rminer) # by Paulo Cortez


# read data:
cat("read passenger time series:")
d=read.csv("walmart.csv",header=TRUE,sep=",")

d1=d[,4] # d1 departamento 1
d2=d[,5] # d2 departamento 2
d3=d[,6] # d1 departamento 3
d4=d[,7] # d1 departamento 4



# Definir as Variaveis

L=length(d1) # Numero de semanas que temos -> 143
K = 4 # Sazonalidade dos dados (achamos que eles se repetem de mês em mês)
H = 4 # Numero de previsões que vamos fazer (4 ultimas semanas)



Rminer <- function(departamento, numeroDepartamento){
  
  srange=diff(range(departamento)) # compute the range of S
  print(srange)
  
  # O rminer so aceita dataframes, temos de usar o CaseSeries para converter o TS para DataFrame
  D = CasesSeries(departamento,c(1:4))
  
  N=nrow(D) # ultimo indice dos dados 
  NTR=N-H   # ultimo indice dos dados de Treino
  TR=1:NTR # Indice dos Dados de treino
  TS=(NTR+1):N # Indice dos dados de teste
  
  # Vamos fazer o fit com os dados de treino
  #y~        - dizer que as variaveis que vamos prever são derivadas do D (que esta à frente)
  #D[TR, ]   - Quais os dados que vão ser usados para dar fit
  #model     - qual o modelo é que vamos usar (mlpe = Neural Network)
  #search    - método de pesquisa a utilizar
  
  # fit a Neural Network (NN) - multilayer perceptron ensemble with training data: 
  mpause("fit a neural network (mlpe):")
  NN=fit(y~.,D[TR,],model="mlpe",search="heuristic")
  
  # fit a random forest
  mpause("fit a random forest (randomForest):")
  RF=fit(y~.,D[TR,],model="randomForest",search="heuristic")
  
  
  #Vamos prever as 4 semanas seguintes (que vai corresponder ao TS)
  #Indices
  
  LTS=length(TS) # length of the test set
  START=nrow(D)-LTS+1 # START is the row from D of the first test example
  #Prever para a Neural Network
  PNN=lforecast(NN,D,start=START,horizon=LTS) # from 1 to TS-ahead predictions
  
  #Prever para o RandomForest
  PRF=lforecast(RF,D,start=START,horizon=LTS)
  
  # Guardar os valores reais em Y do TS
  Y=D[TS,]$y # valores reais
  
  
  # show forecasting measures and graph:
  cat("Departamento - ", numeroDepartamento," \n NN (MLP) predictions:\n")
  print(PNN)
  cat("MAE:",mmetric(Y,PNN,metric="MAE"),"\n")
  cat("NMAE:",mmetric(Y,PNN,metric="NMAE",val=srange),"\n")
  cat("RMSE:",mmetric(Y,PNN,metric="RMSE"),"\n")
  cat("RRSE:",mmetric(Y,PNN,metric="RRSE"),"\n")
  cat("R2:",mmetric(Y,PNN,metric="R22"),"\n") # press R measure
  
  cat("Departamento - ", numeroDepartamento,"RF predictions:\n")
  print(PRF)
  cat("MAE:",mmetric(Y,PRF,metric="MAE"),"\n")
  cat("NMAE:",mmetric(Y,PRF,metric="NMAE",val=srange),"\n")
  cat("RMSE:",mmetric(Y,PRF,metric="RMSE"),"\n")
  cat("RRSE:",mmetric(Y,PRF,metric="RRSE"),"\n")
  cat("R2:",mmetric(Y,PRF,metric="R22"),"\n") # press R measure
  
  # graph: REG - simple Regression Plot For Neural Network
  mae=mmetric(Y,PNN,metric="MAE")
  nmae=mmetric(Y,PNN,metric="NMAE",val=srange)
  r2=mmetric(Y,PNN,metric="R22")
  print("Graph with NN predictions (1-ahead):")
  main=paste("Numero do Departamento", numeroDepartamento, "\n NEURAL NETWORK pred. (MAE=",round(mae,digits=1),", NMAE=",round(nmae,digits=0),"%, R^2=",round(r2,digits=2),")",sep="")
  mgraph(Y,PNN,main=main,graph="REG",Grid=10,lty=1,col=c("black","blue"),leg=list(pos="topright",leg=c("target","predictions")))
  
  mpause()
  
  # graph: REG - simple Regression Plot for Random Forest
  mae=mmetric(Y,PRF,metric="MAE")
  nmae=mmetric(Y,PRF,metric="NMAE",val=srange)
  r2=mmetric(Y,PRF,metric="R22")
  print("Graph with PRF predictions (1-ahead):")
  main=paste("Numero do Departamento", numeroDepartamento, "\n FORECAST (MAE=",round(mae,digits=1),", NMAE=",round(nmae,digits=0),"%, R^2=",round(r2,digits=2),")",sep="")
  mgraph(Y,PRF,main=main,graph="REG",Grid=10,lty=1,col=c("black","blue"),leg=list(pos="topright",leg=c("target","predictions")))
  
  mpause()
  
  
}


Rminer(d1,"1")
Rminer(d2,"2")
Rminer(d3,"3")
Rminer(d4,"4")

