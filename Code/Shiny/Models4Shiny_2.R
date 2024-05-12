library(forecast)
library(rminer)
library(vars)
library(lubridate)

source("multi-utils.R")


data=read.csv("walmart.csv",header=TRUE,sep=",")
data$Week <- week(as.Date(data$Date))
K=4 
LTS=K 
fuel_price = data[,"Fuel_Price"] 
is_holiday = data[,"IsHoliday"]  
d1=data[,"WSdep1"]  
d2=data[,"WSdep2"] 
d3=data[,"WSdep3"]  
d4=data[,"WSdep4"]
week=data[,"Week"] 

#-------------Univariado

Univariado_Forecast = function(departamento, nomedepartamento, modelo, D){
d=read.csv("walmart.csv",header=TRUE,sep=",")
d1=d[,4] 
d2=d[,5]
d3=d[,6] 
d4=d[,7] 

L=length(d1)
K = 4 
H = 4 

NTR = L - D*4   
  
TR = 1: NTR 
  


Pred = NULL

dtr = ts(departamento[TR], frequency = K)

if(modelo == "Holtwinters") {
  
  HW = HoltWinters(dtr)
  Pred=forecast(HW,h=4)$mean  
}

if(modelo == "Arima") {
  
  ARIMA = suppressWarnings(auto.arima(dtr))
  Pred=forecast(ARIMA,h=4)$mean  
}

if(modelo == "NN") {
  
  NN = nnetar(dtr,P=1,repeats=3)
  Pred=forecast(NN,h=4)$mean  
}


if(modelo == "ETS") {
  
  ETS = ets(dtr) 
  Pred=forecast(ETS,h=4)$mean  
}


return(Pred)



}

Univariado_Rminer = function(departamento, nomedepartamento, modelo, D){
d=read.csv("walmart.csv",header=TRUE,sep=",")
d1=d[,4] 
d2=d[,5]
d3=d[,6] 
d4=d[,7] 


C = CasesSeries(departamento,c(1:4))

K = 4 
H = 4 

L=nrow(C)

NTR = L - D*4   

TR = 1: NTR 



Pred = NULL




LTS=4 # length of the test set
START=length(C[TR,])+1

if(modelo == "Random Forest") {
  
  RF=fit(y~.,C[TR,],model="randomForest")
  Pred=lforecast(RF,C,start=START,horizon=LTS) 
}

if(modelo == "mlpe") {
  
  MLPE=fit(y~.,C[TR,],model="mlpe")
  Pred=lforecast(MLPE,C,start=START,horizon=LTS) 
}

if(modelo == "mars") {
  
  mars <- fit(y~., C[TR,], model = "mars")
  Pred=lforecast(mars,C,start=START,horizon=LTS) 
}


if(modelo == "ksvm") {
  
  ksvm <- fit(y~., C[TR,], model = "ksvm")
  Pred=lforecast(ksvm,C,start=START,horizon=LTS)  
}


if(modelo == "xgboost") {
  
  XG=fit(y~.,C[TR,],model="xgboost")
  Pred=lforecast(XG,C,start=START,horizon=LTS)   
}


if(modelo == "lm") {
  
  LM=fit(y~.,C[TR,],model="lm")
  Pred=lforecast(LM,C,start=START,horizon=LTS)  
}


return(Pred)


}

#-------------Multivariado
Multivariado = function(departamento, nomedepartamento, modelo, D,variaveis){
  
  data=read.csv("walmart.csv",header=TRUE,sep=",")
  data$Week <- week(as.Date(data$Date))
  K=4 
  LTS=K 
  fuel_price = data[,"Fuel_Price"] 
  is_holiday = data[,"IsHoliday"]  
  d1=data[,"WSdep1"]  
  d2=data[,"WSdep2"] 
  d3=data[,"WSdep3"]  
  d4=data[,"WSdep4"]
  week=data[,"Week"] 
  Test=K
  H=4
  
  ts = ts(departamento,frequency = K)
  L = length(ts)
  NTR = L - D   
  
  TR = 1: NTR 
  
  cdata <- variaveis

  
  mtr=ts(cdata[TR,],frequency=K)

    
  
  if(modelo == "autoVAR"){
    mvar=autoVAR(mtr,LAGMAX=4)
    Pred = forecastVAR(mvar,h=LTS)
    Pred = Pred[[1]]
  }
  if(modelo == "arimax"){
    arimax=autoARIMAX(mtr, frequency=4)
    Pred=forecastARIMAX(arimax,h=LTS)
    Pred = Pred[[1]]
  }
  if(modelo == "mlpe"){
    
    VINP <- vector("list", length = length(colnames(cdata)))
    for (i in 1:length(VINP)) {
      lags <- vector("list", length(VINP))
      for (j in 1:length(VINP)) {
        if (j == i) {
          lags[[j]] <- 1:4
        } else {
          lags[[j]] <- 1
        }
      }
      VINP[[i]] <- lags
    }

    
    MNN=mfit(mtr,"mlpe",VINP)
    Pred=lforecastm(MNN,h=LTS)
    Pred=Pred[[1]]
  }
  
  plot(Pred, type="l", col="black", lwd=2, xlab="Time", ylab="Value", main=paste("Forecast for Department ", nomedepartamento, "\n ", modelo))
  legend("topright", legend="Pred", col="black", lwd=2)
  
 
}


#-------------Uniobjetivo
#-------------MultiObjetivo
#-------------BestSolution

