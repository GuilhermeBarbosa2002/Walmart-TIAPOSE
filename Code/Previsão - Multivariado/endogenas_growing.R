library(vars)
library(rminer)
library(forecast)
library(lubridate)

source("multi-utils.R") # load multi-variate utility forecasting functions


# load the Canada data:
data=read.csv("walmart.csv",header=TRUE,sep=",")
data$Week <- week(as.Date(data$Date))
data$Month <- month(as.Date(data$Date))

K=4 # seasonal frequency: 4 time periods per year
LTS=K #  1 month, used for the forecasting range, thus 4 forecasts
fuel_price = data[,"Fuel_Price"] # Fuel_Price
is_holiday = data[,"IsHoliday"]  # IsHoliday
d1=data[,"WSdep1"]  # d1
d2=data[,"WSdep2"]  # d2
d3=data[,"WSdep3"]  # d3
d4=data[,"WSdep4"]  # d4
week=data[,"Week"]  # Week
month=data[,"Month"] # Month


print("incremental (growing) window training demonstration:")

Test=K # H, the number of multi-ahead steps, adjust if needed
H=4
S=H # step jump: set in this case to 4 months, a quarter
Runs=8# number of growing window iterations, adjust if needed
timelags = c(1:4)



Growing_window <- function(nome_departamento_a_prever,cdata){
  
  ts = ts(cdata[,1],frequency = K)
  L = length(ts)
  # forecast:
  W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)
  
  YR=diff(range(ts)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  
  # Metricas para o VAR
  VAR_MAE=vector(length=Runs) 
  VAR_NMAE=vector(length=Runs) 
  VAR_RMSE=vector(length=Runs) 
  VAR_RRSE=vector(length=Runs) 
  VAR_R2=vector(length=Runs) 
  
  # Metricas para o ARIMAX
  ARIMAX_MAE=vector(length=Runs) 
  ARIMAX_NMAE=vector(length=Runs) 
  ARIMAX_RMSE=vector(length=Runs) 
  ARIMAX_RRSE=vector(length=Runs) 
  ARIMAX_R2=vector(length=Runs) 
  
  # Metricas para o MLPE
  ML_MAE=vector(length=Runs) 
  ML_NMAE=vector(length=Runs) 
  ML_RMSE=vector(length=Runs) 
  ML_RRSE=vector(length=Runs) 
  ML_R2=vector(length=Runs) 
  
  # Inicialização das variáveis para armazenar previsões e valores reais
  previsoes_VAR <- NULL
  previsoes_ARIMAX <- NULL
  previsoes_MLPE <- NULL
  valores_reais <- NULL

  
  # growing window:
  for(b in 1:Runs)  # cycle of the incremental window training (growing window)
  {
    
    ################################# FORECAST ##################################
    #create timelags for mlpe:
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

    
    H=holdout(ts,ratio=Test,mode="incremental",iter=b,window=W,increment=S)  
    trinit=H$tr[1]
    mtr=ts(cdata[H$tr,],frequency=K) # TS training object, uses forecast library mode!
    Y=cdata[H$ts,] # target values  
    
    #create VAR model:
    mvar=autoVAR(mtr,LAGMAX=4) # 4*K. Also default lags.pt=16 of serial.test
    
    #create ARIMAX model:
    arimax=autoARIMAX(mtr,frequency=4)
    
    #create Mlpe model:
    MNN=mfit(mtr,"mlpe",VINP)
    
    #previsao - VAR
    FV=forecastVAR(mvar,h=LTS) # similar to the forecast library function, multi-step ahead forecasts
    PredVAR=FV[[1]] # predict e
    
    #previsao - ARIMAx
    FA=forecastARIMAX(arimax,h=LTS)
    PredARIMAX=FA[[1]]
    
    #previsão - Mlpe
    FM=lforecastm(MNN,h=LTS)
    PredMLPE=FM[[1]]
    
    
    
    ################################# METRICAS ################################
    VAR_MAE[b]=mmetric(y=ts[H$ts],x=PredVAR,metric="MAE",val=YR)
    VAR_NMAE[b]=mmetric(y=ts[H$ts],x=PredVAR,metric="NMAE",val=YR)
    VAR_RMSE[b]=mmetric(y=ts[H$ts],x=PredVAR,metric="RMSE",val=YR)
    VAR_RRSE[b]=mmetric(y=ts[H$ts],x=PredVAR,metric="RRSE",val=YR)
    VAR_R2[b]=mmetric(y=ts[H$ts],x=PredVAR,metric="R22",val=YR)
    
    ARIMAX_MAE[b]=mmetric(y=ts[H$ts],x=PredARIMAX,metric="MAE",val=YR)
    ARIMAX_NMAE[b]=mmetric(y=ts[H$ts],x=PredARIMAX,metric="NMAE",val=YR)
    ARIMAX_RMSE[b]=mmetric(y=ts[H$ts],x=PredARIMAX,metric="RMSE",val=YR)
    ARIMAX_RRSE[b]=mmetric(y=ts[H$ts],x=PredARIMAX,metric="RRSE",val=YR)
    ARIMAX_R2[b]=mmetric(y=ts[H$ts],x=PredARIMAX,metric="R22",val=YR)
    
    ML_MAE[b]=mmetric(y=ts[H$ts],x=PredMLPE,metric="MAE",val=YR)
    ML_NMAE[b]=mmetric(y=ts[H$ts],x=PredMLPE,metric="NMAE",val=YR)
    ML_RMSE[b]=mmetric(y=ts[H$ts],x=PredMLPE,metric="RMSE",val=YR)
    ML_RRSE[b]=mmetric(y=ts[H$ts],x=PredMLPE,metric="RRSE",val=YR)
    ML_R2[b]=mmetric(y=ts[H$ts],x=PredMLPE,metric="R22",val=YR)
    
    
    # Armazenamento das previsões e valores reais
    previsoes_VAR <- c(previsoes_VAR, PredVAR)
    previsoes_ARIMAX <- c(previsoes_ARIMAX, PredARIMAX)
    previsoes_MLPE <- c(previsoes_MLPE, PredMLPE)
    valores_reais <- c(valores_reais, ts[H$ts])
    
    
  }
      ################################# GRAFICO ##################################
    mgraph(valores_reais, previsoes_VAR, graph = "REG", Grid = 10, col = c("black", "blue", "red"), leg = list(pos = "topleft", leg = c("target", "AutoVar", "ArimaX", "mlpe")))
    lines(previsoes_VAR, pch = 19, cex = 0.5, type = "b", col = "red")
    lines(previsoes_ARIMAX, pch = 19, cex = 0.5, type = "b", col = "green")
    lines(previsoes_MLPE, pch = 19, cex = 0.5, type = "b", col = "orange")
    title(paste("Departamento ", nome_departamento_a_prever, "\n NMAE - AUTOVAR - ", round(median(VAR_NMAE),2), "\n NMAE - ARIMAX - ", round(median(ARIMAX_NMAE),2), "\n NMAE - MLPE - ", round(median(ML_NMAE),2)))

  cat("\n **  DEPARTAMENTO ",nome_departamento_a_prever, " **")
  cat("\n ------- AUTO VAR --------")
  cat("\n MAE: ", round(median(VAR_MAE),2), "\n NMAE: ", round(median(VAR_NMAE),2)," \n RMSE: ", round(median(VAR_RMSE),2),"\n RRSE: ", round(median(VAR_RRSE),2),"\n R2: ", round(median(VAR_R2),2))
  cat("\n------- ARIMAX --------")
  cat("\n MAE: ", round(median(ARIMAX_MAE),2), "\n NMAE: ", round(median(ARIMAX_NMAE),2)," \n RMSE: ", round(median(ARIMAX_RMSE),2),"\n RRSE: ", round(median(ARIMAX_RRSE),2),"\n R2: ", round(median(ARIMAX_R2),2))
  cat("\n------- MLPE --------")
  cat("\n MAE: ", round(median(ML_MAE),2), "\n NMAE: ", round(median(ML_NMAE),2)," \n RMSE: ", round(median(ML_RMSE),2),"\n RRSE: ", round(median(ML_RRSE),2),"\n R2: ", round(median(ML_R2),2))
  cat("\n ---------------------------------------------- \n")
  
}

# Growing_window("1",d1,d2,d3,d4, week,month)
# Growing_window("1", cbind(d1,d2,d3,d4))
# Growing_window("2", cbind(d2,d1,d3,d4))
# Growing_window("3", cbind(d3,d1,d2,d4))
# Growing_window("4", cbind(d4,d1,d2,d3))

# Growing_window("3",d3,d2,d1,d4, week,month)
#("4",d4,d2,d3,d1, is_holiday,month)