library(forecast)
library(rminer)

d=read.csv("walmart.csv",header=TRUE,sep=",")
d1=d[,4] # d1 departamento 1
d2=d[,5] # d2 departamento 2
d3=d[,6] # d1 departamento 3
d4=d[,7] # d1 departamento 4


K=4


print("incremental (growing) window training demonstration:")

Test=K # H, the number of multi-ahead steps, adjust if needed
H=4
S=H # step jump: set in this case to 4 months, a quarter
Runs=8 # number of growing window iterations, adjust if needed
timelags = c(1:4)



Growing_window <- function(departamento, nomedepartamento){
  
  ts = ts(departamento,frequency = K)
  L = length(ts)
  # forecast:
  W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)
  
  YR=diff(range(ts)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  # Metricas para o Holtwinter
  H_MAE=vector(length=Runs) 
  H_NMAE=vector(length=Runs) 
  H_RMSE=vector(length=Runs) 
  H_RRSE=vector(length=Runs) 
  H_R2=vector(length=Runs) 
  
  # Metricas para o ARIMA
  A_MAE=vector(length=Runs) 
  A_NMAE=vector(length=Runs) 
  A_RMSE=vector(length=Runs) 
  A_RRSE=vector(length=Runs) 
  A_R2=vector(length=Runs) 
  
  # Metricas para o NeuralNetwork
  NN_MAE=vector(length=Runs) 
  NN_NMAE=vector(length=Runs) 
  NN_RMSE=vector(length=Runs) 
  NN_RRSE=vector(length=Runs) 
  NN_R2=vector(length=Runs) 
  
  # Metricas para o ETS
  ETS_MAE=vector(length=Runs) 
  ETS_NMAE=vector(length=Runs) 
  ETS_RMSE=vector(length=Runs) 
  ETS_RRSE=vector(length=Runs) 
  ETS_R2=vector(length=Runs) 
  
  
  
  # Inicialização das variáveis para armazenar previsões e valores reais
  previsoes_Holt <- NULL
  previsoes_Arim <- NULL
  previsoes_Ets <- NULL
  previsoes_NN <- NULL
  valores_reais <- NULL
  
  # growing window:
  for(b in 1:Runs)  # cycle of the incremental window training (growing window)
  {
    
    
    ################################# FORECAST ##################################
    H=holdout(ts,ratio=Test,mode="incremental",iter=b,window=W,increment=S)  
    trinit=H$tr[1]
    dtr=ts(ts[H$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
    
    #fit - Holtwinters
    HOLTWINTERS=suppressWarnings(HoltWinters(dtr)) # create forecasting model, suppressWarnings removes warnings from HW method
    #fit - Arima
    ARIMA = suppressWarnings(auto.arima(dtr)) 
    #fit - NeuralNetworks
    NN = nnetar(dtr,P=1,repeats=3)# create forecasting model, suppressWarnings removes warnings from HW method
    #fit - ETS
    ETS = ets(dtr) 
    
    #previsao - Holtwinters
    HOLTWINTERS_Pred=forecast(HOLTWINTERS,h=length(H$ts))$mean[1:Test] # multi-step ahead forecast
    ARIMA_Pred=forecast(ARIMA,h=length(H$ts))$mean[1:Test]
    
    #previsao - NeuralNetworks
    NN_Pred=forecast(NN,h=length(H$ts))$mean[1:Test] # multi-step ahead forecast
    ETS_Pred=forecast(ETS,h=length(H$ts))$mean[1:Test]

    
    ################################# METRICAS ################################
    H_MAE[b]=mmetric(y=ts[H$ts],x=HOLTWINTERS_Pred,metric="MAE",val=YR)
    H_NMAE[b]=mmetric(y=ts[H$ts],x=HOLTWINTERS_Pred,metric="NMAE",val=YR)
    H_RMSE[b]=mmetric(y=ts[H$ts],x=HOLTWINTERS_Pred,metric="RMSE",val=YR)
    H_RRSE[b]=mmetric(y=ts[H$ts],x=HOLTWINTERS_Pred,metric="RRSE",val=YR)
    H_R2[b]=mmetric(y=ts[H$ts],x=HOLTWINTERS_Pred,metric="R22",val=YR)
    
    A_MAE[b]=mmetric(y=ts[H$ts],x=ARIMA_Pred,metric="MAE",val=YR)
    A_NMAE[b]=mmetric(y=ts[H$ts],x=ARIMA_Pred,metric="NMAE",val=YR)
    A_RMSE[b]=mmetric(y=ts[H$ts],x=ARIMA_Pred,metric="RMSE",val=YR)
    A_RRSE[b]=mmetric(y=ts[H$ts],x=ARIMA_Pred,metric="RRSE",val=YR)
    A_R2[b]=mmetric(y=ts[H$ts],x=ARIMA_Pred,metric="R22",val=YR)
    
    NN_MAE[b]=mmetric(y=ts[H$ts],x=NN_Pred,metric="MAE",val=YR)
    NN_NMAE[b]=mmetric(y=ts[H$ts],x=NN_Pred,metric="NMAE",val=YR)
    NN_RMSE[b]=mmetric(y=ts[H$ts],x=NN_Pred,metric="RMSE",val=YR)
    NN_RRSE[b]=mmetric(y=ts[H$ts],x=NN_Pred,metric="RRSE",val=YR)
    NN_R2[b]=mmetric(y=ts[H$ts],x=NN_Pred,metric="R22",val=YR)
    
    ETS_MAE[b]=mmetric(y=ts[H$ts],x=ETS_Pred,metric="MAE",val=YR)
    ETS_NMAE[b]=mmetric(y=ts[H$ts],x=ETS_Pred,metric="NMAE",val=YR)
    ETS_RMSE[b]=mmetric(y=ts[H$ts],x=ETS_Pred,metric="RMSE",val=YR)
    ETS_RRSE[b]=mmetric(y=ts[H$ts],x=ETS_Pred,metric="RRSE",val=YR)
    ETS_R2[b]=mmetric(y=ts[H$ts],x=ETS_Pred,metric="R22",val=YR)
    
    
    # Armazenamento das previsões e valores reais
    previsoes_Holt <- c(previsoes_Holt, HOLTWINTERS_Pred)
    previsoes_Arim <- c(previsoes_Arim, ARIMA_Pred)
    previsoes_NN <- c(previsoes_NN, NN_Pred)
    previsoes_Ets <- c(previsoes_Ets, ETS_Pred)
    valores_reais <- c(valores_reais, ts[H$ts])
    
    ################################# GRAFICO ##################################
    
   # cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
   #      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts))
    
    
   #  mgraph(ts[H$ts],HOLTWINTERS_Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target","Holtwintwers","Arima")))
   #  lines(ARIMA_Pred,pch=19,cex=0.5,type="b",col="red")
   #  title(paste("Departamento ", nomedepartamento, "\n iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
   #              "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts), "\n NMAE - Holtwinter - ", H_NMAE[b], "\n NMAE - Arima - ", A_NMAE[b]))
    
  }
  
  cat("\n **  DEPARTAMENTO ",nomedepartamento, " **")
  cat("\n ------- HOLTWINTERS --------")
  cat("\n MAE: ", round(median(H_MAE),2), "\n NMAE: ", round(median(H_NMAE),2)," \n RMSE: ", round(median(H_RMSE),2),"\n RRSE: ", round(median(H_RRSE),2),"\n R2: ", round(median(H_R2),2))
  cat("\n------- ARIMA --------")
  cat("\n MAE: ", round(median(A_MAE),2), "\n NMAE: ", round(median(A_NMAE),2)," \n RMSE: ", round(median(A_RMSE),2),"\n RRSE: ", round(median(A_RRSE),2),"\n R2: ", round(median(A_R2),2))
  cat("\n------- NEURAL NETWORKS --------")
  cat("\n MAE: ", round(median(NN_MAE),2), "\n NMAE: ", round(median(NN_NMAE),2)," \n RMSE: ", round(median(NN_RMSE),2),"\n RRSE: ", round(median(NN_RRSE),2),"\n R2: ", round(median(NN_R2),2))
  cat("\n------- ETS --------")
  cat("\n MAE: ", round(median(ETS_MAE),2), "\n NMAE: ", round(median(ETS_NMAE),2)," \n RMSE: ", round(median(ETS_RMSE),2),"\n RRSE: ", round(median(ETS_RRSE),2),"\n R2: ", round(median(ETS_R2),2))
  cat("\n ---------------------------------------------- \n")
  
  
    mgraph(valores_reais,previsoes_Holt,graph="REG",Grid=10,col=c("black","blue","red", "green", "orange"),leg=list(pos="topleft",leg=c("target","Holtwintwers","Arima", "NN", "ETS")))
    lines(previsoes_Arim,pch=19,cex=0.5,type="b",col="red")
    lines(previsoes_NN,pch=19,cex=0.5,type="b",col="green")
    lines(previsoes_Ets ,pch=19,cex=0.5,type="b",col="orange")
    title(paste("Departamento ", nomedepartamento, "\n NMAE - Holtwinter - ", round(median(H_MAE),2), " | NMAE - Arima - ", round(median(A_MAE),2), "\n NMAE - NN - ", round(median(NN_MAE),2), " | NMAE - ETS - ", round(median(ETS_MAE),2)))
  
  
}



Growing_window(d1,"1")








