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
Runs=12# number of growing window iterations, adjust if needed
timelags = c(1,2,4,8)



Growing_window <- function(departamento, nomedepartamento){
  
  ts = ts(departamento,frequency = K)
  L = length(ts)
  
  #for rminer:
  W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)
  YR=diff(range(ts)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  D=CasesSeries(ts, timelags)
  W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  
  # Metricas para o Random Forest
  R_MAE=vector(length=Runs) 
  R_NMAE=vector(length=Runs) 
  R_RMSE=vector(length=Runs) 
  R_RRSE=vector(length=Runs) 
  R_R2=vector(length=Runs) 
  
  # Metricas para o MLPE
  M_MAE=vector(length=Runs) 
  M_NMAE=vector(length=Runs) 
  M_RMSE=vector(length=Runs) 
  M_RRSE=vector(length=Runs) 
  M_R2=vector(length=Runs) 
  
  # Metricas MARS
  MR_MAE  <- vector(length = Runs) 
  MR_NMAE <- vector(length = Runs) 
  MR_RMSE <- vector(length = Runs) 
  MR_RRSE <- vector(length = Runs) 
  MR_R2   <- vector(length = Runs) 
  
  # Metricas para o Ksvm
  K_MAE  <- vector(length = Runs) 
  K_NMAE <- vector(length = Runs) 
  K_RMSE <- vector(length = Runs) 
  K_RRSE <- vector(length = Runs) 
  K_R2   <- vector(length = Runs) 
  
  # Metricas para o xgboost
  XG_MAE=vector(length=Runs) 
  XG_NMAE=vector(length=Runs) 
  XG_RMSE=vector(length=Runs) 
  XG_RRSE=vector(length=Runs) 
  XG_R2=vector(length=Runs) 
  
  # Metricas para o LM
  LM_MAE=vector(length=Runs) 
  LM_NMAE=vector(length=Runs) 
  LM_RMSE=vector(length=Runs) 
  LM_RRSE=vector(length=Runs) 
  LM_R2=vector(length=Runs) 
  
  # Inicialização das variáveis para armazenar previsões e valores reais
  previsoes_R <- NULL
  previsoes_M <- NULL
  previsoes_MR <- NULL
  previsoes_K <- NULL
  previsoes_XG <- NULL
  previsoes_LM <- NULL
  valores_reais <- NULL
  
  # growing window:
  for(b in 1:Runs)  # cycle of the incremental window training (growing window)
  {
    
    
    ################################# RMINER ##################################
    #dados
    H=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S) 
    trinit=H$tr[1]
    
    #fit Random Forest
    RF=fit(y~.,D[H$tr,],model="randomForest") # create forecasting model
    
    #fit MLPE
    MLPE=fit(y~.,D[H$tr,],model="mlpe") # create forecasting model
    
    
    #fit MARS
    naive   <- fit(y~., D[H$tr,], model = "mars") # create forecasting model
    
    #fit KSVM
    ksvm <- fit(y~.   , D[H$tr,], model = "ksvm") # create forecasting model
    
    #fit xgboost
    XG=fit(y~.,D[H$tr,],model="xgboost") # create forecasting model
    
    #fit LM
    LM=fit(y~.,D[H$tr,],model="lm") # create forecasting model
    
    
    #previsão
    Mars_Pred <- lforecast(naive, D, start = (length(H$tr) + 1), Test) # multi-step ahead forecasts
    Ksvm_Pred  <- lforecast(ksvm , D, start = (length(H$tr) + 1), Test) # multi-step ahead forecasts
    
    #previsão
    RF_Pred=lforecast(RF,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    MLPE_Pred=lforecast(MLPE,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    
    #previsão
    XG_Pred=lforecast(XG,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    LM_Pred=lforecast(LM,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    
    ################################# METRICAS ################################
    R_MAE[b]=mmetric(y=ts[H$ts],x=RF_Pred,metric="MAE",val=YR)
    R_NMAE[b]=mmetric(y=ts[H$ts],x=RF_Pred,metric="NMAE",val=YR)
    R_RMSE[b]=mmetric(y=ts[H$ts],x=RF_Pred,metric="RMSE",val=YR)
    R_RRSE[b]=mmetric(y=ts[H$ts],x=RF_Pred,metric="RRSE",val=YR)
    R_R2[b]=mmetric(y=ts[H$ts],x=RF_Pred,metric="R22",val=YR)
    
    M_MAE[b]=mmetric(y=ts[H$ts],x=MLPE_Pred,metric="MAE",val=YR)
    M_NMAE[b]=mmetric(y=ts[H$ts],x=MLPE_Pred,metric="NMAE",val=YR)
    M_RMSE[b]=mmetric(y=ts[H$ts],x=MLPE_Pred,metric="RMSE",val=YR)
    M_RRSE[b]=mmetric(y=ts[H$ts],x=MLPE_Pred,metric="RRSE",val=YR)
    M_R2[b]=mmetric(y=ts[H$ts],x=MLPE_Pred,metric="R22",val=YR)

    MR_MAE[b]  <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "MAE" , val = YR)
    MR_NMAE[b] <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "NMAE", val = YR)
    MR_RMSE[b] <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "RMSE", val = YR)
    MR_RRSE[b] <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "RRSE", val = YR)
    MR_R2[b]   <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "R22" , val = YR)
    
    K_MAE[b]  <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "MAE" , val = YR)
    K_NMAE[b] <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "NMAE", val = YR)
    K_RMSE[b] <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "RMSE", val = YR)
    K_RRSE[b] <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "RRSE", val = YR)
    K_R2[b]   <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "R22" , val = YR)
    
    
    XG_MAE[b]=mmetric(y=ts[H$ts],x=XG_Pred,metric="MAE",val=YR)
    XG_NMAE[b]=mmetric(y=ts[H$ts],x=XG_Pred,metric="NMAE",val=YR)
    XG_RMSE[b]=mmetric(y=ts[H$ts],x=XG_Pred,metric="RMSE",val=YR)
    XG_RRSE[b]=mmetric(y=ts[H$ts],x=XG_Pred,metric="RRSE",val=YR)
    XG_R2[b]=mmetric(y=ts[H$ts],x=XG_Pred,metric="R22",val=YR)
    
    LM_MAE[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="MAE",val=YR)
    LM_NMAE[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="NMAE",val=YR)
    LM_RMSE[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="RMSE",val=YR)
    LM_RRSE[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="RRSE",val=YR)
    LM_R2[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="R22",val=YR)
    
    # Armazenamento das previsões e valores reais
    previsoes_R <- c(previsoes_R, RF_Pred)
    previsoes_M <- c(previsoes_M, MLPE_Pred)
    previsoes_MR <- c(previsoes_MR, Mars_Pred)
    previsoes_K <- c(previsoes_K, Ksvm_Pred)
    previsoes_XG <- c(previsoes_XG, XG_Pred)
    previsoes_LM <- c(previsoes_LM, LM_Pred)
    valores_reais <- c(valores_reais, ts[H$ts])
    
    
  }
  
  cat("\n **  DEPARTAMENTO ",nomedepartamento, " **")
  cat("\n ------- Random Forest --------")
  cat("\n MAE: ", round(median(R_MAE),2), "\n NMAE: ", round(median(R_NMAE),2)," \n RMSE: ", round(median(R_RMSE),2),"\n RRSE: ", round(median(R_RRSE),2),"\n R2: ", round(median(R_R2),2))
  cat("\n------- MLPE --------")
  cat("\n MAE: ", round(median(M_MAE),2), "\n NMAE: ", round(median(M_NMAE),2)," \n RMSE: ", round(median(M_RMSE),2),"\n RRSE: ", round(median(M_RRSE),2),"\n R2: ", round(median(M_R2),2))
  cat("\n ------- Mars --------")
  cat("\n MAE: ", round(median(MR_MAE),2), "\n NMAE: ", round(median(MR_NMAE),2)," \n RMSE: ", round(median(MR_RMSE),2),"\n RRSE: ", round(median(MR_RRSE),2),"\n R2: ", round(median(MR_R2),2))
  cat("\n------- Ksvm --------")
  cat("\n MAE: ", round(median(K_MAE),2), "\n NMAE: ", round(median(K_NMAE),2)," \n RMSE: ", round(median(K_RMSE),2),"\n RRSE: ", round(median(K_RRSE),2),"\n R2: ", round(median(K_R2),2))
  cat("\n ------- xgboost --------")
  cat("\n MAE: ", round(median(XG_MAE),2), "\n NMAE: ", round(median(XG_NMAE),2)," \n RMSE: ", round(median(XG_RMSE),2),"\n RRSE: ", round(median(XG_RRSE),2),"\n R2: ", round(median(XG_R2),2))
  cat("\n------- LM --------")
  cat("\n MAE: ", round(median(LM_MAE),2), "\n NMAE: ", round(median(LM_NMAE),2)," \n RMSE: ", round(median(LM_RMSE),2),"\n RRSE: ", round(median(LM_RRSE),2),"\n R2: ", round(median(LM_R2),2))
  cat("\n ---------------------------------------------- \n")
  
  
  
  ################################# GRAFICO ##################################
  
  mgraph(valores_reais,previsoes_R,graph="REG",Grid=10,col=c("black","blue","red", "green", "orange", "pink", "brown"),leg=list(pos="topleft",leg=c("target","Random Forest","MLPE", "Mars", "Ksvm", "xgboost", "LM")))
  lines(previsoes_M,pch=19,cex=0.5,type="b",col="red")
  lines(previsoes_MR,pch=19,cex=0.5,type="b",col="green")
  lines(previsoes_K ,pch=19,cex=0.5,type="b",col="orange")
  lines(previsoes_XG,pch=19,cex=0.5,type="b",col="pink")
  lines(previsoes_LM ,pch=19,cex=0.5,type="b",col="brown")
  title(paste("Departamento ", nomedepartamento, "\n NMAE - RF - ", round(median(R_MAE),2), " | NMAE - MLPE - ", round(median(M_MAE),2), "\n NMAE - MARS - ", round(median(MR_MAE),2), " | NMAE - KSVM - ", round(median(K_MAE),2), "\n NMAE - XGB - ", round(median(XG_MAE),2), " | NMAE - LM - ", round(median(LM_MAE),2)))
  
  
}



Growing_window(d1,"1")








