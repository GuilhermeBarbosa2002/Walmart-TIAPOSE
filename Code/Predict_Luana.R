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
Runs=12 # number of growing window iterations, adjust if needed
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
    
    #previsão
    RF_Pred=lforecast(RF,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    MLPE_Pred=lforecast(MLPE,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    
    
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
    
    
    ################################# GRAFICO ##################################
    
    cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
        "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts))
  
    mgraph(ts[H$ts],RF_Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target","Random Forest","MLPE")))
    lines(MLPE_Pred,pch=19,cex=0.5,type="b",col="red")
    title(paste("Departamento ", nomedepartamento, "\n iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
                "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts), "\n NMAE - RandomForest - ", R_NMAE[b], "\n NMAE - MLPE - ", M_NMAE[b]))
    
     mpause() # wait for enter
    

  }
  
  cat("\n **  DEPARTAMENTO ",nomedepartamento, " **")
  cat("\n ------- Random Forest --------")
  cat("\n MAE: ", round(median(R_MAE),2), "\n NMAE: ", round(median(R_NMAE),2)," \n RMSE: ", round(median(R_RMSE),2),"\n RRSE: ", round(median(R_RRSE),2),"\n R2: ", round(median(R_R2),2))
  cat("\n------- MLPE --------")
  cat("\n MAE: ", round(median(M_MAE),2), "\n NMAE: ", round(median(M_NMAE),2)," \n RMSE: ", round(median(M_RMSE),2),"\n RRSE: ", round(median(M_RRSE),2),"\n R2: ", round(median(M_R2),2))
  cat("\n ---------------------------------------------- \n")
  
  
}



Growing_window(d1,"1")
Growing_window(d2,"2")
Growing_window(d3,"3")
Growing_window(d4,"4")








