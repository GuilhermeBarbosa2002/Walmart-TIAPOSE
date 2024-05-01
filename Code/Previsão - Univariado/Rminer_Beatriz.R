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
  
  
  # growing window:
  for(b in 1:Runs)  # cycle of the incremental window training (growing window)
  {
    
    
    ################################# RMINER ##################################
    #dados
    H=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S) 
    trinit=H$tr[1]
    
    #fit xgboost
    XG=fit(y~.,D[H$tr,],model="xgboost") # create forecasting model
    
    #fit LM
    LM=fit(y~.,D[H$tr,],model="lm") # create forecasting model
    
    #previsão
    XG_Pred=lforecast(XG,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    LM_Pred=lforecast(LM,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    
    
    ################################# METRICAS ################################
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
    
    
    ################################# GRAFICO ##################################
    
    cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
        "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts))
    
    mgraph(ts[H$ts],XG_Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target","xgboost","lm")))
    lines(LM_Pred,pch=19,cex=0.5,type="b",col="red")
    title(paste("Departamento ", nomedepartamento, "\n iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
                "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts), "\n NMAE - xgboost - ", XG_NMAE[b], "\n NMAE - LM - ", LM_NMAE[b]))
    
    mpause() # wait for enter
    
    
  }
  
  cat("\n **  DEPARTAMENTO ",nomedepartamento, " **")
  cat("\n ------- xgboost --------")
  cat("\n MAE: ", round(median(XG_MAE),2), "\n NMAE: ", round(median(XG_NMAE),2)," \n RMSE: ", round(median(XG_RMSE),2),"\n RRSE: ", round(median(XG_RRSE),2),"\n R2: ", round(median(XG_R2),2))
  cat("\n------- LM --------")
  cat("\n MAE: ", round(median(LM_MAE),2), "\n NMAE: ", round(median(LM_NMAE),2)," \n RMSE: ", round(median(LM_RMSE),2),"\n RRSE: ", round(median(LM_RRSE),2),"\n R2: ", round(median(LM_R2),2))
  cat("\n ---------------------------------------------- \n")
  
  
}



Growing_window(d1,"1")
Growing_window(d2,"2")
Growing_window(d3,"3")
Growing_window(d4,"4")


