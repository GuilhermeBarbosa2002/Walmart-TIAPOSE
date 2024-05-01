library(forecast)
library(rminer)

d=read.csv("walmart.csv",header=TRUE,sep=",")
d1=d[,4] # d1 departamento 1
d2=d[,5] # d2 departamento 2
d3=d[,6] # d1 departamento 3
d4=d[,7] # d1 departamento 4


K=1


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
  
  #for rminer:
  D=CasesSeries(ts, timelags)
  W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
 
  MAE=vector(length=Runs) 
  NMAE=vector(length=Runs) 
  RMSE=vector(length=Runs) 
  RRSE=vector(length=Runs) 
  R2=vector(length=Runs) 
  
  
  
  # growing window:
  for(b in 1:Runs)  # cycle of the incremental window training (growing window)
  {
    
    
    ################################# RMINER ##################################
    #dados
    H2=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S) 
    trinit=H2$tr[1]
    #fit
    M2=fit(y~.,D[H2$tr,],model="mlpe") # create forecasting model
    #previsão
    Pred2=lforecast(M2,D,start=(length(H2$tr)+1),Test) # multi-step ahead forecasts
    
    ################################# METRICAS ################################
    MAE[b]=mmetric(y=ts[H2$ts],x=Pred2,metric="MAE",val=YR)
    NMAE[b]=mmetric(y=ts[H2$ts],x=Pred2,metric="NMAE",val=YR)
    RMSE[b]=mmetric(y=ts[H2$ts],x=Pred2,metric="RMSE",val=YR)
    RRSE[b]=mmetric(y=ts[H2$ts],x=Pred2,metric="RRSE",val=YR)
    R2[b]=mmetric(y=ts[H2$ts],x=Pred2,metric="R22",val=YR)
    
    
    
    ################################# GRAFICO ##################################
    
    cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H2$tr)-1),"size:",length(H2$tr),
        "TS from:",H2$ts[1],"to:",H2$ts[length(H2$ts)],"size:",length(H2$ts))
    
    mgraph(ts[H2$ts],Pred2,graph="REG",Grid=10,col=c("black","red"),leg=list(pos="topleft",leg=c("target","mlpe")))
    title(paste("Departamento ", nomedepartamento, "\n iter:", b, "TR from:", trinit, "to:", (trinit + length(H2$tr) - 1), "size:", length(H2$tr),
                       "TS from:", H2$ts[1], "to:", H2$ts[length(H2$ts)], "size:", length(H2$ts), " \n MAE: ", round(MAE[b],2), "  NMAE: ", round(NMAE[b],2),  "\n RMSE: ", round(RMSE[b],2), " RRSE:", round(RRSE[b],2), "R2: ", R2[b]))
    mpause() # wait for enter
  }
  
  cat("**  DEPARTAMENTO ",nomedepartamento, "\n Mlpe median MAE: ", round(median(MAE),2), "\n Mlpe median NMAE: ", round(median(NMAE),2)," \n Mlpe median RMSE: ", round(median(RMSE),2),"\n Mlpe median RRSE: ", round(median(RRSE),2),"\n Mlpe median R2: ", median(R2) )
  
  
}



Growing_window(d1,"1")
Growing_window(d2,"2")
Growing_window(d3,"3")
Growing_window(d4,"4")








