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
  
  ev=vector(length=Runs) # error vector for "HoltWinters"
  
  #for rminer:
  D=CasesSeries(ts, timelags)
  W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  ev2=vector(length=Runs) # error vector for "mlpe"
  
  
  
# growing window:
for(b in 1:Runs)  # cycle of the incremental window training (growing window)
{
  ################################# FORECAST ##################################
  #dados
  H=holdout(ts,ratio=Test,mode="incremental",iter=b,window=W,increment=S)  
  trinit=H$tr[1]
  dtr=ts(ts[H$tr],frequency=K) # create ts object, note that there is no start argument (for simplicity of the code)
  
  #fit
  M=suppressWarnings(HoltWinters(dtr)) # create forecasting model, suppressWarnings removes warnings from HW method
  
  #previsao
  
  Pred=forecast(M,h=length(H$ts))$mean[1:Test] # multi-step ahead forecast
  
  #guardar o erro
  ev[b]=mmetric(y=ts[H$ts],x=Pred,metric="NMAE",val=YR)
  
  
  ################################# RMINER ##################################
  #dados
  H2=holdout(D$y,ratio=Test,mode="incremental",iter=b,window=W2,increment=S) 
  #fit
  M2=fit(y~.,D[H2$tr,],model="mlpe") # create forecasting model
  #previsão
  Pred2=lforecast(M2,D,start=(length(H2$tr)+1),Test) # multi-step ahead forecasts
  ev2[b]=mmetric(y=ts[H2$ts],x=Pred2,metric="NMAE",val=YR)

  
  
  ################################# GRAFICO ##################################
  
  cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
      "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts),
      "nmae:",ev[b],",",ev2[b],"\n")
  
mgraph(ts[H$ts],Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target","HW pred.","mlpe")))
  lines(Pred2,pch=19,cex=0.5,type="b",col="red")
  title(paste("Departamento ", nomedepartamento, "\n iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
              "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts), "\n NMAE - Holtwinter - ", ev[b], "\n NMAE - Mlpe - ", ev2[b] ))
  mpause() # wait for enter
  
  }
  
  cat("****  DEPARTAMENTO ",nomedepartamento , "******* \n Holt-Winters median NMAE:",median(ev),"\n Mlpe median NMAE: ", median(ev2) )
  

}



Growing_window(d1,"1")
Growing_window(d2,"2")
Growing_window(d3,"3")
Growing_window(d4,"4")








