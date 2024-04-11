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
H=4 # Numero de previsões que vamos fazer (4 ultimas semanas)
S=H # step jump: set in this case to 4 months, a quarter
Runs=8 # number of growing window iterations, adjust if needed
timelags = c(1:4)



Growing_window <- function(departamento, nomedepartamento){
  
  ts = ts(departamento,frequency = K)
  L = length(ts)
  
  #for rminer:
  W=(L-Test)-(Runs-1)*S # initial training window size for the ts space (forecast methods)
  YR=diff(range(ts)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  D=CasesSeries(ts, timelags)
  W2=W-max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  
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
    
    #fit LM
    LM=fit(y~.,D[H$tr,],model="lm") # create forecasting model
    
    #previsão
    LM_Pred=lforecast(LM,D,start=(length(H$tr)+1),Test) # multi-step ahead forecasts
    
    
    ################################# METRICAS ################################
    
    LM_MAE[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="MAE",val=YR)
    LM_NMAE[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="NMAE",val=YR)
    LM_RMSE[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="RMSE",val=YR)
    LM_RRSE[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="RRSE",val=YR)
    LM_R2[b]=mmetric(y=ts[H$ts],x=LM_Pred,metric="R22",val=YR)
    
    
    pred = paste0("pred", nomedepartamento)
    assign(pred, c(round(median(LM_MAE),2), round(median(LM_NMAE),2),round(median(LM_RMSE),2), round(median(LM_RRSE),2), round(median(LM_R2),2)),envir = .GlobalEnv)
    
    
    ################################# GRAFICO ##################################
    
    #cat("iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
    #    "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts))
    #
    #mgraph(ts[H$ts],XG_Pred,graph="REG",Grid=10,col=c("black","blue","red"),leg=list(pos="topleft",leg=c("target","xgboost","lm")))
    #lines(LM_Pred,pch=19,cex=0.5,type="b",col="red")
    #title(paste("Departamento ", nomedepartamento, "\n iter:",b,"TR from:",trinit,"to:",(trinit+length(H$tr)-1),"size:",length(H$tr),
    #            "TS from:",H$ts[1],"to:",H$ts[length(H$ts)],"size:",length(H$ts), "\n NMAE - xgboost - ", XG_NMAE[b], "\n NMAE - LM - ", LM_NMAE[b]))
    #
    #mpause() # wait for enter
    
    
  }
  
  cat("\n **  DEPARTAMENTO ",nomedepartamento, " **")
  cat("\n------- LM --------")
  cat("\n MAE: ", round(median(LM_MAE),2), "\n NMAE: ", round(median(LM_NMAE),2)," \n RMSE: ", round(median(LM_RMSE),2),"\n RRSE: ", round(median(LM_RRSE),2),"\n R2: ", round(median(LM_R2),2))
  cat("\n ---------------------------------------------- \n")
  
  
}



Growing_window(d1,"1")
Growing_window(d2,"2")
Growing_window(d3,"3")
Growing_window(d4,"4")





seasonalnaive <- function(departamento, numeroDepartamento){
 
  L = 143
  
  NTR = L - H  # numero de dados de treino        
  TR = 1: NTR  # ficar com os dados de treino
  TS = (NTR + 1):L #teste
  YR=diff(range(departamento))
   
  ts = ts(departamento[TR],frequency = K)

  #Vamos prever as 4 semanas seguintes (que vai corresponder ao TS)
  SN = snaive(ts,h=4)
  
  # Extrair os valores reais dos dados de teste
  valores_reais = departamento[TS]
  
  # Extrair as previsões dos dados de teste
  previsoes = as.numeric(SN$mean)

  LM_MAE=mmetric(valores_reais,previsoes,metric="MAE")
  LM_NMAE=mmetric(valores_reais,previsoes,metric="NMAE",val=YR)
  LM_RMSE=mmetric(valores_reais,previsoes,metric="RMSE")
  LM_RRSE=mmetric(valores_reais,previsoes,metric="RRSE")
  LM_R2=mmetric(valores_reais,previsoes,metric="R2")
  
  pre = paste0("pre", numeroDepartamento)
  assign(pre, c(LM_MAE, LM_NMAE, LM_RMSE, LM_RRSE,LM_R2),envir = .GlobalEnv)
  

  cat("\n **  DEPARTAMENTO ",numeroDepartamento, " **")
  cat("\n------- Snaive --------")
  cat("\n MAE: ", round(median(LM_MAE),2), "\n NMAE: ", round(median(LM_NMAE),2)," \n RMSE: ", round(median(LM_RMSE),2),"\n RRSE: ", round(median(LM_RRSE),2),"\n R2: ", round(median(LM_R2),2))
  cat("\n ---------------------------------------------- \n")
  
}

seasonalnaive(d1,"1")
seasonalnaive(d2,"2")
seasonalnaive(d3,"3")
seasonalnaive(d4,"4")


MAE_Comparacao <- ((pre1[1] - pred1[1]) / pre1[1]) * 100
NMAE_Comparacao <- ((pre1[2] - pred1[2]) / pre1[2]) * 100
RMSE_Comparacao <- ((pre1[3] - pred1[3]) / pre1[3]) * 100
RRSE_Comparacao <- ((pre1[4] - pred1[4]) / pre1[4]) * 100
R2_Comparacao <- ((pre1[5] - pred1[5]) / pre1[5]) * 100


DMAE_Comparacao <- ((pre2[1] - pred2[1]) / pre2[1]) * 100
DNMAE_Comparacao <- ((pre2[2] - pred2[2]) / pre2[2]) * 100
DRMSE_Comparacao <- ((pre2[3] - pred2[3]) / pre2[3]) * 100
DRRSE_Comparacao <- ((pre2[4] - pred2[4]) / pre2[4]) * 100
DR2_Comparacao <- ((pre2[5] - pred2[5]) / pre2[5]) * 100

TMAE_Comparacao <- ((pre3[1] - pred3[1]) / pre3[1]) * 100
TNMAE_Comparacao <- ((pre3[2] - pred3[2]) / pre3[2]) * 100
TRMSE_Comparacao <- ((pre3[3] - pred3[3]) / pre3[3]) * 100
TRRSE_Comparacao <- ((pre3[4] - pred3[4]) / pre3[4]) * 100
TR2_Comparacao <- ((pre3[5] - pred3[5]) / pre3[5]) * 100

QMAE_Comparacao <- ((pre4[1] - pred4[1]) / pre4[1]) * 100
QNMAE_Comparacao <- ((pre4[2] - pred4[2]) / pre4[2]) * 100
QRMSE_Comparacao <- ((pre4[3] - pred4[3]) / pre4[3]) * 100
QRRSE_Comparacao <- ((pre4[4] - pred4[4]) / pre4[4]) * 100
QR2_Comparacao <- ((pre4[5] - pred4[5]) / pre4[5]) * 100


# Definir os valores das diferenças percentuais
d1 <- c(MAE_Comparacao, NMAE_Comparacao, RMSE_Comparacao, RRSE_Comparacao, R2_Comparacao)
d2 <- c(DMAE_Comparacao, DNMAE_Comparacao, DRMSE_Comparacao, DRRSE_Comparacao, DR2_Comparacao)
d3 <- c(TMAE_Comparacao, TNMAE_Comparacao, TRMSE_Comparacao, TRRSE_Comparacao, TR2_Comparacao)
d4 <- c(QMAE_Comparacao, QNMAE_Comparacao, QRMSE_Comparacao, QRRSE_Comparacao, QR2_Comparacao)

# Organizar os valores em uma matriz
tabela <- matrix(c(d1, d2, d3, d4), nrow = 5, byrow = TRUE)

# Definir os nomes das linhas e colunas
linhas <- c("MAE", "NMAE", "RMSE", "RRSE", "R2")
colunas <- c("d1", "d2", "d3", "d4")
rownames(tabela) <- linhas
colnames(tabela) <- colunas

print(tabela)

