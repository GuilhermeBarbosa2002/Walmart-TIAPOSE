library(forecast)
library(rminer)

d  <- read.csv("walmart.csv", header = TRUE, sep = ",")
d1 <- d[,4] # d1 departamento 1
d2 <- d[,5] # d2 departamento 2
d3 <- d[,6] # d1 departamento 3
d4 <- d[,7] # d1 departamento 4

K        <- 4
Test     <- K # H, the number of multi-ahead steps, adjust if needed
H        <- 4
S        <- H # step jump: set in this case to 4 months, a quarter
Runs     <- 12 # number of growing window iterations, adjust if needed
timelags <- c(1:4)



Models <- function(departamento, nomedepartamento){
  
  ts <- ts(departamento, frequency = K)
  L  <- length(ts)
  
  #for rminer:
  W  <- (L - Test) - (Runs - 1) * S # initial training window size for the ts space (forecast methods)
  YR <- diff(range(ts)) # global Y range, use the same range for the NMAE calculation in all iterations
  
  D  <- CasesSeries(ts, timelags)
  W2 <- W - max(timelags) # initial training window size for the D space (CasesSeries, rminer methods)
  
  # Metricas para o Naive
  M_MAE  <- vector(length = Runs) 
  M_NMAE <- vector(length = Runs) 
  M_RMSE <- vector(length = Runs) 
  M_RRSE <- vector(length = Runs) 
  M_R2   <- vector(length = Runs) 
  
  # Metricas para o Ksvm
  K_MAE  <- vector(length = Runs) 
  K_NMAE <- vector(length = Runs) 
  K_RMSE <- vector(length = Runs) 
  K_RRSE <- vector(length = Runs) 
  K_R2   <- vector(length = Runs) 
  
  for(b in 1:Runs){
    ################################# RMINER ##################################
    #dados
    H      <- holdout(D$y, ratio = Test, mode = "incremental", iter = b, window = W2, increment = S) 
    trinit <- H$tr[1]
    
    #fit Naive
    naive   <- fit(y~., D[H$tr,], model = "mars") # create forecasting model
    
    #fit MLPE
    ksvm <- fit(y~.   , D[H$tr,], model = "ksvm") # create forecasting model
    
    #previsão
    Mars_Pred <- lforecast(naive, D, start = (length(H$tr) + 1), Test) # multi-step ahead forecasts
    Ksvm_Pred  <- lforecast(ksvm , D, start = (length(H$tr) + 1), Test) # multi-step ahead forecasts
    
    
    ################################# METRICAS ################################
    M_MAE[b]  <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "MAE" , val = YR)
    M_NMAE[b] <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "NMAE", val = YR)
    M_RMSE[b] <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "RMSE", val = YR)
    M_RRSE[b] <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "RRSE", val = YR)
    M_R2[b]   <- mmetric(y = ts[H$ts], x = Mars_Pred, metric = "R22" , val = YR)
    
    K_MAE[b]  <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "MAE" , val = YR)
    K_NMAE[b] <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "NMAE", val = YR)
    K_RMSE[b] <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "RMSE", val = YR)
    K_RRSE[b] <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "RRSE", val = YR)
    K_R2[b]   <- mmetric(y = ts[H$ts], x = Ksvm_Pred, metric = "R22" , val = YR)
    
    
    ################################# GRAFICO ##################################
    
    cat("iter:", b,
        "TR from:", trinit , "to:", (trinit + length(H$tr) - 1), "size:", length(H$tr),
        "TS from:", H$ts[1], "to:", H$ts[length(H$ts)]         , "size:", length(H$ts))
    
    mgraph(ts[H$ts], Mars_Pred, graph = "REG", Grid = 10, 
           col=c("black", "blue", "red"), 
           leg = list(pos = "topleft", leg = c("target", "Mars", "Ksvm")))
    
    lines(Ksvm_Pred, pch = 19, cex = 0.5, type = "b", col = "red")
    title(paste("Departamento ", nomedepartamento, 
                "\n iter:", b,
                "TR from:", trinit , "to:", (trinit + length(H$tr) - 1), "size:", length(H$tr),
                "TS from:", H$ts[1], "to:", H$ts[length(H$ts)]         , "size:", length(H$ts), 
                "\n NMAE - Mars - ", M_NMAE[b], 
                "\n NMAE - Ksvm - ", K_NMAE[b]))
    
    mpause() # wait for enter
    
    
  }
  
  cat("\n **  DEPARTAMENTO ",nomedepartamento, " **")
  cat("\n ------- Mars --------")
  cat("\n MAE: ", round(median(M_MAE),2), "\n NMAE: ", round(median(M_NMAE),2)," \n RMSE: ", round(median(M_RMSE),2),"\n RRSE: ", round(median(M_RRSE),2),"\n R2: ", round(median(M_R2),2))
  cat("\n------- Ksvm --------")
  cat("\n MAE: ", round(median(K_MAE),2), "\n NMAE: ", round(median(K_NMAE),2)," \n RMSE: ", round(median(K_RMSE),2),"\n RRSE: ", round(median(K_RRSE),2),"\n R2: ", round(median(K_R2),2))
  cat("\n ---------------------------------------------- \n")
  
  
}

Models(d1,"1")
Models(d2,"2")
Models(d3,"3")
Models(d4,"4")
