library(forecast)
library(rminer)

# Load the data
d <- read.csv("walmart.csv", header = TRUE, sep = ",")
d1 <- d[,4] # d1 departamento 1
d2 <- d[,5] # d2 departamento 2
d3 <- d[,6] # d3 departamento 3
d4 <- d[,7] # d4 departamento 4

K <- 4
Test <- K # H, the number of multi-ahead steps, adjust if needed
H <- 4
S <- H # step jump: set in this case to 4 months, a quarter
Runs <- 8 # number of growing window iterations, adjust if needed

timelags_dep1 <- c(1,2,4,8)
timelags_dep2 <- c(1:4)
timelags_dep3 <- c(1:4)
timelags_dep4 <- c(1:4)

Models <- function(departamento1, departamento2, departamento3, departamento4){
  
  # Department 1 KSVM
  ts1 <- ts(departamento1, frequency = K)
  L1 <- length(ts1)
  W1 <- (L1 - Test) - (Runs - 1) * S
  YR1 <- diff(range(ts1))
  
  D_ksvm <- CasesSeries(ts1, timelags_dep1)
  W2_ksvm <- W1 - max(timelags_dep1)
  
  K_MAE <- vector(length = Runs)
  K_NMAE <- vector(length = Runs)
  K_RMSE <- vector(length = Runs)
  K_RRSE <- vector(length = Runs)
  K_R2 <- vector(length = Runs)
  
  Ksvm_Predictions <- c()
  
  for(b in 1:Runs){
    H_ksvm <- holdout(D_ksvm$y, ratio = Test, mode = "incremental", iter = b, window = W2_ksvm, increment = S)
    ksvm <- fit(y ~ ., D_ksvm[H_ksvm$tr,], model = "ksvm")
    Ksvm_Pred <- lforecast(ksvm, D_ksvm, start = (length(H_ksvm$tr) + 1), Test)
    Ksvm_Predictions <- c(Ksvm_Predictions, Ksvm_Pred)
    
    # KSVM Metrics
    K_MAE[b] <- mmetric(y = ts1[H_ksvm$ts], x = Ksvm_Pred, metric = "MAE", val = YR1)
    K_NMAE[b] <- mmetric(y = ts1[H_ksvm$ts], x = Ksvm_Pred, metric = "NMAE", val = YR1)
    K_RMSE[b] <- mmetric(y = ts1[H_ksvm$ts], x = Ksvm_Pred, metric = "RMSE", val = YR1)
    K_RRSE[b] <- mmetric(y = ts1[H_ksvm$ts], x = Ksvm_Pred, metric = "RRSE", val = YR1)
    K_R2[b] <- mmetric(y = ts1[H_ksvm$ts], x = Ksvm_Pred, metric = "R2", val = YR1)
  }
  
  cat("\n **  DEPARTAMENTO 1 (KSVM) **")
  cat("\n MAE: ", round(median(K_MAE),2), "\n NMAE: ", round(median(K_NMAE),2), "\n RMSE: ", round(median(K_RMSE),2), "\n RRSE: ", round(median(K_RRSE),2), "\n R2: ", round(median(K_R2),2))
  
  # Department 2 LM
  ts2 <- ts(departamento2, frequency = K)
  L2 <- length(ts2)
  W2 <- (L2 - Test) - (Runs - 1) * S
  YR2 <- diff(range(ts2))
  
  D_lm2 <- CasesSeries(ts2, timelags_dep2)
  W2_lm2 <- W2 - max(timelags_dep2)
  
  LM2_MAE <- vector(length = Runs)
  LM2_NMAE <- vector(length = Runs)
  LM2_RMSE <- vector(length = Runs)
  LM2_RRSE <- vector(length = Runs)
  LM2_R2 <- vector(length = Runs)
  
  LM2_Predictions <- c()
  
  for(b in 1:Runs){
    H_lm2 <- holdout(D_lm2$y, ratio = Test, mode = "incremental", iter = b, window = W2_lm2, increment = S)
    lm2_model <- fit(y ~ ., D_lm2[H_lm2$tr,], model = "lm")
    LM2_Pred <- lforecast(lm2_model, D_lm2, start = (length(H_lm2$tr) + 1), Test)
    LM2_Predictions <- c(LM2_Predictions, LM2_Pred)
    
    # LM Metrics
    LM2_MAE[b] <- mmetric(y = ts2[H_lm2$ts], x = LM2_Pred, metric = "MAE", val = YR2)
    LM2_NMAE[b] <- mmetric(y = ts2[H_lm2$ts], x = LM2_Pred, metric = "NMAE", val = YR2)
    LM2_RMSE[b] <- mmetric(y = ts2[H_lm2$ts], x = LM2_Pred, metric = "RMSE", val = YR2)
    LM2_RRSE[b] <- mmetric(y = ts2[H_lm2$ts], x = LM2_Pred, metric = "RRSE", val = YR2)
    LM2_R2[b] <- mmetric(y = ts2[H_lm2$ts], x = LM2_Pred, metric = "R2", val = YR2)
  }
  
  cat("\n **  DEPARTAMENTO 2 (LM) **")
  cat("\n MAE: ", round(median(LM2_MAE),2), "\n NMAE: ", round(median(LM2_NMAE),2), "\n RMSE: ", round(median(LM2_RMSE),2), "\n RRSE: ", round(median(LM2_RRSE),2), "\n R2: ", round(median(LM2_R2),2))
  
  # Department 3 MARS
  ts3 <- ts(departamento3, frequency = K)
  L3 <- length(ts3)
  W3 <- (L3 - Test) - (Runs - 1) * S
  YR3 <- diff(range(ts3))
  
  D_mars <- CasesSeries(ts3, timelags_dep3)
  W2_mars <- W3 - max(timelags_dep3)
  
  M_MAE <- vector(length = Runs)
  M_NMAE <- vector(length = Runs)
  M_RMSE <- vector(length = Runs)
  M_RRSE <- vector(length = Runs)
  M_R2 <- vector(length = Runs)
  
  MARS_Predictions <- c()
  
  for(b in 1:Runs){
    H_mars <- holdout(D_mars$y, ratio = Test, mode = "incremental", iter = b, window = W2_mars, increment = S)
    mars_model <- fit(y ~ ., D_mars[H_mars$tr,], model = "mars")
    MARS_Pred <- lforecast(mars_model, D_mars, start = (length(H_mars$tr) + 1), Test)
    MARS_Predictions <- c(MARS_Predictions, MARS_Pred)
    
    # MARS Metrics
    M_MAE[b] <- mmetric(y = ts3[H_mars$ts], x = MARS_Pred, metric = "MAE", val = YR3)
    M_NMAE[b] <- mmetric(y = ts3[H_mars$ts], x = MARS_Pred, metric = "NMAE", val = YR3)
    M_RMSE[b] <- mmetric(y = ts3[H_mars$ts], x = MARS_Pred, metric = "RMSE", val = YR3)
    M_RRSE[b] <- mmetric(y = ts3[H_mars$ts], x = MARS_Pred, metric = "RRSE", val = YR3)
    M_R2[b] <- mmetric(y = ts3[H_mars$ts], x = MARS_Pred, metric = "R2", val = YR3)
  }
  
  cat("\n **  DEPARTAMENTO 3 (MARS) **")
  cat("\n MAE: ", round(median(M_MAE),2), "\n NMAE: ", round(median(M_NMAE),2), "\n RMSE: ", round(median(M_RMSE),2), "\n RRSE: ", round(median(M_RRSE),2), "\n R2: ", round(median(M_R2),2))
  
  # Department 4 LM
  ts4 <- ts(departamento4, frequency = K)
  L4 <- length(ts4)
  W4 <- (L4 - Test) - (Runs - 1) * S
  YR4 <- diff(range(ts4))
  
  D_lm4 <- CasesSeries(ts4, timelags_dep4)
  W2_lm4 <- W4 - max(timelags_dep4)
  
  LM4_MAE <- vector(length = Runs)
  LM4_NMAE <- vector(length = Runs)
  LM4_RMSE <- vector(length = Runs)
  LM4_RRSE <- vector(length = Runs)
  LM4_R2 <- vector(length = Runs)
  
  LM4_Predictions <- c()
  
  for(b in 1:Runs){
    H_lm4 <- holdout(D_lm4$y, ratio = Test, mode = "incremental", iter = b, window = W2_lm4, increment = S)
    lm4_model <- fit(y ~ ., D_lm4[H_lm4$tr,], model = "lm")
    LM4_Pred <- lforecast(lm4_model, D_lm4, start = (length(H_lm4$tr) + 1), Test)
    LM4_Predictions <- c(LM4_Predictions, LM4_Pred)
    
    # LM Metrics
    LM4_MAE[b] <- mmetric(y = ts4[H_lm4$ts], x = LM4_Pred, metric = "MAE", val = YR4)
    LM4_NMAE[b] <- mmetric(y = ts4[H_lm4$ts], x = LM4_Pred, metric = "NMAE", val = YR4)
    LM4_RMSE[b] <- mmetric(y = ts4[H_lm4$ts], x = LM4_Pred, metric = "RMSE", val = YR4)
    LM4_RRSE[b] <- mmetric(y = ts4[H_lm4$ts], x = LM4_Pred, metric = "RRSE", val = YR4)
    LM4_R2[b] <- mmetric(y = ts4[H_lm4$ts], x = LM4_Pred, metric = "R2", val = YR4)
  }
  
  cat("\n **  DEPARTAMENTO 4 (LM) **")
  cat("\n MAE: ", round(median(LM4_MAE),2), "\n NMAE: ", round(median(LM4_NMAE),2), "\n RMSE: ", round(median(LM4_RMSE),2), "\n RRSE: ", round(median(LM4_RRSE),2), "\n R2: ", round(median(LM4_R2),2))
  
  # Save all predictions to CSV
  df <- data.frame(Departamento1_KSVM = Ksvm_Predictions, 
                   Departamento2_LM = LM2_Predictions, 
                   Departamento3_MARS = MARS_Predictions, 
                   Departamento4_LM = LM4_Predictions)
  write.csv(df, "predictions.csv", row.names = FALSE)
}

# Run models for departments 1, 2, 3, and 4
Models(d1, d2, d3, d4)
