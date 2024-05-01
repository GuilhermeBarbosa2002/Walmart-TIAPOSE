#EndogenasExogenas

# install.packages("vars") # if needed, install
# install.packages("fpp2") # if needed, install
library(vars)
library(rminer)
library(forecast)
library(lubridate)
library(ggplot2)
library(reshape2)


source("multi-utils.R") # load multi-variate utility forecasting functions


# load the data:
data=read.csv("walmart.csv",header=TRUE,sep=",")

data$Week <- week(as.Date(data$Date))
data$Month <- month(as.Date(data$Date))

data$IsHoliday <- ifelse(data$IsHoliday == TRUE, 1, 0)
print(summary(data))

mpause()

K=4 # seasonal frequency: 4 time periods per year
LTS=K #  1 month, used for the forecasting range, thus 4 forecasts
fuel_price = data[,"Fuel_Price"]
IsHoliday = data[,"IsHoliday"]
Week = data[,"Week"]
Month = data[,"Month"]
d1=data[,"WSdep1"] # employment
d2=data[,"WSdep2"] # employment
d3=data[,"WSdep3"] # employment
d4=data[,"WSdep4"] # employment



# Create a matrix of exogenous variables
exogen_matrix <- data[, c("Week","IsHoliday")]



hd=holdout(d1,ratio=LTS,mode="order") # simple ordered holdout train and test split, rminer function


cdata=cbind(d2,d3)
cdata2=cbind(Week, IsHoliday)

mtr=ts(cdata[hd$tr,],frequency=K) # TS training object, uses forecast library mode!
exogen2=ts(cdata2[hd$tr,],frequency=K)
exog_future=ts(cdata2[hd$ts,],frequency=K)
exog_future_df <- data.frame(exog_future)


#Para quando so temos uma variavel exogena
#colnames(IsHoliday) <- "exo1"


mvar=VAR(mtr,lag.max=16, exogen = exogen2) # 4*K. Also default lags.pt=16 of serial.test
# get multi-step ahead forecasts

FV=predict(mvar, n.ahead = 4, dumvar = exog_future_df )


Pred=FV
# print(FV)
PredD1 = FV$fcst$d1[,"fcst"]
PredD2 = FV$fcst$d2[,"fcst"]
PredD3 = FV$fcst$d3[,"fcst"]
PredD4 = FV$fcst$d4[,"fcst"]

TS = 140:143

Real1 = d1[TS]
Real2 = d2[TS]
Real3 = d3[TS]
Real4 = d4[TS]

model <- auto.arima(y = mtr, xreg=exogen2)



for (i in 1:2) {
  # Fit ARIMAX model for each endogenous variable
  
  Prediction = predict(model, n.ahead = 4, newxreg = exog_future_df )
  
  assign(paste0("pre", i), Prediction$pred)
  
  # Print summary for each model
  plot(Prediction$pred)
  print(Prediction)
  mpause()
}




arimax <- function(Real, Pred, d) {
  
  
  
  
    # Calcular métricas
    MAE <- round(mmetric(Real, Pred, metric = "MAE"),digits=2)
    NMAE <- round(mmetric(Real, Pred, metric = "NMAE"), digits = 2)
    RMSE <- round(mmetric(Real, Pred, metric = "RMSE"), digits =2)
    RRSE <- round(mmetric(Real, Pred, metric = "RRSE"), digits = 2)
    R2 <- round(mmetric(Real, Pred, metric = "R22"), digits =2)
    cor=round(mmetric(Real,Pred,metric="COR"),digits=2)

    # Criar data frame para o gráfico
    data <- data.frame(
      Time = 140:143,
      Real = Real,
      Pred = Pred
    )

    # Plotar gráfico
    p <- ggplot(data, aes(x = Time)) +
      geom_line(aes(y = Real, color = "Real"), linetype = "solid") +
      geom_line(aes(y = Pred, color = "Pred"), linetype = "dashed") +
      labs(title = paste("Arimax - Comparação Valor Real do Departamento", d, "e Previsão (NMAE =", round(NMAE, 3), ")",
                         x = "Time",
                         y = "Value")) +
      scale_color_manual(name = "Legend",
                         values = c("Real" = "blue", "Pred" = "red")) +
      theme_minimal()

    # Mostrar métricas
    cat("MAE:", MAE, "\n")
    cat("NMAE:", NMAE, "\n")
    cat("RMSE:", RMSE, "\n")
    cat("RRSE:", RRSE, "\n")
    cat("R2:", R2, "\n")
    cat("COR", cor, "\n")

    # Retornar o gráfico
    plot(p)

  }



# Função para calcular métricas e traçar gráfico de comparação
VAR <- function(Real, Pred, d) {
  # Calcular métricas
  MAE <- round(mmetric(Real, Pred, metric = "MAE"), digits = 2)
  NMAE <- round(mmetric(Real, Pred, metric = "NMAE"), digits = 2)
  RMSE <- round(mmetric(Real, Pred, metric = "RMSE"), digits = 2)
  RRSE <- round(mmetric(Real, Pred, metric = "RRSE"), digits = 2)
  R2 <- round(mmetric(Real, Pred, metric = "R22"), digits = 2)
  cor=round(mmetric(Real,Pred,metric="COR"),digits=2)

  # Criar data frame para o gráfico
  data <- data.frame(
    Time = 140:143,
    Real = Real,
    Pred = Pred
  )

  # Plotar gráfico
  p <- ggplot(data, aes(x = Time)) +
    geom_line(aes(y = Real, color = "Real"), linetype = "solid") +
    geom_line(aes(y = Pred, color = "Pred"), linetype = "dashed") +
    labs(title = paste("VAR - Comparação Valor Real do Departamento", d, "e Previsão (NMAE =", round(NMAE, 3), ")",
                       x = "Time",
                       y = "Value")) +
    scale_color_manual(name = "Legend",
                       values = c("Real" = "blue", "Pred" = "red")) +
    theme_minimal()

  # Mostrar métricas
  cat("MAE:", MAE, "\n")
  cat("NMAE:", NMAE, "\n")
  cat("RMSE:", RMSE, "\n")
  cat("RRSE:", RRSE, "\n")
  cat("R2:", R2, "\n")
  cat("COR:", cor, "\n")

  # Retornar o gráfico
  plot(p)

}

# Arimax
arimax(Real = Real2, Pred = pre1, d = 1)
mpause()
arimax(Real = Real3, Pred = pre2, d = 2)
mpause()
arimax(Real = Real3, Pred = pre3, d = 3)
mpause()
arimax(Real = Real4, Pred = pre4, d = 4)

# VAR
VAR(Real = Real1, Pred = PredD1, d = 1)
mpause()
VAR(Real = Real2, Pred = PredD2, d = 2)
mpause()
VAR(Real = Real3, Pred = PredD3, d = 3)
mpause()
VAR(Real = Real4, Pred = PredD4, d = 4)

