# 5-multi.R: multi-variate time series multi-step ahead forecasting example.
# this demo works with ...

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
exogen_matrix <- data[, c("IsHoliday", "Week", "Month")]



hd=holdout(d1,ratio=LTS,mode="order") # simple ordered holdout train and test split, rminer function


cdata=cbind(fuel_price,d1,d2,d3,d4)
cdata2=cbind(IsHoliday, Week, Month)

mtr=ts(cdata[hd$tr,],frequency=K) # TS training object, uses forecast library mode!
exogen2=ts(cdata2[hd$tr,],frequency=K)
exog_future=ts(cdata2[hd$ts,],frequency=K)
IsHoliday <- data.frame(exog_future)


# create VAR model:
mvar=VAR(mtr,lag.max=16, exogen = exogen2) # 4*K. Also default lags.pt=16 of serial.test
# get multi-step ahead forecasts
FV=predict(mvar, n.ahead = 4, dumvar = IsHoliday)


Pred=FV
print(FV)
PredD1 = FV$fcst$d1[,"fcst"]
PredD2 = FV$fcst$d2[,"fcst"]
PredD3 = FV$fcst$d3[,"fcst"]
PredD4 = FV$fcst$d4[,"fcst"]

TS = 140:143

Real1 = d1[TS]
Real2 = d2[TS]
Real3 = d3[TS]
Real4 = d4[TS]


# Função para calcular métricas e traçar gráfico de comparação
compare_and_plot <- function(Real, Pred, d) {
  # Calcular métricas
  MAE <- mmetric(Real, Pred, metric = "MAE")
  NMAE <- mmetric(Real, Pred, metric = "NMAE", val = 173619)
  RMSE <- mmetric(Real, Pred, metric = "RMSE")
  RRSE <- mmetric(Real, Pred, metric = "RRSE")
  R2 <- mmetric(Real, Pred, metric = "R22")
  
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
    labs(title = paste("Comparação Valor Real do Departamento", d, "e Previsão (NMAE =", round(NMAE, 3), ")",
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
  
  # Retornar o gráfico
  plot(p)
  
}

# Exemplo de uso da função
compare_and_plot(Real = Real1, Pred = PredD1, d = 1)
mpause()
compare_and_plot(Real = Real2, Pred = PredD2, d = 2)
mpause()
compare_and_plot(Real = Real3, Pred = PredD3, d = 3)
mpause()
compare_and_plot(Real = Real4, Pred = PredD4, d = 4)

