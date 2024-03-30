#Instalar bibliotecas
source("LibrariesConnections.R")

d=read.table("walmart.csv",sep=",",header=TRUE)
summary(d)


d$Week <- week(as.Date(d$Date))

d$Month <- month(as.Date(d$Date))

head(d, 143)



# simple auxiliary show function: show target values and forecasts
reshow=function(label="",Y,Pred,metric="MAE",PLOT=TRUE)
{ main=paste(label,metric,":",round(mmetric(Y,Pred,metric=metric),digits=1))
LEG=c("target",paste(label,"pred."))
if(PLOT)mgraph(Y,Pred,graph="REG",main=main,Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=LEG))
else cat(main,"\n")
}



TS=d
num_departments <- 4

TS_list <- list()
TR_list <- list()
Y_list <- list()
HW_list <- list()

# Iterate over department indices and extract time series
for (i in 1:num_departments) {
  TS_list[[i]] <- d[, (8 - i)]  # Extract department sales, columns are in reverse order
}

K= 52
L=length(TS_list[[2]])
print(L)
NTS=K # numero de previsoes
H=NTS 
LTR=L- H 


# Create time series objects for each department within a loop
for (i in 1:num_departments) {
  # Extract department sales from the corresponding TS
  TS <- d[, (8 - i)]  # Extract department sales, columns are in reverse order
  
  # Create time series object
  TR_list[[i]] <- ts(TS, frequency = K)
}



# Create a list of time series
#TS_list <- list(TS1, TS2, TS3, TS4)


# Set up the plotting layout with four plots in two rows and two columns
par(mfrow = c(2, 2))


# Plot each time series in a separate plot using a loop
for (i in 1:length(TS_list)) {
  TR <- ts(TS_list[[i]], frequency = K)
  plot(TR, main = paste("Departamento", i), ylab = "Vendas", xlab = "Tempo")
}
mpause() # press enter




# target predictions:

for (i in 1:num_departments) {
  # Extract department sales from the corresponding TS
  TS <- d[, (8 - i)]  # Extract department sales, columns are in reverse order
  
  # Extract target variable Y
  Y <- TS[(LTR + 1):L]
  
  # Store Y in the list
  Y_list[[i]] <- Y}


# holt winters forecasting method:
print("model> HoltWinters")
for (i in 1:num_departments) {
  # Extract department sales from the corresponding time series
  TS <- d[, (8 - i)]  # Extract department sales, columns are in reverse order
  
  # Create time series object
  TR <- ts(TS, frequency = K)
  
  
  # Fit HoltWinters model
  HW <- HoltWinters(TR)
  
  # Store HoltWinters model in the list
  HW_list[[i]] <- HW
}

par(mfrow = c(2, 2))

for (i in 1:num_departments) {
  print(HW_list[[i]])
}

for (i in 1:num_departments) {
  plot(HW_list[[i]])
}
mpause() # press enter



plot(fitted(HW_list[[1]]))
mpause() # press enter
plot(fitted(HW_list[[2]]))
mpause() # press enter
plot(fitted(HW_list[[3]]))
mpause() # press enter
plot(fitted(HW_list[[4]]))
mpause() # press enter





# forecasts, from 1 to H ahead:
# Initialize an empty list to store forecasts
forecast_list <- list()

# Initialize an empty list to store mean forecasts
mean_forecasts <- list()

# Forecast for each HoltWinters model in HW_list
for (i in 1:length(HW_list)) {
  # Forecast using the ith HoltWinters model
  forecast_result <- forecast(HW_list[[i]], h = H)
  forecast_list[[i]] <- forecast_result
  # Extract mean forecasts and store them
  mean_forecasts[[i]] <- forecast_list[[i]]$mean[1:H]
  reshow("HW", Y_list[[i]], mean_forecasts[[i]], "MAE")
}
mpause() # press enter







# arima modeling:
print("model> auto.arima")


# Initialize a list to store forecasts
forecast_list <- list()

# Iterate over each department's time series
for (i in 1:num_departments) {
  # Extract department sales from the corresponding time series
  TS <- d[, (8 - i)]  # Extract department sales, columns are in reverse order
  
  # Create time series object
  TR <- ts(TS, frequency = K)
  
  # Fit auto.arima model
  AR <- auto.arima(TR)
  
  # Forecast using the ARIMA model
  F <- forecast(AR, h = H)
  
  # Store forecasts in the list
  forecast_list[[i]] <- F
}

# Print and display forecasts for each department
for (i in 1:num_departments) {
  print(forecast_list[[i]])
  
  # Extract mean forecasts
  Pred <- forecast_list[[i]]$mean[1:H]
  
  # Show forecasts
  reshow("AR", Y_list[[i]], Pred, "MAE")
}
mpause() # press enter





# NN from forecast:
# Iterate over each department's time series
for (i in 1:num_departments) {
  # Extract department sales from the corresponding time series
  TS <- d[, (8 - i)]  # Extract department sales, columns are in reverse order
  
  # Create time series object
  TR <- ts(TS, frequency = K)
  
  # Fit nnetar model
  NN <- nnetar(TR, P = 2, repeats = 100, size = 50, decay = 0.1)
  
  # Forecast using the nnetar model
  F <- forecast(NN, h = H)
  
  # Store forecasts in the list
  forecast_list[[i]] <- F
}

# Print and display forecasts for each department
for (i in 1:num_departments) {
  print(forecast_list[[i]])
  
  # Extract mean forecasts
  Pred <- forecast_list[[i]]$mean[1:H]
  
  # Show forecasts
  reshow("NN1", Y_list[[i]], Pred, "MAE")
}
mpause() # press enter



# stlffrom forecast:
print("model> ets")


# Iterate over each department's time series
for (i in 1:num_departments) {
  # Extract department sales from the corresponding time series
  TS <- d[, (8 - i)]  # Extract department sales, columns are in reverse order
  
  # Create time series object
  TR <- ts(TS, frequency = K)
  
  # Fit STLF model
  STLF <- stlf(TR)
  
  # Forecast using the STLF model
  F <- forecast(STLF, h = H)
  
  # Store forecasts in the list
  forecast_list[[i]] <- F
}

# Print and display forecasts for each department
for (i in 1:num_departments) {
  print(forecast_list[[i]])
  
  # Extract mean forecasts
  Pred <- forecast_list[[i]]$mean[1:H]
  
  # Show forecasts
  reshow("STLF", Y_list[[i]], Pred, metric = "MAE")
}
mpause() # press enter


# neural network modeling, via rminer:
for (i in 1:num_departments) {
  # Extract department sales from the corresponding time series
  TS <- d[, (8 - i)]  # Extract department sales, columns are in reverse order
  
  # Create lagged dataset with additional lags (1, 4, 52, 104, etc.)
  d_lagged <- CasesSeries(TS, c( 1, 4, 13, 26, 52))
  
  # Split data into training and testing sets
  hd <- holdout(d_lagged$y, ratio = NTS, mode = "order")
  
  # Normalize the lagged dataset if necessary
  
  # Fit neural network model with adjusted parameters
  NN <- fit(y ~ ., d_lagged[hd$tr, ], model = "mlpe", nodes = c(10, 5), repeats = 5, maxit = 1000)
  
  # Forecast using the neural network model
  F <- lforecast(NN, d_lagged, start = hd$ts[1], horizon = H)
  
  # Store forecasts in the list
  forecast_list[[i]] <- F
}

# Print and display forecasts for each department
for (i in 1:num_departments) {
  print(forecast_list[[i]])
  
  # Extract forecasts
  Pred <- forecast_list[[i]]
  
  # Show forecasts
  reshow("NN2", Y_list[[i]], Pred, "MAE")
}


