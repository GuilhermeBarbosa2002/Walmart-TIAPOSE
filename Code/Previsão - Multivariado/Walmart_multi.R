# 5-multi.R: multi-variate time series multi-step ahead forecasting example.
# this demo works with ...

# install.packages("vars") # if needed, install
# install.packages("fpp2") # if needed, install
library(vars)
library(rminer)
library(forecast)
library(lubridate)

source("multi-utils.R") # load multi-variate utility forecasting functions

## auxiliary function, shows 2 forecasts for a given multi-variate method:
fshow=function(Y,Pred1,Pred2,Pred3, Pred4,method,name1,name2,name3, name4)
{
  par(mfrow = c(1, 4)) # three graphs inside a plot
  # note: in this example, NMAE is normalized to the range of only the Y prediction period.
  # since Y only includes 4 periods, the NMAE % values are often high.
  # an alternative would be to use mtr range values or cdata full data range values. 
  #yrange1=c(floor(min(Y[,1])),ceiling(max(Y[,1])))
  #yrange2=c(floor(min(Y[,2])),ceiling(max(Y[,2])))            
    
  
  mae=round(mmetric(Y[,3],Pred1,metric="MAE"),1)
  nmae=round(mmetric(Y[,3],Pred1,metric="NMAE"),1)
  rmse=round(mmetric(Y[,3],Pred1,metric="RMSE"),1)
  rrse=round(mmetric(Y[,3],Pred1,metric="RRSE"),1)
  r2=round(mmetric(Y[,3],Pred1,metric="R2"),1)
  cor=round(mmetric(Y[,3],Pred1,metric="COR"),digits=2)
  main=paste(method," ",name1," (MAE=",mae,", COR=",cor,")",sep="")
  mgraph(Y[,3],Pred1,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target",method)))
  cat("\n **  DEPARTAMENTO 1 ** ", method, " ******")
  cat("\n MAE: ", mae, "\n NMAE: ", nmae," \n RMSE: ", rmse,"\n RRSE: ", rrse,"\n R2: ", r2, "\n COR: ", cor)

  
  mae=round(mmetric(Y[,4],Pred2,metric="MAE"),1)
  nmae=round(mmetric(Y[,4],Pred2,metric="NMAE"),1)
  rmse=round(mmetric(Y[,4],Pred2,metric="RMSE"),1)
  rrse=round(mmetric(Y[,4],Pred2,metric="RRSE"),1)
  r2=round(mmetric(Y[,4],Pred2,metric="R2"),1)
  cor=round(mmetric(Y[,4],Pred2,metric="COR"),digits=2)
  main=paste(method," ",name2," (MAE=",mae,", COR=",cor,")",sep="")
  mgraph(Y[,4],Pred2,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target",method)))
  cat("\n **  DEPARTAMENTO 2 ** ", method, " ******")
  cat("\n MAE: ", mae, "\n NMAE: ", nmae," \n RMSE: ", rmse,"\n RRSE: ", rrse,"\n R2: ", r2,  "\n COR: ", cor)
  
  mae=round(mmetric(Y[,5],Pred3,metric="MAE"),1)
  nmae=round(mmetric(Y[,5],Pred3,metric="NMAE"),1)
  rmse=round(mmetric(Y[,5],Pred3,metric="RMSE"),1)
  rrse=round(mmetric(Y[,5],Pred3,metric="RRSE"),1)
  r2=round(mmetric(Y[,5],Pred3,metric="R2"),1)
  cor=round(mmetric(Y[,5],Pred3,metric="COR"),digits=2)
  main=paste(method," ",name3," (MAE=",mae,", COR=",cor,")",sep="")
  mgraph(Y[,5],Pred3,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target",method)))
  cat("\n **  DEPARTAMENTO 3 ** ", method, " ******")
  cat("\n MAE: ", mae, "\n NMAE: ", nmae," \n RMSE: ", rmse,"\n RRSE: ", rrse,"\n R2: ", r2,  "\n COR: ", cor)
  
  
  mae=round(mmetric(Y[,6],Pred4,metric="MAE"),1)
  nmae=round(mmetric(Y[,6],Pred4,metric="NMAE"),1)
  rmse=round(mmetric(Y[,6],Pred4,metric="RMSE"),1)
  rrse=round(mmetric(Y[,6],Pred4,metric="RRSE"),1)
  r2=round(mmetric(Y[,6],Pred4,metric="R2"),1)
  cor=round(mmetric(Y[,6],Pred4,metric="COR"),digits=2)
  main=paste(method," ",name4," (MAE=",mae,", COR=",cor,")",sep="")
  mgraph(Y[,6],Pred4,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target",method)))
  cat("\n **  DEPARTAMENTO 4 ** ", method, " ******")
  cat("\n MAE: ", mae, "\n NMAE: ", nmae," \n RMSE: ", rmse,"\n RRSE: ", rrse,"\n R2: ", r2,  "\n COR: ", cor, "\n *** ***")
}
##-----------

# load the Canada data:
data=read.csv("walmart.csv",header=TRUE,sep=",")
data$Week <- week(as.Date(data$Date))
data$Month <- month(as.Date(data$Date))

K=4 # seasonal frequency: 4 time periods per year
LTS=K #  1 month, used for the forecasting range, thus 4 forecasts
fuel_price = data[,"Fuel_Price"] # Fuel_Price
is_holiday = data[,"IsHoliday"]  # IsHoliday
d1=data[,"WSdep1"]  # d1
d2=data[,"WSdep2"]  # d2
d3=data[,"WSdep3"]  # d3
d4=data[,"WSdep4"]  # d4
week=data[,"Week"]  # Week
month=data[,"Month"] # Month

hd=holdout(d1,ratio=LTS,mode="order") # simple ordered holdout train and test split, rminer function

cdata=cbind(d1,d2,d3,d4,fuel_price,month)
mtr=ts(cdata[hd$tr,],frequency=K) # TS training object, uses forecast library mode!
Y=cdata[hd$ts,] # target values

# create VAR model:
#mvar=autoVAR(mtr,LAGMAX=16) # 4*K. Also default lags.pt=16 of serial.test

# # get multi-step ahead forecasts
# FV=forecastVAR(mvar,h=LTS) # similar to the forecast library function, multi-step ahead forecasts
# Pred1=FV[[3]] # predict e
# Pred2=FV[[4]] # predict e
# Pred3=FV[[5]] # predict e
# Pred4=FV[[6]] # predict e
# 
# fshow(Y,Pred1,Pred2, Pred3, Pred4,"VAR", "d1", "d2", "d3", "d4")
# mpause()

# 
#generate 2 ARIMAX models, one for prod (1) and other for rw (2)
#mtr - matrix with the training data, one column per to be predicted variable
arimax=autoARIMAX(mtr,frequency=4)
FA=forecastARIMAX(arimax,h=LTS)
Pred21=FA[[3]]
# Pred22=FA[[4]]
# Pred23=FA[[5]]
# Pred24=FA[[6]]
# fshow(Y,Pred21,Pred22,Pred23,Pred24, "ARIMAX","d1","d2","d3","d4")
# mpause()

print(Pred21)
# 
# 
# # # in this recipe, 2 mlpe models are combined, each with 4 time lags:
# # # in this recipe, 2 mlpe models are combined, each with 4 time lags:
# # 
# # first step: creation of the data.frame with all required inputs and target outputs:
# x1lags=1:4
# x2lags=1:4
# x3lags=1:4
# x4lags=1:4
# x5lags=1:4
# x6lags=1:4
# # set vector list with the lagged inputs used by each model
# VINP=vector("list",length=6)
# VINP[[1]]=list(x1lags,1,1,1,1,1) # 1 to 4 lags for x1, 1 lag for x2, 1 lag for x3
# VINP[[2]]=list(1,x2lags,1,1,1,1) # 1 lag for x1, 1 to 4 lags for x2, 1 lag for x3
# VINP[[3]]=list(1,1,x3lags,1,1,1) # 1 lag for x1, 1 lag for x3, 1 to 4 lags for x3
# VINP[[4]]=list(1,1,1,x4lags,1,1) # 1 to 4 lags for x1, 1 lag for x2, 1 lag for x3
# VINP[[5]]=list(1,1,1,1,x5lags,1) # 1 lag for x1, 1 to 4 lags for x2, 1 lag for x3
# VINP[[6]]=list(1,1,1,1,1,x6lags) # 1 lag for x1, 1 lag for x3, 1 to 4 lags for x3
# 
# MNN=mfit(mtr,"mlpe",VINP)
# Pred3=lforecastm(MNN,h=LTS)
# Pred31=Pred3[[1]]
#Pred32=Pred3[[4]]
#Pred33=Pred3[[5]]
#Pred34=Pred3[[6]]

#fshow(Y,Pred31,Pred32,Pred33, Pred34, "3mlpe","d1","d2","d3","d4")

