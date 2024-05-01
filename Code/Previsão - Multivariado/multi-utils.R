
### Recipe for VAR  -------------------------------------
## autoVAR function - create a VAR model from a mtr training data
# mtr - matrix with the training data, one column per to be predicted variable
# LAGMAX - maximum lag order considered
autoVAR=function(mtr,LAGMAX)
{  
  # note: here the default type="const" value is assumed. there are other options, see: help(VAR)
  # p lag order selection for a multi-variate VAR model with 2 time series:
  # VARselect(mtr,lag.max=LAGMAX,type="const")[["selection"]]
  vselect=VARselect(mtr,lag.max=LAGMAX,type="const")
  # R. Hyndman book recipe:
  # The R output shows the lag length selected by each of the information criteria available in the vars package. 
  # There is a large discrepancy between the VAR(5) selected by the AIC and the VAR(1) selected by the BIC -> SC(n). 
  # This is not unusual. As a result we first fit a VAR(1), as selected by the BIC.
  # o is the initial order
  omin=as.numeric(vselect$selection[3]) # BIC
  omax=as.numeric(vselect$selection[1]) # AIC
  # Adapted recipe of R. Hyndman book:
  stop=FALSE
  pvalueref=0.10
  o=omin
  while(!stop)
  {
    mvar=VAR(mtr,p=o,type="const")
    # Portmanteau Test (asymptotic)
    st=serial.test(mvar,lags.pt=LAGMAX,type="PT.asymptotic")
    pvalue=as.numeric(st$serial$p.value)
    cat("order:",o,"pvalue:",pvalue,"\n")
    if(pvalue> pvalueref) stop=TRUE
    else if( (o+1)==omax) {stop=TRUE;o=o+1} 
    else o=o+1
  }
  # set final VAR model
  mvar=VAR(mtr,p=o,type="const")
  return(mvar) # returns a VAR model
}

## forecastVAR - get multi-step ahead forecasts for all mtr variables
# model - VAR model
# h - horizon
forecastVAR=function(model,h)
{
  models=length(model$varresult)
  res=vector("list",length=models)
  F1=forecast(model,h=h)$forecast # similar to the forecast library function, multi-step ahead forecasts
  for(i in 1:models) res[[i]]=as.numeric(F1[[i]]$mean) 
  return(res)  # res is a vector list of length ncol(mtr)
}
#------------------------------------------------------------------------------

### Recipe for ARIMAX  -------------------------------------
## autoARIMAX function - create an ARIMAX model from a mtr training data
# mtr - matrix with the training data, one column per to be predicted variable
# frequency - if 1, no seasonal model is assumed, if > 1, SARIMA is assumed.
autoARIMAX=function(mtr,frequency=1)
{
  models=ncol(mtr)
  nr=nrow(mtr)
  # create one arimax model per variable
  res=vector("list",length=models) # one arimax per variable
  res2=vector("list",length=models) # one arimax per variable
  for(i in 1:models)
  {
    if(frequency>1) s_tr=ts(mtr[1:nr,i],frequency=frequency) else s_tr=ts(mtr[1:nr,i])
    # arimax:
    res[[i]]=auto.arima(s_tr,xreg=mtr[1:nr,-i])
    # pure arima: (heuristic)
    res2[[i]]=auto.arima(s_tr)
  }
  
  # returns a list with arimax=res and arima=res2
  return(list(arimax=res,arima=res2))
}  

## forecastARIMAX - get multi-step ahead forecasts for all mtr variables
# model - arimax model
# h - horizon
forecastARIMAX=function(model,h)
{
  models=length(model$arimax)
  res=vector("list",length=models)
  # get xreg estimates: heuristic -> pure auto.arima for each variable
  xreg=matrix(ncol=models,nrow=h)
  for(i in 1:models) xreg[,i]=forecast(model$arima[[i]],h=h)$mean
  
  # get arimax forecasts:  
  for(i in 1:models)
  {
    # if needed, convert xaux to matrix:
    if(length(xreg[1,-i])==1) {xaux=matrix(ncol=1,nrow=h);xaux[,1]=xreg[,-i]}  
    else xaux=xreg[,-i] # already is a matrix
    F1=suppressWarnings(forecast(model$arimax[[i]],h=h,xreg=xaux))
    res[[i]]=as.numeric(F1$mean)
  }
  return(res)
}

#------------------------------------------------------------------------------
# auxiliary function
vreverse=function(x) x[length(x):1]

### Recipe for "entwined" Machine Learning models:
# mtr - matrix with the training data, one column per to be predicted variable
# model -- rminer fit model
# VINP -- vector list with time lags for each variable, see demo example.
# important notes !!!: VINP only works with consecutive lags, like 1,1:2, 1:4, etc.
#                      VINP only works if maximum lags is consistent with all variables (see example)
#                      VINP can mix 1 lag with 1:3 lags, as shown in example. But not other configurations.
mfit=function(mtr,model,VINP)
{
  models=ncol(mtr) # number of predicted models
  
  mdata=vector("list",length=models)
  for(i in 1:models) mdata[[i]]=CasesSeries(mtr[,i],VINP[[i]][[i]])
  
  # create mdata2 datasets:
  mdata2=vector("list",length=models)
  for(i in 1:models) # 1 dataset per variable 
  {  
    for(j in 1:models) # cycle all lags for VINP
    { if(j==1) 
      { D=mdata[[j]][,VINP[[i]][[j]]]
        if(is.vector(D)) 
          { D=data.frame(cbind(D))
          }
       NCD=ncol(D)
       dnames=vreverse(paste("lag",VINP[[i]][[j]],sep=""))
       ini=1;end=ini+length(dnames)-1
       names(D)[ini:end]=paste("x",j,dnames,sep="")
      }
      else { NCD=ncol(D)
             dnames=vreverse(paste("lag",VINP[[i]][[j]],sep=""))
             D=cbind(D,mdata[[j]][,dnames])
             ini=NCD+1;end=ini+length(dnames)-1
             names(D)[ini:end]=paste("x",j,dnames,sep="")
           }
    }
    D=cbind(D,y=mdata[[i]]$y)
    mdata2[[i]]=D
  }
  # train all ML models:
  mmodels=vector("list",length=models)
  for(i in 1:models) # 1 dataset per variable 
  { 
    mmodels[[i]]=fit(y~.,mdata2[[i]],model=model)
  }
  return(list(mdata=mdata2,mmodels=mmodels,vinp=VINP))
}

## lforecastm - get multi-step ahead forecasts for all mtr variables
# model - multi-variate ML model (several ML models, one per variable)
# h - horizon
lforecastm=function(model,h)
{
  models=length(model$mmodels)
  # model$mmodels
  # model$mdata
  # model$vinp
  
  # init res
  res=vector("list",length=models)
  for(j in 1:models) res[[j]]=vector(length=h)
  
  # init VEX
  VEX=vector("list",length=models)
  vnames=vector("list",length=models)
  VI=vector("list",length=models)
  for(i in 1:h)
  { 
    for(j in 1:models)
    { start=nrow(model$mdata[[j]])-h+1
      VEX[[j]]=model$mdata[[j]][start+i-1,]
      # change VEX if needed:
      if(i>1){
        for(k in 1:models)
        {
          VI[[k]]=intersect( 1:(i-1), model$vinp[[j]][[k]] )  
          vnames[[k]]=paste("x",k,"lag",VI[[k]],sep="")
          vnames[[k]]=vreverse(vnames[[k]])
          # compute right the res values:
          VI[[k]]=as.numeric(VI[[k]])
          VEX[[j]][,vnames[[k]] ]=res[[k]][ (i-1) - vreverse(VI[[k]]) +1 ]
        }   
      }
  #cat("i:",i,"j:",j,"\n")
  #print(VEX[[j]])
      res[[j]][i]=predict(model$mmodels[[j]],VEX[[j]])
    } # for j
  } # i horizon cycle
  return(res)
} 
#-------------------------------