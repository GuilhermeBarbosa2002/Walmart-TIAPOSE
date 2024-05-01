### montecarlo.R file ###

# montecarlo uniform search method
#    fn - evaluation function
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    N - number of samples
#    type - "min" or "max"
#    ... - extra parameters for fn
mcsearch=function(fn,lower,upper,N,type="min",...)
{ D=length(lower)
  s=matrix(nrow=N,ncol=D) # set the search space 
  for(i in 1:N) s[i,]=runif(D,lower,upper)
  fsearch(s,fn,type,...) # best solution
}
