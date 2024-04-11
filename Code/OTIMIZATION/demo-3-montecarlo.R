source("blind.R") # fsearch is defined here
source("montecarlo.R") # mcsearch is defined here

# dimension
D=1

# evaluation function:
sphere=function(x) sum(x^2)

N=100000 # number of searches
# monte carlo search with D=2 and x in [-10.4,10.4]
lower=rep(-10.4,D) # lower bounds
upper=rep(10.4,D) #  upper bounds
MC=mcsearch(fn=sphere,lower=lower,upper=upper,N=N,type="min")
cat("best solution:",MC$sol,"evaluation function",MC$eval," (found at iteration:",MC$index,")\n")
