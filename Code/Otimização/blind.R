### blind.R file ###

# full bind search method
#    search - matrix with solutions x D
#    fn - evaluation function
#    type - "min" or "max"
#    ... - extra parameters for fn
fsearch=function(search,fn,type="min",...)
{
 x=apply(search,1,fn,...) # run fn over all search rows
 ib=switch(type,min=which.min(x),max=which.max(x))
 return(list(index=ib,sol=search[ib,],eval=x[ib]))
}

# depth-first full search method
#    l - level of the tree
#    b - branch of the tree
#    domain - vector list of size D with domain values
#    fn - eval function
#    type - "min" or "max"
#    D - dimension (number of variables)
#    par - current parameters of solution vector
#    bcur - current best sol
#    ... - extra parameters for fn
dfsearch=function(l=1,b=1,domain,fn,type="min",D=length(domain),
                  par=rep(NA,D),
                  bcur=switch(type,min=list(sol=NULL,eval=Inf),
                                   max=list(sol=NULL,eval=-Inf)),
                  ...)
{ if((l-1)==D) # "leave" with solution par to be tested:
     { f=fn(par,...);fb=bcur$eval
       ib=switch(type,min=which.min(c(fb,f)),
                      max=which.max(c(fb,f)))
       if(ib==1) return (bcur) else return(list(sol=par,eval=f))
     }
  else # go through sub branches
     { for(j in 1:length(domain[[l]]))
          { par[l]=domain[[l]][j]
            bcur=dfsearch(l+1,j,domain,fn,type,D=D,
                          par=par,bcur=bcur,...)
          }
       return(bcur)
     }
}
