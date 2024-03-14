libs <- c("ggplot2")

for(lib in libs)
    if(!require(lib))
        install.packages(lib)