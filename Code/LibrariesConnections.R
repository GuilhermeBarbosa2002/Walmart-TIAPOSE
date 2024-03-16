libs <- c("this.path","ggplot2", "rminer",)

for(lib in libs){
    if(!require(lib, character.only = T))
        install.packages(lib)
    library(lib, character.only = T)
}

setwd(this.path::this.dir())
