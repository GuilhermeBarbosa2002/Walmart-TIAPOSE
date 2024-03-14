libs <- c("ggplot2", "rminer", "lubridate")

for(lib in libs){
    if(!require(lib, character.only = T))
        install.packages(lib)
    library(lib, character.only = T)
}

setwd(this.path::this.dir())
