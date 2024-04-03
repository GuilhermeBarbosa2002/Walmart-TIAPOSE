# Instalar e carregar o pacote
library(timeDate)

# Função para verificar se uma data é feriado nos EUA
is_holiday <- function(data) {
  # Carregar feriados dos EUA
  holidays <-getNYSEHoliday(as.integer(format(data, "%Y")))
  
  # Verificar se a data é um feriado nos EUA
  feriado <- as.character(data) %in% as.character(holidays)
  
  return(feriado)
}
