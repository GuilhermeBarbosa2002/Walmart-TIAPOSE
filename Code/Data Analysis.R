d=read.table("walmart.csv",sep=",",header=TRUE)
summary(d)

#  fuel_price – preço de combustível na região da loja e na mesma semana
#  isholiday – se existiu um feriado nacional na semana em causa.
#  wsdep1 – valor semanal de vendas no departamento 1.
#  wsdep2 – valor semanal de vendas no departamento 2.
#  wsdep3 – valor semanal de vendas no departamento 3.
#  wsdep4 – valor semanal de vendas no departamento 4.

#Podemos adicionar colunas ao código | Semana / Mes do ano -> Estudar a possibilidade de algum tipo de sanozalidade


# Ter em atenção o horizonte temporal, que deverá envolveruma previsão até 1 “mês” (quatro semanas) - 4 linhas 

#Instalar bibliotecas

library(ggplot2)

library(lubridate)


d$Week <- week(as.Date(d$Date))

d$Month <- month(as.Date(d$Date))

head(d, 143)

ggplot(d, aes(x = seq_along(WSdep1), y = WSdep1)) +
  geom_line(color = "blue") +
  geom_line(aes(y = WSdep2), color = "red") +
  geom_line(aes(y = WSdep3), color = "green") +
  geom_line(aes(y = WSdep4), color = "orange") +
  labs(x = "Índice", y = "Valor", title = "Variação de WSdep1, WSdep2, WSdep3 e WSdep4") +
  theme_minimal()


