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

