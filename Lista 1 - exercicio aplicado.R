rm(list=ls())

library("readxl")
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)

setwd("C:/Users/Daniel/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 1/database")


########################################################

#@author: Kelly G. Santos
#MPE 2021 - Econometrics - MONITORIA 1

#SEMPRE COMENTEM E ORGANIZEM O C�DIGO!!!!!

########################################################

######################################################################################################
#1 Abertura
######################################################################################################


df <- read_excel('PNAD-COVID-5-2020.xlsx',sheet=1)


######################################################################################################
# Estat�sticas descritivas
######################################################################################################

install.packages('dplyr')
library(dplyr)

#2 CAPITAL==35
df %>% count(CAPITAL) #s� observa��es de S�o Paulo

#3 Etnia: A004 -> 1 branca, 2 preta, 3 amarela, 4 parda, 5 indigena

atendimento <- as.numeric( df$B002 == 1)

df <- cbind(df, atendimento)

aggregate( df$atendimento, list(df$A004), mean, na.rm = TRUE)


#1- Branca: 11,6%
#2- Preta: 13,8%
#preto e pardos buscaram mais atendimento que brancos


#4 Histograma do rendimento -> C01012

install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x=C01012)) + 
  geom_histogram(bins=5) + xlim(0, 5000) + 
  labs(x = "Rendimento", y="Densidade")

######################################################################################################
#5 Regress�o principal
######################################################################################################


#Regress�o 1
lwage<- log(df$C01012)

df<- cbind(df, lwage)

#A005 -> 1	Sem instru��o, 2	Fundamental incompleto, 3	Fundamental completa, 4	M�dio incompleto, 
#5	M�dio completo, 6	Superior incompleto, 7	Superior completo, 8	P�s-gradua��o, mestrado ou doutorado
#N�o podemos interpretar o coeficiente, pois escolaridade � uma vari�vel categ�rica

reg1<- lm(lwage~A005, data=df , na.action=na.omit)
summary(reg1)

#Podemos fazer a regress�o usando dummies para cada uma das categorias
reg1<- lm(lwage ~ factor(A005), data=df, na.action=na.omit)
summary(reg1)

#Dica:
install.packages('stargazer')
library(stargazer)

stargazer(reg1, type = 'text')

######################################################################################################
# 6 Regress�o com hip�tese de identifica��o
######################################################################################################


reg2 <- df %>% group_by(atendimento) %>% do(model = lm(lwage ~ factor(A005), data = .))

# 1 � o caso de atendimento e 0 � caso contr�rio.
coef_natend<-reg2[[2]][[1]]$coefficients #atendimento==0 -> n�o precisaram
coef_atend<-reg2[[2]][[2]]$coefficients #atendimento==1 -> precisaram


#diferen�a dos coef
diff <- coef_atend[2:8]-coef_natend[2:8]
diff


######################################################################################################
# 7 Regress�o com dummy
######################################################################################################
 
reg3<- lm( lwage ~ factor(A005) + factor(A005)*atendimento, data=df , na.action=na.omit)

diff_dummy<-reg3$coefficients
diff_dummy<-diff_dummy[10:16]

diff_dummy

merge<- cbind(diff, diff_dummy)
merge

#Corre-> lwage,atedimento

cor(df$lwage, df$atendimento,  method = c("pearson"), na.rm = TRUE)

reg<-lm(lwage ~ atendimento, data=df)
summary(reg)













######################################################################################################
# Calcular taxa de desemprego
######################################################################################################

#A002 -> PEA: acima dos 14 anos
N<-dim(df)[1] #total
PEA<-nrow(subset(df, A002>=14))

#C015-> 1 desocupado 
PD<-nrow(subset(df, C015==1))

#Taxa de desemprego:
u=PD/PEA*100
round(u,2)







