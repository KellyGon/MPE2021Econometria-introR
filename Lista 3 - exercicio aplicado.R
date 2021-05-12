rm(list=ls())

install.packages("readxl")
install.packages("readtext")
install.packages("readr")
install.packages("stringr")
install.packages("ggplot2")

library(readtext)
library("readxl")
library("readr")
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#Pasta: 
setwd("C:/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 3/database/Exercicio Aplicado")

load("TSE2012.Rda")
load("TSE2010.Rda")

#merge_cand_resultado<- merge_cand_resultado %>% drop_na(TOTAL_VOTOS)

#Qtd de coligação pra UF de SP
qtd_coligação_SP<-merge_cand_resultado[which(merge_cand_resultado$SIGLA_UF == "SP"),] %>% 
  count(NOME_COLIGACAO) #só observações de São Paulo
dim(qtd_coligação_SP)[1]

#Qtd de mulheres/homem para vereador.
aggregate( merge_cand_resultado$TOTAL_VOTOS,
           list(merge_cand_resultado$DESCRICAO_SEXO), mean, na.rm = TRUE)
#população inteira -> nao precisamos fazer teste de hipótese



######################################################################################################
# Regressão principal
######################################################################################################

reg<-lm(TOTAL_VOTOS ~ DESPESA_MAX_CAMPANHA + factor(DESCRICAO_SEXO)+ factor(COD_GRAU_INSTRUCAO), 
        data=merge_cand_resultado)
summary(reg)

eleito <- as.numeric( merge_cand_resultado$DESC_SIT_CAND_TOT =="ELEITO")

merge_cand_resultado <- cbind(merge_cand_resultado, eleito)

reg_eleito<-lm(eleito ~ DESPESA_MAX_CAMPANHA + factor(DESCRICAO_SEXO)+
                 factor(COD_GRAU_INSTRUCAO), data=merge_cand_resultado)
summary(reg_eleito)


#####################################################################################################
# Calculando eleito passada
######################################################################################################

eleito <- as.numeric(votacao_2010$DESC_SIT_CAND_TOT =="ELEITO")

votacao_2010 <- cbind(votacao_2010, eleito)

df<-aggregate(votacao_2010$eleito, list(votacao_2010$SEQUENCIAL_LEGENDA), mean, na.rm = TRUE)
names(df)[names(df) == 'Group.1'] <- 'SEQUENCIAL_LEGENDA'
names(df)[names(df) == 'x'] <- 'eleitoPassada'

#Se processo ficou muito demorado, pegue uma amostra da base
#samply = sample_n(merge_cand_resultado, 1000)

merge_2010_2012 <- merge(merge_cand_resultado, df, by="SEQUENCIAL_LEGENDA")

reg_corr<-lm(DESPESA_MAX_CAMPANHA~eleitoPassada, data=merge_2010_2012)
summary(reg_corr) #correlaçao significante

#2SLS:
library(AER) #package pra rodar o ivreg
iv<-ivreg(TOTAL_VOTOS ~ DESPESA_MAX_CAMPANHA + factor(DESCRICAO_SEXO) +
            factor(COD_GRAU_INSTRUCAO) | eleitoPassada + factor(DESCRICAO_SEXO)+
            factor(COD_GRAU_INSTRUCAO) , data=merge_2010_2012)
summary(iv)















#Eq. Simultanea:
install.packages("systemfit")
library(systemfit)
data(mroz, package='wooldridge')
oursample <- subset(mroz,!is.na(wage))

# Define system of equations and instruments
eq.hrs   <- hours    ~ log(wage)+educ+age+kidslt6+nwifeinc
eq.wage  <- log(wage)~ hours    +educ+exper+I(exper^2)
eq.system<- list(eq.hrs, eq.wage)
instrum  <- ~educ+age+kidslt6+nwifeinc+exper+I(exper^2)

# 2SLS of whole system (run Example-16-5-systemfit-prep.R first!)
summary(systemfit(eq.system,inst=instrum,data=oursample,method="2SLS"))







