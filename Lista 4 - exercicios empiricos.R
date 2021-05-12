####################################################################
#MPE - Economia 2021 S1

#Arquivo com respostas do exercícios empíricos 

#Lista 1

####################################################################
spin('C:/Users/Daniel/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 4/respostas/Lista 4 - exercicios empiricos.R', precious = TRUE, doc = '#')


library('wooldridge')


####################################################################

#Exercicio 1

####################################################################

data('loanapp')

#Item a - probit
probit<-glm(approve~white, family=binomial(link=probit),data=loanapp)
summary(probit)

#linear:
linear<-lm(approve~white,data=loanapp)
summary(linear)
#Estimador de white no probit é sig e 0.78395, no linear é sig e  0.20060.

#Item b
probit<-glm(approve~white+ hrat+obrat+ loanprc+ unem+ male+ married+ dep+ sch+ cosign+ chist+ pubrec+
             mortlat1+ mortlat2+ vr, family=binomial(link=probit),data=loanapp)
summary(probit)
#Sim, estimador de white é sig. e é de 0.520254

#Item c
logit<-glm(approve~white+ hrat+obrat+ loanprc+ unem+ male+ married+ dep+ sch+ cosign+ chist+ pubrec+
           mortlat1+ mortlat2+ vr, family=binomial(link=logit),data=loanapp)

# Summary of results:
summary(logit)
#Estimador de white é sig. e é de  0.93776


# Item d - Tamanho do efeito
# Calculation of linear index at individual values:
xb.log <- predict(logit)
xb.prob<- predict(probit)
# APE factors = average(g(xb))
factor.log <- mean( dlogis(xb.log) )
factor.prob<- mean( dnorm(xb.prob) )
cbind(factor.log,factor.prob)

# average partial effects = beta*factor:
APE.log <- coef(logit) * factor.log
APE.prob<- coef(probit) * factor.prob

# Table of APEs
cbind(APE.log, APE.prob)

# Automatic APE calculations with package mfx
library(mfx)
logitmfx(approve~white+ hrat+obrat+ loanprc+ unem+ male+ married+ dep+ sch+ cosign+ chist+ pubrec+
           mortlat1+ mortlat2+ vr, data=loanapp, atmean=FALSE)

####################################################################

#Exercicio 2

####################################################################

# install.packages("haven")
library(haven)
# install.packages("plm")
library(plm)
library(readxl)

setwd('C:/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 4/database')

dados <- read_excel('wage_dataset.xls')

# Item a
mqo_agrupado <- lm(data = dados, formula = lwage ~ educ + exper + expersq)
summary(mqo_agrupado)

# Item b
re <- plm(data = dados, formula = lwage ~ educ + exper + expersq, index = c('id','year'), model = 'random')
summary(re)

# Item c
fe <- plm(data = dados, formula = lwage ~ educ + exper + expersq, index = c('id','year'), model = 'within')
summary(fe)

# Item d
phtest(fe,re)

# Tabela de resultados
stargazer::stargazer(list(mqo_agrupado,re,fe), type = "text" )


####################################################################

#Exercicio 3

####################################################################


library(ggplot2);library(ggfortify)  #autoplot
library(stringr)  #str_split_fixed
library(sandwich) # NeweyWest
library(lmtest)   # coeftest
library(tidyr)    # gather
library(mFilter)  # hpfilter
library(readxl) 

setwd("C:/Users/Daniel/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 4/database")

PIB = read_excel("PIB.xlsx")
names(PIB)=c("aux","data","PIB")

PIB$aux = NULL

PIB = PIB[-(1:3),]

PIB = PIB[-98,]

PIB$PIB = as.numeric(PIB$PIB)

# Serie de tempo
PIB_ts = ts(PIB$PIB, start=c(1996, 1), frequency=4)

#Item a
autoplot(PIB_ts) + 
  ggtitle('PIB Trimestral ( Base 100 = 1995)') + xlab('Year')


aux = as.data.frame(str_split_fixed(PIB$data,
                                    " ", 3))
aux$V2 = NULL
names(aux) = c("tri","ano")

PIB = cbind(PIB,aux)

PIB$Time = (1:nrow(PIB)) # Tendencia

# Item b
reg = lm(PIB ~ Time + factor(tri),data=PIB)
summary(reg)

#Item c
# NeweyWest()
N = nrow(PIB)
m = floor(N^(1/4))

# Matrix de var-cov
nwvcov = NeweyWest(lm(PIB ~ Time + factor(tri),data=PIB), 
                   lag = m - 1, prewhite = F, 
                   adjust = T)

coeftest(reg, vcov = nwvcov)

#Item d
y=diff(log(PIB_ts))

autoplot(y) + 
  ggtitle('Crescimento PIB Trimestral ( Base 100 = 1995)') + xlab('Year')

#Item e
# Vamos rodar AR(1), então precisamos eliminar a primeira obsevação
y_lag = y[1:length(y)-1]
y = y[2:length(y)]

# Definindo T1
T1 = 150
T = length(y)

# Vamos rodar o modelo especificado acima
fac = factor(c(rep(1, T1), rep(2, T-T1)))
reg1 = lm(y ~ -1 + fac/(y_lag))
coeftest(reg1,vcov=vcovHAC(reg1))

vcovHAC(reg1)

# Teste F de acordo com as Hypothesis apresentadas acima
hyp = matrix(0,2,4)
hyp[1,1] = hyp [2,3] = 1
hyp[1,2] = hyp [2,4] = -1

linearHypothesis(reg1,hyp,vcov=vcovHAC(reg1))

#Item f
#Caso seja constata uma tendência linear, a primeira diferente é suficiente para encontrarmos uma variável estacionário
#ou seja, o crescimento do PIB seria estacionário. Caso encontremos que o crescimento do PIB é estacionário,
#então PIB possui uma raiz unitária.

adf.test(y, alternative = c("stationary", "explosive"))
