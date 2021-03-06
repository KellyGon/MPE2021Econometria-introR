####################################################################
#@author: Kelly G. dos Santos
#MPE - Economia 2021 S1

#Arquivo com respostas dos empiricos

#Lista 1

####################################################################
#install.packages('knitr')

library(knitr)
spin('C:/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 1/respostas/Lista 1 - exercicios emp�ricos.R', precious = TRUE, doc = '#')

install.packages('logr')
library(logr)
log_open("mytest.log")


install.packages('wooldridge')
library('wooldridge')

####################################################################

#Exercicio 1

####################################################################

install.packages("ggpubr")
library("ggpubr")

data('jtrain2')

#Item a: fracao de homens que receberam treinamento
print(paste0("A fra��o de indiv�duos que receberam treinamento �: ", mean(jtrain2$train)))

#Item b: m�dia de rendimento dos homens que receberam o treinamento (train==1)
aggregate(jtrain2$re78, list(jtrain2$train), mean)

#Item c: m�dia de desempregados dos homens que receberam o treinamento (train==1)
aggregate(jtrain2$unem78, list(jtrain2$train), mean)

#Item d: correla��o entre treinamento e desemprego.
cor(jtrain2$train, jtrain2$unem78, method = "pearson")
#N�o podemos afirmar que o treinamento tem efeito causal negativo no desemprego.
#Podemos apenas afirmar que treinamento tem uma correla��o negativa com desemprego.


####################################################################

#Exercicio 2

####################################################################

data('fertil2')

#Item a: menor, maior e m�dia de children
min(fertil2$children)
max(fertil2$children)
mean(fertil2$children)

#Item b: propor��o de eletricidade em casa?
print(paste0("A fra��o de mulheres que possuem eletricidade �: ", mean(fertil2$electric, na.rm=TRUE)))

#Item c: m�dia de crian�as sem eletricidade e com eletricidade
aggregate(fertil2$children, list(fertil2$electric), mean)
#Aqueles lares sem eletricidade tem uma quantidade menor de crian�as do que os lares com eletricidade

#Item d: N�o podemos afirmar que eletricidade tem um efeito causal no n�mero de crian�as.
# � possivel que uma vari�vel omitida tenha correla��o com o fato de a mulher ter eletricidade em casa
# e isso tamb�m esteja correlacionada com a quantidade de crian�as. O acesso a m�todos contraceptivos 
# � uma dessas vari�veis, que impactam a quantidade de crian�as e est� correlacionado com eletricidade

####################################################################

#Exercicio 3

####################################################################

data('hprice1')

#Item a: regress�o
reg<-lm(price~sqrft+ bdrms,data=hprice1)
summary(reg)
# Se quarto aumentar em uma unidade, o pre�o aumenta em 15.2 aprox.

#Item b: R^2= 0.6319. Cerca de 63% da varia��o do pre�o � explicada pelas vari�veis nessa amostra

#Item c: 
price1<- reg$coefficients[1]+ reg$coefficients[2]*2438 +  reg$coefficients[3]*4
#ou:
price1_function<- reg$fitted.values[1]

#Item d:
resid1<- hprice1$price[1] - price1
#ou:
resid1_function<- reg$residuals[1]
#O res�duo � negativo, ou seja, o comprador pagou menos do que o esperado 
# o pre�o esparado era de 350 e o comprador pagou 300.


####################################################################

#Exercicio 4

####################################################################

data('wage2')

#Item a: 
reg<-lm(IQ~educ,data=wage2)
#delta:
delta_tilde<-reg$coefficients[2]

#Item b: 
reg<-lm(lwage ~ educ,data=wage2)
beta_tilde<-reg$coefficients[2]

#Item c:
reg<-lm(lwage~educ+IQ ,data=wage2)
beta1_hat<-reg$coefficients[2]
beta2_hat<-reg$coefficients[3]

#Item d:
beta1_hat+ beta2_hat*delta_tilde
beta_tilde


log_close()
