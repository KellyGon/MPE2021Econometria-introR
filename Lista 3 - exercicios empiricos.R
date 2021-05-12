####################################################################
#@author: Kelly G. dos Santos
#MPE - Econometria 2021 S1

#Arquivo com respostas dos exercícios empíricos 

#Lista 3
####################################################################
install.packages('spin')
library(knitr)
spin('C:/Users/Daniel/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 3/respostas/Lista 3 - exercicios empiricos.R', precious = TRUE, doc = '#')

install.packages('wooldridge')
library('wooldridge')

####################################################################

#Exercicio 1

####################################################################

data('meap93')

reg<-lm(lsalary~lenroll+ lstaff+ droprate+ gradrate,data=meap93)
summary(reg)

#Item a
meap93_exclude<-meap93[!(meap93$bensal<0.01),]
dim(meap93)[1]-dim(meap93_exclude)[1] #apenas 4 obs

#Item b
reg2<-lm(lsalary~lenroll+ lstaff+ droprate+ gradrate,data=meap93_exclude)
summary(reg2)
#estimadores ficaram relativamente parecidos

#Item c
reg3<-lm(I(lsalary*2)~I(lenroll*2)+ I(lstaff*2)+ I(droprate*2)+ I(gradrate*2),data=meap93_exclude)
summary(reg3)
#Apenas o b0 se alterou. Os outros estimadores ficaram iguais.

####################################################################

#Exercicio 2

####################################################################

library(AER) #package pra rodar o ivreg
library(haven)
library(stargazer)

#'Escrever o diretorio do seu computador'
setwd('C:/Users/Daniel/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 3/database')  

##################### Exercício Adicional
# Importando a base de dados
mydata = read_dta("C:/Users/Daniel/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 3/database/QOB_dataset.dta")

attach(mydata)  # para puxar os dados direto de mydata

#### Item 1
## subitem a
reg.ols = lm(lnw~s)
summary(reg.ols)
## subitem b
# E(s|u) = 0
# E(u) = 0
# O que pode comprometer é que anos de estudo pode estar correlacionado com algum termo no u, de
# tal forma que E(s|u) é diferente de 0, o que enviesaria os estimadores. Por exemplo, anos de
# escolaridade poderia estar ligada a gênero, mulheres costumavam casar mais cedo e paravam de
# frequentar a escola para cuidar da casa.

#### Item 2
## subitem a
quarter1 = rep(x=c(0),times=length(s))
quarter2 = quarter1
quarter3 = quarter1
quarter4 = quarter1


n = length(qob)  

for (i in 1:n){
  if (mydata$qob[i] == 1){
    quarter1[i] = 1
  }
}

for (j in 1:n){
  if (mydata$qob[j] == 2){
    quarter2[j] = 1
  }
}

for (l in 1:n){
  if (mydata$qob[l] == 3){
    quarter3[l] = 1
  }
}

for (m in 1:n){
  if (mydata$qob[m] == 4){
    quarter4[m] = 1
  }
}



mydata = cbind(mydata,quarter1,quarter2,quarter3,quarter4)


## subitem a
reg.IV = ivreg(lnw ~ s|quarter4)
summary(reg.IV)
summary(reg.IV, vcov = sandwich, df = 329507, diagnostics = TRUE)


## subitem b
# 1o estagio
reg.1o = lm(s~quarter4)
summary(reg.1o)

# 2o estagio
reg.2o = lm(lnw~fitted(reg.1o))
summary(reg.2o)
summary(reg.2o, vcov = sandwich, df = 329507, diagnostics = TRUE)


## subitem c
stargazer(reg.ols,reg.IV,reg.2o, type="text")



### Exercicio 3
## subitem a
reg.aux1 = lm(lnw~quarter4)
reg.aux2 = lm(s~quarter4)

pi = reg.aux1$coefficients[2]
alfa = reg.aux2$coefficients[2]
(beta = pi/alfa)


####################################################################

#Exercicio 3

####################################################################

data('wage2')
wage2<-wage2[complete.cases(wage2), ]

#Item a
reg<-lm(lwage~educ,data=wage2)
summary(reg)

#Item b
install.packages('ivreg')
library('ivreg')
iv<-ivreg(lwage~educ | sibs,data=wage2)
summary(iv)
reg$coefficients[2]
iv$coefficients[2]
#O estimador dobrou de tamanho e continua sig.

#Item c
#Negativamente correlacionado-> os filhos que nasceram depois podem ter uma educação pior
reg1<-lm(educ~brthord,data=wage2)
summary(reg1)
#existe uma relação negativa e significativa

#Item d
iv1<-ivreg(lwage~educ | brthord,data=wage2)
summary(iv1)
#aumento de 1 ano de educação esta correlacionado com o aumento de 13% do salário

#Item e
#A ordem de nascimento impacta o salário apenas via educação -> os filhos que nasceram depois
#possuem uma educação pior apenas porque nasceram depois.

#Item f
iv2<-ivreg(lwage~educ+sibs | brthord+sibs,data=wage2)
summary(iv2)
#efeito de educ tornou-se não sig.
#Os dois efeitos são positivos, entao a quantidade de filhos e a educação são positivamente
#correlacionadas com salário

#Item g
reg2<-lm(educ~brthord+sibs,data=wage2)
wage2$educ_hat<-reg2$fitted.values
reg3<-lm(educ_hat ~sibs,data=wage2)
summary(reg3) #relação negativa -> quanto maior a quantidade de filhos, menos a educação

####################################################################

#Exercicio 4

####################################################################

data('mroz')
mroz<- mroz[complete.cases(mroz), ]
  
  
#Item a
reg<-lm(hours~lwage+educ+age+ kidslt6 + nwifeinc, data=mroz)
reg_elast<-lm(I(log(hours))~lwage+educ+age+ kidslt6 + nwifeinc, data=mroz)
summary(reg_elast) #elast: 0,043216

#Item b
iv<-ivreg(hours~lwage+educ+age+ kidslt6 + nwifeinc | 
            motheduc+ fatheduc+lwage+age+ kidslt6 + nwifeinc , data=mroz)
summary(iv)

#Item c
u_hat<-iv$residuals
reg<-lm(u_hat~lwage+educ+age+ kidslt6 + nwifeinc+motheduc+ fatheduc, data=mroz)

LM=dim(mroz)[1]*summary(reg)$r.squared
qchisq(.05, df=2,lower.tail = FALSE)  
LM
#H0: coef=0 -> Exogeneidade dos IV
#LM<VC -> não rejeitamos a nula -> IV é exógeno

####################################################################

#Exercicio 5

####################################################################

library("readxl")

df = read_excel("C:/Users/Daniel/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 3/database/SupplyDemand.xlsx")

# Item a - Estimate reduced-form parameters
q.lm <- lm(q ~ ps + di + pf, data = df)
p.lm <- lm(p ~ ps + di + pf, data = df)

# Step 2. Use the predicted value of P and plug into the right-hand side of the structural equations. 
df$phat <- p.lm$fitted.values
demand.lm <- lm(q ~ phat + ps + di, data = df)
supply.lm <- lm(q ~ phat + pf, data = df)

summary(demand.lm)
summary(supply.lm)

#Ou automático:
library(sem)
demand <- tsls(q ~ p + ps + di, ~ ps + di + pf, data = df)   
supply <- tsls(q ~ p + pf, ~ ps + di + pf, data = df)




library('wooldridge')
iv<-ivreg(lincome~age +ageq| educ  , data=smoke)
summary(iv)
data(smoke)

