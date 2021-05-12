rm(list=ls())

install.packages("readxl")
install.packages("readr")
install.packages("stringr")
install.packages('dynlm')
install.packages("ggplot2")

library("readxl")
library("readr")
library(dplyr)
library(stringr)
library(tidyr)
library(dynlm) 
library(ggplot2)

######################################################################################################
#Opening
######################################################################################################

#Path:
setwd("C:/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 2/database")

df<-read_excel('Data_Extract_From_World_Development_Indicators.xlsx',sheet=1)

#excluding missings
df <- df[complete.cases(df), ]

#only important variables
df_wide <- subset(df, select = c("Country Code", "Series Code", "ind" ))

#long-> wide
df_wide <- spread(df_wide, key = "Series Code", value = "ind")

# #Arrumando o nome das variáveis
old_name<-read_excel('Data_Extract_From_World_Development_Indicators.xlsx',col_names = FALSE,sheet=2)
old_name<-as.character(old_name[1,which(!is.na(old_name[1,]))])

new_name<-read_excel('Data_Extract_From_World_Development_Indicators.xlsx',col_names = FALSE,sheet=3)
new_name<-as.character(new_name[1,which(!is.na(new_name[1,]))])

df_wide<-df_wide %>% rename_at(vars(old_name), ~ new_name)

######################################################################################################
# Estatísticas descritivas
######################################################################################################

#Com ajuste
ggplot(df_wide, aes(x=gini, y=gdpg)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE)+
  geom_text(label=df_wide$country) + xlab("Gini Index") + ylab("GDP Growth")


#Sem ajuste
ggplot(df_wide, aes(x=`SI.POV.GINI`, y=`NY.GDP.PCAP.KD.ZG`)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE)+
  geom_text(label=df_wide$`Country Code`) + xlab("Gini Index") + ylab("GDP Growth")



######################################################################################################
# Regressão principal
######################################################################################################

#Com ajuste
df_reg<- df_wide %>% drop_na(eletricity, savings, inflation)
reg = lm(gdpg ~ eletricity + savings	+inflation , data=df_reg)
summary(reg)

library(stargazer)
stargazer (reg, type  = "text")


######################################################################################################
# Std errors robustos
######################################################################################################

#Como pegar std errors:
vcov(reg)
diag(vcov(reg))

#Calcular std errors robustos: 
install.packages('lmtest')
install.packages('sandwich')

library(lmtest)
library(sandwich)
coeftest(reg, vcov = vcovHC(reg, type="HC1"))

length(reg$residuals) #tamanho

#Um N pequeno faz com que o poder do teste seja pequeno, ou seja, que o falso-negativo seja grande
#(1-beta)=poder. beta=aceita H0 quando é falsa, ou seja, diz que estimador é insig quando é sig.
#Outro problema do missing é que se não for aleatória a observação dos países, podemos estar estimando
#a correlação para uma parcela selecionada de países

summary(reg)
#Interpretação e sifnificância

#teste conjunto
library(car) 
linearHypothesis(reg, c("savings=0","inflation=0"))
#conjuntamente significantes


#Pela regressão (3), estimamos apenas as correlações e não a causalidade





















######################################################################################################
# Bônus 1: Std errors robustos
######################################################################################################

#Como pegar std errors:
vcov(reg)
diag(vcov(reg))

#Calcular std errors robustos: 
coeftest(reg, vcov = vcovHC(reg, type="HC1"))


######################################################################################################
# Bônus 2: Calcular taxa de crescimento
######################################################################################################

PIB<-as.data.frame(c(1:10))

#Convert PIB into a ts
PIB_ts<-ts(PIB)

#First Difference
PIB_diff<-diff(PIB_ts)
PIB_diff<- as.vector(PIB_diff)

#Growth
n<-dim(PIB)[1]
Growth<- PIB_diff/PIB[1:(n-1),]

#Growth with ln
growth<-diff(ts(log(PIB))) #log() by default computes the natural logarithms (Ln, with base e):


######################################################################################################
# Bônus 3: Índice de Gini
######################################################################################################

install.packages("ineq")
library(ineq)

ineq(df_wide$gdpc,type="Gini")

plot(Lc(df_wide$gdpc),col="darkred",lwd=2)

######################################################################################################
# Bônus 4: Calcular F na mao
######################################################################################################

#Item d: testando se b2=b3=0
std_errors<-diag(vcov(reg))
N=length(reg$residuals) #tamanho da amostra
SSR_unr <- sum(reg$resid^2)

reg<- lm(gdpg ~ eletricity  , data=df_reg) 
SSR_r <- sum( reg$resid^2 )
k=3
q=2

F_obs=((SSR_r-SSR_unr)/q)/(SSR_unr/(N-k-1))
F_critico=qf(p=.05, df1=q, df2=N-k-1, lower.tail=FALSE)



