####################################################################
#@author: Kelly G. dos Santos
#MPE - Econometria 2021 S1

#Arquivo com respostas dos exercícios empíricos 

#Lista 2

####################################################################
spin('C:/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 2/respostas/Lista 2 - exercicios empiricos.R', precious = TRUE, doc = '#')

#install.packages('wooldridge')
library('wooldridge')

####################################################################

#Exercicio 1

####################################################################

#install.packages('car')
library('car')

data('wage2')

reg<- lm(lwage~educ  + exper+tenure, data=wage2) 
std_errors<-diag(vcov(reg))
N=length(reg$residuals) #tamanho da amostra
SSR_unr <- sum( reg$resid^2 )

betahat1<-reg$coefficients[2]
se_betahat1<-std_errors[2]
betahat2<-reg$coefficients[3]
se_betahat2<-std_errors[3]
betahat3<-reg$coefficients[4]
se_betahat3<-std_errors[4]

#Item a: Teste b1=1
t_obs=abs((betahat1-1)/(se_betahat1)^(1/2))
t_critico=abs(qt(0.025, N-3-1))

#rejeitamos H0 beta1=1 se |t_obs|> t_critico. Como 141>1.96 rejeitamos a nula.
#Conclusaõ: Rejeitamos que beta1=1.

#Item b: Teste b2>b3
se_beta1xbeta2= (se_betahat2^2 + se_betahat3^2 -2*vcov(reg)[4,3])^(1/2)
#std. errors:
t_obs=abs((betahat2-betahat3)/(se_beta1xbeta2))
t_critico=abs(qt(0.05, N-3-1))

#rejeitamos H0 com H1 (beta2>beta3) se t_obs> t_critico. Como 0,41>1.64 não rejeitamos a nula.
#Conclusaõ: Não rejeitamos que beta2=beta3.

##Item c: testando se b2=b3 (teste de médias)
t_critico=abs(qt(0.025, N-3-1))
CI=NA
CI[1]=(betahat2-betahat3)-t_critico*se_beta1xbeta2
CI[2]=(betahat2-betahat3)+t_critico*se_beta1xbeta2
CI #intervalo de confiança está em torno do 0. O que indica que os efeitos parecem iguais.

#rejeitamos H0 (beta2=beta3) se |t_obs|> t_critico. Como 0,41<1.96 nao rejeitamos a nula.
#Conclusaõ: Não rejeitamos que os coeficientes sejam iguais novamente.

#Item d: testando se b2=b3=0
reg<- lm(lwage~educ , data=wage2) 
SSR_r <- sum( reg$resid^2 )
k=3
q=2

F_obs=((SSR_r-SSR_unr)/q)/(SSR_unr/(N-k-1))
F_critico=qf(p=.05, df1=q, df2=N-k-1, lower.tail=FALSE)

#rejeitamos H0 (b2=b3=0) se F_obs> F_critico. Como 31>3, rejeitamos a nula.
#Conclusaõ: Rejeitamos que os coeficientes sejam conjuntamente insignificantes.

#Ou, automático:
reg<- lm(lwage~educ  + exper+tenure, data=wage2) 
linearHypothesis(reg, c("exper=0", "tenure=0"))
#Rejeitamos nula -> estimadores sao conj sig.

#Item e
reg<- lm(lwage~educ  + exper+tenure, data=wage2) 
u_hat<-reg$residuals
uq_hat<-u_hat^2

#Teste de BP:
reg2<- lm(uq_hat~educ  + exper+tenure, data=wage2) 
#teste F
linearHypothesis(reg2, c("educ=0","exper=0", "tenure=0"))
#Rejeitamos Nula-> var. são sig -> u^2 depende das var. ind -> hetero

#teste LM
LM<-dim(wage2)[1]*summary(reg2)$r.squared
LM>qchisq(.05, df=3,lower.tail = FALSE)  
#LM> critical-> rejeitamos nula -> var. são sig -> u^2 depende das var. ind -> hetero

#Teste de White
y_hat<-reg$fitted.values
yq_hat<-y_hat^2
reg3<- lm(uq_hat~y_hat  +yq_hat , data=wage2) 
#teste F
linearHypothesis(reg3, c("y_hat=0","yq_hat=0"))
#Não rejeitamos Nula-> var. não são sig -> u^2 não depende de y_hat -> homo

#teste LM
LM<-dim(wage2)[1]*summary(reg3)$r.squared
LM>qchisq(.05, df=3,lower.tail = FALSE)  
#LM> critical-> não rejeitamos nula -> var. não são sig -> u^2 não depende de y_hat -> homo

#Ou automático:
#BP:
bptest(reg)
#H0: homo -> p-valor pequeno -> rejeitamos H0 -> hetero

#White
#install.packages('skedastic')
library('skedastic')

white_lm(reg)
#H0: homo -> p-valor pequeno -> rejeitamos H0 -> hetero
#OU
bptest(reg, ~ fitted(reg) + I(fitted(reg)^2) )

#Estimação robusta:
reg<- lm(lwage~educ  + exper+tenure, data=wage2) 
summary(reg)
coeftest(reg, vcov = vcovHC(reg, type="HC1"))
#Estimadores continuam significativos

####################################################################

#Exercicio 2

####################################################################

data('smoke')

#Item a
reg<- lm(lincome~cigs  + educ+age+ agesq, data=smoke) 
summary(reg)
#Beta1-> Um aumento de um cigarro fumado por dia está correlacionado com uma renda de 0,1731% maior.
#Beta4-> efeito parcial do termo quadrático de idade. Um aumento de 1 ano na idade está correlacionado
#com o aumento de 5,7% na renda.
reg$coefficients[4]+reg$coefficients[5]

#Item b
#O preço dos cigarros provavelmente é determinado pela renda 
#e a adoção da restrição pode também ser determinado pela renda
# Caso essas variáveis sejam exógenas, esperamos que delta5 seja negativo (um aumento do preço
# está relacionado com a diminuição do consumo) e que delta6 seja negativa também ( uma restrição
# maior, o consumo tende a cair)

#Item c
reg<- lm(cigs~lincome  + educ+age+ agesq + lcigpric + restaurn, data=smoke) 
summary(reg)
#A diferença de estados é dada pelo estimador associado à restaurn: -2,825
#essa diferença é significante à 5%.

#Item d
library(sandwich)
library(lmtest)
coeftest(reg, vcov = vcovHC(reg, type="HC1"))
# O estimadpr continuou o mesmo, mas o erro padrão se alterou de tal forma que o estimador ficou 
#significante ao nível de 1%.


####################################################################

#Exercicio 3

####################################################################

data('gpa1')

#Item a
reg<- lm(colGPA~hsGPA  + ACT+skipped+ PC, data=gpa1) 
summary(reg)
#Intercepto, hsGPA, skipped e PC sao significantes à 5% de significância

u_hat<-reg$residuals

#Item b
y_hat<-reg$fitted.values
yq_hat<-y_hat^2
reg2<-lm(I(u_hat^2)~y_hat  + yq_hat, data=gpa1) 
#teste F
linearHypothesis(reg2, c("y_hat=0","yq_hat=0"))
#Rejeitamos Nula a 5 %-> var. são sig -> u^2 depende de y_hat -> heterocedasticidade

h_hat<-reg2$fitted.values

#Item c
summary(h_hat) #todos sao positivos

#oU wls:
WLS<- lm(colGPA~hsGPA  + ACT+skipped+ PC, data=gpa1, weight=1/h_hat) 

summary(WLS)
summary(reg)
#Os coeficientes são parecidos, mas o estimador skipped tornou-se mais significativo. 
#ACT continuou insignificante

#Item d:
#FGLS usando exponencial como forma funcional:
log_uq_hat<-log(resid(reg)^2)
varreg<- lm(log_uq_hat~hsGPA  + ACT+skipped+ PC, data=gpa1) 
h_hat <- exp(fitted(varreg))
FGLS<- lm(colGPA~hsGPA  + ACT+skipped+ PC, data=gpa1,weight=1/h_hat) 
summary(FGLS)
#Estimadores para os erros-padrões ficaram bem parecido com item c.

#Automático:
coeftest(reg, vcov = vcovHC(reg, type="HC1")) #erros-padrões robustos à hetero

####################################################################

#Exercicio 4

####################################################################

data('vote1')

reg<- lm(voteA~prtystrA  + democA+lexpendA+ lexpendB, data=vote1) 
summary(reg)
#Os estimadores são significativos

u_hat<-reg$residuals
reg2<- lm(u_hat~prtystrA  + democA+lexpendA+ lexpendB, data=vote1) 
summary(reg)$r.squared
#R^2 muito próximo de 0, pois a estimação dos betas é tal que os resíduos não
#sejam correlacionados com as var. independentes. 

#Item b
#BP:
reg3<-lm(I(u_hat^2)~prtystrA  + democA+lexpendA+ lexpendB, data=vote1) 
linearHypothesis(reg3, c("prtystrA=0","democA=0", "lexpendA=0","lexpendB=0"))
#Não rejeitamos Nula-> var. não são sig -> u^2 não depende das var. independentes -> homo

#Item c
#Teste de White
y_hat<-reg$fitted.values
yq_hat<-y_hat^2
reg3<- lm(I(u_hat^2)~y_hat  +yq_hat , data=vote1) 
#teste F
linearHypothesis(reg3, c("y_hat=0","yq_hat=0"))
#Não rejeitamos Nula-> var. não são sig -> u^2 não depende de y_hat -> homo


####################################################################

#Exercicio 5

####################################################################

data('wage2')

#Item a

reg<- lm(lwage~educ  +exper+ tenure+ married+south+urban+black+KWW , data=wage2) 
summary(reg)
#Um ano a mais de educação (educ) está relacionado a um aumento de 5,7% no salário.

#Item b
#A diferença estimada é de 18,94% positivo para os casados e3 essa diferença é sig.

#Item c
reg<- lm(lwage~educ  +exper+ tenure+ married+south+urban+black+KWW + IQ, data=wage2) 
summary(reg)
#Um ano a mais de educação (educ) está relacionado a um aumento de 4,98% no salário. Ou seja, 
#o efeito diminuiu. Ou seja, parte do efeito de educ é medido pela variável IQ.

#Item d
#KWW e IQ são sig. à nivel de 5%
linearHypothesis(reg, c("KWW=0","IQ=0"))
#Rejeita H0 de que os parametros são 0, então KWW e IQ são conj. sig.

#Item e
white_lm(reg)
#H0: homo. p-valor pequeno-> rejeitamos H0-> hetero

#Automático:
coeftest(reg, vcov = vcovHC(reg, type="HC1")) #erros-padrões robustos à hetero
#IQ continuou significante a nivel de 1%, porém KWW deixou de ser sig a nivel de 5% e passou
#a ser sig apenas a nivel de 10%.
