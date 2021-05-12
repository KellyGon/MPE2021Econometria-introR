rm(list=ls())

library(readxl)
library(aod)     # Wald teste
library(car) # Teste de hipoteses lineares
library(ggplot2) #autoplot
  library(dynlm) # Lag das variaveis
library(tseries)

#Pasta:
setwd("C:/Users/Kelly/OneDrive - Fundacao Getulio Vargas - FGV/Monitoria/MPE - Econometria 2021 S1/Listas/Lista 4/database")

data = read_excel('PPP_dataset.xlsx')

#Item 1

cambio_raw = ts(data$Cambio,start=c(2000,1,1),frequency=12)
# Se quiserem pegar uma parte do período: window: subset the time series, take from 2012 until the end
cambio_nom = window(cambio_raw,start=c(2012))

plot(cambio_nom)
#autoplot(cambio_nom) + 
  # ggtitle('Cambio Nominal Diario') + xlab('Ano') + ylab('R$/US$')

cambio_clean = cambio_nom[ !is.na(cambio_nom)]

#Item 2
cambio = diff(log(cambio_clean))

#Item 3
# Grafico da serie
plot(cambio)
qplot(1:length(cambio),cambio) + 
  ggtitle('Variacao Cambial (DifLog)') + xlab('Dia (1 = 2/1/2000)') + ylab('%')

#Item 4
# Gerando a dummy
cambio = as.data.frame(cambio)
cambio$sobe_desce = 0
cambio$sobe_desce[cambio$cambio>=0] = 1


# Regressao subida
reg1 = lm(sobe_desce ~ 1, data=cambio)
summary(reg1)


#Item 5 - ordem
library(forecast) # ggAcf

cambio_ts = ts(cambio$cambio,start=c(2012,1,1),frequency=12)

# Correlogramas
ggAcf(cambio_ts) + ggtitle("Autocorrelacao")
ggPacf(cambio_ts) + ggtitle("Autocorrelacao Parcial")
#Parece ser um AR(1)
#

AR1 <- arima(cambio_ts,order = c(1, 0, 0))

AR1 = dynlm(cambio_ts ~ L(cambio_ts,1))
summary(AR1) #estimador significativo para um período atrás

MA1 <- arima(cambio_ts,order = c(0, 0, 1))
summary(MA1) 



#Item 6: hipótese que o modelo seja estacionariamente fraco. Ou seja, que média e correlações independem do tempo
plot(cambio_ts)
adf.test(cambio_ts) #evidencias de que seja estacionaria








#Item 7
#projeção:
print(autoplot(fitted(arima(cambio_ts,order = c(1, 0, 0))),colour = 'black') +
        autolayer(cambio_ts)+ ggtitle(paste('ARMA (',1,',',0,')',sep='')) + 
        xlab('Tempo') + ylab('Cambio'))

print(autoplot(fitted(arima(cambio_ts,order = c(0, 0, 1))),colour = 'black') +
        autolayer(cambio_ts)+ ggtitle(paste('ARMA (',0,',',1,')',sep='')) + 
        xlab('Tempo') + ylab('Cambio'))

print(autoplot(fitted(arima(cambio_ts,order = c(1, 0, 1))), colour = 'black') +
        autolayer(cambio_ts)+ ggtitle(paste('ARMA (',1,',',1,')',sep='')) + 
        xlab('Tempo') + ylab('Cambio'))








#Item 8
fit <- Arima(cambio_raw, xreg=data$TBF, order=c(1,1,0))
summary(fit)
fcast <- forecast(fit, xreg=data$TBF, h=5)
autoplot(fcast) + xlab("Year") +
  ylab("Cambio")

#Auto.arima
(fit <- auto.arima(cambio_raw,
                   xreg=data$TBF))

fcast <- forecast(fit, xreg=data$TBF, h=5)
autoplot(fcast) + xlab("Year") +
  ylab("Cambio")


#Item 9

reg = dynlm(Cambio ~ IPCA+CPI + L(Cambio),data=data)
summary(reg)
adf.test(reg$residuals)
acf(reg$residuals)
#H0: estacionariedade (sao cointegrados) -> Nao rejeita -> Aparentemente nao há cointegração

#Outros testes de cointegração (arquivo separado)


#######################################################
#Extensao
######################################################

devtools::install_github('ipea/ipeaData')
install.packages('ipeadatar') 
series_ipea <- available_series(language = "br")

