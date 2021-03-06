<<<<<<< HEAD
setwd("C:\\Users\\fabri\\Desktop\\Projetos\\Portfolio")
##### PACKAGES

library(quantmod)
library(rugarch)
library(PortfolioAnalytics)
library(moments)
library(DEoptim)

###
# O objetivo deste programa � utilizar um GARCH com controles para estimar stressed Value-at-Risk.
# Tomando o �ndice BOvespa como nosso portf�lio, vamos estimar a volatilidade considerando a infla��o mensal como
# uma vari�vel proxy para desempenho econ�mico. Em seguida vamos projetar a volatilidade considerando diferentes
# cen�rios de infla��o e calcular o VaR do portf�lio.
###

#### Coletando os dados

getSymbols('^BVSP' ,from ='2007-02-01' , to = '2020-07-01')
ipca = t(read.csv('ipca.csv' , header = F , nrows = 162))
ipca = as.numeric(ipca)


ipca = xts(ipca[-1] , order.by = seq(as.Date("2007/2/1"), as.Date("2020/6/1"), by = "month"))


# Considerando �ndice bovespa como portf�lio

db = CalculateReturns(BVSP$BVSP.Adjusted)[-1]

# Ajustando missing via interpola��o linear

summary(db)
db = na.approx(db)

# Avaliando normalidade

jarque.test(as.data.frame(db)[,1])
skewness(db)


# Transformando dados mensais em di�rio
ipca.d = na.spline(merge(ipca,foo=zoo(NA, order.by=seq(start(db), end(db),
                                                       "day",drop=F)))[, 1])
ipca.d = merge(ipca.d , db , all = F)[,1]
                         

#### Projetando VaR

# Condi��es normais

spec = ugarchspec(mean.model = list(armaOrder = c(0,0)) , 
                  variance.model = list( model = 'sGARCH' , external.regressors = ipca.d), 
                  distribution.model = 'sstd')

# A partir da 1000 obs inicia-se o processo de previs�o da volatilidade. A cada 200 observa��es o modelo � reestimado
# e a previs�o � refeita.

roll = ugarchroll(spec , data = db , refit.every = 100 , refit.window = 'moving' , n.start = 1000)

# Calculando VaR
garchVaR = quantile(roll , probs=.05)

realized = xts(as.data.frame(roll)$Realized , time(garchVaR))

VaRplot(alpha = .05 , actual = realized , VaR = garchVaR)

# Precis�o hist�rica do VaR
mean(realized<garchVaR)

# VaR em 30/06/2020
tail(garchVaR)



#### Cen�rio de stress

ipca.s = ipca.d

ipca.s[3220:3323] = ipca.s[3220:3323]+.5

spec.s = ugarchspec(mean.model = list(armaOrder = c(0,0)) , 
                  variance.model = list( model = 'sGARCH' , external.regressors = matrix(ipca.s)), 
                  distribution.model = 'sstd')

roll.s = ugarchroll(spec.s , data = db , refit.every = 100 , refit.window = 'moving' , n.start = 1000)

# Calculando VaR
sgarchVaR = quantile(roll.s , probs=.05)

realized = xts(as.data.frame(roll.s)$Realized , time(sgarchVaR))

VaRplot(alpha = .05 , actual = realized , VaR = sgarchVaR)

mean(realized<garchVaR)

# VaR em 30/06/2020
tail(sgarchVaR)
=======
setwd("C:\\Users\\fabri\\Desktop\\Projetos\\Portfolio")
##### PACKAGES

library(quantmod)
library(rugarch)
library(PortfolioAnalytics)
library(moments)
library(DEoptim)

###
# O objetivo deste programa � utilizar um GARCH com controles para estimar stressed Value-at-Risk.
# Tomando o �ndice BOvespa como nosso portf�lio, vamos estimar a volatilidade considerando a infla��o mensal como
# uma vari�vel proxy para desempenho econ�mico. Em seguida vamos projetar a volatilidade considerando diferentes
# cen�rios de infla��o e calcular o VaR do portf�lio.
###

#### Coletando os dados

getSymbols('^BVSP' ,from ='2007-02-01' , to = '2020-07-01')
ipca = t(read.csv('ipca.csv' , header = F , nrows = 162))
ipca = as.numeric(ipca)


ipca = xts(ipca[-1] , order.by = seq(as.Date("2007/2/1"), as.Date("2020/6/1"), by = "month"))


# Considerando �ndice bovespa como portf�lio

db = CalculateReturns(BVSP$BVSP.Adjusted)[-1]

# Ajustando missing via interpola��o linear

summary(db)
db = na.approx(db)

# Avaliando normalidade

jarque.test(as.data.frame(db)[,1])
skewness(db)


# Transformando dados mensais em di�rio
ipca.d = na.spline(merge(ipca,foo=zoo(NA, order.by=seq(start(db), end(db),
                                                       "day",drop=F)))[, 1])
ipca.d = merge(ipca.d , db , all = F)[,1]
                         

#### Projetando VaR

# Condi��es normais

spec = ugarchspec(mean.model = list(armaOrder = c(0,0)) , 
                  variance.model = list( model = 'sGARCH' , external.regressors = ipca.d), 
                  distribution.model = 'sstd')

# A partir da 1000 obs inicia-se o processo de previs�o da volatilidade. A cada 200 observa��es o modelo � reestimado
# e a previs�o � refeita.

roll = ugarchroll(spec , data = db , refit.every = 100 , refit.window = 'moving' , n.start = 1000)

# Calculando VaR
garchVaR = quantile(roll , probs=.05)

realized = xts(as.data.frame(roll)$Realized , time(garchVaR))

VaRplot(alpha = .05 , actual = realized , VaR = garchVaR)

# Precis�o hist�rica do VaR
mean(realized<garchVaR)

# VaR em 30/06/2020
tail(garchVaR)



#### Cen�rio de stress

ipca.s = ipca.d

ipca.s[3220:3323] = ipca.s[3220:3323]+.5

spec.s = ugarchspec(mean.model = list(armaOrder = c(0,0)) , 
                  variance.model = list( model = 'sGARCH' , external.regressors = matrix(ipca.s)), 
                  distribution.model = 'sstd')

roll.s = ugarchroll(spec.s , data = db , refit.every = 100 , refit.window = 'moving' , n.start = 1000)

# Calculando VaR
sgarchVaR = quantile(roll.s , probs=.05)

realized = xts(as.data.frame(roll.s)$Realized , time(sgarchVaR))

VaRplot(alpha = .05 , actual = realized , VaR = sgarchVaR)

mean(realized<garchVaR)

# VaR em 30/06/2020
tail(sgarchVaR)
>>>>>>> 0c6d90e24fad9b131d97cf59b18b716e3d9cff0c
