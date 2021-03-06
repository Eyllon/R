##### PACKAGES

library(PortfolioAnalytics)
library(quantmod)
library(rugarch)
library(forecast)
library(DEoptim)
library(stochvol)
library(mcmc)
library(MSGARCH)
library(moments)
#### Coletando os dados

# Para compor o portf�lio estamos utilizando as a��es recomendadas pela xp em agosto de 2019.
# Os dados s�o retirado do Yahoofinance utilizando o comando getSymbols do pacote quantmod.

acoes = c('AZUL4.SA' , 'BBDC4.SA' , 'CPLE6.SA' , 'BBDC4.SA' , 'ENBR3.SA' , 'IRBR3.SA' , 'JBSS3.SA' , 'LREN3.SA' , 
          'RENT3.SA' , 'PETR4.SA')

db = matrix(ncol =length(acoes) , nrow = 705)

# Partimos de agosto de 2017
for (i in 1:length(acoes)) {

  getSymbols(acoes[i] , from = '2017-08-01' , to = '2020-06-01')
  aux = get(acoes[i])
  db[,i] = aux[,6]
  aux = NULL
}


colnames(db) = acoes

# Checando missing

summary(db)

# Trantando missing - interpola��o linear

db = na.approx(db)

summary(db)

# Calculando retornos

db = xts(db , order.by = index(AZUL4.SA))

return = Return.calculate(db , method = 'log')[-1,]


plot.zoo(return)
summary(return)

# Avaliando normalidade e assimetria
apply(return , 2 , jarque.test)
apply(return , 2 , skewness)

# TOdas as s�ries compondo a base de dados apresentam distribui��es n�o normais.
# Em geral identificamos assimetria em todos os dados, sendo Global Macro o mais pr�ximo de ser sim�trico
# Para aplica��o do GARCH, vamos generalizar para um GARCH com distribui��o t-student com assimetria (sstd)

# Vamos comparar quatro modelos de otmiza��o: M�dia/Vari�ncia Padr�o, GARCH-sstd, GARCH JRG-sstd e Markov-Switch GARCH
# A janela de an�lise ser� de 2010 at� final de 2018. Considerando os impactos e mudan�as causadas pela crise de 2008/09
# Julgo que o Processo Gerador de dados tenha sofrido altera��es com a crise. Sendo assim, � prov�vel que estender a 
# janela de an�lise para o per�odo pr�-crise seja prejudicial para a precis�o das estimativas.


##### CONSTRUINDO PORTFOLIOS

# O objetivo da an�lise � determinar qual a melhor medida de risco e retorno para otimiza��o do portf�lio.
# Sendo assim vamos estimar o retorno e a volatilidade de diferentes formas, otimizar com minimiza��o do risco
# e avaliar o desempenho e a exposi��o dos portf�lios.

# Per�odo de aplica��o
app_data = return[625:704,]

## M�dia/Vari�ncia

# Especificando portf�lio

port_spec = portfolio.spec(assets = acoes)
port_spec = add.constraint(portfolio = port_spec , type = "full_investment")
# definindo peso m�ximo para portf�lios
port_spec = add.constraint(portfolio = port_spec , type = "box" , min = .01 , max = .3)
port_spec = add.objective(portfolio = port_spec , type = 'risk' , name = 'StdDev')

# Otimizando portf�lio
port_mv = optimize.portfolio.rebalancing(R = return , portfolio = port_spec , optimize_method = 'DEoptim'
                             , rebalance_on = 'quarters' , 
                             training_period = 624)
# Extraindo retorno
returns_mv = Return.portfolio(exp(app_data)-1 , weights = extractWeights(port_mv))



## Modelo GARCH(1,1)

#Fun��o para calcular retorno e varian�a esperados
garchsstd = function(R , portfolio){
  out = list()
  sigma = matrix(nrow = nrow(R) , ncol = ncol(R))
  
  # Por simplicidade, n�o vamos utilizar modelo para a m�dia e vamos considerar a covari�ncia como est�tica no tempo
  for(i in 1:ncol(R)){
    spec = ugarchspec(mean.model = list(armaOrder = c(0,0)) , 
                      variance.model = list( model = 'sGARCH' ), 
                      distribution.model = 'sstd')
    
    fit = ugarchfit(spec , data = R[,i])
    pred = ugarchforecast(fit , n.ahead = nrow(R))
    sigma[,i] = sigma(pred)
  }
  # Salvando resultado no vetor  
  out$StdDev = cov(R)
  diag(out$StdDev) = colMeans(sigma)
  # Definindo volatilidade esperada utilizando GARCH(1,1)  
  
  
  return(out)
}



# Otimizando portf�lio
port_garch = optimize.portfolio.rebalancing(R = return, portfolio = port_spec, optimize_method = "DEoptim", momentFUN = garchsstd , 
                                rebalance_on = 'quarters' , 
                                training_period = 624)
# Extraindo retorno
returns_garch = Return.portfolio(exp(app_data)-1 , weights = extractWeights(port_garch))


## Modelo GARCH-GJR

#Fun��o para calcular retorno e varian�a esperados
garchgjr = function(R , portfolio){
  out = list()
  sigma = matrix(nrow = nrow(R) , ncol = ncol(R))
  
  # Estimando a vari�ncia por GJR GARCH
  for(i in 1:ncol(R)){
    spec = ugarchspec(mean.model = list(armaOrder = c(0,0)) , 
                      variance.model = list( model = 'gjrGARCH' ), 
                      distribution.model = 'sstd')
    
    fit = ugarchfit(spec , data = R[,i])
    pred = ugarchforecast(fit , n.ahead = nrow(R))
    sigma[,i] = sigma(pred)
  }
  # Salvando resultado no vetor  
  out$StdDev = cov(R)
  diag(out$StdDev) = colMeans(sigma)
  # Definindo volatilidade esperada utilizando GARCH(1,1)  
  
  
  return(out)
}

# Otimizando portf�lio
port_gjrgarch = optimize.portfolio.rebalancing(R = return, portfolio = port_spec, optimize_method = "DEoptim", 
                                               momentFUN = garchgjr , rebalance_on = 'quarters' , 
                                               training_period = 624)
# Extraindo retorno
returns_gjrgarch = Return.portfolio(exp(app_data)-1 , weights = extractWeights(port_gjrgarch))




## Modelo Stoch Vol


#Fun��o para calcular retorno e varian�a esperados
volest = function(R , portfolio , theta){
  out = list()
  sigma = matrix(nrow = nrow(R) , ncol = ncol(R))
  
  # Definindo ponto de partida para sorteio
  tet0 = matrix(ncol = 2 , nrow = length(acoes))
  
  for(i in 1:ncol(return)){
    
    # Utilizando o GARCH(1,1) como ponto de partida da estima��o via MCMC
    aux = ugarchspec(mean.model = list(armaOrder = c(0,0)) , 
                     variance.model = list( model = 'sGARCH' ), 
                     distribution.model = 'norm')
    aux = ugarchfit(aux , return[,i])
    # Armazenando par�metros
    tet0[i,1]=coef(aux)[1]
    tet0[i,2]=sqrt(uncvariance(aux))
    
    
  }  
  
  # Estimando a vari�ncia por StochVOl
  for(i in 1:ncol(R)){
    
    draws = svsample(R[,i] , priormu = tet0[i,])
    
    medias=apply(draws$latent,2,median)
    
    teste = predict(draws , steps = nrow(R))
    
    sigma[,i] = exp(apply(teste$h,2,median)/2)
  }
  # Salvando resultado no vetor  
  out$StdDev = cov(R)
  diag(out$StdDev) = colMeans(sigma)
  # Definindo volatilidade esperada utilizando GARCH(1,1)  
  
  
  return(out)
}

# Otimizando portf�lio
port_sv = optimize.portfolio.rebalancing(R = return, portfolio = port_spec, optimize_method = "DEoptim", 
                                               momentFUN = volest , rebalance_on = 'quarters' , 
                                               training_period = 624)

# Extraindo retorno
returns_sv = Return.portfolio(exp(app_data)-1 , weights = extractWeights(port_sv))



## Modelo MS-GARCH 


#Fun��o para calcular retorno e varian�a esperados
garchms = function(R , portfolio){
  out = list()
  sigma = matrix(nrow = nrow(R) , ncol = ncol(R))
  
  tet0 = matrix(ncol = 6 , nrow = length(acoes))
  
  for(i in 1:ncol(return)){
    
    # Utilizando o GARCH(1,1) como ponto de partida da estima��o via MCMC
    aux = ugarchspec(mean.model = list(armaOrder = c(0,0)) , 
                     variance.model = list( model = 'sGARCH' ), 
                     distribution.model = 'sstd')
    aux = ugarchfit(aux , return[,i])
    
    # Armazenando par�metros
    tet0[i,]=coef(aux)
    
    
  }  
  
  spec = CreateSpec(variance.spec = list(model = c("sGARCH", "sGARCH")),
                    distribution.spec = list(distribution = c("sstd", "sstd")))
  
    
  for(i in 1:ncol(R)){
    
    # Fun��o para iniciar a estima��o via MCMC
    f_MCMC <- function(f_posterior, data, spec, par0 = tet0[i,], ctr){
      par <- metrop(f_posterior, initial = par0, nbatch = ctr$nmcmc + ctr$nburn,
                    data = data, spec = spec)$batch
      return(par)
    }
    
    # Estima��o mcmc
    mcmc.g = FitMCMC(spec = spec , data = R[,i] , 
                     ctr = list(SamplerFUN = f_MCMC,nburn = 1000L, nmcmc = 10000L, nthin = 1L))
    
    sigma[,i] = Volatility(mcmc.g)
    
  }
  # Salvando resultado no vetor  
  out$StdDev = cov(R)
  diag(out$StdDev) = colMeans(sigma)
  
  
  
  return(out)
}


# Otimizando portf�lio
port_msgarch = optimize.portfolio.rebalancing(R = return, portfolio = port_spec, optimize_method = "DEoptim", 
                                              momentFUN = garchms , rebalance_on = 'quarters' , 
                                              training_period = 624)

# Armazenando retornos
returns_msgarch = Return.portfolio(R = exp(app_data)-1 , weights = extractWeights(port_msgarch))



## Resultados

# Retorno Acumulado
charts.PerformanceSummary(returns_mv)
charts.PerformanceSummary(returns_garch)
charts.PerformanceSummary(returns_gjrgarch)
charts.PerformanceSummary(returns_sv)
charts.PerformanceSummary(returns_msgarch)


# Pesos
chart.Weights(port_mv)
chart.Weights(port_garch)
chart.Weights(port_gjrgarch)
chart.Weights(port_sv)
chart.Weights(port_msgarch)


# Indicadores de Desempenho
all.ret = cbind(returns_mv , returns_garch , returns_gjrgarch , returns_sv , returns_msgarch)
colnames(all.ret) = c('returns_mv' , 'returns_garch' , 'returns_gjr' , 'returns_sv' , 'returns_msgarch')

Return.cumulative(all.ret)

StdDev(all.ret)

VaR(R = all.ret)

SharpeRatio(all.ret)

# Pelos indicadores verificamos a superioridade do modelo MS-GARCH. Em um per�odo de tanta incerteza e choques no 
# no mercado, � consistente que um modelo que permita a transi��o entre diferentes n�veis de volatilidade apresente
# desempenho superior aos demais.
# Devemos destacar o fato de termos utilizado uma vers�o simplificada do modelo SV, futuramente replicaremos as
# avalia��es utilizando uma distribui��o t-student ou skewed t-student.
