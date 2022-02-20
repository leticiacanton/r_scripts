
## Problema 1 ##

## a ##

## Gráfico de dispersão ##

y = read.table("fosfato.txt",header=T) 
y
attach(y)
plot(eixo_y, eixo_y_1, xlab="Yt-1", ylab="Yt", main="Gráfico de Dispersão - Yt vs. Yt-1")

y1 = read.table("fosfatocor.txt",header=T) 
y1
attach(y1)
cor(y1$eixo_y, y1$eixo_y_1) ## coef de autocorrelação ##

fosfato = read.table("fosfato1.txt",header=T)
attach(fosfato)
summary(fosfato)
var(fosfato)

## normalidade ##
norm = read.table("fosfato2.txt",header=T) 
norm
attach(norm)
qqnorm(acidez, main = "QQ-plot para os dados da variável ACTOT")
qqline(acidez, col = "blue") 

## b ## 

## Amplitude Móvel ##

require(qcc)
dados = read.table("fosfato2.txt",header=T) 
dados
dados2<-matrix(cbind(dados[1:length(dados[,2])-1,2], dados[2:length(dados[,2]),2]), ncol=2)
dados2
qcc(dados2,type="R",ylab = "Alcance das amostras", xlab = "Amostras")

## X individual ##

require(qcc)
dados = read.table("fosfato1.txt",header=T) 
dados
qcc(dados,type="xbar.one",ylab = "Valor Individual", xlab = "Número de observações")


## d - CUSUM e EWMA ##

norm = read.table("fosfato2.txt",header=T) 
norm
attach(norm)

q <- cusum(acidez,ylab = "Somas Acumuladas", xlab = "Amostras",las=1)

q <- ewma(acidez,ylab = "Média Móvel Ponderada", xlab = "Amostras")


## Problema 2 ##

## a ##

require(MASS)
rm(list=ls())
dados = read.table("eletronico.txt",header=FALSE)
x=as.matrix(dados)
x
min(x)
max(x)

## modelo exponencial ##

fitdistr(x,densfun=("exponential"))
lambda = 0.001837

L1=function(x){ exp(-lambda*x)}
L1

## gráfico da função confiabilidade ##

curve(L1, xlim=c(40, 2010), col="red",type="l", 
      xlab="Falhas (h)", ylab= "R(x)",
      main="Modelo Exponencial")


## modelo weibull ##

fitdistr(x,densfun=("weibull"))
beta = 1.247572
eta = 594.719292

L1=function(x){ exp(-((x/eta)^beta))}
L1

## gráfico da função confiabilidade ##

curve(L1, xlim=c(40, 2010), col="red",type="l", 
      xlab="Falhas (h)", ylab= "R(x)", main="Modelo Weibull")

## modelo log-normal ##

fitdistr(x,densfun=("lognormal"))
mean = 5.900655
sd=1.005422

## gráfico da função confiabilidade ##
plot(function(x){1-dlnorm(x, mean, sd)},
     xlim=c(40, 2010), col="red",type="l", xlab="Falhas (h)", ylab= "R(x)",
     main="Log-Normal")


## b ##

## modelo exponencial - gráfico da taxa de falha ##

plot(function(x){0.001837*x/x},xlim=c(40, 2010), col="red",type="l", xlab="Falhas (h)", 
     ylab= "h(x)", main="Função Falha da Exponencial")

## modelo weibull - gráfico da taxa de falha ##

plot(function(x){1.247572/594.719292 *((x/594.719292)^0.247572)},xlim=c(40, 2010), col="red",type="l", xlab="Falhas (h)", 
     ylab= "h(x)", main="Função Falha da Weibull")


## modelo log-normal - gráfico da taxa de falha ##

plot(function(x){dlnorm(x, mean=5.900655, sd=1.005422 )/1-plnorm(x, mean=5.900655, 
                                                                 sd=1.005422)},xlim=c(40, 2010), col="red",type="l", xlab="Falhas (h)", 
     ylab= "h(x)", main="Função Falha da Log-Normal")


## Problema 4 ##

## a - função de autocorrelação ##

## Gráfico de dispersão ##

y1 = read.table("quimico22.txt",header=T) 
y1
attach(y1)
plot(eixo_y, eixo_y_1, xlab="Yt-1", ylab="Yt", main="Gráfico de Dispersão - Yt vs. Yt-1")

y11 = read.table("quimico22cor.txt",header=T) 
y11
attach(y11)
cor(y11$eixo_y, y11$eixo_y_1) ## coef de autocorrelação ##

## b - gráfico X e AM ##

## Amplitude Móvel ##

require(qcc)
dados = read.table("quimico1.txt",header=T) 
dados
dados2<-matrix(cbind(dados[1:length(dados[,2])-1,2], dados[2:length(dados[,2]),2]), ncol=2)
dados2
qcc(dados2,type="R",ylab = "Alcance das amostras", xlab = "Amostras")

## X individual ##

require(qcc)
dados = read.table("quimico.txt",header=T) 
dados
qcc(dados,type="xbar.one",ylab = "Valor Individual", xlab = "número de observações")

## c ##

## pré-requisitos para ajustar o modelo autoregressivo ##

## Correlograma  - forma de detectar autocorrelação em uma defasagem/lag k ##

require(graphics)

concentracao = read.table("quimico.txt",header=T)
concentracao
serie <- ts(concentracao)
autocor <- acf(serie, lag.max=16, main="Correlograma Função ACF")
autocor$acf
autocor$lag

## observe que poderia ser ajustado um modelo arima até de ordem 5 ##
## no lag 0 é fixado em 1 por convenção ##

## Ajustando Modelo ARIMA ##

peso_mole = read.table("quimico.txt",header=T) 
serie <- ts(concentracao)
arima(serie, order = c(1,0,0)) ## ordem 1 ##

## s.e. é o erro padrão ##

## Gráfico X individual dos resíduos ##

require(qcc)
dados = read.table("quimico.txt",header=T) 
serie <- ts(dados)
#Para um ARIMA de ordem 1, grau de diferenciação 0 e ordem da média móvel 0:
fit<-arima(serie,order=c(1,0,0))
summary(fit)
fit$residuals
ordem_coleta <- dados[,1]
resconcent <- fit$residuals
qcc(resconcent,type="xbar.one",ylab = "Valor Individual", 
    xlab = "número de observações") 


## d ##

## Resíduos ##

## gráfico de dispersão dos resíduos ##
## para verificar se estão autorrelacionados, ou seja, observar tendência ##

plot(ordem_coleta, fit$residuals) 
cor(ordem_coleta, fit$residuals)

## Autocorrelação dos Resíduos ## 

resconcent <- fit$residuals
resconcent

autocor <- acf(resconcent, lag.max=100, main="Correlograma Função ACF")

plot(autocor[1:99], type="h",
     main="Função de Autocorrelação do Resíduo",
     xlab="Lag",
     ylab="ACF",
     ylim=c(-0.21, 0.660),
     las=1,
     xaxt="n")


## e - CUSUM ##

## normalidade ##

shapiro.test(resconcent)
qqnorm(resconcent, main = "QQ-plot para os resíduos do modelo AR(1)")
qqline(resconcent, col = "blue")           

q <- cusum(resconcent,ylab = "Somas Acumuladas", xlab = "Amostras",las=1)


## f - EWMA ##

q <- ewma(resconcent,ylab = "Média Móvel Ponderada", xlab = "Amostras")


## Problema 5 ##

dure = read.table("dureza.txt", header=T)
dure

require(qcc)

## Gráfico X individual ##

qcc(dure,type="xbar.one",ylab = "Média da amostra", xlab = "Amostra",las=1)

## Gráfico X AM ##

require(qcc)
dados = read.table("durezaam.txt",header=T) 
dados
dados2<-matrix(cbind(dados[1:length(dados[,2])-1,2], dados[2:length(dados[,2]),2]), ncol=2)
dados2
qcc(dados2,type="R",ylab = "Alcance das amostras", xlab = "Amostras") 
