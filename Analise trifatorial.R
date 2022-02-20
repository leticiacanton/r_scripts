dados <- read.table("dados_trifatorial.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1ª linha ##
dados

attach(dados)
## ANÁLISE DESCRITIVA ##

adub <- as.factor(adubacao)       
var <- as.factor(variedade)  
esp <- as.factor(espacamento)

## resumo descritivo para cada fator ##
tapply(acidez, adub, summary)  
tapply(acidez, var, summary)  
tapply(acidez, esp, summary)  


## média do etanol, para cada tratamento ##
mean1 <- tapply(acidez, list(var,adub,esp), mean)
mean1

## média do etanol, para cada tratamento ##
mean2 <- tapply(acidez, list(adub,esp,var), mean)
mean2

## média do etanol, para cada tratamento ##
mean3 <- tapply(acidez, list(esp,var,adub), mean)
mean3


## boxplot ##
par(mfrow = c(1,3))  
boxplot(acidez ~ var, xlab = "Variedade", ylab = "Acidez") 
boxplot(acidez ~ esp, xlab = "Espaçamento", ylab = "Acidez") 
boxplot(acidez ~ adub, xlab = "Adubação", ylab = "Acidez") 

## GRÁFICOS DE INTERAÇÃO ##

interaction.plot(adub, esp, acidez)
interaction.plot(esp, adub, acidez)

interaction.plot(adub, var, acidez)
interaction.plot(var, adub, acidez)

interaction.plot(var, esp, acidez)
interaction.plot(esp, var, acidez)


## ANÁLISE DE VARIÂNCIA E COMPARAÇÃO DE MÉDIAS ##
## instale o pacote "laercio" ##

anv <- aov(acidez ~ var + esp + adub + var:esp + var:adub + esp:adub + esp:var:adub)  ## cálculo da análise de variância ##
summary(anv)


## NO PACOTE ExpDes ##

require(ExpDes)
fat3.crd(var, esp, adub, acidez, quali = c(TRUE, TRUE, TRUE), mcomp = "tukey", fac.names = c("Variedade", "Espaçamento", "Adubação"), sigT = 0.05, sigF = 0.05)



## 2º EXEMPLO ##

dados1 <- read.table("dados2_fatorial.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1ª linha ##
dados1

attach(dados1)

require(ExpDes)
fat2.rbd(Admine, Adorg, Bloco, Rend, quali = c(TRUE, TRUE), mcomp = "tukey", 
         fac.names = c("Mineral", "Organico"), sigT = 0.05, sigF = 0.05)

