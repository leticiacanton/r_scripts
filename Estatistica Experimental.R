rm(list=ls(all=TRUE)) ## limpar memória do R ##
cat('\014')

dados <- read.table("dados.txt", header = T)   
dados

attach(dados)      

## ANÁLISES DESCRITIVAS ##

## Variável resposta - produção ##

summary(producao)                               ## resumo descritivo geral ##
var(producao)                                       ## variância geral ## 
sd(producao)                                    ## desvio padrão geral ##                               
((sd(producao))/(mean(producao)))*100             ## coeficiente de variação ##
sum(producao)

require(moments)

skewness(producao)                ## assimetria ##
kurtosis(producao)                ## curtose ##

hist(producao, xlab = "Produção", ylab = "Frequência", main = "") ## histograma ##

boxplot(producao, ylab = "Produção")                  ## boxplot ##


## Fator principal- Variedade ##

tapply(producao, variedade, sum)
tapply(producao, variedade, summary)
tapply(producao, variedade, mean)
tapply(producao, variedade, var)
tapply(producao, variedade, sd)
tapply(producao, variedade, skewness)
tapply(producao, variedade, kurtosis)  

cv<-(tapply(producao, variedade, mean)/tapply(producao, variedade, sd))
cv

## gráficos boxplot das variedades com as médias ##
(médias = tapply(producao, variedade,mean))
boxplot(producao~variedade, xlab='Variedade',
        ylab='Produção',names = 
          c("404","Bluebelle","Patna"))
points(médias, pch='+', cex=1.5, col='red')

## Fator secundário - densidade ##

tapply(producao, densidade, sum)
tapply(producao, densidade, summary)
tapply(producao, densidade, mean)
tapply(producao, densidade, var)
tapply(producao, densidade, sd)
tapply(producao, densidade, skewness)
tapply(producao, densidade, kurtosis)  


cv<-(tapply(producao, densidade, mean)/tapply(producao, densidade, sd))
cv

## gráficos boxplot das densidades com as médias ##
(médias = tapply(producao, densidade,mean))
boxplot(producao~densidade, xlab='Densidade',
        ylab='Produção',names = 
          c("120","180","240","300"))
points(médias, pch='+', cex=1.5, col='red')


## Bloco ##

tapply(producao, bloco, sum)
tapply(producao, bloco, summary)
tapply(producao, bloco, mean)
tapply(producao, bloco, var)
tapply(producao, bloco, sd)
tapply(producao, bloco, skewness)
tapply(producao, bloco, kurtosis)  

cv<-(tapply(producao, bloco, mean)/tapply(producao, bloco, sd))
cv

## gráficos boxplot dos blocos com as médias ##
(médias = tapply(producao, bloco,mean))
boxplot(producao~bloco, xlab='Blocos',
        ylab='Produção',names = 
          c("I","II","III","IV"))
points(médias, pch='+', cex=1.5, col='red')


dados <- read.table("dados1.txt", header = T)   
dados

attach(dados)

## Tratamento ##

require(moments)
tapply(producao, tratamento, sum)
tapply(producao, tratamento, summary)
tapply(producao, tratamento, mean)
tapply(producao, tratamento, var)
tapply(producao, tratamento, sd)
tapply(producao, tratamento, skewness)
tapply(producao, tratamento, kurtosis)  

cv<-(tapply(producao, tratamento, mean)/tapply(producao, tratamento, sd))
cv

## gráficos boxplot dos tratamentos com as médias ##
(médias = tapply(producao, tratamento,mean))
boxplot(producao ~ tratamento, xlab='Tratamentos',
        ylab='Produção',names = 
          c("1", "2", "3","4","5","6","7","8","9","10","11","12"))
points(médias, pch='+', cex=1.5, col='red')


#######################################################################
#######################################################################

## gráfico de dispersão ##

disp <- read.table("var404.txt", header = T)   
disp
attach(disp)

plot(densidade,producao, xlab = "Densidade", ylab = "Produção (hg)", 
     main = "Variedade 404")

cor(densidade, producao) 

disp2 <- read.table("varpatna.txt", header = T)   
disp2
attach(disp2)

plot(densidade,producao, xlab = "Densidade", ylab = "Produção (hg)", 
     main = "Variedade Patna")

cor(densidade, producao) 

disp3 <- read.table("varblue.txt", header = T)   
disp3
attach(disp3)

plot(densidade,producao, xlab = "Densidade", ylab = "Produção (hg)", 
     main = "Variedade Bluebelle")

cor(densidade, producao) 


require(ExpDes)

split2.rbd(variedade, densidade, bloco, producao, quali = c(TRUE, FALSE), mcomp = "tukey", fac.names = c("Variedade", "Densidade"), sigT = 0.05, sigF = 0.05)

