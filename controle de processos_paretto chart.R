## Técnicas Estatísticas de Controle de Processos ##

## Exercício 1 ##

## A ##

dados = read.table("exerc1linhas.txt", header = T)
dados

attach(dados)
names(dados)

## Média ##

x = colMeans(dados)
x

## Mediana ##

median(dados$D1)
median(dados$D2)
median(dados$D3)
median(dados$D4)
median(dados$D5)
median(dados$D6)
median(dados$D7)
median(dados$D8)
median(dados$D9)
median(dados$D10)
median(dados$D11)
median(dados$D12)
median(dados$D13)
median(dados$D14)
median(dados$D15)
median(dados$D16)
median(dados$D17)
median(dados$D18)
median(dados$D19)
median(dados$D20)
median(dados$D21)
median(dados$D22)
median(dados$D23)
median(dados$D24)
median(dados$D25)
median(dados$D26)
median(dados$D27)
median(dados$D28)
median(dados$D29)
median(dados$D30)

## Variância ## 

var(dados$D1)
var(dados$D2)
var(dados$D3)
var(dados$D4)
var(dados$D5)
var(dados$D6)
var(dados$D7)
var(dados$D8)
var(dados$D9)
var(dados$D10)
var(dados$D11)
var(dados$D12)
var(dados$D13)
var(dados$D14)
var(dados$D15)
var(dados$D16)
var(dados$D17)
var(dados$D18)
var(dados$D19)
var(dados$D20)
var(dados$D21)
var(dados$D22)
var(dados$D23)
var(dados$D24)
var(dados$D25)
var(dados$D26)
var(dados$D27)
var(dados$D28)
var(dados$D29)
var(dados$D30)

## Desvio Padrão ##

sd(dados$D1)
sd(dados$D2)
sd(dados$D3)
sd(dados$D4)
sd(dados$D5)
sd(dados$D6)
sd(dados$D7)
sd(dados$D8)
sd(dados$D9)
sd(dados$D10)
sd(dados$D11)
sd(dados$D12)
sd(dados$D13)
sd(dados$D14)
sd(dados$D15)
sd(dados$D16)
sd(dados$D17)
sd(dados$D18)
sd(dados$D19)
sd(dados$D20)
sd(dados$D21)
sd(dados$D22)
sd(dados$D23)
sd(dados$D24)
sd(dados$D25)
sd(dados$D26)
sd(dados$D27)
sd(dados$D28)
sd(dados$D29)
sd(dados$D30)


## CV ##

(sd(dados$D1)/mean(dados$D1))*100
(sd(dados$D2)/mean(dados$D2))*100
(sd(dados$D3)/mean(dados$D3))*100
(sd(dados$D4)/mean(dados$D4))*100
(sd(dados$D5)/mean(dados$D5))*100
(sd(dados$D6)/mean(dados$D6))*100
(sd(dados$D7)/mean(dados$D7))*100
(sd(dados$D8)/mean(dados$D8))*100
(sd(dados$D9)/mean(dados$D9))*100
(sd(dados$D10)/mean(dados$D10))*100
(sd(dados$D11)/mean(dados$D11))*100
(sd(dados$D12)/mean(dados$D12))*100
(sd(dados$D13)/mean(dados$D13))*100
(sd(dados$D14)/mean(dados$D14))*100
(sd(dados$D15)/mean(dados$D15))*100
(sd(dados$D16)/mean(dados$D16))*100
(sd(dados$D17)/mean(dados$D17))*100
(sd(dados$D18)/mean(dados$D18))*100
(sd(dados$D19)/mean(dados$D19))*100
(sd(dados$D20)/mean(dados$D20))*100
(sd(dados$D21)/mean(dados$D21))*100
(sd(dados$D22)/mean(dados$D22))*100
(sd(dados$D23)/mean(dados$D23))*100
(sd(dados$D24)/mean(dados$D24))*100
(sd(dados$D25)/mean(dados$D25))*100
(sd(dados$D26)/mean(dados$D26))*100
(sd(dados$D27)/mean(dados$D27))*100
(sd(dados$D28)/mean(dados$D28))*100
(sd(dados$D29)/mean(dados$D29))*100
(sd(dados$D30)/mean(dados$D30))*100

## B ##

## normalidade ##

qqnorm(x, main = "Média das sacas de arroz durante trinta dias")
qqline(x, col = "blue")    

shapiro.test(x)

require(mvsf)

ad.test(x)

lillie.test(x)


## Exercicio 2 ##

dados2 = read.table("exerci2.txt", header = T)
dados2

colMeans(dados2)


## Exercicio 3 ##

dados3 = read.table("exercicio3.txt", header = T)
dados3

attach(dados3)
names(dados3)

## A ##

mean(A)
mean(B)
median(A)
median(B)
summary(dados3)
sd(A)
sd(B)
(sd(A)/mean(A))*100
(sd(B)/mean(B))*100

## B ##

boxplot(dados3)

## C ## 

bartlett.test(dados3)

## D ##

ma = mean(A)
ma
mb = mean(B)
mb

na = 10

nb = 10

SA = sd(A) 
SA
SA^2

SB = sd(B)
SB
SB^2

S2 = ((na*(SA^2))+(nb*(SB^2)))/(na+nb-2)
S2

T = (ma - mb)/(sqrt(S2/5))
T

## Exercício 5 ##

require(qcc)

cause.and.effect(cause = list(Imprudência = c("Ingestão de bebida álcoolica", "Uso de
drogas","Uso de medicamentos", "Alta velocidade", "Forçar ultrapassagem"), 
                              "Falha Mecânica" = c("Pneu furado", "Freios", "Pane elétrica"), "Fatores climáticos" 
                              = c("Neblina", "Chuva", "Granizo", "Vento"), "Defeitos na Pista" = c("Buracos",
                                                                                                   "Detritos", "Drenagem ineficaz"), "Interferência de Terceiros" = c("Imprudência de
outro motorista", "Animais na pista"), "Fatores Aleatórios" = c ("Estresse",
                                                                 "Sonolência", "Cansaço", "Tráfego intenso", "Dirigir à noite")),
                 effect = "Acidente no trânsito", title = "Diagrama de Causa - Efeito")


## Exercício 6 ## 


################################################################################################
#GRAFICO DE PARETO
#################################################################################################

require(qcc)
galoes <- c(99, 11, 13, 33, 150, 88, 20)
galoes
names(galoes)<- c("BD", "BC", "LL", "LR", "RJ", "T", "D")
pareto.chart(galoes, ylab = "Frequência", ylab2 = "Percentagem Acumulada",
             xlab = "Fontes",las=1, col=rainbow(length(galoes)),
             main = 'Fontes de consumo de água')
legend("right", c("Regar o jardim","Banho e Ducha",
                  "Toalete","Lavagem de roupa","Diversos","Lavagem de louça","Beber e Cozinhar"),
       fill=c(col=rainbow(length(galoes))))

##########################################################
GRÁFICO DE CONTROLE
##########################################################

require(qcc)
galoes
qcc(galoes,type="xbar.one",ylab = "Média Amostra", xlab = "Amostras",las=1)


## Exercício 7 ##

################################################################################################
GRAFICO DE PARETO
#################################################################################################

### total de defeitos por dia da semana ###
require(qcc)
defeitos <- c(102, 112, 110, 121, 111)
defeitos
names(defeitos)<- c("Segunda", "Terça", "Quarta", "Quinta", "Sexta")
pareto.chart(defeitos, ylab = "Frequência", ylab2 = "Percentagem Acumulada",
             xlab = "Dias da Semana",las=1, main='Total de defeitos por dia da semana')

### total de tipos de defeito por turno ##
require(qcc)
defeitos <- c(288, 268)
defeitos
names(defeitos)<- c("Manhã", "Tarde")
pareto.chart(defeitos, ylab = "Frequência", xlab = "Turno",las=1, main=' Totais de defeitos por turno')


### tipos de defeitos na segunda ###
require(qcc)
defeitos <- c(13, 23, 46, 16, 4)
defeitos
names(defeitos)<- c("A", "B", "C", "D", "E")
pareto.chart(defeitos, ylab = "frequência", xlab = "causas",las=1, main='Defeitos ocorridos na segunda')

### tipos de defeitos na semana ###
require(qcc)
defeitos <- c(84, 104, 268, 87, 13)
defeitos
names(defeitos)<- c("A", "B", "C", "D", "E")
pareto.chart(defeitos, ylab = "Frequência", ylab2 = "Percentagem Acumulada", 
             xlab = "Defeitos",las=1, main='Tipos de defeitos na semana')


### tipo de defeitos no periodo da manha ###
require(qcc)
defeitos <- c(45, 62, 130, 44, 7)
defeitos
names(defeitos)<- c("defeito A", "defeito B", "defeito C", "defeito D", "defeito E")
pareto.chart(defeitos, ylab = "frequência", xlab = "causas",las=1, main='Tipos de defeitos ocorridos no período da manhã')


### tipo de defeitos no periodo da tarde ###
require(qcc)
defeitos <- c(39, 42, 138, 43, 6)
defeitos
names(defeitos)<- c("defeito A", "defeito B", "defeito C", "defeito D", "defeito E")
pareto.chart(defeitos, ylab = "frequência", xlab = "causas",las=1, main='Tipos de defeitos ocorridos no período da tarde')

### Total dos tipos de defeito por operador ###
require(qcc)
operador <- c(198, 116, 115, 127)
operador
names(operador)<- c("A", "B", "C", "D")
pareto.chart(operador, ylab = "Frequência", ylab2 = "Percentagem Acumulada", 
             xlab = "Operador",las=1, col=c("red","orange","yellow","green"),
             main=' Total dos tipos de defeito por operador')

### total de defeitos no periodo da manha por operador ###
par(mfrow = c(1,2))
require(qcc)
operador <- c(102, 58, 55, 73)
operador
names(operador)<- c("A", "B", "C", "D")
pareto.chart(operador, ylab = "Frequência", xlab = "Operador",las=1, 
             col=c("red", "orange","yellow","green"), 
             main='Total de defeitos por operador pela manhã')

### total de defeitos no periodo da tarde por operador ###
require(qcc)
operador <- c(96, 58, 60, 54)
operador
names(operador)<- c("A", "B", "C", "D")
pareto.chart(operador, ylab = "Frequência", ylab2 = "Percentagem Acumulada", 
             xlab = "Operador",las=1, col=c("red","green", "yellow", "orange"), 
             main='Total de defeitos por operador pela tarde')


## total dos tipos de defeitos do operador A ##

require(qcc)
operador <- c(33, 40, 82, 37, 6)
operador
names(operador)<- c("A", "B", "C", "D", "E")
pareto.chart(operador, ylab = "Frequência", ylab2 = "Percentagem Acumulada", 
             xlab = "Defeitos",las=1, main = 'Total dos tipos de defeitos do operador A')