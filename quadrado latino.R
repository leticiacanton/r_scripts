#######################################
EXEMPLO SALA
#######################################
ex <- read.table("ex_qualatino.txt", header=T)
ex

attach(ex)       ## colunas da tabela podem ser chamadas apenas pelo nome ##

## dados dispostos em linha (dosagem) ##

## análise de variância ##

Dos <- as.factor(Dos)
Pac <- as.factor(Pac)
Trat <- as.factor(Trat)

av <- aov(Resp ~ Dos + Pac + Trat)     ## no lugar de * pode ser usado : ##

summary(av)

## fator estranho dosagem precisava controlar, o outro fator estranho paciente não ##
## fator sob estudo tratamento (medicamentos) tbm nao foi significativo ##
## mas não se pode inferir se foi pelo numero de fatores estranhos ou nao ##


## EXEMPLO QUADRADO LATINO ##

require(ExpDes)


## Na versão em inglês ##

latsd(treat, row, column, resp, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)

## Teste Tukey ## ## colocou 1, ..., 5 pq nao foi significativo, então todos sao iguais recebem mesma letra ##

latsd(Trat, Dos, Pac, Resp, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)


## tem que acrescentar a essa rotina as suposições do modelo e a est. descritiva ##
