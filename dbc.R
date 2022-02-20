dados <- read.table("dados_dbc.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1ª linha ##
dados


## 3 marcas de salsichas, 8 avaliadores, cada avaliador representa um bloco,
## para que o gosto particular de cada um não interfira no resultado final

attach(dados)

## ANÁLISE DESCRITIVA ##

summary(Nota)   ## resumo descritivo geral ##
sd(Nota)        ## desvio padrão geral ##
var(Nota)        ## variância geral ##

(sd(Nota)/mean(Nota))*100

boxplot(Nota ~ Marca, xlab = "Marca", ylab = "Nota (1 a 8)")

boxplot(Nota ~ Bloco, xlab = "Bloco", ylab = "Nota (1 a 8)")


## ANÁLISE DE VARIÂNCIA E COMPARAÇÃO DE MÉDIAS ##
## instale o pacote "laercio" ##

Marca <- as.factor(Marca)       ## informação de que a marca é um fator ##
Bloco <- as.factor(Bloco)       ## informação de que a bloco é um fator ##

anv <- aov(Nota ~ Marca + Bloco)  ## cálculo da análise de variância ##
anova(anv)                        ## demonstrativo da Tabela ANOVA ##

## bloco não foi significativo, mas marca (fator sob estudo) foi ##

anv1 <- aov(Nota ~ Marca)  ## cálculo da análise de variância ##
anova(anv1)                ## demonstrativo da Tabela ANOVA ##


require(laercio)                            ## pacote laercio ##
LTukey(anv,"Marca")                         ## teste Tukey, default = 5%, com pacote "laercio" ##
LTukey(anv,"Marca", conf.level = 0.99)      ## teste Tukey, alfa = 1% ##

## ANÁLISE COM O PACOTE "ExpDes" ##

## rbd(trat, bloco, resp, quali = TRUE, mcomp = "tukey", sigT = 0.05, sigF = 0.05)

require(ExpDes)
rbd(Marca, Bloco, Nota, quali = TRUE, mcomp='tukey', sigT = 0.05, sigF = 0.05)

## a conclusao que se pode tirar é que pelo menos uma das marcas tem influencia 
## sobre a nota, e pelo Tukey que somente as marcas 2 e 3 sao estatisticamente 
## diferentes

## a marca 1 é a media do meio mesmo, vc observa pelas letras dos grupos,
## que a diferença entre a 2 e a 1 e a 1 e 3 nao sao significativas
## somente entre a 2 e 3 sao significativas as medias das notas


rbd(Marca, Bloco, Nota, quali = TRUE, mcomp='tukey', sigT = 0.01, sigF = 0.01)

