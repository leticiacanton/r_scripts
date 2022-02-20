dados <- read.table("dados3.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1ª linha ##
dados

attach(dados)      

## LETRA C ##

## Análise descritiva geral ##

summary(rendi)                               ## resumo descritivo geral ##
var(rendi)                                       ## variância geral ## 
sd(rendi)                                    ## desvio padrão geral ##                               
((sd(rendi))/(mean(rendi)))*100             ## coeficiente de variação ##

media = mean(rendi)
media

## ASSIMETRIA (AS) ##
## AS = 0 distribuição simétrica; 
## AS > 0 distribuição assimétrica positiva; 
## AS < 0 distribuição assimétrica negativa. 

## CURTOSE (CUR) ##
## CUR = 3 distribuição com caudas neutras (normais - mesocúrtica); 
## CUR > 3 distribuição com caudas longas ou pesadas (leptocúrtica); 
## CUR < 3 distribuição com caudas  curtas ou leves (platicúrtica). 

require(moments)

skewness(rendi) ## AS ##
kurtosis(rendi) ## CUR ##

hist(rendi, main = "", xlab = "Rendimento de raiz", ylab = "Frequência")

boxplot(rendi, ylab = "Rendimento de raiz")                  
points(media, pch='+', cex=1.5, col='red')

## Análise descritiva por tratamento ##

tapply(rendi, cultivar, sum)
tapply(rendi, cultivar, summary)
tapply(rendi, cultivar, mean)
tapply(rendi, cultivar, var)
tapply(rendi, cultivar, sd)

cv1 = (sd(rendi[1:7])/ mean(rendi[1:7]))
cv1
skewness(rendi[1:7])
kurtosis(rendi[1:7])

cv2 = (sd(rendi[8:14])/ mean(rendi[8:14]))
cv2
skewness(rendi[8:14])
kurtosis(rendi[8:14])

cv3 = (sd(rendi[15:21])/ mean(rendi[15:21]))
cv3
skewness(rendi[15:21])
kurtosis (rendi[15:21])

cv4 = (sd(rendi[22:28])/ mean(rendi[22:28]))
cv4
skewness(rendi[22:28])
kurtosis (rendi[22:28])

cv5 = (sd(rendi[29:35])/ mean(rendi[29:35]))
cv5
skewness(rendi[29:35])
kurtosis (rendi[29:35])

## gráficos boxplot dos tratamentos com as médias ##

(médias = tapply(rendi, cultivar, mean))
boxplot(rendi~cultivar, xlab='Cultivares de mandioca',
        ylab='Rendimento de raiz',names = 
          c("Metro II","Paxiúba","Pretinha","Pirarucu","Paxiubão"))
points(médias, pch='+', cex=1.5, col='red')


## LETRA D ##

## ANÁLISE DE VARIÂNCIA ## 

cultivar <- as.factor(cultivar)      
anv <- aov(rendi ~ cultivar)  
anova(anv)                               
model.tables(anv,"means")               


## LETRA E ##

## estimativas em anv ##

names(anv)
anv$model                      
anv$coef                        
anv$fi                          
rendi - anv$fi                
anv$residuals                   
cbind(dados,anv$residuals)


## ANÁLISE DAS PRESSUPOSIÇÕES DO MODELO ##

## Normalidade ##

## graficamente ##
par(family = "serif")
par(mfrow = c(1,2))     qqnorm(rendi, main = "QQ-plot para o rendimento")
qqline(rendi, col = "blue")           ## qq-plot
qqnorm(anv$residuals, main = "QQ-plot para o resíduo")
qqline(anv$residuals, col = "blue")           ## qq-plot

## teste de normalidade de Shapiro Wilk para VR ##
shapiro.test(rendi)    

## teste de normalidade de Shapiro Wilk para o resíduo ##
shapiro.test(anv$residuals)    


## Homocedasticidade ##

## teste de Bartlett ##
bartlett.test(rendi ~ cultivar)   

residuos <- anv$residuals
residuos
var.res <- tapply(residuos,cultivar,var) 
var.res


n <- length(rendi)                         ## nº total de parcelas
I <- length(levels(cultivar))                  ## nº de tratamentos
r <- n/I                                  ## nº de repetições

repmean <- rep(tapply(rendi, cultivar, mean), each = r)
plot(repmean,anv$residuals, xlab = "Médias dos tratamentos", ylab = "Resíduos")


## Independência ##

plot(1:n, anv$residuals, xlab = "Ordem da coleta", ylab = "Resíduos")


## LETRA F ## 

## ANOVA E DEMAIS ANÁLISES PELO PACOTE ExpDes ##
## crd: Completely random design ##

require(ExpDes)

## Teste Tukey ##
crd(cultivar, rendi, quali = T, sigF = 0.05, sigT = 0.05) 

qtukey(0.95,5,30) ## valor da tabela de Tukey ##

TukeyHSD(anv)                    

## Gráfico do intervalo de confiança para as diferenças médias ##
plot(TukeyHSD(anv,wich='cultivar',ordered=TRUE,conf.level=0.95))

## Gráfico do teste Tukey ##
cul <- c("Metro II","Paxiúba","Pretinha","Pirarucu","Paxiubão")
cul <- as.factor(cul)
medi <- c(51.80714,61.71143,59.60571,52.90857, 41.78857) ## medias tratamentos ##
medi

let <- c("b","a","a","b","c")  ## letras dos tratamentos ##
let <- as.vector(let)
max(medi)

die <- data.frame(cul, medi, let)

require(lattice)

barchart(medi~cul, data=die, horiz=FALSE,
         ylab="Média do Rendimento de raiz", ylim=c(35,65),
         xlab="Cultivar de Mandioca",col = "gray",
         panel=function(x, y, subscripts, ...){
           panel.barchart(x, y, subscripts=subscripts, ...)
           panel.text(x, y, label=die[subscripts,"let"], pos=3, cex = 1)
         })


## LETRA H ##

## Contrastes ##
## Contraste1 = 2mMe + 2mPax + 2mPre - 3mPi - 3mPbao
## Contraste2 = 2mMe - mPax - mPre
## Contraste3 = mPax - mPre
## Contraste4 = mPi - mPbao

con1 <- matrix(c(2,2,2,-3,-3,2,-1,-1,0,0,0,1,-1,0,0,0,0,0,1,-1), nrow = 5, ncol = 4)
con1

cultivar <- as.factor(cultivar)      

contrasts(cultivar) <- con1
contrasts(cultivar)

## ANOVA desdobrada ##
anv <- aov(rendi ~ cultivar)  
anova(anv) 

summary.aov(anv, split=list("cultivar"=list("c1" = 1, "c2" = 2, "c3"=3, "c4"=4)))
