require(geoR)

dados = read.geodata("potassio.txt",head=T,coords.col=2:3,data.col=4)
dados

## Transformar as coordenadas de m para Km ##

dadosnewcoord=(dados$coords)/1000 
dados$coords=dadosnewcoord
dados$coords ## só com as coordenadas ##
dados

dados$data ## só com os valores do atributo potássio ##

## A ##

## Análise Exploratória dos dados ##

## min, Q1, mediana, média, Q3 e máx dos dados do atributo potássio ##

summary(dados$data) 

## média do atributo ##

mean(dados$data) 

## variância do atributo ##

var(dados$data)

## desvio padrão do atributo ##

sd(dados$data)

## coeficiente de variação (CV =dp/média x 100) do atributo ##

CV = sd(dados$data)*100/mean(dados$data)
CV

library(e1071)

## coeficiente de assimetria e curtose ##

skewness(dados$data)
kurtosis(dados$data)

## Apresenta em uma tela os gráficos de tendências direcionais, post-plot e histograma ##

a = dados$data
a

b = dados$coords
b

c = as.matrix(b)
c

d=c[,1]
d

plot(d, a, xlab="X Coord", ylab="Teor de Potássio")

e=c[,2]
e

plot(a, e, ylab="Y Coord", xlab="Teor de Potássio")

plot(dados)


## Apresenta o gráfico box-plot ## 

boxplot(dados$data,main='Gráfico Boxplot', ylab = "Teores de Potássio")    

##  Apresenta o gráfico Histograma ##

hist(dados$data,breaks=4,col="white", border="red", main='Gráfico Histograma', 
     ylab="Frequência")


## B e C ##

## Fornece a maior distância da área considerando as coordenadas dados$coords para obter o cutoff de 50% ##
max(dist(dados$coords))

## Cálculo do cutoff de 50% da distância máxima - sugerido por Clark ## 
max(dist(dados$coords)/2)

## chamar o arquivo com a borda e transformar para Km ##

borda<-read.table("borda.txt")
borda

bordanew=borda/1000
bordanew
borda=bordanew
borda

plot.new()
points(dados, pch="*")

## Apresenta os dados da borda ##
borda  
plot(borda)

## une todas as coordenadas, porém pode haver falta de concordância com a escala ##
polygon(borda)

##  Coloca o Norte ##
text(239.3,7238.5,expression(bolditalic(NA%up%N)),cex=2)

## Post-Plot com borda ##

points(dados,pt.div="quartile",col=c("yellow","green","red","blue"),main="Post-Plot")
legend(240.3,7238.54,c("Min - Q1","Q1 - Mediana","Mediana - Q3","Q3 - Max"),fill=c("yellow", "green", "red", "blue")) 
polygon(borda)
text(239.3,7238.5,expression(bolditalic(NA%up%N)),cex=2)


## D ## 

a
m = cbind(c,a)
m


d3 = read.table("teste.txt", head=T)
d3

require(gstat)
require(car)
require(sp)
attach(d3)
names(d3)
coord <- cbind(d3$x,d3$y)
coord
coordinates(d3)=~x+y
hscat(K~1,d3, c(0,0.3,0.6,0.9,1.2,1.5,1.78))

## E ##


## Estudo da anisotropia dos dados em 0, 45, 90 e 135 considerando o cutoff de 0.88 km (50% da distância máxima)
plot(variog4(dados,l=1,max.dist= 0.88), xlab="Distância", ylab="Semivariância",
     legend=FALSE)


## G ## 

## Construção do semivariograma experimental omnidirecional de Matheron ##

d.var <- variog(dados,uvec=seq(0,0.88,l=11), estimator.type="classical",
                pairs.min=30) 

## Visualização da semivariograma experimental omnidirecional de Matheron ##
plot(d.var, main= '', xlab="Distância", ylab="Semivariância)")

## Informações do semivariograma experimental construído ##
d=dados
d

distancia <-  d.var$u 
semivariancia <- d.var$v
pares <- d.var$n
tabela <- cbind(distancia,semivariancia,pares)
tabela

################################################
### GRÁFICO DE ENVELOPE SIMULADO  ####
################################################

dados.var <- variog(d,uvec=seq(0,0.88,l=11), estimator.type="classical",pairs.min=30) 
dados.var 
dados.env<-variog.mc.env(d, obj.v=dados.var,nsim = 99)
plot(dados.var,env=dados.env, ylab="Semivariância", xlab="Distância",
     yaxp=c(0,0.005,25), xaxp=c(0,1,10))

## H ##

d=dados
d

## Modelo Exponencial ##

## Utiliza o comando VARIOFIT 
## Com chute inicial ini=c(0.001, 0.6) de 0.001 para a contribuição (phi2) e alcance de 600m (0.6) ## 
## Utilizando a estimação por OLS (weights= “equal")  

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dexp.ols') 
dexp.ols <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model="exp") 
dexp.ols 

##  Mostra o gráfico das semivariâncias com o modelo ajustado e o rótulo ##
lines(dexp.ols,col="blue")

## Apresenta um resumo das estatísticas espaciais dos modelos ajustados ##
summary(dexp.ols)

## Modelo Gaussiano ##

## Utiliza o comando VARIOFIT 
## Com chute inicial ini=c(0.001, 0.6) de 0.001 para a contribuição (phi2) e alcance de 600m (0.6) ## 
## Utilizando a estimação por OLS (weights= “equal")  

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dgaus.ols') 
dgaus.ols <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model="gaus") 
dgaus.ols 

##  Mostra o gráfico das semivariâncias com o modelo ajustado e o rótulo ##
lines(dgaus.ols,col="blue")

## Apresenta um resumo das estatísticas espaciais dos modelos ajustados ##
summary(dgaus.ols)

## Modelo Matérn com k=1 ##

## Utiliza o comando VARIOFIT 
## Com chute inicial ini=c(0.001, 0.6) de 0.001 para a contribuição (phi2) e alcance de 600m (0.6) ## 
## Utilizando a estimação por OLS (weights= “equal")  

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dmatern1.ols') 
dmatern1.ols <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model= "matern", kappa = 1) 
dmatern1.ols 

##  Mostra o gráfico das semivariâncias com o modelo ajustado e o rótulo ##
lines(dmatern1.ols,col="blue")

## Apresenta um resumo das estatísticas espaciais dos modelos ajustados ##
summary(dmatern1.ols)

## Modelo Matérn com k=1.5 ##

## Utiliza o comando VARIOFIT 
## Com chute inicial ini=c(0.001, 0.6) de 0.001 para a contribuição (phi2) e alcance de 600m (0.6) ## 
## Utilizando a estimação por OLS (weights= “equal")  

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dmatern1.5.ols') 
dmatern15.ols <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model= "matern", kappa = 1.5) 
dmatern15.ols 

##  Mostra o gráfico das semivariâncias com o modelo ajustado e o rótulo ##
lines(dmatern15.ols,col="blue")

## Apresenta um resumo das estatísticas espaciais dos modelos ajustados ##
summary(dmatern15.ols)

## Modelo Matérn com k=2.5 ##

## Utiliza o comando VARIOFIT 
## Com chute inicial ini=c(0.001, 0.6) de 0.001 para a contribuição (phi2) e alcance de 600m (0.6) ## 
## Utilizando a estimação por OLS (weights= “equal")  

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dmatern2.5.ols') 
dmatern25.ols <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model= "matern", kappa = 2.5) 
dmatern25.ols 

##  Mostra o gráfico das semivariâncias com o modelo ajustado e o rótulo ##
lines(dmatern25.ols,col="blue")

## Apresenta um resumo das estatísticas espaciais dos modelos ajustados ##
summary(dmatern25.ols)

## I ##

## Validação Cruzada para modelos ajustados por OLS ##


## Validação Cruzada e Erro Absoluto do modelo exponencial OLS dexp.ols (vcdexpols=exponencial com OLS) ##

vcdexpols=xvalid(d,model=dexp.ols)
EAexpols=sum(abs(vcdexpols$predicted-vcdexpols$data))
EAexpols
summary(vcdexpols)


## Validação Cruzada e Erro Absoluto do modelo gaussiano OLS dgaus.ols (vcdgausols=gaussiano com OLS) ##

vcdgausols=xvalid(d,model=dgaus.ols)
EAgausols=sum(abs(vcdgausols$predicted-vcdgausols$data))
EAgausols
summary(vcdgausols)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=1 OLS dmatern1.ols (vcdmatern1ols=Matern com k=1 com OLS) ##

vcdmatern1ols=xvalid(d,model=dmatern1.ols)
EAmatern1ols=sum(abs(vcdmatern1ols$predicted-vcdmatern1ols$data))
EAmatern1ols
summary(vcdmatern1ols)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=1.5 OLS dmatern15.ols (vcdmatern15ols=Matern com k=1.5 com OLS) ##

vcdmatern15ols=xvalid(d,model=dmatern15.ols)
EAmatern15ols=sum(abs(vcdmatern15ols$predicted-vcdmatern15ols$data))
EAmatern15ols
summary(vcdmatern15ols)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=2.5 OLS dmatern25.ols (vcdmatern15ols=Matern com k=2.5 com OLS) ##

vcdmatern25ols=xvalid(d,model=dmatern25.ols)
EAmatern25ols=sum(abs(vcdmatern25ols$predicted-vcdmatern25ols$data))
EAmatern25ols
summary(vcdmatern25ols)

## J ##

borda

## Mostra o mínimo e máximo das coordenadas, para determinar o grid de interpolação ##

apply(borda,2,range) 
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, by=0.005)) 
plot(gr)

## Monta o grid de interpolação ##

points(d, pt.div="equal")

## Carregando o módulo geoestatístico “splancs” ##

require(splancs)  
gi <- polygrid(gr, bor=borda)

##  O novo grid considerando apenas a região limitada pelas bordas ##

points(gi, pch="+", col=3)


## Mapa para o modelo gaussiano utilizando OLS ##

KC=krige.control(obj=dgaus.ols, lam=1) 
KC

d.k=krige.conv(d,loc=gr,krige=KC)
max(d.k$pred)
min(d.k$pred)
image(d.k, loc=gr, border=borda,col=gray(seq(1,0,l=5)),zlim=range(d.k$predict))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.30, 7238.55, fill=gray(c(1,0.8,0.6,0.4,0.2,0)), 
       c("0,1628 |-- 0,1680","0,1680 |-- 0,1730","0,1730 |-- 0,1780","0,1780 |-- 0,1831", 
         "0,1831 |-- 0,1881"),cex=0.88)
require(classInt)
valores<-d.k$predict
classIntervals(valores, 5, style="equal", intervalClosure="right")
title(main = "OLS - Gaussiano", font.main = 4)

## calculando a área usando a borda (caso a área não fosse informada no enunciado) ##

poli = borda

area<-function(poli){
  poli<-rbind(poli,poli[1,])
  x<-poli[,1]; y<-poli[,2]; lx<-length(x)
  +sum((x[2:lx]-x[1:lx-1])*(y[2:lx]+y[1:lx-1]))/2
}

area(poli)

## K ##

KC=krige.control(obj=dgaus.ols, lam=1) 
KC

d.k=krige.conv(d,loc=gr,krige=KC)
max(d.k$pred)
min(d.k$pred)

## image(d.k, loc=gr, border=borda,col=gray(seq(1,0,l=5)),zlim=range(d.k$predict)) ##
## O zlim = range(d.k$predict), obtem o mínimo e o máximo preditos ##
## Substituindo o zlim = range(d.k$predict) por zlim = (0.1, 0.3) e l=5 por l=4 em col=gray ##

image(d.k, loc=gr, border=borda,col=gray(seq(1,0,l=4)),zlim=c(0.1, 0.3))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.50, 7238.55, fill=gray(c(1,0.8,0.6,0.4)), 
       c("0,00 |--| 0,10","0,11 |--| 0,20","0,21 |--| 0,30","0,30 -- "),cex=1)
title(main = "OLS - Gaussiano", font.main = 4)


###############################################################################
###############################################################################
LISTA 2
###############################################################################
###############################################################################

###########################
QUESTÃO 1
###########################

require(gstat)

show.vgms(max=10, n=102, sill=2, range=4, nugget=1, models="Sph", plot=TRUE, 
          as.groups=F, main="Modelo Esférico", sub="", subtitile="", ylab="Semivariância", 
          xlab="Distância")

show.vgms(max=10, n=102, sill=2, range=4, nugget=1, models="Exp", plot=TRUE, 
          as.groups=F, main="Modelo Exponencial", sub="", subtitile="", ylab="Semivariância", 
          xlab="Distância")

show.vgms(max=10, n=102, sill=2, range=4, nugget=1, models="Gau", plot=TRUE, 
          as.groups=F, main="Modelo Gaussiano", sub="", subtitile="", ylab="Semivariância", 
          xlab="Distância")

###############################
QUESTÃO 2
###############################

## Estimação dos parâmetros por WLS1 ##

d=dados
d

## Modelo Exponencial ##

## Utiliza o comando VARIOFIT 
## Com chute inicial ini=c(0.001, 0.6) de 0.001 para a contribuição (phi2) e alcance de 600m (0.6) ## 
## Utilizando a estimação por WLS1 (weights= “equal")  

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dexp.wls') 
dexp.wls <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model="exp") 
dexp.wls 
lines(dexp.wls,col="blue")
summary(dexp.wls)

## Modelo Gaussiano ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dgaus.wls') 
dgaus.wls <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model="gaus") 
dgaus.wls 
lines(dgaus.wls,col="blue")
summary(dgaus.wls)

## Modelo Matérn com k=1 ##

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dmatern1.wls') 
dmatern1.wls <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model= "matern", kappa = 1) 
dmatern1.wls 
lines(dmatern1.wls,col="blue")
summary(dmatern1.wls)

## Modelo Matérn com k=1.5 ##

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dmatern1.5.wls') 
dmatern15.wls <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model= "matern", kappa = 1.5) 
dmatern15.wls 
lines(dmatern15.wls,col="blue")
summary(dmatern15.wls)

## Modelo Matérn com k=2 ##

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma dmatern2.wls') 
dmatern2.wls <- variofit(d.var,ini=c(0.001,0.6),weights= "equal",cov.model= "matern", kappa = 2) 
dmatern2.wls 
lines(dmatern2.wls,col="blue")
summary(dmatern2.wls)

## Validação Cruzada para modelos ajustados por WLS1 ##


## Validação Cruzada e Erro Absoluto do modelo exponencial WLS1 ##

vcdexpwls=xvalid(d,model=dexp.wls)
EAexpwls=sum(abs(vcdexpwls$predicted-vcdexpwls$data))
EAexpwls
summary(vcdexpwls)


## Validação Cruzada e Erro Absoluto do modelo gaussiano WLS1 ##

vcdgauswls=xvalid(d,model=dgaus.wls)
EAgauswls=sum(abs(vcdgauswls$predicted-vcdgauswls$data))
EAgauswls
summary(vcdgauswls)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=1 WLS1 ##

vcdmatern1wls=xvalid(d,model=dmatern1.wls)
EAmatern1wls=sum(abs(vcdmatern1wls$predicted-vcdmatern1wls$data))
EAmatern1wls
summary(vcdmatern1wls)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=1.5 WLS1 ##

vcdmatern15wls=xvalid(d,model=dmatern15.wls)
EAmatern15wls=sum(abs(vcdmatern15wls$predicted-vcdmatern15wls$data))
EAmatern15wls
summary(vcdmatern15wls)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=2 WLS1 ##

vcdmatern2wls=xvalid(d,model=dmatern2.wls)
EAmatern2wls=sum(abs(vcdmatern2wls$predicted-vcdmatern2wls$data))
EAmatern2wls
summary(vcdmatern2wls)

## Para o método WLS1 ==> Modelo escolhido: Gaussiano ==> com base no critério de VC ##

## Mapa para o modelo gaussiano utilizando WLS1 ##

borda

apply(borda,2,range) 
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, by=0.005)) 
plot(gr)
points(d, pt.div="equal")
require(splancs)  
gi <- polygrid(gr, bor=borda)
points(gi, pch="+", col=3)


KC1=krige.control(obj=dgaus.wls, lam=1) 
KC1

d.k1=krige.conv(d,loc=gi,krige=KC1)
max(d.k1$pred)
min(d.k1$pred)
require(classInt)
valores<-d.k1$predict
classIntervals(valores, 5, style="equal", intervalClosure="right")
image(d.k1, loc=gr, border=borda,col=gray(seq(1,0,l=5)),zlim=range(d.k1$predict))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.30, 7238.55, fill=gray(c(1,0.8,0.6,0.4,0.2,0)), 
       c("0,1628 |-- 0,1680","0,1680 |-- 0,1730","0,1730 |-- 0,1780","0,1780 |-- 0,1831", 
         "0,1831 |-- 0,1881"),cex=0.88)
title(main = "WLS1 - Gaussiano", font.main = 4)


## Estimação dos parâmetros por ML ##

d=dados
d

## Utiliza o comando LIKFIT ## 

## Modelo Exponencial ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Exp.ml') 
exp.ml<-likfit(d,ini=c(0.001,0.6),lambda=1, lik.method= "ML", cov.model="exp") 
exp.ml 
lines(exp.ml,col="blue")
summary(exp.ml)

## Modelo Gaussiano ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma gaus.ml') 
gaus.ml<-likfit(d,ini=c(0.001,0.6),lambda=1, lik.method= "ML", cov.model="gaus") 
gaus.ml 
lines(gaus.ml,col="blue")
summary(gaus.ml)

## Modelo Matérn k=1 ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Matérn1.ml') 
matern1.ml<-likfit(d,ini=c(0.001,0.6),lambda=1, lik.method= "ML", cov.model="matern") 
matern1.ml 
lines(matern1.ml,col="blue")
summary(matern1.ml)

## Modelo Matérn k=1.5 ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Matérn15.ml') 
matern15.ml<-likfit(d,ini=c(0.001,0.6),lambda=1, lik.method= "ML", cov.model="matern") 
matern15.ml 
lines(matern15.ml,col="blue")
summary(matern15.ml)

## Modelo Matérn k=2 ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Matérn2.ml') 
matern2.ml<-likfit(d,ini=c(0.001,0.6),lambda=1, lik.method= "ML", cov.model="matern") 
matern2.ml 
lines(matern2.ml,col="blue")
summary(matern2.ml)

AIC(matern2.ml)

## Validação Cruzada para modelos ajustados por ML ##


## Validação Cruzada e Erro Absoluto do modelo exponencial ML ##

vcdexpml=xvalid(d,model=exp.ml)
EAexpml=sum(abs(vcdexpml$predicted-vcdexpml$data))
EAexpml
summary(vcdexpml)


## Validação Cruzada e Erro Absoluto do modelo gaussiano ML ##

vcdgausml=xvalid(d,model=gaus.ml)
EAgausml=sum(abs(vcdgausml$predicted-vcdgausml$data))
EAgausml
summary(vcdgausml)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=1 ML ##

vcdmatern1ml=xvalid(d,model=matern1.ml)
EAmatern1ml=sum(abs(vcdmatern1ml$predicted-vcdmatern1ml$data))
EAmatern1ml
summary(vcdmatern1ml)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=1.5 ML ##

vcdmatern15ml=xvalid(d,model=matern15.ml)
EAmatern15ml=sum(abs(vcdmatern15ml$predicted-vcdmatern15ml$data))
EAmatern15ml
summary(vcdmatern15ml)

## Validação Cruzada e Erro Absoluto do modelo Matérn k=2 ML ##

vcdmatern2ml=xvalid(d,model=matern2.ml)
EAmatern2ml=sum(abs(vcdmatern2ml$predicted-vcdmatern2ml$data))
EAmatern2ml
summary(vcdmatern2ml)

## Para o método ML ==> Modelo escolhido: Gaussiano ==> com base no critério de Akaike ##

## Mapa para o modelo gaussiano utilizando ML ##

borda

apply(borda,2,range) 
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, by=0.005)) 
plot(gr)
points(d, pt.div="equal")
require(splancs)  
gi <- polygrid(gr, bor=borda)
points(gi, pch="+", col=3)


KC2=krige.control(obj=gaus.ml, lam=1) 
KC2

d.k2=krige.conv(d,loc=gi,krige=KC2)
max(d.k2$pred)
min(d.k2$pred)
require(classInt)
valores<-d.k2$predict
classIntervals(valores, 5, style="equal", intervalClosure="right")
image(d.k2, loc=gr, border=borda,col=gray(seq(1,0,l=5)),zlim=range(d.k2$predict))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.30, 7238.55, fill=gray(c(1,0.8,0.6,0.4,0.2,0)), 
       c("0,1696 |-- 0,1722","0,1722 |-- 0,1746","0,1746 |-- 0,1770","0,1770 |-- 0,1794", 
         "0,1794 |-- 0,1819"),cex=0.88)
title(main = "ML - Gaussiano", font.main = 4)

## Comparar os mapas por krigagem do WLS1 e do ML 
## usando o mínimo dos mínimos e o máximo dos máximos, ou seja, alterando as escalas
## tem que fazer isso pra padronizar na mesma escala e poder comparar
## como o min dos minimos e o max dos maximos é do wls1, vamos alterar a escala
## somente no ml 

## Mapa para o modelo gaussiano utilizando ML com escalas alteradas ##

borda

apply(borda,2,range) 
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, 
                                                          by=0.005)) 
plot(gr)
points(d, pt.div="equal")
require(splancs)  
gi <- polygrid(gr, bor=borda)
points(gi, pch="+", col=3)


KC2=krige.control(obj=gaus.ml, lam=1) 
KC2

d.k2=krige.conv(d,loc=gi,krige=KC2)
require(classInt)
max(d.k2$pred)
min(d.k2$pred)
valores<-d.k2$predict
image(d.k2, loc=gr, border=borda,col=gray(seq(1,0,l=5)),zlim=range(d.k1$predict))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.30, 7238.55, fill=gray(c(1,0.8,0.6,0.4,0.2,0)), 
       c("0,1628 |-- 0,1680","0,1680 |-- 0,1730","0,1730 |-- 0,1780","0,1780 |-- 0,1831", 
         "0,1831 |-- 0,1881"),cex=0.88)
title(main = "ML - Gaussiano", font.main = 4)
classIntervals(valores, 5, style="fixed", fixedBreaks=c(0.1629765, 0.1679996, 
                                                        0.1730228, 0.178046, 0.1830691, 0.1880923))





#####################
A PARTIR DA D 
#######################

d1 = as.matrix(d.k1$predict)
d1
dim(d1)

f1<-function(d1){
  fx1<-matrix(0,dim(d1),1)
  fx1[d1<0.1679996]<-1
  fx1[d1>=0.1679996&d1<0.1730228]<-2
  fx1[d1>=0.1730228&d1<0.178046]<-3
  fx1[d1>=0.178046&d1<0.1830691]<-4
  fx1[d1>=0.1830691]<-5
  return(fx1)
}

a = f1(d1)
a

d2 = as.matrix(d.k2$predict)
d2
dim(d2)

f2<-function(d2){
  fx2<-matrix(0,dim(d2),1)
  fx2[d2<0.1679996]<-1
  fx2[d2>=0.1679996&d2<0.1730228]<-2
  fx2[d2>=0.1730228&d2<0.178046]<-3
  fx2[d2>=0.178046&d2<0.1830691]<-4
  fx2[d2>=0.1830691]<-5
  return(fx2)
}

b = f2(d2)
b


## matriz de erro ##

m<-table(as.vector(b),as.vector(a))
m