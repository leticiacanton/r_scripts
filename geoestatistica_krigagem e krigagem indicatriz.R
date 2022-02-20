require(geoR)

dados = read.geodata("prod.txt",head=T,coords.col=2:3,data.col=4)
dados

## Transformar as coordenadas de m para Km ##

dadosnewcoord=(dados$coords)/1000 
dados$coords=dadosnewcoord
dados$coords ## só com as coordenadas ##
dados

dados$data ## só com os valores do atributo produção ##

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

boxplot(dados$data,main='Gráfico Boxplot', ylab = "Produtividade de soja")    

##  Apresenta o gráfico Histograma ##

hist(dados$data,breaks=5,col="white", border="red", main='Gráfico Histograma', 
     ylab="Frequência")


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

## h-scatterplot ##

a
m = cbind(c,a)
m


d3 = read.table("prod2.txt", head=T)
d3

require(gstat)
require(car)
require(sp)
attach(d3)
names(d3)
coord <- cbind(d3$x,d3$y)
coord
coordinates(d3)=~x+y
hscat(prod~1,d3, c(0,0.3,0.6,0.9,1.2,1.5,1.78))


## anisotropia ##


## Estudo da anisotropia dos dados em 0, 45, 90 e 135 considerando o cutoff de 0.88 km (50% da distância máxima)
plot(variog4(dados,l=1,max.dist= 0.88), xlab="Distância", ylab="Semivariância")


## Construção do semivariograma experimental omnidirecional de Matheron ##

d.var <- variog(dados,uvec=seq(0,0.88,l=11), estimator.type="classical",pairs.min=30) 

## Visualização da semivariograma experimental omnidirecional de Matheron ##
plot(d.var, main= '', xlab="Distância (km)", ylab="Semivariância")

## Informações do semivariograma experimental construído ##

distancia <-  d.var$u 
semivariancia <- d.var$v
pares <- d.var$n
tabela <- cbind(distancia,semivariancia,pares)
tabela

## Estimação dos parâmetros por ML ##

d=dados
d

## Utiliza o comando LIKFIT ## 

## Modelo Exponencial ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Exp.ml') 
exp.ml<-likfit(d,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="exp") 
exp.ml 
lines(exp.ml,col="blue")
summary(exp.ml)

## Modelo Gaussiano ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Gaus.ml') 
gaus.ml<-likfit(d,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="gaus") 
gaus.ml 
lines(gaus.ml,col="blue")
summary(gaus.ml)

## Modelo Matérn k=1 ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Matérn1.ml') 
matern1.ml<-likfit(d,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="matern") 
matern1.ml 
lines(matern1.ml,col="blue")
summary(matern1.ml)

## Modelo Matérn k=1.5 ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Matérn15.ml') 
matern15.ml<-likfit(d,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="matern") 
matern15.ml 
lines(matern15.ml,col="blue")
summary(matern15.ml)

## Modelo Matérn k=2 ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Matérn2.ml') 
matern2.ml<-likfit(d,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="matern") 
matern2.ml 
lines(matern2.ml,col="blue")
summary(matern2.ml)


## Modelo Matérn k=2.5 ## 

plot(d.var,xlab='Distância',ylab='Semivariância',main='Semivariograma Matérn25.ml') 
matern25.ml<-likfit(d,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="matern") 
matern25.ml 
lines(matern25.ml,col="blue")
summary(matern25.ml)


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

## Validação Cruzada e Erro Absoluto do modelo Matérn k=2.5 ML ##

vcdmatern25ml=xvalid(d,model=matern25.ml)
EAmatern25ml=sum(abs(vcdmatern25ml$predicted-vcdmatern25ml$data))
EAmatern25ml
summary(vcdmatern25ml)


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
       c("2,58 |-- 3,23","3,23 |-- 3,86","3,86 |-- 4,49","4,49 |-- 5,13", 
         "5,13 |-- 5,77"),cex=0.88)
title(main = "ML - Gaussiano", font.main = 4)


## Krigagem indicatriz ##

## MAPA CASCAVEL ##

cascavel = read.geodata("cascavel.txt",head=T,coords.col=1:2,data.col=4)
cascavel

## ajuste do modelo ##

exp.ml<-likfit(cascavel,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="exp") 
exp.ml
summary(exp.ml)

gaus.ml<-likfit(cascavel,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="gaus") 
gaus.ml 
summary(gaus.ml)

matern1.ml<-likfit(cascavel,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="matern") 
matern1.ml 
summary(matern1.ml)

## krigagem ##

borda

apply(borda,2,range) 
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, by=0.005)) 
plot(gr)
points(cascavel, pt.div="equal")
require(splancs)  
gi <- polygrid(gr, bor=borda)
points(gi, pch="+", col=3)


KC2=krige.control(obj=gaus.ml, lam=1) 
KC2

d.k22=krige.conv(cascavel,loc=gi,krige=KC2)
max(d.k22$pred)
min(d.k22$pred)
require(classInt)
valores<-d.k22$predict
classIntervals(valores, 2, style="fixed", fixedBreaks=c(0.4007618,0.6632137,
                                                        0.9256656))
image(d.k22, loc=gr, border=borda,col=gray(seq(1,0)),zlim=range( 0.4007618
                                                                 , 0.9256656))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.30, 7238.55, fill=gray(c(1,0)),
       c("Zi < = 3,7","Zi >  3,7"),cex=1)
title(main = "ML - Gaussiano", font.main = 4)


## MAPA ESTADUAL ##

estadual = read.geodata("estadual.txt",head=T,coords.col=1:2,data.col=4)
estadual

## ajuste do modelo ##

exp.ml<-likfit(estadual,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="exp") 
exp.ml
summary(exp.ml)

gaus.ml<-likfit(estadual,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="gaus") 
gaus.ml 
summary(gaus.ml)

matern1.ml<-likfit(estadual,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="matern") 
matern1.ml 
summary(matern1.ml)

## krigagem ##

borda

apply(borda,2,range) 
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, by=0.005)) 
plot(gr)
points(estadual, pt.div="equal")
require(splancs)  
gi <- polygrid(gr, bor=borda)
points(gi, pch="+", col=3)


KC2=krige.control(obj=gaus.ml, lam=1) 
KC2

d.k222=krige.conv(estadual,loc=gi,krige=KC2)
max(d.k222$pred)
min(d.k222$pred)
require(classInt)
valores<-d.k222$predict
classIntervals(valores, 2, style="fixed", fixedBreaks=c( 0.4554061, 0, 7053597, 0.9553133))
image(d.k222, loc=gr, border=borda,col=gray(seq(1,0)),zlim=range(0.4554061, 0.9553133
))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.30, 7238.55, fill=gray(c(1,0)),
       c("Zi < = 3,5","Zi >  3,5"),cex=1)
title(main = "ML - Gaussiano", font.main = 4)


## MAPA NACIONAL ##

nacional = read.geodata("nacional.txt",head=T,coords.col=1:2,data.col=4)
nacional

## ajuste do modelo ##

exp.ml<-likfit(nacional,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="exp") 
exp.ml
summary(exp.ml)

gaus.ml<-likfit(nacional,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="gaus") 
gaus.ml 
summary(gaus.ml)

matern1.ml<-likfit(nacional,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="matern") 
matern1.ml 
summary(matern1.ml)

## krigagem ##

borda

apply(borda,2,range) 
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, by=0.005)) 
plot(gr)
points(nacional, pt.div="equal")
require(splancs)  
gi <- polygrid(gr, bor=borda)
points(gi, pch="+", col=3)


KC2=krige.control(obj=gaus.ml, lam=1) 
KC2

d.k2222=krige.conv(nacional,loc=gi,krige=KC2)
max(d.k2222$pred)
min(d.k2222$pred)
require(classInt)
valores<-d.k2222$predict
classIntervals(valores, 2, style="fixed", fixedBreaks=c(-0.007201157,0.556133421, 1.119468))
image(d.k2222, loc=gr, border=borda,col=gray(seq(1,0)),zlim=range(-0.007201157,1.119468
))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.30, 7238.55, fill=gray(c(1,0)),
       c("Zi < = 2,99","Zi > 2,99"),cex=1)
title(main = "ML - Gaussiano", font.main = 4)

## LUCRO ##

p60 = read.geodata("p60.txt", head=T, coords.col=3:4,data.col=6) 
p60

## ajuste do modelo ##

exp.ml<-likfit(p60,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="exp") 
exp.ml
summary(exp.ml)

gaus.ml<-likfit(p60,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="gaus") 
gaus.ml 
summary(gaus.ml)

matern1.ml<-likfit(p60,ini=c(0.13,0.17),lambda=1, lik.method= "ML", cov.model="matern") 
matern1.ml 
summary(matern1.ml)

## krigagem ##

borda

apply(borda,2,range) 
gr<-expand.grid(x=seq(239.1828, 241.0279, by=0.005),y=seq(7236.744, 7238.593, by=0.005)) 
plot(gr)
points(p60, pt.div="equal")
require(splancs)  
gi <- polygrid(gr, bor=borda)
points(gi, pch="+", col=3)


KC2=krige.control(obj=gaus.ml, lam=1) 
KC2

d.k60=krige.conv(p60,loc=gi,krige=KC2)
max(d.k60$pred)
min(d.k60$pred)
require(classInt)
valores<-d.k60$predict
classIntervals(valores, 2, style="fixed", fixedBreaks=c(0.1474707, 0.53189455, 0.9163184
))
image(d.k60, loc=gr, border=borda,col=gray(seq(1,0)),zlim=range(0.1474707, 0.9163184
))
text(239.4, 7238.2, expression(bolditalic(NA%up%N)),cex=2)
legend(240.30, 7238.55, fill=gray(c(1,0)),
       c("Zi < = 4,34","Zi > 4,34"),cex=1)
title(main = "ML - Gaussiano", font.main = 4)
