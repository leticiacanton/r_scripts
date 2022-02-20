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

