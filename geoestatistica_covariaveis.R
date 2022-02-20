#Variável: Produtividade de soja
#Covariáveis: Fe, PH e K.

#Carregando o pacote geoR
require(geoR)

#Carregado o conjunto de dados
dados<-read.geodata("dados_12_13_Fe_PH_K.txt",coords.col=1:2,data.col=3,covar.col=4:6,head=TRUE)

#Carregando as coordenadas do contorno
bor<-read.table("contorno.txt", head=FALSE)

#Visualizando o mapa dos pontos e contorno.
points(dados,xlim=c(239000,241000))
polygon(bor)

#Estatísticas

summary(dados$data)
summary(dados$cov)
summary(dados$cov[,2])
boxplot(dados$data)
hist(dados$cov[,2])

#Análise exploratória espacial
points(dados,pt.div="quartile",col=c("green","yellow","orange","red"), main="post-plot")

#Se desejar construir para as covariáveis, carregue um novo conjunto de dados.
#Exemplo: Post-plot do Fe

dados_fe<-read.geodata("dados_12_13_Fe_PH_K.txt",coords.col=1:2,data.col=4,head=TRUE)
points(dados_fe,pt.div="quartile",col=c("green","yellow","orange","red"), main="post-plot")


#Ajuste de um modelo

#50% da distância máxima
cutoff<-max(dist(dados$coords))/2
cutoff

#Semivariograma experimental
dados.var=variog(dados,uvec=seq(0,882,l=12),trend=~Fe+PH+K,estimator.type="classical",pairs.min=30)
plot(dados.var,xlab='Distância',font.lab=1,ylab='Semivariâncias',main='Semivariograma',font.main = 4)

#Ajuste de um modelo Exponencial utilizando ML
exp.ml<-likfit(dados,ini=c(0.05,50),lik.method="ML",trend=trend.spatial(~Fe+PH+K),cov.model='exp')
exp.ml

lines(exp.ml)

#Validação cruzada

vce=xvalid(dados,model=exp.ml)
EA=sum(abs(vce$predicted-vce$data))
summary(vce)
EA
summary(exp.ml)


#Utilizar os modelos Matérn com k = 1, 1.5, 2.5 e o modelo Gaussiano.

#Ajuste de um modelo Gauss utilizando ML
gaus.ml<-likfit(dados,ini=c(0.05,50),lik.method="ML",trend=trend.spatial(~Fe+PH+K),cov.model='gaus')
gaus.ml

#Validação cruzada

vce=xvalid(dados,model=gaus.ml)
EA=sum(abs(vce$predicted-vce$data))
summary(vce)
EA
summary(gaus.ml)

#Ajuste de um modelo Matern k=1 utilizando ML
m1.ml<-likfit(dados,ini=c(0.05,50),lik.method="ML",trend=trend.spatial(~Fe+PH+K),cov.model='matern', kappa=1)
m1.ml

#Validação cruzada

vce=xvalid(dados,model=m1.ml)
EA=sum(abs(vce$predicted-vce$data))
summary(vce)
EA
summary(m1.ml)

#Ajuste de um modelo Matern k=1.5 utilizando ML
m15.ml<-likfit(dados,ini=c(0.05,50),lik.method="ML",trend=trend.spatial(~Fe+PH+K),cov.model='matern', kappa=15)
m15.ml

#Validação cruzada

vce=xvalid(dados,model=m15.ml)
EA=sum(abs(vce$predicted-vce$data))
summary(vce)
EA
summary(m15.ml)

#Ajuste de um modelo Matern k=25 utilizando ML
m25.ml<-likfit(dados,ini=c(0.05,50),lik.method="ML",trend=trend.spatial(~Fe+PH+K),cov.model='matern', kappa=25)
m25.ml

#Validação cruzada

vce=xvalid(dados,model=m25.ml)
EA=sum(abs(vce$predicted-vce$data))
summary(vce)
EA
summary(m25.ml)