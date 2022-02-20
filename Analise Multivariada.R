## QUESTÃO 1 ##

## A ##

dados1 <- read.table("quadrop1.txt", header = T)   
dados1

attach(dados1)
names(dados1)

is.data.frame(dados1)

dim(dados1)

colMeans(dados1)

S <- cov(dados1,dados1)
S

R <- cor(dados1,dados1)
R

cor.test(X1,X2)
cor.test(X2,X2)
cor.test(X3,X2)
cor.test(X4,X2)
cor.test(X5,X2)
cor.test(X6,X2)
cor.test(X7,X2)
cor.test(X8,X2)
cor.test(X9,X2)

cor.test(X1,X3)
cor.test(X2,X3)
cor.test(X3,X3)
cor.test(X4,X3)
cor.test(X5,X3)
cor.test(X6,X3)
cor.test(X7,X3)
cor.test(X8,X3)
cor.test(X9,X3)

cor.test(X1,X4)
cor.test(X2,X4)
cor.test(X3,X4)
cor.test(X4,X4)
cor.test(X5,X4)
cor.test(X6,X4)
cor.test(X7,X4)
cor.test(X8,X4)
cor.test(X9,X4)

cor.test(X1,X5)
cor.test(X2,X5)
cor.test(X3,X5)
cor.test(X4,X5)
cor.test(X5,X5)
cor.test(X6,X5)
cor.test(X7,X5)
cor.test(X8,X5)
cor.test(X9,X5)

cor.test(X1,X6)
cor.test(X2,X6)
cor.test(X3,X6)
cor.test(X4,X6)
cor.test(X5,X6)
cor.test(X6,X6)
cor.test(X7,X6)
cor.test(X8,X6)
cor.test(X9,X6)

cor.test(X1,X7)
cor.test(X2,X7)
cor.test(X3,X7)
cor.test(X4,X7)
cor.test(X5,X7)
cor.test(X6,X7)
cor.test(X7,X7)
cor.test(X8,X7)
cor.test(X9,X7)

cor.test(X1,X8)
cor.test(X2,X8)
cor.test(X3,X8)
cor.test(X4,X8)
cor.test(X5,X8)
cor.test(X6,X8)
cor.test(X7,X8)
cor.test(X8,X8)
cor.test(X9,X8)

cor.test(X1,X9)
cor.test(X2,X9)
cor.test(X3,X9)
cor.test(X4,X9)
cor.test(X5,X9)
cor.test(X6,X9)
cor.test(X7,X9)
cor.test(X8,X9)
cor.test(X9,X9)

## TESTE DE ESFERICIDADE DE BARTLETT ##

n <- nrow(dados1)
p <- ncol(dados1)
chi2 <- -(n-1-((2*p+5)/6))*log(det(R))
ddl <- p*(p-1)/2
print(chi2)
print(ddl)
print(pchisq(chi2,ddl,lower.tail=F)) 


require(psych)
cortest.bartlett(R,n)

require(psych)
KMO(R) 


## COMPONENTES PRINCIPAIS USANDO A MATRIZ R ##

eigen(R)

cp2 <- princomp(dados1, cor = T)  
cp2

summary(cp2)

par(mfrow = c(2,2))
screeplot(cp2, main = "" )
screeplot(cp2, main = "", type = "lines")


names(cp2)
cp2$loa       ## coeficientes de cada componente principal ##
cp2$score

par(mfrow = c(1,2))
plot(cp2$score[,3], cp2$score[,4])
biplot(cp2)

biplot (cp2, choices=3:4, scale=0.2,pc.biplot=FALSE, xlab="Comp.3", ylab= "Comp.4")

cor(cp2$score[,1],X1)
cor(cp2$score[,1],X2)   
cor(cp2$score[,1],X3)
cor(cp2$score[,1],X4)
cor(cp2$score[,1],X5)
cor(cp2$score[,1],X6)
cor(cp2$score[,1],X7)
cor(cp2$score[,1],X8)
cor(cp2$score[,1],X9)

cor(cp2$score[,2],X1)
cor(cp2$score[,2],X2)
cor(cp2$score[,2],X3)
cor(cp2$score[,2],X4)
cor(cp2$score[,2],X5)
cor(cp2$score[,2],X6)
cor(cp2$score[,2],X7)
cor(cp2$score[,2],X8)
cor(cp2$score[,2],X9)

cor(cp2$score[,3],X1)
cor(cp2$score[,3],X2)
cor(cp2$score[,3],X3)
cor(cp2$score[,3],X4)
cor(cp2$score[,3],X5)
cor(cp2$score[,3],X6)
cor(cp2$score[,3],X7)
cor(cp2$score[,3],X8)
cor(cp2$score[,3],X9)

cor(cp2$score[,4],X1)
cor(cp2$score[,4],X2)
cor(cp2$score[,4],X3)
cor(cp2$score[,4],X4)
cor(cp2$score[,4],X5)
cor(cp2$score[,4],X6)
cor(cp2$score[,4],X7)
cor(cp2$score[,4],X8)
cor(cp2$score[,4],X9)


## B ##

auto = eigen (R)
auto

## CARGAS FATORIAIS ## 

carga1 <- sqrt(auto$values[1]) * auto$vectors[,1]
carga2 <- sqrt(auto$values[2]) * auto$vectors[,2]
carga3 <- sqrt(auto$values[3]) * auto$vectors[,3]
carga4 <- sqrt(auto$values[4]) * auto$vectors[,4]

carga1
carga2
carga3
carga4

## MATRIZ L ##

L <- cbind(carga1,carga2,carga3,carga4)
L

L3 = cbind(carga3, carga4)
L3

L4 = cbind(carga1, carga2)
L4

rownames = colnames(dados1)
rownames

plot(carga1,carga2, col = "blue", xlab = "Fator 1", ylab = "Fator 2")
text(carga1, carga2, rownames, adj=1)

plot(carga3, carga4, col = "blue", xlab = "Fator 3", ylab = "Fator 4")
text(carga3, carga4, rownames, adj=1)


## COMUNALIDADE ##

com <- carga1^2 + carga2^2 + carga3^2 + carga4^2
com 

## MATRIZ PSI ##

psi = R-L%*%t(L)
psi

## M?TODO VARIMAX ##

T1 = matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)), ncol = 2, nrow = 2)
T1 ## faz rotação de 45º no sentido anti-horário ##

L1 = cbind(carga1, carga2)
L1

L2 = cbind(carga3, carga4)
L2

LT1 = L1 %*% T1
LT1

LT2 = L2%*% T1
LT2

plot(LT1[,1],LT1[,2], col = "blue", xlab = "Fator 1", ylab = "Fator 2")
text(LT1[,1],LT1[,2], rownames, adj=1)

plot(LT2[,1],LT2[,2], col = "blue", xlab = "Fator 3", ylab = "Fator 4")
text(LT2[,1],LT2[,2], rownames, adj=1)

varimax(L1, normalize = F)
varimax(L2, normalize = F)

T2 = matrix(c(cos(pi/3), sin(pi/3), -sin(pi/3), cos(pi/3)), ncol = 2, nrow = 2)
T2  ## faz rotação de 60º no sentido anti-horário ##

L1 = cbind(carga1, carga2)
L1

L2 = cbind(carga3, carga4)
L2

LT11 = L1 %*% T2
LT11

LT22 = L2%*% T2
LT22

plot(LT11[,1],LT11[,2], col = "blue", xlab = "Fator 1", ylab = "Fator 2")
text(LT11[,1],LT11[,2], rownames, adj=1)

plot(LT22[,1],LT22[,2], col = "blue", xlab = "Fator 3", ylab = "Fator 4")
text(LT22[,1],LT22[,2], rownames, adj=1)

a = varimax(L1, normalize = F)
a
l = a$loading
l

b = varimax(L2, normalize = F)
b
ll = b$loading
ll

plot(a$loading, col = "blue", xlab = "Fator 1", ylab = "Fator 2")
text(l[,1], l[,2], rownames, adj=1)

plot(b$loading, col = "blue", xlab = "Fator 3", ylab = "Fator 4")
text(ll[,1], ll[,2], rownames, adj=1)

com = carga1^2 + carga2^2 + carga3^2 + carga4^2
com

## C ##

## AGRUPAMENTO DE INDIVÍDUOS ##

disteuc <- dist(dados1) 
disteuc

single <- hclust(disteuc, "single")    ## ligação simples ##
single

d2 <- cophenetic(single)
cor(disteuc,d2)              ## coeficiente de correlação cofenética ##
cor.test(disteuc,d2)

complete <- hclust(disteuc, "complete")    ## ligação completa ##
complete

d2 <- cophenetic(complete)
cor(disteuc,d2)          
cor.test(disteuc,d2)

average <- hclust(disteuc, "average")    ## ligação média ##
average

d2 <- cophenetic(average)
cor(disteuc,d2)              
cor.test(disteuc,d2)

## DENDROGRAMA ##

single$height

plot(single, xlab = "Países", ylab = "Distância Euclidiana", main = "", 
hang = -1)
rect.hclust(single, k = 6)

complete$height
plot(complete, xlab = "Países", ylab = "Distância Euclidiana", main = "", 
hang = -1)

average$height
plot(average, xlab = "Países", ylab = "Distância Euclidiana", main = "", 
hang = -1)

## D ##

## AGRUPAMENTO DE VARIÁVEIS ##

R

r <- as.dist((1-(R^2)))  ## SUGERIDA POR RENCHER (2002) ##
r     ## usada para trabalhar com dissimilaridade ##

single1 <- hclust(r, "single")  
single1

d2 <- cophenetic(single1)
cor(r,d2)             
cor.test(r,d2)


average1 <- hclust(r, "average")   
average1

d2 <- cophenetic(average1)
cor(r,d2)              
cor.test(r,d2)

complete1 = hclust(r, "complete")          
complete1

d2 <- cophenetic(complete1)
cor(r,d2)              
cor.test(r,d2)


## DENDROGRAMA ## 

plot(single1, xlab = "Variáveis", ylab = "Função da Correlação", main = "", hang = -1)
rect.hclust(single1, k = 4) ## k = número de grupos ##
single1$height

plot(average1, xlab = "Variáveis", ylab = "Função da Correlação", main = "", hang = -1)


plot(complete1, xlab = "Variáveis", ylab = "Função da Correlação", main = "", hang = -1)


c1 <- cutree(single1, k = 4)    
c1  ## explicita para cada variável o grupo ao qual pertence ## 