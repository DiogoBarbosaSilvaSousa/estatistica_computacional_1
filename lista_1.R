###
# Lista 1
###

###
# 1 - Utilize comandos R para gerar os vetores abaixo:
# V=(3,6,9,12,...,99)
# V=(5,25,125,625,...,9765625)
# V=(1,-1,1,-1,...,1,-1)(200 termos)
# V=(1,2,2,3,3,3,4,4,4,4,5,5,5,5,5)
###
x=seq(3,99,3)
x

x=5^seq(1,10)
x

x=(-1)^seq(0,199)
x

x=c(1,2,2,3,3,3,4,4,4,4,5,5,5,5,5)
x

###
# 2 - Supondo que x<-2, escreva um comando de atribuição que calcule a seguinte 
# expressão 3x^3+2x^2+6x+1 (tente minimizar o número de operações)..
###

x=2
resolva_equacao <- function(x) {
  k = (3*(x)^3 + 2*(x)^2 + 6*(x) +1)
  return(k)
}
resolva_equacao(2)

###
# 3 - Utilize os comandos do R para gerar um vetor contendo todos os inteiros 
# de 1 até 100 que não são divisíveis por 2,3 ou 7.
###
x = 1:100
x[(x%%2!=0)&(x%%3!=0)&(x%%7!=0)]

###
# 4 - Supondo que x<-2, y<-4, z<-8 e w<-3, determine os resultados dasseguintes expressões:
###
x<-2 
y<-4 
z<-8 
w<-3
cat(x>y & z+y>w)
cat(x==y%%2 | y^2>sqrt(z))
cat(y<z & w+z%%2<1)

###
# 5 - Considerando o comando x<-2, calcule a seguinte expressão: log2(x^2+1) + e^4x+1
###

x<-2
cat(log2(x^2 + 1) + exp(1)^(4*x + 1))

###
# 6 - Considere o vetor V<-sample(20,40,replace=TRUE). Determine o produtório 
# dos elementos de V excluindo repetições.
###

V<-sample(20,40,replace=TRUE)
V
V<-sort(V)
V
V<-union(V,V)
V
cat(prod(V))

###
# 7 - Considere os vetores X<-sample(20,40,replace=TRUE) e Y<-sample(20,40,replace=TRUE).
# Determine as somas acumuladas dos elementos que são comuns a X e a Y.
###
X<-sample(20,40,replace=TRUE)
X
Y<-sample(20,40,replace=TRUE)
Y

Z<-intersect(X,Y)
Z

cat(sum(Z))

###
# 8 - Considere o comando "a<-sample(1000,100)". Escreva um comando que retorne
# o número de elementos de a que não são divisíveis por 4.
###

a<-sample(1000,100)
a
k<-a[(a%%4!=0)]

###
# 9 - Escreva um conjunto de comandos para determinar quais são os divisores do
# número 128.
###

x = 1:128
x
z = x[128%%x==0]
z
