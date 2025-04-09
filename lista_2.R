###
# Lista 2
###

###
# 1 - Escreva comandos para construir as a matrizes abaixo:
# | 0 1 1 |   |   2   4   8 |
# | 1 0 1 | , |  16  32  64 | 
# | 1 1 0 |   | 128 256 512 |
###

matriz_1 = matrix(c(0,1,1,1,0,1,1,1,0),nrow = 3, ncol = 3)
matriz_1

matriz_2 = matrix(c(2,4,8,16,32,64,128,256,512),nrow = 3, ncol = 3, byrow = TRUE)
matriz_2

###
# 2 - Na teoria dos sistemas , define-se o elemento minmax de uma matriz, composta de
# elementos distintos, como sendo o maior elemento da linha onde se encontra o menor 
# elemento da matriz. Considerando que tenhamos uma matriz M definida por 
# M<-matrix(sample(20,16),4,4), determine o seu elemento minmax.
##

# Carregar o pacote necessário
# set.seed(123)  # Definindo a semente para gerar resultados reprodutíveis

# Gerar a matriz M
M = matrix(sample(20, 16), nrow = 4, ncol = 4)

# Exibir a matriz gerada
print("Matriz M:")
print(M)

# Passo 1: Encontrar o menor elemento da matriz
min_element = min(M)
print(min_element)

# Passo 2: Encontrar a linha onde o menor elemento está. 
# O (arr.ind = TRUE) retorna a linha e a coluna onde se enconta o elemento da matriz
# dentro de uma matriz com duas colunas sendo o primeiro a linha e o segundo a coluna.
min_pos = which(M == min_element, arr.ind = TRUE)
min_row = min_pos[1, 1]
print(min_row)

# Passo 3: Encontrar o maior elemento na linha onde o menor elemento está
max_in_row = max(M[min_row, ])

# Exibir o resultado
cat("Elemento minmax: ", max_in_row, "\n")

###
# 3 - Considerando que a matriz definida por M<-cbind(sample(10,4),sample(10,4))
# tenha as coordenadas x e y de quatro pontos no R^2, calcule a distância euclidiana
# entre todos os pontos tomados dois a dois (utilize o mínimo de comandos). 
# Resolva este problema de duas formas diferentes.

##############################
# SOLUÇÃO 1                  #
##############################

# Gerando a matriz M com as coordenadas dos pontos

M = cbind(sample(10, 4), sample(10, 4))

# Exibindo a matriz com as coordenadas
print("Matriz com as coordenadas dos pontos:")
print(M)

# Função para calcular a distância euclidiana entre dois pontos
distancia = function(p1, p2) {
  sqrt((p2[1] - p1[1])^2 + (p2[2] - p1[2])^2)
}

# Calculando as distâncias entre todos os pontos, dois a dois
distancias = matrix(NA, nrow = 4, ncol = 4)  # Matriz de distâncias

for (i in 1:4) {
  for (j in i:4) {  # Calculando apenas a metade superior da matriz
    if (i != j) {
      distancias[i, j] <- distancia(M[i, ], M[j, ])  # Distância entre ponto i e ponto j
      
      # preenchendo a metade de baixo da matriz
      distancias[j, i] <- distancias[i, j]  # A matriz é simétrica
    }
  }
}

# Exibindo a matriz de distâncias
print("Matriz de distâncias entre os pontos:")
print(distancias)

################################
# SOLUÇÃO 2                    #  
################################

# Gerando a matriz M com as coordenadas dos pontos
#M = cbind(sample(10, 4), sample(10, 4))

# Exibindo a matriz com as coordenadas
print("Matriz com as coordenadas dos pontos:")
print(M)

# Usando a função dist para calcular a matriz de distâncias
distancias = dist(M)

# Exibindo a matriz de distâncias
print("Matriz de distâncias entre os pontos (usando dist):")
print(as.matrix(distancias))

###
# 4 - Considerando o vetor P<-1:100 , calcule a média de P e calcule as médias 
# de todos os elementos de P tomados dois a dois. Em seguida, calcule a média destas médias.
###

# Definindo o vetor P
P = 1:100

# Passo 1: Calcular a média de P
media_P = mean(P)
cat("Média de P:", media_P, "\n")

# Passo 2: Calcular as médias de todos os elementos de P tomados dois a dois
pares = combn(P, 2)  # Combinações de 2 elementos de P

media_pares = apply(pares, 2, mean)  # Calcula a média de cada par de elementos
print(media_pares)

# Passo 3: Calcular a média das médias
media_das_medias <- mean(media_pares)
cat("Média das médias dos pares:", media_das_medias, "\n")

###
# 5 - Dados dois conjuntos X = {x_1,X_2,x_3,...,X_n} e Y = {y_1,Y_2,y_3,...,y_n}
# define-se o produto cartesiano de X e Y como conjunto de todos os pares ordenados
# cujo primeiro elemento pertence a X e o segundo a Y . Supondo que X = seq(1,15,3)
# e Y=seq(2,20,2) , escreva um conjunto de comandos que produzam o produto cartesiano.
##

# Gerar os conjuntos X e Y
X = seq(1, 15, 3)
Y = seq(2, 20, 2)
print(X)
print(Y)

# Calcular o produto cartesiano usando expand.grid
produto_cartesiano = expand.grid(X = X, Y = Y)

# Exibir o resultado
print(produto_cartesiano)

###
# 6 - Considere o vetor V<-sample(20,40,replace = TRUE). Determine o produtório
# dos elementos de V excluindo os elementos repetidos de V.
###

# Gerar o vetor V
V = sample(20, 40, replace = TRUE)

# Exibir o vetor V
cat("Vetor V:", V, "\n")

# Excluir os elementos repetidos de V
V_unicos = unique(V)

# Exibir os elementos únicos
cat("Elementos únicos de V:", V_unicos, "\n")

# Calcular o produtório dos elementos únicos
produtorio = prod(V_unicos)

# Exibir o resultado
cat("Produtório dos elementos únicos de V:", produtorio, "\n")

###
# 7 - Considere os vetores X<-sample(20,40, replace=TRUE) e Y<-sample(20,40, replace=TRUE).
# Determine as somas acumuladas dos elementos que são comuns a X e Y.
###

# Gerar os vetores X e Y
X = sample(20, 40, replace = TRUE)
Y = sample(20, 40, replace = TRUE)

# Exibir os vetores X e Y
cat("Vetor X:", X, "\n")
cat("Vetor Y:", Y, "\n")

# Encontrar os elementos comuns entre X e Y
comuns = intersect(X, Y)

# Exibir os elementos comuns
cat("Elementos comuns a X e Y:", comuns, "\n")

# Calcular as somas acumuladas dos elementos comuns
somas_acumuladas = cumsum(comuns)

# Exibir o resultado
cat("Somas acumuladas dos elementos comuns:", somas_acumuladas, "\n")


###
# 8 - Uma matriz é dita diagonal dominante se, para todas as linhas da matriz, o 
# módulo  do valor da matriz na diagonal é maior que a soma dos módulos de todos os 
# demais valores(não-diagonais). Considerando M<-matrix(c(-4,1,2,3,5,1,7,9,12),3,3,byrow = TRUE),
# escreva uma expressão em R que avalie se M é diagonal dominante (TRUE) ou não.
##

# Definir a matriz M
M = matrix(c(-7, 1, 2, 3, 5, 1, 7, 9, 12), 3, 3, byrow = TRUE)
M

# Função para verificar se a matriz é diagonal dominante
is_diagonal_dominant = function(M) {
  diagonal = 0
  soma_nao_diagonal = 0
  
  n = nrow(M)
  for (i in 1:n) {
    diagonal = diagonal + abs(M[i, i])  # Módulo do elemento diagonal
    soma_nao_diagonal = soma_nao_diagonal + sum(abs(M[i, -i]))  # Soma dos módulos dos elementos não diagonais
  }
  
  if (diagonal <= soma_nao_diagonal) {
    return(FALSE)  # Se a condição não for satisfeita, retorna FALSE
  }
  
  return(TRUE)  # Se todas as linhas satisfizerem a condição, retorna TRUE
}

# Verificar se a matriz M é diagonal dominante
resultado = is_diagonal_dominant(M)

# Exibir o resultado
cat("A matriz M é diagonal dominante?", resultado, "\n")

###
# 9 - Considere a matriz definida por: ma = matrix(sample(1:10,49,T)/sample(seq(10,100,10),49,T),7,7) .
# Determine o maior elemento da diagonal principal e as somas acumuladas dos elementos de cada uma das colunas de ma.
###

# Gerando a matriz ma
ma = matrix(sample(1:10, 49, TRUE) / sample(seq(10, 100, 10), 49, TRUE), 7, 7)

# Exibindo a matriz ma
cat("Matriz ma:\n")
print(ma)

# Parte 1: Determinar o maior elemento da diagonal principal
ma_diagonal = ma[cbind(1:7, 1:7)]  # Elementos da diagonal principal
ma_maior_diagonal = max(ma_diagonal)  # Maior valor da diagonal principal
cat("Maior elemento da diagonal principal:", ma_maior_diagonal, "\n")

# Parte 2: Calcular as somas acumuladas das colunas
somas_acumuladas_colunas = apply(ma, 2, cumsum)  # Calcula soma acumulada por coluna
cat("Somas acumuladas das colunas:\n")
print(somas_acumuladas_colunas)

###
# 10 - Considere um vetor q<-c(1,5,10,20,50,100). Determine todas as somas dos elementos de
# q tomados dois a dois, três a três, quatro a quatro, cinco a cinco e seis a seis.
##

# Definir o vetor q
q = c(1, 5, 10, 20, 50, 100)

# Função para calcular as somas das combinações
calcular_somas = function(vetor, n) {
  # Gerar todas as combinações de n elementos
  combinacoes <- combn(vetor, n)
  
  # Calcular a soma de cada combinação
  somas <- apply(combinacoes, 2, sum)
  
  return(somas)
}

exibir_combinacao = function (vetor, n) {
  combinacoes <- combn(vetor, n)
  print(combinacoes)
}



# Calcular e exibir as somas para diferentes combinações
exibir_combinacao(q, 2)
cat("Somas dos elementos de q tomados dois a dois:\n")
print(calcular_somas(q, 2))


exibir_combinacao(q, 3)
cat("Somas dos elementos de q tomados três a três:\n")
print(calcular_somas(q, 3))

exibir_combinacao(q, 4)
cat("Somas dos elementos de q tomados quatro a quatro:\n")
print(calcular_somas(q, 4))

exibir_combinacao(q, 5)
cat("Somas dos elementos de q tomados cinco a cinco:\n")
print(calcular_somas(q, 5))

exibir_combinacao(q, 6)
cat("Somas dos elementos de q tomados seis a seis:\n")
print(calcular_somas(q, 6))
