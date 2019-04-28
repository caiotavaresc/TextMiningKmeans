install.packages("fpc")
install.packages("ptstem")
install.packages("tm")
install.packages("SwarmSVM")

setwd("C:/Users/ctcca/Documents/USP/POS_BigData/MOD1/Business_BigData")

library(fpc)
library(ptstem)
library(tm)

reclam <- read.csv("Corpus_Bruto.csv",sep=";",header=T)
corpus <- Corpus(DataframeSource(reclam))

#Remocao de pontuacao, numeros e deixar todos os caracteres minusculos
corpus_fi <- tm_map(corpus, content_transformer(tolower))
corpus_fi <- tm_map(corpus_fi, content_transformer(removePunctuation))
corpus_fi <- tm_map(corpus_fi, content_transformer(removeNumbers))

#Remocao de stopwords (palavras irrelevantes)
corpus_fi <- tm_map(corpus_fi, removeWords, c(stopwords("portuguese"), c("safra", "safrapay", "pay", "banco", "pagseguro", "get", "net")))

#Redução dos termos ao seu radical (stemming)
corpus_fi3 <- tm_map(corpus_fi, ptstem)

#Criar a matriz de frequencia de termos
corpus_tf2 <- DocumentTermMatrix(corpus_fi3, control = list(minWordLength=1,minDocFreq=1))

#Colocar o peso (TF-IDF)
corpus_tf_idf2 <- weightTfIdf(corpus_tf2, normalize=TRUE)

#Remover a esparsidao
corpus_tf_idf3 <- removeSparseTerms(corpus_tf_idf2, 0.8)
corpus_tf_idf3

#Rodar o K-means
matriz_tfidf3 <- as.matrix(corpus_tf_idf3)
kmeans <- kmeans(matriz_tfidf3, 8)

#Resultado -> A que cluster corresponde cada observação
kmeans$cluster