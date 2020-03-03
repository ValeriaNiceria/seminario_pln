# Instalando os pacotes
# install.packages("rtweet")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("tidyverse")

# Carregando os pacotes
library(tm)
library(wordcloud)
library(tidyverse)

# Lendo dados do resultado da busca da api do Twitter
datascience_tweet <- readRDS("dados/dados_busca_twitter.rds")
