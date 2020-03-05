# Instalando os pacotes
# install.packages("rtweet")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("tidyverse")

# Carregando os pacotes
library(tm)
library(rtweet)
library(wordcloud)
library(tidyverse)

# Buscando os tweets com a #datascience
datascience_tweets <- search_tweets(
  "#datascience",
  n = 18000,
  include_rts = FALSE,
  lang = "en"
)

# Gerando um gráfico com a frequencia dos tweets no intervalo de 1 hora
datascience_tweets %>% 
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de #datascience no Twitter",
    subtitle = "Tweets no intervalo de 1 hora",
    caption = "\nSource: Dados coletados do Twitter's REST API via rtweet"
  )

# Atribuindo os textos a uma variável
datascience_texto <- datascience_tweets$text

# Transformando os textos em um corpus
datascience_corpus <- VCorpus(VectorSource(datascience_texto))

# Realizando a limpeza do corpus
datascience_corpus <- 
  tm_map(
    datascience_corpus,
    content_transformer(
      function(x) iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    )
  ) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords("english"))

# Lista de cores em hexadecimal
paleta <- brewer.pal(8, "Dark2")

# Criando uma nuvem de palavras, com no máximo 100 palavras
# onde tenha se repetido ao menos 2 vezes.
wordcloud(
  datascience_corpus,
  min.freq = 2,
  max.words = 100,
  random.order=FALSE,
  colors = paleta
)

# Criando uma matriz de termos
datascience_document <- DocumentTermMatrix(datascience_corpus)

# Removendo os termos menos frequentes
datascience_doc <- removeSparseTerms(datascience_document, 0.98)

# Gerando uma matrix ordenada, com o termos mais frequentes
datascience_freq <- 
  datascience_doc %>% 
  as.matrix() %>% 
  colSums() %>% 
  sort(decreasing = T)

# Criando um dataframe com as palavras mais frequentes
df_datascience <- data.frame(
  word = names(datascience_freq),
  freq = datascience_freq
)

# Gerando um gráfico da frequência
df_datascience %>%
  filter(!word %in% c("datascience", "via")) %>% 
  subset(freq > 450) %>%
  ggplot(aes(x = reorder(word, freq),
             y = freq)) +
  geom_bar(stat = "identity", fill='#0c6cad', color="#075284") +
  theme(axis.text.x = element_text(angle = 45, hjus = 1)) +
  ggtitle("Termos relacionados a Data Science mais frequentes no Twitter") +
  labs(y = "Frequência", x = "Termos") +
  coord_flip()


# Carregando o pacote 'devtools'
library(devtools)

# Instalando o pacote 'wordcloud2'
devtools::install_github("lchiffon/wordcloud2")

# Carregando o pacote 'wordcloud2'
library(wordcloud2)

wordcloud2(data = df_datascience)


# Removendo os termos menos frequentes
datascience_doc1 <- removeSparseTerms(datascience_document, 0.95)

# Clustering 1 = Dendograma
distancia <- dist(t(datascience_doc1), method = "euclidian")
dendograma <- hclust(d = distancia, method = "complete")
plot(dendograma, habg = -1, main = "Dendograma Tweets Data Science",
     xlab = "Distância",
     ylab = "Altura")

# Clustering 2 - k-means
kmeans_datascience <- kmeans(distancia, 10)
clusplot(as.matrix(distancia), kmeans_datascience$cluster, color = T, share = T, labels = 3, lines = 0,
         main = "K-Means Tweets Data Science",
         xlab = "PC1",
         ylab = "PC2")
