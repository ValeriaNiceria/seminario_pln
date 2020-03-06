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
library(tidytext)

# Buscando os tweets com a #economia
economia_tweet <- search_tweets(
  "#economia",
  n = 18000,
  include_rts = FALSE,
  lang = "pt"
)

View(economia_tweet)

# Gerando um gráfico com a frequencia dos tweets no intervalo de 1 hora
economia_tweet %>% 
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de #economia no Twitter",
    subtitle = "Tweets no intervalo de 1 hora",
    caption = "\nSource: Dados coletados do Twitter's REST API via rtweet"
  )

# Atribuindo os textos a uma variável
economia_texto <- economia_tweet$text

# Transformando os textos em um corpus
economia_corpus <- VCorpus(VectorSource(economia_texto))

# Realizando a limpeza do corpus
economia_corpus <- 
  tm_map(
    economia_corpus,
    content_transformer(
      function(x) iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    )
  ) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords("portuguese")) %>% 
  tm_map(removeWords, c("nao", "vai", "sobre"))

# Lista de cores em hexadecimal
paleta <- brewer.pal(8, "Dark2")

# Criando uma nuvem de palavras, com no máximo 100 palavras
# onde tenha se repetido ao menos 2 vezes.
wordcloud(
  economia_corpus,
  min.freq = 2,
  max.words = 100,
  random.order = F,
  colors = paleta
)

# Criando uma matriz de termos
economia_document <- DocumentTermMatrix(economia_corpus)

# Removendo os termos menos frequentes
economia_doc <- removeSparseTerms(economia_document, 0.98)

# Gerando uma matrix ordenada, com o termos mais frequentes
economia_freq <- 
  economia_doc %>% 
  as.matrix() %>% 
  colSums() %>% 
  sort(decreasing = T)

# Criando um dataframe com as palavras mais frequentes
df_economia <- data.frame(
  word = names(economia_freq),
  freq = economia_freq
)

# Gerando um gráfico da frequência
df_economia %>%
  filter(!word %in% c("economia", "irb")) %>% 
  subset(freq > 10) %>%
  ggplot(aes(x = reorder(word, freq),
             y = freq)) +
  geom_bar(stat = "identity", fill='#0c6cad', color="#075284") +
  theme(axis.text.x = element_text(angle = 45, hjus = 1)) +
  ggtitle("Termos relacionados a Data Science mais frequentes no Twitter") +
  labs(y = "Frequência", x = "Termos") +
  coord_flip()


# Removendo os termos menos frequentes
economia_doc1 <- removeSparseTerms(economia_doc, 0.96)

# Clustering => Dendograma
distancia <- dist(t(economia_doc1), method = "euclidian")
dendograma <- hclust(d = distancia, method = "complete")
plot(dendograma, habg = -1, main = "Dendograma Tweets economia",
     xlab = "Distância",
     ylab = "Altura")



# Carregando o pacote 'devtools'
library(devtools)

# Instalando o pacote 'wordcloud2'
devtools::install_github("lchiffon/wordcloud2")

# Carregando o pacote 'wordcloud2'
library(wordcloud2)

wordcloud2(data = df_economia)
