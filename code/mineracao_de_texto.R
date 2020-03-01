# Instalando os pacotes
# install.packages("tm")
# install.packages("rtweet")
# install.packages("wordcloud")
# install.packages("tidyverse")

# Carregando os pacotes
library(tm)
library(rtweet)
library(wordcloud)
library(tidyverse)

# Buscando os dados no twitter
datascience_tweet <- search_tweets(
  "#datascience",
  n = 18000,
  include_rts = FALSE,
  lang = "en"
)

# Visualizando a frequência de tweets, em um intervalo de tempo.
datascience_tweet %>% 
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de #datascience no Twitter",
    subtitle = "Tweets no intervalo de 3 horas",
    caption = "\nSource: Dados coletados do Twitter's REST API via rtweet"
  )


# Iniciando a mineração do texto
datascience_texto <- datascience_tweet$text

# Criando e limpando o corpus
datascience_corpus <- VCorpus(VectorSource(datascience_texto))
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

# Visualizando a nuvem de palavras
wordcloud(
  datascience_corpus,
  min.freq = 5,
  max.words = 100
)

paleta <- brewer.pal(8, "Dark2")
wordcloud(
  datascience_corpus,
  min.freq = 2,
  max.words = 100,
  random.order = T,
  colors = paleta
)

# Removendo algumas palavras

# Limpando o texto com a Document Term Matrix
datascience_document <- DocumentTermMatrix(datascience_corpus)
datascience_document <- as.matri