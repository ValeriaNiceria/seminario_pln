# Instalando os pacotes
# install.packages("tidyverse")
# install.packages("rtweet")
# sudo apt-get install libgsl0-dev
# install.packages("topicmodels")

# Carregando os pacotes
library(tidyverse)
library(rtweet)
library(NLP)
library(syuzhet)
library(stringi)
library(tm)
library(SnowballC)
library(topicmodels)
library(syuzhet)
library(wordcloud2)

# Buscando tweets relacionados a empresas de smartphone
# iphone, samsung, xiaomi, huawei
iphone_tweets <- search_tweets(
  "#iphone",
  n = 1000,
  include_rts = FALSE,
  lang = "en"
)

samsung_tweets <- search_tweets(
  "#samsung",
  n = 1000,
  include_rts = FALSE,
  lang = "en"
)

xiaomi_tweets <- search_tweets(
  "#xiaomi",
  n = 1000,
  include_rts = FALSE,
  lang = "en"
)

smartphone_tweets <- search_tweets(
  "#smartphone",
  n = 1000,
  include_rts = FALSE,
  lang = "en"
)

tec_tweets <- search_tweets(
  "#technology",
  n = 1000,
  include_rts = FALSE,
  lang = "en"
)


# Visualizando os dados
View(iphone_tweets)
View(samsung_tweets)
View(xiaomi_tweets)
View(smartphone_tweets)
View(tec_tweets)


# Separando o texto
iphone_text <- iphone_tweets$text
samsung_text <- samsung_tweets$text
xiaomi_text <- xiaomi_tweets$text
smartphone_text <- smartphone_tweets$text
tec_text <- tec_tweets$text

# Função para limpeza dos textos
limpar_texto <- function(texto) {
  # Convertendo o texto para minúsculo
  texto <- tolower(texto)
  # Removendo o usuário adicionado no comentário
  texto <- gsub("@\\w+", "", texto)
  # Removendo as pontuações
  texto <- gsub("[[:punct:]]", "", texto)
  # Removendo links
  texto <- gsub("http\\w+", "", texto)
  # Removendo tabs 
  texto <- gsub("[ |\t]{2,}", "", texto)
  # Removendo espaços no início do texto
  texto <- gsub("^ ", "", texto)
  # Removendo espaços no final do texto
  texto <- gsub(" $", "", texto)
  return(texto)
}


# Limpando os textos
iphone_text <- limpar_texto(iphone_text)
samsung_text <- limpar_texto(samsung_text)
xiaomi_text <- limpar_texto(xiaomi_text)
smartphone_text <- tolower(smartphone_text)
tec_text <- limpar_texto(tec_text)


# Convertendo os textos em corpus
iphone_corpus <- VCorpus(VectorSource(iphone_text))
samsung_corpus <- VCorpus(VectorSource(samsung_text))
xiaomi_corpus <- VCorpus(VectorSource(xiaomi_text))
smartphone_corpus <- VCorpus(VectorSource(smartphone_text))
tec_corpus <- VCorpus(VectorSource(tec_text))

# Removendo stopwords
iphone_corpus <-  iphone_corpus %>% tm_map(removeWords, stopwords("english"))
samsung_corpus <- samsung_corpus %>% tm_map(removeWords, stopwords("english"))
xiaomi_corpus <- xiaomi_corpus %>% tm_map(removeWords, stopwords("english"))
smartphone_corpus <- smartphone_corpus %>%  tm_map(removeWords, stopwords("english"))
tec_corpus <- tec_corpus %>%  tm_map(removeWords, stopwords("english"))


# Visualizando os dados em uma nuvem de palavras

# Lista de cores em hexadecimal
paleta <- brewer.pal(8, "Dark2")

wordcloud(iphone_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)
wordcloud(samsung_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)
wordcloud(xiaomi_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)
wordcloud(smartphone_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)
wordcloud(tec_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)


# Transformando o corpus em matriz de termos
iphone_doc <-  DocumentTermMatrix(iphone_corpus)
samsung_doc <- DocumentTermMatrix(samsung_corpus)
xiaomi_doc <- DocumentTermMatrix(xiaomi_corpus)
smartphone_doc <- DocumentTermMatrix(smartphone_corpus)
tec_doc <- DocumentTermMatrix(tec_corpus)


# Iniciando a análise de sentimentos ----

# Obtendo os emoções
iphone_sentiment <- get_nrc_sentiment(iphone_doc$dimnames$Terms)
samsung_sentiment <- get_nrc_sentiment(samsung_doc$dimnames$Terms)
xiaomi_sentiment <- get_nrc_sentiment(xiaomi_doc$dimnames$Terms)
smartphone_sentiment <- get_nrc_sentiment(smartphone_doc$dimnames$Terms)
tec_sentiment <- get_nrc_sentiment(tec_doc$dimnames$Terms)


# Calculando a frequência dos sentimentos
iphone_sentiment_freq <- iphone_sentiment %>% colSums() %>% sort(decreasing = T)
samsung_sentiment_freq <- samsung_sentiment %>% colSums() %>% sort(decreasing = T)
xiaomi_sentiment_freq <- xiaomi_sentiment %>% colSums() %>% sort(decreasing = T)
smartphone_sentiment_freq <- smartphone_sentiment %>% colSums() %>% sort(decreasing = T)
tec_sentiment_freq <- tec_sentiment %>% colSums() %>% sort(decreasing = T)


gerar_dataframe_sentimentos <- function(dados) {
  sentimetos_translate <- 
    data.frame(
      sentiment = c(
        "positive",
        "negative",
        "trust",
        "anticipation",
        "fear",
        "joy",
        "sadness",
        "surprise",
        "anger",
        "disgust"
        ),
      sentimentos = c(
        "Positivo",
        "Negativo",
        "Confiança",
        "Antecipação",
        "Medo",
        "Alegria",
        "Tristeza",
        "Surpresa",
        "Raiva",
        "Nojo"
        )
    )
  
  df_sentiment <- 
    data.frame(
      sentiment = names(dados),
      freq = dados
    ) %>% 
    left_join(sentimetos_translate, by = "sentiment") %>% 
    dplyr::select(-sentiment) %>% 
    arrange(desc(freq))
  
  return(df_sentiment)
}

gerar_grafico <- function(dados, titulo) {
  plot <- 
    ggplot(data = dados,
           aes(x = sentimentos, y = freq)) +
      geom_bar(aes(fill=sentimentos), stat = "identity") +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjus = 1)) +
      xlab("Sentimentos") +
      ylab("Frequência") +
      ggtitle(titulo)
  
  return(plot)
}

# Transformando a frequência dos sentimentos em dataframe
df_iphone_sentiment <- 
  gerar_dataframe_sentimentos(iphone_sentiment_freq)

df_samsung_sentiment <- 
  gerar_dataframe_sentimentos(samsung_sentiment_freq)

df_xiaomi_sentiment <- 
  gerar_dataframe_sentimentos(xiaomi_sentiment_freq)

df_smartphone_sentiment <- 
  gerar_dataframe_sentimentos(smartphone_sentiment_freq)

df_tec_sentiment <- 
  gerar_dataframe_sentimentos(tec_sentiment_freq)



# Visualizando os sentimentos
gerar_grafico(df_iphone_sentiment, titulo = "Sentimentos das pessoas em relação ao Iphone")
