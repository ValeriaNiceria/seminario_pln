# Instalando os pacotes
# install.packages("tidyverse")
# install.packages("rtweet")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("syuzhet")

# Carregando os pacotes
library(tidyverse)
library(rtweet)
library(tm)
library(wordcloud)
library(syuzhet)

# Buscando tweets relacionados a empresas de smartphone
# apple, samsung, xiaomi, huawei
apple_tweets <- search_tweets(
  "#apple",
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

huawei_tweets <- search_tweets(
  "#huawei",
  n = 1000,
  include_rts = FALSE,
  lang = "en"
)


# Visualizando os dados
View(apple_tweets)
View(samsung_tweets)
View(xiaomi_tweets)
View(huawei_tweets)


# Separando o texto
apple_text <- apple_tweets$text
samsung_text <- samsung_tweets$text
xiaomi_text <- xiaomi_tweets$text
huawei_text <- huawei_tweets$text

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
apple_text <- limpar_texto(apple_text)
samsung_text <- limpar_texto(samsung_text)
xiaomi_text <- limpar_texto(xiaomi_text)
huawei_text <- limpar_texto(huawei_text)


# Convertendo os textos em corpus
apple_corpus <- VCorpus(VectorSource(apple_text))
samsung_corpus <- VCorpus(VectorSource(samsung_text))
xiaomi_corpus <- VCorpus(VectorSource(xiaomi_text))
huawei_corpus <- VCorpus(VectorSource(huawei_text))

# Removendo stopwords
apple_corpus <-  apple_corpus %>% tm_map(removeWords, stopwords("english"))
samsung_corpus <- samsung_corpus %>% tm_map(removeWords, stopwords("english"))
xiaomi_corpus <- xiaomi_corpus %>% tm_map(removeWords, stopwords("english"))
huawei_corpus <- huawei_corpus %>%  tm_map(removeWords, stopwords("english"))

# Visualizando os dados em uma nuvem de palavras

# Lista de cores em hexadecimal
paleta <- brewer.pal(8, "Dark2")

wordcloud(apple_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)
wordcloud(samsung_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)
wordcloud(xiaomi_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)
wordcloud(huawei_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)

# Transformando o corpus em matriz de termos
apple_doc <-  DocumentTermMatrix(apple_corpus)
samsung_doc <- DocumentTermMatrix(samsung_corpus)
xiaomi_doc <- DocumentTermMatrix(xiaomi_corpus)
huawei_doc <- DocumentTermMatrix(huawei_corpus)


# Iniciando a análise de sentimentos ----

# Obtendo os emoções
apple_sentiment <- get_nrc_sentiment(apple_doc$dimnames$Terms)
samsung_sentiment <- get_nrc_sentiment(samsung_doc$dimnames$Terms)
xiaomi_sentiment <- get_nrc_sentiment(xiaomi_doc$dimnames$Terms)
huawei_sentiment <- get_nrc_sentiment(huawei_doc$dimnames$Terms)


View(apple_sentiment)

# Calculando a frequência dos sentimentos
apple_sentiment_freq <- apple_sentiment %>% colSums() %>% sort(decreasing = T)
samsung_sentiment_freq <- samsung_sentiment %>% colSums() %>% sort(decreasing = T)
xiaomi_sentiment_freq <- xiaomi_sentiment %>% colSums() %>% sort(decreasing = T)
huawei_sentiment_freq <- huawei_sentiment %>% colSums() %>% sort(decreasing = T)

# Função responsável por traduzir os sentimentos e transformar em dataframe
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

# Função responsável por criar um gráfico da frequência dos sentimentos
gerar_grafico <- function(dados, titulo) {
  plot <- 
    ggplot(data = dados,
           aes(x = reorder(sentimentos, -freq), y = freq)) +
      geom_bar(aes(fill=sentimentos), stat = "identity") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjus = 1)) +
      xlab("Sentimentos") +
      ylab("Frequência") +
      ggtitle(titulo)
  
  return(plot)
}

# Transformando a frequência dos sentimentos em dataframe
df_apple_sentiment <- 
  gerar_dataframe_sentimentos(apple_sentiment_freq)

df_samsung_sentiment <- 
  gerar_dataframe_sentimentos(samsung_sentiment_freq)

df_xiaomi_sentiment <- 
  gerar_dataframe_sentimentos(xiaomi_sentiment_freq)

df_huawei_sentiment <- 
  gerar_dataframe_sentimentos(huawei_sentiment_freq)


# Visualizando os sentimentos
gerar_grafico(df_apple_sentiment, titulo = "Sentimentos das pessoas em relação a Apple")

gerar_grafico(df_samsung_sentiment, titulo = "Sentimentos das pessoas em relação a Samsung")

gerar_grafico(df_xiaomi_sentiment, titulo = "Sentimentos das pessoas em relação a Xiaomi")

gerar_grafico(df_huawei_sentiment, titulo = "Sentimentos das pessoas em relação a Huawei")
