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

# Buscando tweets relacionados a economia
economia_tweets_pt <- search_tweets(
  "#economia",
  n = 1500,
  include_rts = FALSE,
  lang = "pt"
)

# Visualizando o resultado da busca
View(economia_tweets_pt)


# Separando o texto
economia_text_pt <- economia_tweets_pt$text

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
economia_text_pt <- limpar_texto(economia_text_pt)


# Convertendo os textos em corpus
economia_corpus_pt <- VCorpus(VectorSource(economia_text_pt))

# Removendo stopwords 
economia_corpus_pt <-  economia_corpus_pt %>%
  tm_map(
    content_transformer(
      function(x) iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    )
  ) %>% 
  tm_map(removeWords, stopwords("portuguese"))

# Visualizando os dados em uma nuvem de palavras

# Lista de cores em hexadecimal
paleta <- brewer.pal(8, "Dark2")

wordcloud(economia_corpus_pt, min.freq = 15, max.words = 250, random.order = F, colors = paleta)

# Transformando o corpus em matriz de termos
economia_doc_pt <-  DocumentTermMatrix(economia_corpus_pt)

# Removendo os termos menos frequentes
economia_doc1_pt <- removeSparseTerms(economia_doc_pt, 0.97)

# Dendograma -> Visualizando os grupos
distancia_pt <- dist(t(economia_doc1_pt), method = "euclidian")
dendrograma_pt <- hclust(d = distancia_pt, method = "complete")
plot(dendrograma_pt, habg = -1, main = "Dendrograma Tweets Economia",
     xlab = "Distância",
     ylab = "Altura")


# Gerando uma matrix ordenada, com o termos mais frequentes
economia_freq1_pt <- 
  economia_doc1_pt %>% 
  as.matrix() %>% 
  colSums() %>% 
  sort(decreasing = T)

# Criando um dataframe com as palavras mais frequentes
df_economia_freq1_pt <- data.frame(
  word = names(economia_freq1_pt),
  freq = economia_freq1_pt
)

# Gerando uma nuvem de palavras, onde os termos esparsos foram removidos
library(wordcloud2)
wordcloud2(data = df_economia_freq1_pt)


# Iniciando a análise de sentimentos ----

# Obtendo os emoções
economia_sentimento_pt <- get_nrc_sentiment(economia_doc_pt$dimnames$Terms, language = "portuguese")


View(economia_sentimento_pt)

# Calculando a frequência dos sentimentos
economia_sentimento_freq_pt <- economia_sentimento_pt %>% colSums() %>% sort(decreasing = T)

# Função responsável por traduzir os sentimentos e transformar em dataframe
gerar_dataframe_sentimentos <- function(dados) {
  sentimetos_traducao <- 
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
  
  df_sentimento <- 
    data.frame(
      sentiment = names(dados),
      freq = dados
    ) %>% 
    left_join(sentimetos_traducao, by = "sentiment") %>% 
    dplyr::select(-sentiment) %>% 
    arrange(desc(freq))
  
  return(df_sentimento)
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
df_economia_sentimento_pt <- 
  gerar_dataframe_sentimentos(economia_sentimento_freq_pt)



# Visualizando os sentimentos
gerar_grafico(df_economia_sentimento_pt, titulo = "Sentimentos das pessoas em relação a Economia")



