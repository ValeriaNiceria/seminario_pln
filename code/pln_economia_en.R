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
economia_tweets <- search_tweets(
  "#economy",
  n = 2000,
  include_rts = FALSE,
  lang = "en"
)

# Visualizando o resultado da busca
View(economia_tweets)


# Gerando um gráfico com a frequência dos tweets no intervalo de 1 hora
economia_tweets %>% 
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequência de #economy no Twitter",
    subtitle = "Tweets no intervalo de 1 hora",
    caption = "\nSource: Dados coletados do Twitter's REST API via rtweet"
  )


# Separando o texto
economia_text <- economia_tweets$text

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
economia_text <- limpar_texto(economia_text)


# Convertendo os textos em corpus
economia_corpus <- VCorpus(VectorSource(economia_text))

# Removendo stopwords
economia_corpus <-  economia_corpus %>% tm_map(removeWords, stopwords("english"))

# Visualizando os dados em uma nuvem de palavras

# Lista de cores em hexadecimal
paleta <- brewer.pal(8, "Dark2")

wordcloud(economia_corpus, min.freq = 15, max.words = 250, random.order = F, colors = paleta)

# Transformando o corpus em matriz de documentos-termos
economia_doc <-  DocumentTermMatrix(economia_corpus)

# Removendo os termos menos frequentes
economia_doc1 <- removeSparseTerms(economia_doc, 0.97)


# Gerando uma matrix ordenada, com o termos mais frequentes
economia_freq <- 
  economia_doc1 %>% 
  as.matrix() %>% 
  colSums() %>% 
  sort(decreasing = T)


# Criando um dataframe com as palavras mais frequentes
df_economia_freq <- data.frame(
  word = names(economia_freq),
  freq = economia_freq
)


# Gerando um gráfico da frequência
df_economia_freq %>%
  filter(!word %in% c("economy")) %>% 
  subset(freq > 50) %>%
  ggplot(aes(x = reorder(word, freq),
             y = freq)) +
  geom_bar(stat = "identity", fill='#0c6cad', color="#075284") +
  theme(axis.text.x = element_text(angle = 45, hjus = 1)) +
  ggtitle("Termos relacionados a Economia mais frequentes no Twitter") +
  labs(y = "Frequência", x = "Termos") +
  coord_flip()

# Dendograma -> Visualizando os grupos
distancia <- dist(t(economia_doc1), method = "euclidian")
dendograma <- hclust(d = distancia, method = "complete")
plot(dendograma, habg = -1, main = "Dendograma Tweets Economia",
     xlab = "Distância",
     ylab = "Altura")



# Iniciando a análise de sentimentos ----

# Obtendo os emoções
economia_sentimento <- get_nrc_sentiment(economia_doc$dimnames$Terms, language = "english")


View(economia_sentimento)

# Calculando a frequência dos sentimentos
economia_sentimento_freq <- economia_sentimento %>% colSums() %>% sort(decreasing = T)

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
df_economia_sentimento <- 
  gerar_dataframe_sentimentos(economia_sentimento_freq)



# Visualizando os sentimentos
gerar_grafico(df_economia_sentimento, titulo = "Sentimentos das pessoas em relação a Economia")
