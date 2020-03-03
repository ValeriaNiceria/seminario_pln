# Instalando os pacotes
# install.packages("tidyverse")
# install.packages("rtweet")
# install.packages("udpipe", dependencies = T)

# Carregando os pacotes
library(tidyverse)
library(rtweet)
library(udpipe)

# Buscando os tweets com a #datascience em português
datascience_tweets <- search_tweets(
  "#datascience",
  n = 100,
  include_rts = FALSE,
  lang = "pt-br"
)

View(datascience_tweets)

arquivo <- data.frame(FILE = datascience_tweets$text)
arquivo$ID = 1:length(arquivo$FILE)

View(arquivo)

# Download do modelo
modelo = udpipe::udpipe_download_model(language = "portuguese")

# Carregando o modelo baixado
modelo = udpipe::udpipe_load_model("portuguese-bosque-ud-2.4-190531.udpipe")

# Anotações no formato CoNLL-U
anotar <- 
  udpipe::udpipe_annotate(
    modelo,
    x = arquivo$FILE,
    doc_id = arquivo$ID
  ) %>% as.data.frame()

View(anotar)
