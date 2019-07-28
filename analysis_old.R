# pacotes necessarios

library(subtools) # devtools::install_github("fkeck/subtools")
library(tm)
library(ptstem)
library(tidyverse)
theme_set(theme_bw())
library(gridExtra)
library(reshape2)



#######################
### bojack horseman ###
#######################

# leitura das legendas dos episodios

bojack <- read.subtitles.serie(dir = "subtitles/bojack_horseman/")

# limpeza do texto

bojack_corpus <- tmCorpus(bojack)

bojack_corpus <- tm_map(bojack_corpus, content_transformer(tolower))
bojack_corpus <- tm_map(bojack_corpus, removePunctuation)
bojack_corpus <- tm_map(bojack_corpus, removeNumbers)
bojack_corpus <- tm_map(bojack_corpus, removeWords, stopwords("portuguese"))
bojack_corpus <- tm_map(bojack_corpus, stripWhitespace)

bojack_corpus <- tm_map(bojack_corpus, text_tokens, stemmer = stem_list)


#bojack_corpus <- tm_map(bojack_corpus, ptstem)
bojack_corpus <- TermDocumentMatrix(bojack_corpus)

bojack_corpus_matrix <- as.matrix(bojack_corpus)

temporadas <- rep(1:5, each = 12)
bojack_corpus <- t(apply(bojack_corpus_matrix, 1, function(x) tapply(x, temporadas, sum)))
colnames(bojack_corpus) <- paste("S0", 1:5, sep = "")




# wordcloud(rownames(bojack_corpus), bojack_corpus[, 1], title.size = 1.2, max.words = 50, random.order = TRUE)

#

bojack_corpus_melt <- melt(bojack_corpus)
names(bojack_corpus_melt) <- c("palavra", "temporada", "ocorrencias")

bojack_corpus_melt

#bojack_corpus_melt %>%
#  group_by(temporada) %>%
#  top_n(n = 10, wt = ocorrencias) %>%
#  arrange(desc(ocorrencias)) %>%
#  ggplot(., aes(x = palavra, y = ocorrencias, fill = temporada)) +
#  geom_col(show.legend = FALSE) +
#  facet_wrap(~ temporada, scales = "free") +
#  labs(x = "Número de Ocorrências", y = "Palavras") +
#  coord_flip() +
#  scale_fill_viridis_d()

plot.seriado <- function(dados, season, cor = 0){
  
  grafico <- dados %>%
    filter(temporada == season) %>%
    top_n(n = 10, wt = ocorrencias) %>%
    arrange(desc(ocorrencias)) %>%
    ggplot(., aes(x = reorder(palavra, ocorrencias), y = ocorrencias, fill = temporada)) +
    geom_col(show.legend = FALSE) +
    labs(x = "", y = "", title = season) +
    coord_flip() +
    scale_fill_viridis_d(begin = cor)
  
  return(grafico)  
  
}

s01 <- plot.seriado(bojack_corpus_melt, "S01", 0.00)
s02 <- plot.seriado(bojack_corpus_melt, "S02", 0.25)
s03 <- plot.seriado(bojack_corpus_melt, "S03", 0.50)
s04 <- plot.seriado(bojack_corpus_melt, "S04", 0.75)
s05 <- plot.seriado(bojack_corpus_melt, "S05", 1.00)

grid.arrange(s01, s02, s03, s04, s05, left = "Palavras", bottom = "Número de Ocorrências")

# analise de sentimento

bojack_corpus_matrix

#pos <- as.vector(read.csv(file = "sentiment/positive_words_pt.txt")[, 1])
#neg <- as.vector(read.csv(file = "sentiment/negative_words_pt.txt")[, 1])

sentimentos <- read.table(file = "sentiment/sentiword.txt", sep = "\t", header = TRUE)

sentimentos <- sentimentos %>%
  group_by(Termo) %>%
  summarise(positivo = max(PosScore), negativo = max(NegScore)) %>%
  mutate(Termo = trimws(Termo, which = "left"))

pos <- sentimentos[, c(1, 2)]
neg <- sentimentos[, c(1, 3)]

# sentimento para cada episodio

bojack_sentimento_positivo <- 0
bojack_sentimento_negativo <- 0

for (j in 1:dim(bojack_corpus_matrix)[2]){
  
  # palavras do j-esimo episodio
  
  a <- bojack_corpus_matrix[, j]
  a <- data.frame(Termo = names(a),
                  Ocorrencias = a)

  row.names(a) <- NULL
  
  # juntando sentimentos com as palavras do episodio - caso positivo
  
  x <- left_join(a, pos, by = "Termo") %>%
    na.omit()

  sentimento_positivo <- sum(x$Ocorrencias*x$positivo)/sum(a$Ocorrencias)
  
  # juntando sentimentos com as palavras do episodio - caso negativo

  x <- left_join(a, neg, by = "Termo") %>%
    na.omit()

  sentimento_negativo <- sum(x$Ocorrencias*x$negativo)/sum(a$Ocorrencias)
  
  bojack_sentimento_positivo[j] <- sentimento_positivo
  bojack_sentimento_negativo[j] <- sentimento_negativo
}

bojack_plot <- data.frame(episodio = 1:length(bojack_sentimento_positivo),
                          temporada = factor(temporadas),
                          positivo = bojack_sentimento_positivo,
                          negativo = bojack_sentimento_negativo)

ggplot(bojack_plot, aes(x = episodio, colour = temporada)) +
  geom_line(aes(y = positivo)) +
  geom_line(aes(y = -negativo)) +
  scale_x_continuous(breaks = seq(12, 60, 12)) +
  labs(x = "Episódio", y = "Sentimento", colour = "Temporada", title = "Bojack Horseman") +
  scale_colour_viridis_d()
  


###################
### brooklyn 99 ###
###################

# leitura das legendas dos episodios

brooklyn99 <- read.subtitles.serie(dir = "subtitles/brooklyn99/")

# limpeza do texto

brooklyn99_corpus <- tmCorpus(brooklyn99)

brooklyn99_corpus <- tm_map(brooklyn99_corpus, content_transformer(tolower))
brooklyn99_corpus <- tm_map(brooklyn99_corpus, removePunctuation)
brooklyn99_corpus <- tm_map(brooklyn99_corpus, removeNumbers)
brooklyn99_corpus <- tm_map(brooklyn99_corpus, removeWords, stopwords("portuguese"))
brooklyn99_corpus <- tm_map(brooklyn99_corpus, stripWhitespace)
brooklyn99_corpus <- tm_map(brooklyn99_corpus, ptstem)
brooklyn99_corpus <- TermDocumentMatrix(brooklyn99_corpus)

brooklyn99_corpus_matrix <- as.matrix(brooklyn99_corpus)

temporadas <- c(rep(1, 22), 
                rep(2, 23), 
                rep(3, 23), 
                rep(4, 22), 
                rep(5, 22))
brooklyn99_corpus <- t(apply(brooklyn99_corpus_matrix, 1, function(x) tapply(x, temporadas, sum)))
colnames(brooklyn99_corpus) <- paste("S0", 1:5, sep = "")


brooklyn99_corpus_melt <- melt(brooklyn99_corpus)
names(brooklyn99_corpus_melt) <- c("palavra", "temporada", "ocorrencias")



plot.seriado <- function(dados, season, cor = 0){
  
  grafico <- dados %>%
    filter(temporada == season) %>%
    top_n(n = 10, wt = ocorrencias) %>%
    arrange(desc(ocorrencias)) %>%
    ggplot(., aes(x = reorder(palavra, ocorrencias), y = ocorrencias, fill = temporada)) +
    geom_col(show.legend = FALSE) +
    labs(x = "", y = "", title = season) +
    coord_flip() +
    scale_fill_viridis_d(begin = cor)
  
  return(grafico)  
  
}

s01 <- plot.seriado(brooklyn99_corpus_melt, "S01", 0.00)
s02 <- plot.seriado(brooklyn99_corpus_melt, "S02", 0.25)
s03 <- plot.seriado(brooklyn99_corpus_melt, "S03", 0.50)
s04 <- plot.seriado(brooklyn99_corpus_melt, "S04", 0.75)
s05 <- plot.seriado(brooklyn99_corpus_melt, "S05", 1.00)

grid.arrange(s01, s02, s03, s04, s05, left = "Palavras", bottom = "Número de Ocorrências")

# analise de sentimento

brooklyn99_corpus_matrix

sentimentos <- read.table(file = "sentiment/sentiword.txt", sep = "\t", header = TRUE)

sentimentos <- sentimentos %>%
  group_by(Termo) %>%
  summarise(positivo = max(PosScore), negativo = max(NegScore)) %>%
  mutate(Termo = trimws(Termo, which = "left"))

pos <- sentimentos[, c(1, 2)]
neg <- sentimentos[, c(1, 3)]

# sentimento para cada episodio

brooklyn99_sentimento_positivo <- 0
brooklyn99_sentimento_negativo <- 0

for (j in 1:dim(brooklyn99_corpus_matrix)[2]){
  
  # palavras do j-esimo episodio
  
  a <- brooklyn99_corpus_matrix[, j]
  a <- data.frame(Termo = names(a),
                  Ocorrencias = a)
  
  row.names(a) <- NULL
  
  # juntando sentimentos com as palavras do episodio - caso positivo
  
  x <- left_join(a, pos, by = "Termo") %>%
    na.omit()
  
  sentimento_positivo <- sum(x$Ocorrencias*x$positivo)/sum(a$Ocorrencias)
  
  # juntando sentimentos com as palavras do episodio - caso negativo
  
  x <- left_join(a, neg, by = "Termo") %>%
    na.omit()
  
  sentimento_negativo <- sum(x$Ocorrencias*x$negativo)/sum(a$Ocorrencias)
  
  brooklyn99_sentimento_positivo[j] <- sentimento_positivo
  brooklyn99_sentimento_negativo[j] <- sentimento_negativo
}

brooklyn99_plot <- data.frame(episodio = 1:length(brooklyn99_sentimento_positivo),
                              temporada = factor(temporadas),
                              positivo = brooklyn99_sentimento_positivo,
                              negativo = brooklyn99_sentimento_negativo)

ggplot(brooklyn99_plot, aes(x = episodio, colour = temporada)) +
  geom_line(aes(y = positivo)) +
  geom_line(aes(y = -negativo)) +
  scale_x_continuous(breaks = c(22, 45, 68, 90, 112)) +
  labs(x = "Episódio", y = "Sentimento", colour = "Temporada", title = "Brooklyn 99") +
  scale_colour_viridis_d()





udmodel <- udpipe_load_model(file = "portuguese-bosque-ud-2.4-190531.udpipe")

udpipe_annotate(udmodel, "t")


