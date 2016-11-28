library(xml2)
library(rvest)
library(dplyr)
library(syuzhet)
library(readr)
library(stringr)
library(ggplot2)
library(viridis)
library(reshape2)

# Pintar sentimiento de cada episodio

leer_episodio <- function(path){
  raw <- read_html(path)
  raw %>%
    html_node('body') %>%
    html_text() -> raw_text
  lineas <- read_lines(raw_text)
  return(lineas)
}

limpiar_texto <- function(lineas){
  # Quitar las notas adicionales de créditos, anuncios, etc
  texto_limpio <- gsub("^Commercial Break.*", "", lineas, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^End.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^Closing Credits.*", "", texto_limpio, ignore.case = TRUE, perl = TRUE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^Opening Credits.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}The One.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^The Last One.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}(Originally )?written by.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Story.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Teleplay.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Directed by.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Produced by.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Photographer.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}(Episodes )?(Ori?ginally )?tran?scribed by.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}(Episodes )?(Ori?ginally )?tran?script by.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Note.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Additional transcribing by.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}With Mino(r|t) Adjustments by.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Aired:.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}Minor additions.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}With Help From.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub("^( ){0,4}HTMLed.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
  texto_limpio <- gsub(".*Previously on Friends.*", "", texto_limpio, ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar cuando dicen "Previuously on Friends"
  texto_limpio <- gsub("\\(.{,100}?\\)", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar paréntesis, non-greedy
  texto_limpio <- gsub("\\(.*", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar paréntesis inicial y todo lo que le sigue hasta el final
  texto_limpio <- gsub(".*\\)", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar paréntesis final y todo lo que le precede desde el principio
  texto_limpio <- gsub("\\{.{,100}?\\}", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar llaves, non-greedy
  texto_limpio <- gsub("\\{.*", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar llave inicial y todo lo que le sigue hasta el final
  texto_limpio <- gsub(".*\\}", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar llave final y todo lo que le precede desde el principio
  texto_limpio <- gsub("\\[.{,100}?\\]", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar corchetes, non-greedy
  texto_limpio <- gsub("\\[.*", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar corchete inicial y todo lo que le sigue hasta el final
  texto_limpio <- gsub(".*\\]", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Quitar corchete final y todo lo que le precede desde el principio
  texto_limpio <- gsub("\\.*", "", texto_limpio, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # Eliminar líneas con un punto
  texto_limpio <- texto_limpio[texto_limpio != ' '] # Eliminar líneas con un espacio
  texto_limpio <- texto_limpio[texto_limpio != '&nbsp;'] # Eliminar líneas con un espacio fino
  texto_limpio <- texto_limpio[texto_limpio != ''] # Eliminar líneas vacías
  return(texto_limpio)
}

pintar_sentimiento <- function(df_nrc, episodiolimpio, nombre_episodio) {
  df_nrc <- cbind(linenumber = seq_along(episodiolimpio), get_nrc_sentiment(episodiolimpio))
  episodioplot <- df_nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
  episodio_rawsentiment <- ggplot(data = episodioplot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
    theme_minimal() +
    scale_x_discrete(expand=c(0.02,0)) +
    theme(axis.text.y=element_text(margin=margin(r=-10))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank())
  episodioft <- as.numeric(get_transformed_values(episodioplot$sentiment, 
                                                low_pass_size = 3,
                                                x_reverse_len = 200,
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
  episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
  episodio_sentiment <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
    geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
    theme_minimal() +
    ylab("Transformed Sentiment Value") +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank())
  return(episodio_sentiment)
}

analizar_ep <- function(path) {
  episodio <- leer_episodio(path)
  episodiolimpio <- limpiar_texto(episodio)
  episodionrc <- cbind(linenumber = seq_along(episodiolimpio), get_nrc_sentiment(episodiolimpio))
  episodio_sentiment <- pintar_sentimiento(episodionrc, episodiolimpio, episodio)
  return(episodio_sentiment)
}

# Pintar sentimiento de cada temporada

obtener_texto <- function(path){
  texto <- leer_episodio(path)
  texto <- limpiar_texto(texto)
  return(texto)
}

pintar_sentimiento_temp <- function(df_nrc, episodiolimpio, nombre_episodio) {
  episodioplot <- df_nrc %>% select(linenumber, negative, positive) %>% mutate(negative = -negative)
  episodio_rawsentiment <- ggplot(data = episodioplot, aes(x = linenumber, y = negative + positive)) + geom_bar(stat = "identity")
  df_nrc <- cbind(linenumber = seq_along(episodiolimpio), get_nrc_sentiment(episodiolimpio))
  episodioplot <- df_nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
  episodio_rawsentiment <- ggplot(data = episodioplot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
    theme_minimal() +
    scale_x_discrete(expand=c(0.02,0)) +
    theme(axis.text.y=element_text(margin=margin(r=-10))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank())
  episodioft <- as.numeric(get_transformed_values(episodioplot$sentiment, 
                                                  low_pass_size = 3,
                                                  x_reverse_len = 240,
                                                  scale_vals = TRUE,
                                                  scale_range = FALSE))
  episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
  episodio_sentiment <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
    geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
    theme_minimal() +
    ylab("Transformed Sentiment Value") +
    ggtitle(expression(paste("Sentiment in ", italic(nombre_episodio)))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank()) +
    geom_text(data = annotatetext, aes(x,y,label=label), hjust = 0.5, 
              size = 3, inherit.aes = FALSE) +
    geom_segment(data = annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
                 arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)
  return(episodio_sentiment)
}

# Pintar una emoción concreta en una temporada (default = joy) (sin uso)

pintar_joy_temp <- function(df_nrc, episodiolimpio, nombre_episodio) {
  episodioplot <- df_nrc %>% select(linenumber, joy)
  episodio_rawsentiment <- ggplot(data = episodioplot, aes(x = linenumber, y = joy)) + geom_bar(stat = "identity")
  df_nrc <- cbind(linenumber = seq_along(episodiolimpio), get_nrc_sentiment(episodiolimpio))
  episodioplot <- df_nrc %>% select(linenumber, joy)
  episodioft <- data.frame(cbind(linenumber = seq_along(episodioplot)))
  episodio_sentiment <- ggplot(data = episodioft, aes(x = linenumber), y = joy) +
    geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
    theme_minimal() +
    ylab("Joy") +
    ggtitle(expression(paste("Joy in ", italic(nombre_episodio)))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank()) +
    geom_text(data = annotatetext, aes(x,y,label=label), hjust = 0.5, 
              size = 3, inherit.aes = FALSE) +
    geom_segment(data = annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
                 arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)
  return(episodio_sentiment)
}

# Primera temporada

## Sentimiento de cada episodio

s01e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0101.html")
s01e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0102.html")
s01e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0103.html")
s01e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0104.html")
s01e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0105.html")
s01e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0106.html")
s01e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0107.html")
s01e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0108.html")
s01e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0109.html")
s01e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0110.html")
s01e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0111.html")
s01e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0112.html")
s01e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0113.html")
s01e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0114.html")
s01e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0115.html")
s01e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0116.html")
s01e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0117.html")
s01e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0118.html")
s01e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0119.html")
s01e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0120.html")
s01e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0121.html")
s01e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0122.html")
s01e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0123.html")
s01e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0124.html")

## Sentimiento de la temporada 1

s01e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0101.html')
s01e01nrc <- cbind(linenumber = seq_along(s01e01texto), get_nrc_sentiment(s01e01texto))
s01e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0102.html')
s01e02nrc <- cbind(linenumber = seq_along(s01e02texto), get_nrc_sentiment(s01e02texto))
s01e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0103.html')
s01e03nrc <- cbind(linenumber = seq_along(s01e03texto), get_nrc_sentiment(s01e03texto))
s01e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0104.html')
s01e04nrc <- cbind(linenumber = seq_along(s01e04texto), get_nrc_sentiment(s01e04texto))
s01e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0105.html')
s01e05nrc <- cbind(linenumber = seq_along(s01e05texto), get_nrc_sentiment(s01e05texto))
s01e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0106.html')
s01e06nrc <- cbind(linenumber = seq_along(s01e06texto), get_nrc_sentiment(s01e06texto))
s01e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0107.html')
s01e07nrc <- cbind(linenumber = seq_along(s01e07texto), get_nrc_sentiment(s01e07texto))
s01e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0108.html')
s01e08nrc <- cbind(linenumber = seq_along(s01e08texto), get_nrc_sentiment(s01e08texto))
s01e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0109.html')
s01e09nrc <- cbind(linenumber = seq_along(s01e09texto), get_nrc_sentiment(s01e09texto))
s01e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0110.html')
s01e10nrc <- cbind(linenumber = seq_along(s01e10texto), get_nrc_sentiment(s01e10texto))
s01e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0111.html')
s01e11nrc <- cbind(linenumber = seq_along(s01e11texto), get_nrc_sentiment(s01e11texto))
s01e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0112.html')
s01e12nrc <- cbind(linenumber = seq_along(s01e12texto), get_nrc_sentiment(s01e12texto))
s01e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0113.html')
s01e13nrc <- cbind(linenumber = seq_along(s01e13texto), get_nrc_sentiment(s01e13texto))
s01e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0114.html')
s01e14nrc <- cbind(linenumber = seq_along(s01e14texto), get_nrc_sentiment(s01e14texto))
s01e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0115.html')
s01e15nrc <- cbind(linenumber = seq_along(s01e15texto), get_nrc_sentiment(s01e15texto))
s01e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0116.html')
s01e16nrc <- cbind(linenumber = seq_along(s01e16texto), get_nrc_sentiment(s01e16texto))
s01e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0117.html')
s01e17nrc <- cbind(linenumber = seq_along(s01e17texto), get_nrc_sentiment(s01e17texto))
s01e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0118.html')
s01e18nrc <- cbind(linenumber = seq_along(s01e18texto), get_nrc_sentiment(s01e18texto))
s01e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0119.html')
s01e19nrc <- cbind(linenumber = seq_along(s01e19texto), get_nrc_sentiment(s01e19texto))
s01e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0120.html')
s01e20nrc <- cbind(linenumber = seq_along(s01e20texto), get_nrc_sentiment(s01e20texto))
s01e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0121.html')
s01e21nrc <- cbind(linenumber = seq_along(s01e21texto), get_nrc_sentiment(s01e21texto))
s01e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0122.html')
s01e22nrc <- cbind(linenumber = seq_along(s01e22texto), get_nrc_sentiment(s01e22texto))
s01e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0123.html')
s01e23nrc <- cbind(linenumber = seq_along(s01e23texto), get_nrc_sentiment(s01e23texto))
s01e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0124.html')
s01e24nrc <- cbind(linenumber = seq_along(s01e24texto), get_nrc_sentiment(s01e24texto))

s01texto <- c("s01e01", s01e01texto, "s01e02", s01e02texto, "s01e03", s01e03texto, "s01e04", s01e04texto, "s01e05", s01e05texto, "s01e06", s01e06texto, "s01e07", s01e07texto, "s01e08", s01e08texto, "s01e09", s01e09texto, "s01e10", s01e10texto, "s01e11", s01e11texto, "s01e12", s01e12texto, "s01e13", s01e13texto, "s01e14", s01e14texto, "s01e15", s01e15texto, "s01e16", s01e16texto, "s01e17", s01e17texto, "s01e18", s01e18texto, "s01e19", s01e19texto, "s01e20", s01e20texto, "s01e21", s01e21texto, "s01e22", s01e22texto, "s01e23", s01e23texto, "s01e24", s01e24texto)
s01nrc <- cbind(linenumber = seq_along(s01texto), get_nrc_sentiment(s01texto))
s01annotatetext <- data.frame(x = c(14, 50, 180, 234), y = c(2.4, -1.4, -2.2, 2.4), 
                           label = c("Ross sort of\nasks Rachel out", "Rachel kisses Ross\nat the laundry place", "Girls win at poker", "Rachel finds out\nabout Ross"))
s01annotatearrow <- data.frame(x = c(14, 50, 180, 234), 
                            y1 = c(2, -1, -2, 2), y2 = c(1.2, -0.2, -1.2, 1.2))
s01plot <- s01nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s01_rawsentiment <- ggplot(data = s01plot, aes(x = linenumber, y = sentiment)) +
  geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s01ft <- as.numeric(get_transformed_values(s01plot$sentiment, 
                                                  low_pass_size = 3,
                                                  x_reverse_len = 240,
                                                  scale_vals = TRUE,
                                                  scale_range = FALSE))
s01ft <- data.frame(cbind(linenumber = seq_along(s01ft), ft = s01ft))
s01_sentiment <- ggplot(data = s01ft, aes(x = linenumber, y = ft)) +
    geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
    theme_minimal() +
    ylab("Sentiment") +
    ggtitle(expression(paste("Sentiment in ", italic("Season 1")))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank()) +
    geom_text(data = s01annotatetext, aes(x,y,label=label), hjust = 0.5, 
              size = 4, inherit.aes = FALSE) +
    geom_segment(data = s01annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
              arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s01e13annotatetext <- data.frame(x = c(4, 24, 58, 92, 122, 146, 162, 177, 194, 212, 238), y = c(1.4, 2.2, -1.2, 1.4, -1.8, 2.8, -1.2, 1.6, -2.2, 1.4, -1.8), 
                              label = c("Chandler sees\nRachel naked", 
                                        "Roger psycho-analyzes Chandler", 
                                        "Joey finds out\nhis father has a lover", 
                                        "Roger psycho-analyzes\nthe gang, they hate him", 
                                        "Joey meets his\nfather's lover", 
                                        "Joey and\nChandler talk", 
                                        "Rachel sees\nJoey naked", 
                                        "The gang tells Phoebe\nthey hate Roger", 
                                        "Joey talks with his mother", 
                                        "Roger laughs\nat the gang\nto Phoebe", 
                                        "Joey sees\nMonica naked,\nMonica sees Joey's\nfather naked"))
s01e13annotatearrow <- data.frame(x = c(4, 24, 58, 92, 122, 146, 162, 177, 194, 212, 238), 
                               y1 = c(1, 2, -1, 1, -1.6, 2.4, -1, 1.2, -2, 1, -1.4), y2 = c(0.2, 1.2, -0.2, 0.2, -0.8, 1.6, -0.2, 0.4, -1.2, 0.2, -0.6))
s01e13plot <- s01e13nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s01e13_rawsentiment <- ggplot(data = s01e13plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s01e13ft <- as.numeric(get_transformed_values(s01e13plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s01e13ft <- data.frame(cbind(linenumber = seq_along(s01e13ft), ft = s01e13ft))
s01e13_sentiment <- ggplot(data = s01e13ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 1 Episode 13")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s01e13annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 4, inherit.aes = FALSE) +
  geom_segment(data = s01e13annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s01_joy <- pintar_joy_temp(s01nrc, s01texto, "Season 1")

# para sacar dónde van las flechas
write_lines(x = s01texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/01.txt', append = FALSE)

episodioplot <- s01nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 1")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s01annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s01annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s01textocorto <- character()
for (i in seq_along(s01texto)) {
  if (i%%68 == 1) s01textocorto[ceiling(i/68)] <- str_c(s01texto[i], 
                                                      s01texto[i+1],
                                                      s01texto[i+2],
                                                      s01texto[i+3],
                                                      s01texto[i+4],
                                                      s01texto[i+5],
                                                      s01texto[i+6],
                                                      s01texto[i+7],
                                                      s01texto[i+8],
                                                      s01texto[i+9],
                                                      s01texto[i+10],
                                                      s01texto[i+11],
                                                      s01texto[i+12],
                                                      s01texto[i+13],
                                                      s01texto[i+14],
                                                      s01texto[i+15],
                                                      s01texto[i+16],
                                                      s01texto[i+17],
                                                      s01texto[i+18],
                                                      s01texto[i+19],
                                                      s01texto[i+20],
                                                      s01texto[i+21],
                                                      s01texto[i+22],
                                                      s01texto[i+23],
                                                      s01texto[i+24],
                                                      s01texto[i+25],
                                                      s01texto[i+26],
                                                      s01texto[i+27],
                                                      s01texto[i+28],
                                                      s01texto[i+29],
                                                      s01texto[i+30],
                                                      s01texto[i+31],
                                                      s01texto[i+32],
                                                      s01texto[i+33],
                                                      s01texto[i+34],
                                                      s01texto[i+35],
                                                      s01texto[i+36],
                                                      s01texto[i+37],
                                                      s01texto[i+38],
                                                      s01texto[i+39],                                                      
                                                      s01texto[i+40],
                                                      s01texto[i+41],
                                                      s01texto[i+42],
                                                      s01texto[i+43],
                                                      s01texto[i+44],
                                                      s01texto[i+45],
                                                      s01texto[i+46],
                                                      s01texto[i+47],
                                                      s01texto[i+48],
                                                      s01texto[i+49],                                                      
                                                      s01texto[i+50],
                                                      s01texto[i+51],
                                                      s01texto[i+52],
                                                      s01texto[i+53],
                                                      s01texto[i+54],
                                                      s01texto[i+55],
                                                      s01texto[i+56],
                                                      s01texto[i+57],
                                                      s01texto[i+58],
                                                      s01texto[i+59],                                                      
                                                      s01texto[i+60],
                                                      s01texto[i+61],
                                                      s01texto[i+62],
                                                      s01texto[i+63],
                                                      s01texto[i+64],
                                                      s01texto[i+65],
                                                      s01texto[i+66],
                                                      s01texto[i+67], sep = " ")
}

s01cortoplot <- s01cortonrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s01corto_rawsentiment <- ggplot(data = s01cortoplot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s01cortoft <- as.numeric(get_transformed_values(s01cortoplot$sentiment, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240,
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
s01cortoft <- data.frame(cbind(linenumber = seq_along(s01cortoft), cortoft = s01cortoft))
s01corto_sentiment <- ggplot(data = s01cortoft, aes(x = linenumber, y = cortoft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 2")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s01annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 4, inherit.aes = FALSE) +
  geom_segment(data = s01annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s01cortonrc <- cbind(linenumber = seq_along(s01textocorto), get_nrc_sentiment(s01textocorto))
episodioplot <- as.data.frame(s01cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s01_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 1")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s01annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s01annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s01_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s01annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s01annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s01cortonrc <- cbind(linenumber = seq_along(s01textocorto), get_nrc_sentiment(s01textocorto))
s01cortonrc$volume <- "Season 1"
emotions <- s01cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s01emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 1")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Segunda temporada

s02e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0201.html")
s02e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0202.html")
s02e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0203.html")
s02e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0204.html")
s02e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0205.html")
s02e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0206.html")
s02e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0207.html")
s02e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0208.html")
s02e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0209.html")
s02e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0210.html")
s02e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0211.html")
s02e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0212.html")
s02e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0213.html")
s02e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0214.html")
s02e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0215.html")
s02e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0216.html")
s02e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0217.html")
s02e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0218.html")
s02e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0219.html")
s02e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0220.html")
s02e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0221.html")
s02e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0222.html")
s02e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0223.html")
s02e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0224.html")

## Sentimiento de la temporada 2

s02e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0201.html')
s02e01nrc <- cbind(linenumber = seq_along(s02e01texto), get_nrc_sentiment(s02e01texto))
s02e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0202.html')
s02e02nrc <- cbind(linenumber = seq_along(s02e02texto), get_nrc_sentiment(s02e02texto))
s02e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0203.html')
s02e03nrc <- cbind(linenumber = seq_along(s02e03texto), get_nrc_sentiment(s02e03texto))
s02e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0204.html')
s02e04nrc <- cbind(linenumber = seq_along(s02e04texto), get_nrc_sentiment(s02e04texto))
s02e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0205.html')
s02e05nrc <- cbind(linenumber = seq_along(s02e05texto), get_nrc_sentiment(s02e05texto))
s02e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0206.html')
s02e06nrc <- cbind(linenumber = seq_along(s02e06texto), get_nrc_sentiment(s02e06texto))
s02e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0207.html')
s02e07nrc <- cbind(linenumber = seq_along(s02e07texto), get_nrc_sentiment(s02e07texto))
s02e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0208.html')
s02e08nrc <- cbind(linenumber = seq_along(s02e08texto), get_nrc_sentiment(s02e08texto))
s02e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0209.html')
s02e09nrc <- cbind(linenumber = seq_along(s02e09texto), get_nrc_sentiment(s02e09texto))
s02e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0210.html')
s02e10nrc <- cbind(linenumber = seq_along(s02e10texto), get_nrc_sentiment(s02e10texto))
s02e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0211.html')
s02e11nrc <- cbind(linenumber = seq_along(s02e11texto), get_nrc_sentiment(s02e11texto))
s02e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0212.html')
s02e12nrc <- cbind(linenumber = seq_along(s02e12texto), get_nrc_sentiment(s02e12texto))
s02e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0213.html')
s02e13nrc <- cbind(linenumber = seq_along(s02e13texto), get_nrc_sentiment(s02e13texto))
s02e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0214.html')
s02e14nrc <- cbind(linenumber = seq_along(s02e14texto), get_nrc_sentiment(s02e14texto))
s02e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0215.html')
s02e15nrc <- cbind(linenumber = seq_along(s02e15texto), get_nrc_sentiment(s02e15texto))
s02e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0216.html')
s02e16nrc <- cbind(linenumber = seq_along(s02e16texto), get_nrc_sentiment(s02e16texto))
s02e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0217.html')
s02e17nrc <- cbind(linenumber = seq_along(s02e17texto), get_nrc_sentiment(s02e17texto))
s02e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0218.html')
s02e18nrc <- cbind(linenumber = seq_along(s02e18texto), get_nrc_sentiment(s02e18texto))
s02e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0219.html')
s02e19nrc <- cbind(linenumber = seq_along(s02e19texto), get_nrc_sentiment(s02e19texto))
s02e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0220.html')
s02e20nrc <- cbind(linenumber = seq_along(s02e20texto), get_nrc_sentiment(s02e20texto))
s02e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0221.html')
s02e21nrc <- cbind(linenumber = seq_along(s02e21texto), get_nrc_sentiment(s02e21texto))
s02e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0222.html')
s02e22nrc <- cbind(linenumber = seq_along(s02e22texto), get_nrc_sentiment(s02e22texto))
s02e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0223.html')
s02e23nrc <- cbind(linenumber = seq_along(s02e23texto), get_nrc_sentiment(s02e23texto))
s02e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0224.html')
s02e24nrc <- cbind(linenumber = seq_along(s02e24texto), get_nrc_sentiment(s02e24texto))

s02texto <- c("s02e01", s02e01texto, "s02e02", s02e02texto, "s02e03", s02e03texto, "s02e04", s02e04texto, "s02e05", s02e05texto, "s02e06", s02e06texto, "s02e07", s02e07texto, "s02e08", s02e08texto, "s02e09", s02e09texto, "s02e10", s02e10texto, "s02e11", s02e11texto, "s02e12", s02e12texto, "s02e13", s02e13texto, "s02e14", s02e14texto, "s02e15", s02e15texto, "s02e16", s02e16texto, "s02e17", s02e17texto, "s02e18", s02e18texto, "s02e19", s02e19texto, "s02e20", s02e20texto, "s02e21", s02e21texto, "s02e22", s02e22texto, "s02e23", s02e23texto, "s02e24", s02e24texto)
s02nrc <- cbind(linenumber = seq_along(s02texto), get_nrc_sentiment(s02texto))
s02annotatetext <- data.frame(x = c(67, 139), y = rep(2.2, 1), 
                              label = c("Ross and Rachel kiss", "Ross and Rachel start again"))

s02annotatearrow <- data.frame(x = c(67, 139), 
                               y1 = rep(2, 1), y2 = c(1.2))
s02plot <- s02nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s02_rawsentiment <- ggplot(data = s02plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s02ft <- as.numeric(get_transformed_values(s02plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s02ft <- data.frame(cbind(linenumber = seq_along(s02ft), ft = s02ft))
s02_sentiment <- ggplot(data = s02ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 2")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s02annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 4, inherit.aes = FALSE) +
  geom_segment(data = s02annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s02_joy <- pintar_joy_temp(s02nrc, s02texto, "Season 2")

# para sacar dónde van las flechas
write_lines(x = s02texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/02.txt', append = FALSE)

episodioplot <- s02nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 2")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s02annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s02annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s02textocorto <- character()
for (i in seq_along(s02texto)) {
  if (i%%68 == 1) s02textocorto[ceiling(i/68)] <- str_c(s02texto[i], 
                                                        s02texto[i+1],
                                                        s02texto[i+2],
                                                        s02texto[i+3],
                                                        s02texto[i+4],
                                                        s02texto[i+5],
                                                        s02texto[i+6],
                                                        s02texto[i+7],
                                                        s02texto[i+8],
                                                        s02texto[i+9],
                                                        s02texto[i+10],
                                                        s02texto[i+11],
                                                        s02texto[i+12],
                                                        s02texto[i+13],
                                                        s02texto[i+14],
                                                        s02texto[i+15],
                                                        s02texto[i+16],
                                                        s02texto[i+17],
                                                        s02texto[i+18],
                                                        s02texto[i+19],
                                                        s02texto[i+20],
                                                        s02texto[i+21],
                                                        s02texto[i+22],
                                                        s02texto[i+23],
                                                        s02texto[i+24],
                                                        s02texto[i+25],
                                                        s02texto[i+26],
                                                        s02texto[i+27],
                                                        s02texto[i+28],
                                                        s02texto[i+29],
                                                        s02texto[i+30],
                                                        s02texto[i+31],
                                                        s02texto[i+32],
                                                        s02texto[i+33],
                                                        s02texto[i+34],
                                                        s02texto[i+35],
                                                        s02texto[i+36],
                                                        s02texto[i+37],
                                                        s02texto[i+38],
                                                        s02texto[i+39],                                                      
                                                        s02texto[i+40],
                                                        s02texto[i+41],
                                                        s02texto[i+42],
                                                        s02texto[i+43],
                                                        s02texto[i+44],
                                                        s02texto[i+45],
                                                        s02texto[i+46],
                                                        s02texto[i+47],
                                                        s02texto[i+48],
                                                        s02texto[i+49],                                                      
                                                        s02texto[i+50],
                                                        s02texto[i+51],
                                                        s02texto[i+52],
                                                        s02texto[i+53],
                                                        s02texto[i+54],
                                                        s02texto[i+55],
                                                        s02texto[i+56],
                                                        s02texto[i+57],
                                                        s02texto[i+58],
                                                        s02texto[i+59],                                                      
                                                        s02texto[i+60],
                                                        s02texto[i+61],
                                                        s02texto[i+62],
                                                        s02texto[i+63],
                                                        s02texto[i+64],
                                                        s02texto[i+65],
                                                        s02texto[i+66],
                                                        s02texto[i+67], sep = " ")
}

s02cortoannotatetext <- data.frame(x = c(67, 139), y = c(-1.8, 2.2), 
                              label = c("Ross and Rachel kiss", "Ross and Rachel start again"))
s02cortoannotatearrow <- data.frame(x = c(67, 139), 
                               y1 = c(-1.6, 2), y2 = c(-0.8, 1.2))
s02cortoplot <- s02cortonrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s02corto_rawsentiment <- ggplot(data = s02cortoplot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s02cortoft <- as.numeric(get_transformed_values(s02cortoplot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s02cortoft <- data.frame(cbind(linenumber = seq_along(s02cortoft), cortoft = s02cortoft))
s02corto_sentiment <- ggplot(data = s02cortoft, aes(x = linenumber, y = cortoft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 2")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s02cortoannotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 4, inherit.aes = FALSE) +
  geom_segment(data = s02cortoannotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s02cortonrc <- cbind(linenumber = seq_along(s02textocorto), get_nrc_sentiment(s02textocorto))
episodioplot <- as.data.frame(s02cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s02_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 2")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s02annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s02annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s02_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s02annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s02annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s02cortonrc <- cbind(linenumber = seq_along(s02textocorto), get_nrc_sentiment(s02textocorto))
s02cortonrc$volume <- "Season 2"
emotions <- s02cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s02emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 2")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Tercera temporada

s03e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0301.html")
s03e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0302.html")
s03e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0303.html")
s03e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0304.html")
s03e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0305.html")
s03e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0306.html")
s03e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0307.html")
s03e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0308.html")
s03e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0309.html")
s03e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0310.html")
s03e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0311.html")
s03e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0312.html")
s03e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0313.html")
s03e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0314.html")
s03e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0315.html")
s03e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0316.html")
s03e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0317.html")
s03e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0318.html")
s03e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0319.html")
s03e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0320.html")
s03e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0321.html")
s03e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0322.html")
s03e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0323.html")
s03e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0324.html")
s03e25 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0325.html")

## Sentimiento de la temporada 3

s03e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0301.html')
s03e01nrc <- cbind(linenumber = seq_along(s03e01texto), get_nrc_sentiment(s03e01texto))
s03e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0302.html')
s03e02nrc <- cbind(linenumber = seq_along(s03e02texto), get_nrc_sentiment(s03e02texto))
s03e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0303.html')
s03e03nrc <- cbind(linenumber = seq_along(s03e03texto), get_nrc_sentiment(s03e03texto))
s03e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0304.html')
s03e04nrc <- cbind(linenumber = seq_along(s03e04texto), get_nrc_sentiment(s03e04texto))
s03e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0305.html')
s03e05nrc <- cbind(linenumber = seq_along(s03e05texto), get_nrc_sentiment(s03e05texto))
s03e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0306.html')
s03e06nrc <- cbind(linenumber = seq_along(s03e06texto), get_nrc_sentiment(s03e06texto))
s03e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0307.html')
s03e07nrc <- cbind(linenumber = seq_along(s03e07texto), get_nrc_sentiment(s03e07texto))
s03e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0308.html')
s03e08nrc <- cbind(linenumber = seq_along(s03e08texto), get_nrc_sentiment(s03e08texto))
s03e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0309.html')
s03e09nrc <- cbind(linenumber = seq_along(s03e09texto), get_nrc_sentiment(s03e09texto))
s03e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0310.html')
s03e10nrc <- cbind(linenumber = seq_along(s03e10texto), get_nrc_sentiment(s03e10texto))
s03e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0311.html')
s03e11nrc <- cbind(linenumber = seq_along(s03e11texto), get_nrc_sentiment(s03e11texto))
s03e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0312.html')
s03e12nrc <- cbind(linenumber = seq_along(s03e12texto), get_nrc_sentiment(s03e12texto))
s03e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0313.html')
s03e13nrc <- cbind(linenumber = seq_along(s03e13texto), get_nrc_sentiment(s03e13texto))
s03e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0314.html')
s03e14nrc <- cbind(linenumber = seq_along(s03e14texto), get_nrc_sentiment(s03e14texto))
s03e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0315.html')
s03e15nrc <- cbind(linenumber = seq_along(s03e15texto), get_nrc_sentiment(s03e15texto))
s03e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0316.html')
s03e16nrc <- cbind(linenumber = seq_along(s03e16texto), get_nrc_sentiment(s03e16texto))
s03e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0317.html')
s03e17nrc <- cbind(linenumber = seq_along(s03e17texto), get_nrc_sentiment(s03e17texto))
s03e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0318.html')
s03e18nrc <- cbind(linenumber = seq_along(s03e18texto), get_nrc_sentiment(s03e18texto))
s03e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0319.html')
s03e19nrc <- cbind(linenumber = seq_along(s03e19texto), get_nrc_sentiment(s03e19texto))
s03e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0320.html')
s03e20nrc <- cbind(linenumber = seq_along(s03e20texto), get_nrc_sentiment(s03e20texto))
s03e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0321.html')
s03e21nrc <- cbind(linenumber = seq_along(s03e21texto), get_nrc_sentiment(s03e21texto))
s03e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0322.html')
s03e22nrc <- cbind(linenumber = seq_along(s03e22texto), get_nrc_sentiment(s03e22texto))
s03e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0323.html')
s03e23nrc <- cbind(linenumber = seq_along(s03e23texto), get_nrc_sentiment(s03e23texto))
s03e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0324.html')
s03e24nrc <- cbind(linenumber = seq_along(s03e24texto), get_nrc_sentiment(s03e24texto))
s03e25texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0325.html')
s03e25nrc <- cbind(linenumber = seq_along(s03e24texto), get_nrc_sentiment(s03e25texto))

s03texto <- c("s03e01", s03e01texto, "s03e02", s03e02texto, "s03e03", s03e03texto, "s03e04", s03e04texto, "s03e05", s03e05texto, "s03e06", s03e06texto, "s03e07", s03e07texto, "s03e08", s03e08texto, "s03e09", s03e09texto, "s03e10", s03e10texto, "s03e11", s03e11texto, "s03e12", s03e12texto, "s03e13", s03e13texto, "s03e14", s03e14texto, "s03e15", s03e15texto, "s03e16", s03e16texto, "s03e17", s03e17texto, "s03e18", s03e18texto, "s03e19", s03e19texto, "s03e20", s03e20texto, "s03e21", s03e21texto, "s03e22", s03e22texto, "s03e23", s03e23texto, "s03e24", s03e24texto, "s03e25", s03e25texto)
s03nrc <- cbind(linenumber = seq_along(s03texto), get_nrc_sentiment(s03texto))
s03annotatetext <- data.frame(x = c(14, 235), y = rep(2.2, 1), 
                           label = c("Ross sort of asks Rachel out", "Ross has a baby"))
s03annotatearrow <- data.frame(x = c(14, 235), 
                            y1 = rep(2, 1), y2 = c(1.2))
s03plot <- s03nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s03_rawsentiment <- ggplot(data = s03plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s03ft <- as.numeric(get_transformed_values(s03plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s03ft <- data.frame(cbind(linenumber = seq_along(s03ft), ft = s03ft))
s03_sentiment <- ggplot(data = s03ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 3")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s03annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 4, inherit.aes = FALSE) +
  geom_segment(data = s03annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s03_joy <- pintar_joy_temp(s03nrc, s03texto, "Season 1")

# para sacar dónde van las flechas
write_lines(x = s03texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/03.txt', append = FALSE)

episodioplot <- s03nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 1")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s03annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s03annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s03textocorto <- character()
for (i in seq_along(s03texto)) {
  if (i%%68 == 1) s03textocorto[ceiling(i/68)] <- str_c(s03texto[i], 
                                                        s03texto[i+1],
                                                        s03texto[i+2],
                                                        s03texto[i+3],
                                                        s03texto[i+4],
                                                        s03texto[i+5],
                                                        s03texto[i+6],
                                                        s03texto[i+7],
                                                        s03texto[i+8],
                                                        s03texto[i+9],
                                                        s03texto[i+10],
                                                        s03texto[i+11],
                                                        s03texto[i+12],
                                                        s03texto[i+13],
                                                        s03texto[i+14],
                                                        s03texto[i+15],
                                                        s03texto[i+16],
                                                        s03texto[i+17],
                                                        s03texto[i+18],
                                                        s03texto[i+19],
                                                        s03texto[i+20],
                                                        s03texto[i+21],
                                                        s03texto[i+22],
                                                        s03texto[i+23],
                                                        s03texto[i+24],
                                                        s03texto[i+25],
                                                        s03texto[i+26],
                                                        s03texto[i+27],
                                                        s03texto[i+28],
                                                        s03texto[i+29],
                                                        s03texto[i+30],
                                                        s03texto[i+31],
                                                        s03texto[i+32],
                                                        s03texto[i+33],
                                                        s03texto[i+34],
                                                        s03texto[i+35],
                                                        s03texto[i+36],
                                                        s03texto[i+37],
                                                        s03texto[i+38],
                                                        s03texto[i+39],                                                      
                                                        s03texto[i+40],
                                                        s03texto[i+41],
                                                        s03texto[i+42],
                                                        s03texto[i+43],
                                                        s03texto[i+44],
                                                        s03texto[i+45],
                                                        s03texto[i+46],
                                                        s03texto[i+47],
                                                        s03texto[i+48],
                                                        s03texto[i+49],                                                      
                                                        s03texto[i+50],
                                                        s03texto[i+51],
                                                        s03texto[i+52],
                                                        s03texto[i+53],
                                                        s03texto[i+54],
                                                        s03texto[i+55],
                                                        s03texto[i+56],
                                                        s03texto[i+57],
                                                        s03texto[i+58],
                                                        s03texto[i+59],                                                      
                                                        s03texto[i+60],
                                                        s03texto[i+61],
                                                        s03texto[i+62],
                                                        s03texto[i+63],
                                                        s03texto[i+64],
                                                        s03texto[i+65],
                                                        s03texto[i+66],
                                                        s03texto[i+67], sep = " ")
}

s03cortoannotatetext <- data.frame(x = c(5, 90, 103, 136, 149, 187, 239), y = c(2, 3, -1.8, 1.2, -1.6, 2.2, 1.4), 
                                   label = c("Monica misses\nRichard", "Rachel's first\njob in fashion", "Ross starts\ngetting jealous", "Ross and Rachel\ntake a break", "Ross and Rachel\nbreak up", "Joey hooks up with Kate", "Phoebe knows\nher mother"))
s03cortoannotatearrow <- data.frame(x = c(5, 90, 103, 136, 149, 187, 239), 
                                    y1 = c(1.6, 2.6, -1.6, 1, -1.4, 2, 1), y2 = c(0.8, 1.8, -0.8, 0.2, -0.6, 1.2, 0.2))
s03cortoplot <- s03cortonrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s03corto_rawsentiment <- ggplot(data = s03cortoplot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s03cortoft <- as.numeric(get_transformed_values(s03cortoplot$sentiment, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240,
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
s03cortoft <- data.frame(cbind(linenumber = seq_along(s03cortoft), cortoft = s03cortoft))
s03corto_sentiment <- ggplot(data = s03cortoft, aes(x = linenumber, y = cortoft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Sentiment") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 3")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s03cortoannotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 4, inherit.aes = FALSE) +
  geom_segment(data = s03cortoannotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s03cortonrc <- cbind(linenumber = seq_along(s03textocorto), get_nrc_sentiment(s03textocorto))
episodioplot <- as.data.frame(s03cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s03_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 3")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s03annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s03annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s03_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s03annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s03annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s03cortonrc <- cbind(linenumber = seq_along(s03textocorto), get_nrc_sentiment(s03textocorto))
s03cortonrc$volume <- "Season 3"
emotions <- s03cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s03emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 3")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Cuarta temporada

s04e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0401.html")
s04e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0402.html")
s04e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0403.html")
s04e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0404.html")
s04e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0405.html")
s04e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0406.html")
s04e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0407.html")
s04e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0408.html")
s04e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0409.html")
s04e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0410.html")
s04e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0411.html")
s04e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0412.html")
s04e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0413.html")
s04e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0414.html")
s04e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0415.html")
s04e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0416.html")
s04e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0417.html")
s04e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0418.html")
s04e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0419.html")
s04e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0420.html")
s04e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0421.html")
s04e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0422.html")
s04e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0423.html")
s04e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0424.html")

## Sentimiento de la temporada 4

s04e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0401.html')
s04e01nrc <- cbind(linenumber = seq_along(s04e01texto), get_nrc_sentiment(s04e01texto))
s04e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0402.html')
s04e02nrc <- cbind(linenumber = seq_along(s04e02texto), get_nrc_sentiment(s04e02texto))
s04e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0403.html')
s04e03nrc <- cbind(linenumber = seq_along(s04e03texto), get_nrc_sentiment(s04e03texto))
s04e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0404.html')
s04e04nrc <- cbind(linenumber = seq_along(s04e04texto), get_nrc_sentiment(s04e04texto))
s04e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0405.html')
s04e05nrc <- cbind(linenumber = seq_along(s04e05texto), get_nrc_sentiment(s04e05texto))
s04e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0406.html')
s04e06nrc <- cbind(linenumber = seq_along(s04e06texto), get_nrc_sentiment(s04e06texto))
s04e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0407.html')
s04e07nrc <- cbind(linenumber = seq_along(s04e07texto), get_nrc_sentiment(s04e07texto))
s04e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0408.html')
s04e08nrc <- cbind(linenumber = seq_along(s04e08texto), get_nrc_sentiment(s04e08texto))
s04e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0409.html')
s04e09nrc <- cbind(linenumber = seq_along(s04e09texto), get_nrc_sentiment(s04e09texto))
s04e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0410.html')
s04e10nrc <- cbind(linenumber = seq_along(s04e10texto), get_nrc_sentiment(s04e10texto))
s04e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0411.html')
s04e11nrc <- cbind(linenumber = seq_along(s04e11texto), get_nrc_sentiment(s04e11texto))
s04e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0412.html')
s04e12nrc <- cbind(linenumber = seq_along(s04e12texto), get_nrc_sentiment(s04e12texto))
s04e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0413.html')
s04e13nrc <- cbind(linenumber = seq_along(s04e13texto), get_nrc_sentiment(s04e13texto))
s04e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0414.html')
s04e14nrc <- cbind(linenumber = seq_along(s04e14texto), get_nrc_sentiment(s04e14texto))
s04e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0415.html')
s04e15nrc <- cbind(linenumber = seq_along(s04e15texto), get_nrc_sentiment(s04e15texto))
s04e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0416.html')
s04e16nrc <- cbind(linenumber = seq_along(s04e16texto), get_nrc_sentiment(s04e16texto))
s04e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0417.html')
s04e17nrc <- cbind(linenumber = seq_along(s04e17texto), get_nrc_sentiment(s04e17texto))
s04e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0418.html')
s04e18nrc <- cbind(linenumber = seq_along(s04e18texto), get_nrc_sentiment(s04e18texto))
s04e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0419.html')
s04e19nrc <- cbind(linenumber = seq_along(s04e19texto), get_nrc_sentiment(s04e19texto))
s04e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0420.html')
s04e20nrc <- cbind(linenumber = seq_along(s04e20texto), get_nrc_sentiment(s04e20texto))
s04e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0421.html')
s04e21nrc <- cbind(linenumber = seq_along(s04e21texto), get_nrc_sentiment(s04e21texto))
s04e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0422.html')
s04e22nrc <- cbind(linenumber = seq_along(s04e22texto), get_nrc_sentiment(s04e22texto))
s04e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0423.html')
s04e23nrc <- cbind(linenumber = seq_along(s04e23texto), get_nrc_sentiment(s04e23texto))
s04e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0424.html')
s04e24nrc <- cbind(linenumber = seq_along(s04e24texto), get_nrc_sentiment(s04e24texto))

s04texto <- c("s04e01", s04e01texto, "s04e02", s04e02texto, "s04e03", s04e03texto, "s04e04", s04e04texto, "s04e05", s04e05texto, "s04e06", s04e06texto, "s04e07", s04e07texto, "s04e08", s04e08texto, "s04e09", s04e09texto, "s04e10", s04e10texto, "s04e11", s04e11texto, "s04e12", s04e12texto, "s04e13", s04e13texto, "s04e14", s04e14texto, "s04e15", s04e15texto, "s04e16", s04e16texto, "s04e17", s04e17texto, "s04e18", s04e18texto, "s04e19", s04e19texto, "s04e20", s04e20texto, "s04e21", s04e21texto, "s04e22", s04e22texto, "s04e23", s04e23texto, "s04e24", s04e24texto) 
s04nrc <- cbind(linenumber = seq_along(s04texto), get_nrc_sentiment(s04texto))
s04annotatetext <- data.frame(x = c(14, 235), y = rep(2.2, 1), 
                              label = c("Ross sort of asks Rachel out", "Ross has a baby"))
s04annotatearrow <- data.frame(x = c(14, 235), 
                               y1 = rep(2, 1), y2 = c(1.2))
s04plot <- s04nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s04_rawsentiment <- ggplot(data = s04plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s04ft <- as.numeric(get_transformed_values(s04plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s04ft <- data.frame(cbind(linenumber = seq_along(s04ft), ft = s04ft))
s04_sentiment <- ggplot(data = s04ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 1")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s04annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s04annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s04_joy <- pintar_joy_temp(s04nrc, s04texto, "Season 1")

# para sacar dónde van las flechas
write_lines(x = s04texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/04.txt', append = FALSE)

episodioplot <- s04nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 4")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s04annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s04annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s04textocorto <- character()
for (i in seq_along(s04texto)) {
  if (i%%68 == 1) s04textocorto[ceiling(i/68)] <- str_c(s04texto[i], 
                                                        s04texto[i+1],
                                                        s04texto[i+2],
                                                        s04texto[i+3],
                                                        s04texto[i+4],
                                                        s04texto[i+5],
                                                        s04texto[i+6],
                                                        s04texto[i+7],
                                                        s04texto[i+8],
                                                        s04texto[i+9],
                                                        s04texto[i+10],
                                                        s04texto[i+11],
                                                        s04texto[i+12],
                                                        s04texto[i+13],
                                                        s04texto[i+14],
                                                        s04texto[i+15],
                                                        s04texto[i+16],
                                                        s04texto[i+17],
                                                        s04texto[i+18],
                                                        s04texto[i+19],
                                                        s04texto[i+20],
                                                        s04texto[i+21],
                                                        s04texto[i+22],
                                                        s04texto[i+23],
                                                        s04texto[i+24],
                                                        s04texto[i+25],
                                                        s04texto[i+26],
                                                        s04texto[i+27],
                                                        s04texto[i+28],
                                                        s04texto[i+29],
                                                        s04texto[i+30],
                                                        s04texto[i+31],
                                                        s04texto[i+32],
                                                        s04texto[i+33],
                                                        s04texto[i+34],
                                                        s04texto[i+35],
                                                        s04texto[i+36],
                                                        s04texto[i+37],
                                                        s04texto[i+38],
                                                        s04texto[i+39],                                                      
                                                        s04texto[i+40],
                                                        s04texto[i+41],
                                                        s04texto[i+42],
                                                        s04texto[i+43],
                                                        s04texto[i+44],
                                                        s04texto[i+45],
                                                        s04texto[i+46],
                                                        s04texto[i+47],
                                                        s04texto[i+48],
                                                        s04texto[i+49],                                                      
                                                        s04texto[i+50],
                                                        s04texto[i+51],
                                                        s04texto[i+52],
                                                        s04texto[i+53],
                                                        s04texto[i+54],
                                                        s04texto[i+55],
                                                        s04texto[i+56],
                                                        s04texto[i+57],
                                                        s04texto[i+58],
                                                        s04texto[i+59],                                                      
                                                        s04texto[i+60],
                                                        s04texto[i+61],
                                                        s04texto[i+62],
                                                        s04texto[i+63],
                                                        s04texto[i+64],
                                                        s04texto[i+65],
                                                        s04texto[i+66],
                                                        s04texto[i+67], sep = " ")
}

s04cortonrc <- cbind(linenumber = seq_along(s04textocorto), get_nrc_sentiment(s04textocorto))
episodioplot <- as.data.frame(s04cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s04_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 4")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s04annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s04annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s04_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s04annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s04annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s04cortonrc <- cbind(linenumber = seq_along(s04textocorto), get_nrc_sentiment(s04textocorto))
s04cortonrc$volume <- "Season 4"
emotions <- s04cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s04emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 4")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Quinta temporada

s05e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0501.html")
s05e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0502.html")
s05e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0503.html")
s05e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0504.html")
s05e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0505.html")
s05e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0506.html")
s05e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0507.html")
s05e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0508.html")
s05e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0509.html")
s05e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0510.html")
s05e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0511.html")
s05e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0512.html")
s05e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0513.html")
s05e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0514.html")
s05e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0515.html")
s05e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0516.html")
s05e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0517.html")
s05e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0518.html")
s05e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0519.html")
s05e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0520.html")
s05e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0521.html")
s05e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0522.html")
s05e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0523.html")
s05e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0524.html")

## Sentimiento de la temporada 5

s05e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0501.html')
s05e01nrc <- cbind(linenumber = seq_along(s05e01texto), get_nrc_sentiment(s05e01texto))
s05e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0502.html')
s05e02nrc <- cbind(linenumber = seq_along(s05e02texto), get_nrc_sentiment(s05e02texto))
s05e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0503.html')
s05e03nrc <- cbind(linenumber = seq_along(s05e03texto), get_nrc_sentiment(s05e03texto))
s05e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0504.html')
s05e04nrc <- cbind(linenumber = seq_along(s05e04texto), get_nrc_sentiment(s05e04texto))
s05e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0505.html')
s05e05nrc <- cbind(linenumber = seq_along(s05e05texto), get_nrc_sentiment(s05e05texto))
s05e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0506.html')
s05e06nrc <- cbind(linenumber = seq_along(s05e06texto), get_nrc_sentiment(s05e06texto))
s05e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0507.html')
s05e07nrc <- cbind(linenumber = seq_along(s05e07texto), get_nrc_sentiment(s05e07texto))
s05e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0508.html')
s05e08nrc <- cbind(linenumber = seq_along(s05e08texto), get_nrc_sentiment(s05e08texto))
s05e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0509.html')
s05e09nrc <- cbind(linenumber = seq_along(s05e09texto), get_nrc_sentiment(s05e09texto))
s05e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0510.html')
s05e10nrc <- cbind(linenumber = seq_along(s05e10texto), get_nrc_sentiment(s05e10texto))
s05e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0511.html')
s05e11nrc <- cbind(linenumber = seq_along(s05e11texto), get_nrc_sentiment(s05e11texto))
s05e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0512.html')
s05e12nrc <- cbind(linenumber = seq_along(s05e12texto), get_nrc_sentiment(s05e12texto))
s05e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0513.html')
s05e13nrc <- cbind(linenumber = seq_along(s05e13texto), get_nrc_sentiment(s05e13texto))
s05e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0514.html')
s05e14nrc <- cbind(linenumber = seq_along(s05e14texto), get_nrc_sentiment(s05e14texto))
s05e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0515.html')
s05e15nrc <- cbind(linenumber = seq_along(s05e15texto), get_nrc_sentiment(s05e15texto))
s05e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0516.html')
s05e16nrc <- cbind(linenumber = seq_along(s05e16texto), get_nrc_sentiment(s05e16texto))
s05e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0517.html')
s05e17nrc <- cbind(linenumber = seq_along(s05e17texto), get_nrc_sentiment(s05e17texto))
s05e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0518.html')
s05e18nrc <- cbind(linenumber = seq_along(s05e18texto), get_nrc_sentiment(s05e18texto))
s05e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0519.html')
s05e19nrc <- cbind(linenumber = seq_along(s05e19texto), get_nrc_sentiment(s05e19texto))
s05e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0520.html')
s05e20nrc <- cbind(linenumber = seq_along(s05e20texto), get_nrc_sentiment(s05e20texto))
s05e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0521.html')
s05e21nrc <- cbind(linenumber = seq_along(s05e21texto), get_nrc_sentiment(s05e21texto))
s05e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0522.html')
s05e22nrc <- cbind(linenumber = seq_along(s05e22texto), get_nrc_sentiment(s05e22texto))
s05e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0523.html')
s05e23nrc <- cbind(linenumber = seq_along(s05e23texto), get_nrc_sentiment(s05e23texto))
s05e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0524.html')
s05e24nrc <- cbind(linenumber = seq_along(s05e24texto), get_nrc_sentiment(s05e24texto))

s05texto <- c("s05e01", s05e01texto, "s05e02", s05e02texto, "s05e03", s05e03texto, "s05e04", s05e04texto, "s05e05", s05e05texto, "s05e06", s05e06texto, "s05e07", s05e07texto, "s05e08", s05e08texto, "s05e09", s05e09texto, "s05e10", s05e10texto, "s05e11", s05e11texto, "s05e12", s05e12texto, "s05e13", s05e13texto, "s05e14", s05e14texto, "s05e15", s05e15texto, "s05e16", s05e16texto, "s05e17", s05e17texto, "s05e18", s05e18texto, "s05e19", s05e19texto, "s05e20", s05e20texto, "s05e21", s05e21texto, "s05e22", s05e22texto, "s05e23", s05e23texto, "s05e24", s05e24texto)
s05nrc <- cbind(linenumber = seq_along(s05texto), get_nrc_sentiment(s05texto))
s05annotatetext <- data.frame(x = c(14, 235), y = rep(2.2, 1), 
                           label = c("Ross sort of asks Rachel out", "Ross has a baby"))
s05annotatearrow <- data.frame(x = c(14, 235), 
                            y1 = rep(2, 1), y2 = c(1.2))
s05plot <- s05nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s05_rawsentiment <- ggplot(data = s05plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s05ft <- as.numeric(get_transformed_values(s05plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s05ft <- data.frame(cbind(linenumber = seq_along(s05ft), ft = s05ft))
s05_sentiment <- ggplot(data = s05ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 5")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s05annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s05annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s05_joy <- pintar_joy_temp(s05nrc, s05texto, "Season 5")

# para sacar dónde van las flechas
write_lines(x = s05texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/05.txt', append = FALSE)

episodioplot <- s05nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 5")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s05annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s05annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s05textocorto <- character()
for (i in seq_along(s05texto)) {
  if (i%%68 == 1) s05textocorto[ceiling(i/68)] <- str_c(s05texto[i], 
                                                        s05texto[i+1],
                                                        s05texto[i+2],
                                                        s05texto[i+3],
                                                        s05texto[i+4],
                                                        s05texto[i+5],
                                                        s05texto[i+6],
                                                        s05texto[i+7],
                                                        s05texto[i+8],
                                                        s05texto[i+9],
                                                        s05texto[i+10],
                                                        s05texto[i+11],
                                                        s05texto[i+12],
                                                        s05texto[i+13],
                                                        s05texto[i+14],
                                                        s05texto[i+15],
                                                        s05texto[i+16],
                                                        s05texto[i+17],
                                                        s05texto[i+18],
                                                        s05texto[i+19],
                                                        s05texto[i+20],
                                                        s05texto[i+21],
                                                        s05texto[i+22],
                                                        s05texto[i+23],
                                                        s05texto[i+24],
                                                        s05texto[i+25],
                                                        s05texto[i+26],
                                                        s05texto[i+27],
                                                        s05texto[i+28],
                                                        s05texto[i+29],
                                                        s05texto[i+30],
                                                        s05texto[i+31],
                                                        s05texto[i+32],
                                                        s05texto[i+33],
                                                        s05texto[i+34],
                                                        s05texto[i+35],
                                                        s05texto[i+36],
                                                        s05texto[i+37],
                                                        s05texto[i+38],
                                                        s05texto[i+39],                                                      
                                                        s05texto[i+40],
                                                        s05texto[i+41],
                                                        s05texto[i+42],
                                                        s05texto[i+43],
                                                        s05texto[i+44],
                                                        s05texto[i+45],
                                                        s05texto[i+46],
                                                        s05texto[i+47],
                                                        s05texto[i+48],
                                                        s05texto[i+49],                                                      
                                                        s05texto[i+50],
                                                        s05texto[i+51],
                                                        s05texto[i+52],
                                                        s05texto[i+53],
                                                        s05texto[i+54],
                                                        s05texto[i+55],
                                                        s05texto[i+56],
                                                        s05texto[i+57],
                                                        s05texto[i+58],
                                                        s05texto[i+59],                                                      
                                                        s05texto[i+60],
                                                        s05texto[i+61],
                                                        s05texto[i+62],
                                                        s05texto[i+63],
                                                        s05texto[i+64],
                                                        s05texto[i+65],
                                                        s05texto[i+66],
                                                        s05texto[i+67], sep = " ")
}

s05cortonrc <- cbind(linenumber = seq_along(s05textocorto), get_nrc_sentiment(s05textocorto))
episodioplot <- as.data.frame(s05cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s05_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 5")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s05annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s05annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s05_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s05annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s05annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s05cortonrc <- cbind(linenumber = seq_along(s05textocorto), get_nrc_sentiment(s05textocorto))
s05cortonrc$volume <- "Season 5"
emotions <- s05cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s05emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 5")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Sexta temporada - Haciendo control de calidad

s06e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0601.html")
s06e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0602.html")
s06e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0603.html")
s06e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0604.html")
s06e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0605.html")
s06e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0606.html")
s06e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0607.html")
s06e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0608.html")
s06e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0609.html")
s06e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0610.html")
s06e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0611.html")
s06e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0612.html")
s06e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0613.html")
s06e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0614.html")
s06e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0615.html")
s06e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0616.html")
s06e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0617.html")
s06e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0618.html")
s06e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0619.html")
s06e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0620.html")
s06e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0621.html")
s06e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0622.html")
s06e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0623.html")
s06e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0624.html")
s06e25 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0625.html")

## Sentimiento de la temporada 6

s06e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0601.html')
s06e01nrc <- cbind(linenumber = seq_along(s06e01texto), get_nrc_sentiment(s06e01texto))
s06e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0602.html')
s06e02nrc <- cbind(linenumber = seq_along(s06e02texto), get_nrc_sentiment(s06e02texto))
s06e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0603.html')
s06e03nrc <- cbind(linenumber = seq_along(s06e03texto), get_nrc_sentiment(s06e03texto))
s06e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0604.html')
s06e04nrc <- cbind(linenumber = seq_along(s06e04texto), get_nrc_sentiment(s06e04texto))
s06e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0605.html')
s06e05nrc <- cbind(linenumber = seq_along(s06e05texto), get_nrc_sentiment(s06e05texto))
s06e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0606.html')
s06e06nrc <- cbind(linenumber = seq_along(s06e06texto), get_nrc_sentiment(s06e06texto))
s06e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0607.html')
s06e07nrc <- cbind(linenumber = seq_along(s06e07texto), get_nrc_sentiment(s06e07texto))
s06e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0608.html')
s06e08nrc <- cbind(linenumber = seq_along(s06e08texto), get_nrc_sentiment(s06e08texto))
s06e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0609.html')
s06e09nrc <- cbind(linenumber = seq_along(s06e09texto), get_nrc_sentiment(s06e09texto))
s06e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0610.html')
s06e10nrc <- cbind(linenumber = seq_along(s06e10texto), get_nrc_sentiment(s06e10texto))
s06e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0611.html')
s06e11nrc <- cbind(linenumber = seq_along(s06e11texto), get_nrc_sentiment(s06e11texto))
s06e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0612.html')
s06e12nrc <- cbind(linenumber = seq_along(s06e12texto), get_nrc_sentiment(s06e12texto))
s06e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0613.html')
s06e13nrc <- cbind(linenumber = seq_along(s06e13texto), get_nrc_sentiment(s06e13texto))
s06e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0614.html')
s06e14nrc <- cbind(linenumber = seq_along(s06e14texto), get_nrc_sentiment(s06e14texto))
s06e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0615.html')
s06e15nrc <- cbind(linenumber = seq_along(s06e15texto), get_nrc_sentiment(s06e15texto))
s06e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0616.html')
s06e16nrc <- cbind(linenumber = seq_along(s06e16texto), get_nrc_sentiment(s06e16texto))
s06e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0617.html')
s06e17nrc <- cbind(linenumber = seq_along(s06e17texto), get_nrc_sentiment(s06e17texto))
s06e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0618.html')
s06e18nrc <- cbind(linenumber = seq_along(s06e18texto), get_nrc_sentiment(s06e18texto))
s06e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0619.html')
s06e19nrc <- cbind(linenumber = seq_along(s06e19texto), get_nrc_sentiment(s06e19texto))
s06e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0620.html')
s06e20nrc <- cbind(linenumber = seq_along(s06e20texto), get_nrc_sentiment(s06e20texto))
s06e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0621.html')
s06e21nrc <- cbind(linenumber = seq_along(s06e21texto), get_nrc_sentiment(s06e21texto))
s06e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0622.html')
s06e22nrc <- cbind(linenumber = seq_along(s06e22texto), get_nrc_sentiment(s06e22texto))
s06e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0623.html')
s06e23nrc <- cbind(linenumber = seq_along(s06e23texto), get_nrc_sentiment(s06e23texto))
s06e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0624.html')
s06e24nrc <- cbind(linenumber = seq_along(s06e24texto), get_nrc_sentiment(s06e24texto))
s06e25texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0625.html')
s06e25nrc <- cbind(linenumber = seq_along(s06e24texto), get_nrc_sentiment(s06e25texto))

s06texto <- c("s06e01", s06e01texto, "s06e02", s06e02texto, "s06e03", s06e03texto, "s06e04", s06e04texto, "s06e05", s06e05texto, "s06e06", s06e06texto, "s06e07", s06e07texto, "s06e08", s06e08texto, "s06e09", s06e09texto, "s06e10", s06e10texto, "s06e11", s06e11texto, "s06e12", s06e12texto, "s06e13", s06e13texto, "s06e14", s06e14texto, "s06e15", s06e15texto, "s06e16", s06e16texto, "s06e17", s06e17texto, "s06e18", s06e18texto, "s06e19", s06e19texto, "s06e20", s06e20texto, "s06e21", s06e21texto, "s06e22", s06e22texto, "s06e23", s06e23texto, "s06e24", s06e24texto, "s06e25", s06e25texto)
s06nrc <- cbind(linenumber = seq_along(s06texto), get_nrc_sentiment(s06texto))
s06annotatetext <- data.frame(x = c(14, 235), y = rep(2.2, 1), 
                           label = c("Ross sort of asks Rachel out", "Ross has a baby"))
s06annotatearrow <- data.frame(x = c(14, 235), 
                            y1 = rep(2, 1), y2 = c(1.2))
s06plot <- s06nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s06_rawsentiment <- ggplot(data = s06plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s06ft <- as.numeric(get_transformed_values(s06plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s06ft <- data.frame(cbind(linenumber = seq_along(s06ft), ft = s06ft))
s06_sentiment <- ggplot(data = s06ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 6")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s06annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s06annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s06_joy <- pintar_joy_temp(s06nrc, s06texto, "Season 6")

# para sacar dónde van las flechas
write_lines(x = s06texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/06.txt', append = FALSE)

episodioplot <- s06nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 6")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s06annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s06annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s06textocorto <- character()
for (i in seq_along(s06texto)) {
  if (i%%68 == 1) s06textocorto[ceiling(i/68)] <- str_c(s06texto[i], 
                                                        s06texto[i+1],
                                                        s06texto[i+2],
                                                        s06texto[i+3],
                                                        s06texto[i+4],
                                                        s06texto[i+5],
                                                        s06texto[i+6],
                                                        s06texto[i+7],
                                                        s06texto[i+8],
                                                        s06texto[i+9],
                                                        s06texto[i+10],
                                                        s06texto[i+11],
                                                        s06texto[i+12],
                                                        s06texto[i+13],
                                                        s06texto[i+14],
                                                        s06texto[i+15],
                                                        s06texto[i+16],
                                                        s06texto[i+17],
                                                        s06texto[i+18],
                                                        s06texto[i+19],
                                                        s06texto[i+20],
                                                        s06texto[i+21],
                                                        s06texto[i+22],
                                                        s06texto[i+23],
                                                        s06texto[i+24],
                                                        s06texto[i+25],
                                                        s06texto[i+26],
                                                        s06texto[i+27],
                                                        s06texto[i+28],
                                                        s06texto[i+29],
                                                        s06texto[i+30],
                                                        s06texto[i+31],
                                                        s06texto[i+32],
                                                        s06texto[i+33],
                                                        s06texto[i+34],
                                                        s06texto[i+35],
                                                        s06texto[i+36],
                                                        s06texto[i+37],
                                                        s06texto[i+38],
                                                        s06texto[i+39],                                                      
                                                        s06texto[i+40],
                                                        s06texto[i+41],
                                                        s06texto[i+42],
                                                        s06texto[i+43],
                                                        s06texto[i+44],
                                                        s06texto[i+45],
                                                        s06texto[i+46],
                                                        s06texto[i+47],
                                                        s06texto[i+48],
                                                        s06texto[i+49],                                                      
                                                        s06texto[i+50],
                                                        s06texto[i+51],
                                                        s06texto[i+52],
                                                        s06texto[i+53],
                                                        s06texto[i+54],
                                                        s06texto[i+55],
                                                        s06texto[i+56],
                                                        s06texto[i+57],
                                                        s06texto[i+58],
                                                        s06texto[i+59],                                                      
                                                        s06texto[i+60],
                                                        s06texto[i+61],
                                                        s06texto[i+62],
                                                        s06texto[i+63],
                                                        s06texto[i+64],
                                                        s06texto[i+65],
                                                        s06texto[i+66],
                                                        s06texto[i+67], sep = " ")
}

s06cortonrc <- cbind(linenumber = seq_along(s06textocorto), get_nrc_sentiment(s06textocorto))
episodioplot <- as.data.frame(s06cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s06_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 6")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s06annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s06annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s06_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s06annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s06annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s06cortonrc <- cbind(linenumber = seq_along(s06textocorto), get_nrc_sentiment(s06textocorto))
s06cortonrc$volume <- "Season 6"
emotions <- s06cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s06emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 6")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Séptima temporada

s07e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0701.html")
s07e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0702.html")
s07e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0703.html")
s07e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0704.html")
s07e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0705.html")
s07e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0706.html")
s07e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0707.html")
s07e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0708.html")
s07e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0709.html")
s07e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0710.html")
s07e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0711.html")
s07e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0712.html")
s07e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0713.html")
s07e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0714.html")
s07e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0715.html")
s07e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0716.html")
s07e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0717.html")
s07e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0718.html")
s07e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0719.html")
s07e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0720.html")
s07e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0721.html")
s07e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0722.html")
s07e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0723.html")
s07e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0724.html")

## Sentimiento de la temporada 7

s07e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0701.html')
s07e01nrc <- cbind(linenumber = seq_along(s07e01texto), get_nrc_sentiment(s07e01texto))
s07e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0702.html')
s07e02nrc <- cbind(linenumber = seq_along(s07e02texto), get_nrc_sentiment(s07e02texto))
s07e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0703.html')
s07e03nrc <- cbind(linenumber = seq_along(s07e03texto), get_nrc_sentiment(s07e03texto))
s07e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0704.html')
s07e04nrc <- cbind(linenumber = seq_along(s07e04texto), get_nrc_sentiment(s07e04texto))
s07e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0705.html')
s07e05nrc <- cbind(linenumber = seq_along(s07e05texto), get_nrc_sentiment(s07e05texto))
s07e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0706.html')
s07e06nrc <- cbind(linenumber = seq_along(s07e06texto), get_nrc_sentiment(s07e06texto))
s07e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0707.html')
s07e07nrc <- cbind(linenumber = seq_along(s07e07texto), get_nrc_sentiment(s07e07texto))
s07e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0708.html')
s07e08nrc <- cbind(linenumber = seq_along(s07e08texto), get_nrc_sentiment(s07e08texto))
s07e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0709.html')
s07e09nrc <- cbind(linenumber = seq_along(s07e09texto), get_nrc_sentiment(s07e09texto))
s07e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0710.html')
s07e10nrc <- cbind(linenumber = seq_along(s07e10texto), get_nrc_sentiment(s07e10texto))
s07e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0711.html')
s07e11nrc <- cbind(linenumber = seq_along(s07e11texto), get_nrc_sentiment(s07e11texto))
s07e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0712.html')
s07e12nrc <- cbind(linenumber = seq_along(s07e12texto), get_nrc_sentiment(s07e12texto))
s07e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0713.html')
s07e13nrc <- cbind(linenumber = seq_along(s07e13texto), get_nrc_sentiment(s07e13texto))
s07e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0714.html')
s07e14nrc <- cbind(linenumber = seq_along(s07e14texto), get_nrc_sentiment(s07e14texto))
s07e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0715.html')
s07e15nrc <- cbind(linenumber = seq_along(s07e15texto), get_nrc_sentiment(s07e15texto))
s07e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0716.html')
s07e16nrc <- cbind(linenumber = seq_along(s07e16texto), get_nrc_sentiment(s07e16texto))
s07e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0717.html')
s07e17nrc <- cbind(linenumber = seq_along(s07e17texto), get_nrc_sentiment(s07e17texto))
s07e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0718.html')
s07e18nrc <- cbind(linenumber = seq_along(s07e18texto), get_nrc_sentiment(s07e18texto))
s07e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0719.html')
s07e19nrc <- cbind(linenumber = seq_along(s07e19texto), get_nrc_sentiment(s07e19texto))
s07e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0720.html')
s07e20nrc <- cbind(linenumber = seq_along(s07e20texto), get_nrc_sentiment(s07e20texto))
s07e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0721.html')
s07e21nrc <- cbind(linenumber = seq_along(s07e21texto), get_nrc_sentiment(s07e21texto))
s07e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0722.html')
s07e22nrc <- cbind(linenumber = seq_along(s07e22texto), get_nrc_sentiment(s07e22texto))
s07e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0723.html')
s07e23nrc <- cbind(linenumber = seq_along(s07e23texto), get_nrc_sentiment(s07e23texto))
s07e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0724.html')
s07e24nrc <- cbind(linenumber = seq_along(s07e24texto), get_nrc_sentiment(s07e24texto))

s07texto <- c("s07e01", s07e01texto, "s07e02", s07e02texto, "s07e03", s07e03texto, "s07e04", s07e04texto, "s07e05", s07e05texto, "s07e06", s07e06texto, "s07e07", s07e07texto, "s07e08", s07e08texto, "s07e09", s07e09texto, "s07e10", s07e10texto, "s07e11", s07e11texto, "s07e12", s07e12texto, "s07e13", s07e13texto, "s07e14", s07e14texto, "s07e15", s07e15texto, "s07e16", s07e16texto, "s07e17", s07e17texto, "s07e18", s07e18texto, "s07e19", s07e19texto, "s07e20", s07e20texto, "s07e21", s07e21texto, "s07e22", s07e22texto, "s07e23", s07e23texto, "s07e24", s07e24texto)
s07nrc <- cbind(linenumber = seq_along(s07texto), get_nrc_sentiment(s07texto))
s07annotatetext <- data.frame(x = c(14, 235), y = rep(2.2, 1), 
                              label = c("Ross sort of asks Rachel out", "Ross has a baby"))
s07annotatearrow <- data.frame(x = c(14, 235), 
                               y1 = rep(2, 1), y2 = c(1.2))
s07plot <- s07nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s07_rawsentiment <- ggplot(data = s07plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s07ft <- as.numeric(get_transformed_values(s07plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s07ft <- data.frame(cbind(linenumber = seq_along(s07ft), ft = s07ft))
s07_sentiment <- ggplot(data = s07ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 7")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s07annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s07annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s07_joy <- pintar_joy_temp(s07nrc, s07texto, "Season 1")

# para sacar dónde van las flechas
write_lines(x = s07texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/07.txt', append = FALSE)

episodioplot <- s07nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 7")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s07annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s07annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s07textocorto <- character()
for (i in seq_along(s07texto)) {
  if (i%%68 == 1) s07textocorto[ceiling(i/68)] <- str_c(s07texto[i], 
                                                        s07texto[i+1],
                                                        s07texto[i+2],
                                                        s07texto[i+3],
                                                        s07texto[i+4],
                                                        s07texto[i+5],
                                                        s07texto[i+6],
                                                        s07texto[i+7],
                                                        s07texto[i+8],
                                                        s07texto[i+9],
                                                        s07texto[i+10],
                                                        s07texto[i+11],
                                                        s07texto[i+12],
                                                        s07texto[i+13],
                                                        s07texto[i+14],
                                                        s07texto[i+15],
                                                        s07texto[i+16],
                                                        s07texto[i+17],
                                                        s07texto[i+18],
                                                        s07texto[i+19],
                                                        s07texto[i+20],
                                                        s07texto[i+21],
                                                        s07texto[i+22],
                                                        s07texto[i+23],
                                                        s07texto[i+24],
                                                        s07texto[i+25],
                                                        s07texto[i+26],
                                                        s07texto[i+27],
                                                        s07texto[i+28],
                                                        s07texto[i+29],
                                                        s07texto[i+30],
                                                        s07texto[i+31],
                                                        s07texto[i+32],
                                                        s07texto[i+33],
                                                        s07texto[i+34],
                                                        s07texto[i+35],
                                                        s07texto[i+36],
                                                        s07texto[i+37],
                                                        s07texto[i+38],
                                                        s07texto[i+39],                                                      
                                                        s07texto[i+40],
                                                        s07texto[i+41],
                                                        s07texto[i+42],
                                                        s07texto[i+43],
                                                        s07texto[i+44],
                                                        s07texto[i+45],
                                                        s07texto[i+46],
                                                        s07texto[i+47],
                                                        s07texto[i+48],
                                                        s07texto[i+49],                                                      
                                                        s07texto[i+50],
                                                        s07texto[i+51],
                                                        s07texto[i+52],
                                                        s07texto[i+53],
                                                        s07texto[i+54],
                                                        s07texto[i+55],
                                                        s07texto[i+56],
                                                        s07texto[i+57],
                                                        s07texto[i+58],
                                                        s07texto[i+59],                                                      
                                                        s07texto[i+60],
                                                        s07texto[i+61],
                                                        s07texto[i+62],
                                                        s07texto[i+63],
                                                        s07texto[i+64],
                                                        s07texto[i+65],
                                                        s07texto[i+66],
                                                        s07texto[i+67], sep = " ")
}

s07cortonrc <- cbind(linenumber = seq_along(s07textocorto), get_nrc_sentiment(s07textocorto))
episodioplot <- as.data.frame(s07cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s07_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 7")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s07annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s07annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s07_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s07annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s07annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s07cortonrc <- cbind(linenumber = seq_along(s07textocorto), get_nrc_sentiment(s07textocorto))
s07cortonrc$volume <- "Season 7"
emotions <- s07cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s07emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 7")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Octava temporada

s08e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0801.html")
s08e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0802.html")
s08e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0803.html")
s08e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0804.html")
s08e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0805.html")
s08e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0806.html")
s08e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0807.html")
s08e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0808.html")
s08e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0809.html")
s08e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0810.html")
s08e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0811.html")
s08e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0812.html")
s08e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0813.html")
s08e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0814.html")
s08e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0815.html")
s08e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0816.html")
s08e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0817.html")
s08e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0818.html")
s08e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0819.html")
s08e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0820.html")
s08e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0821.html")
s08e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0822.html")
s08e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0823.html")
s08e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0824.html")

## Sentimiento de la temporada 8

s08e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0801.html')
s08e01nrc <- cbind(linenumber = seq_along(s08e01texto), get_nrc_sentiment(s08e01texto))
s08e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0802.html')
s08e02nrc <- cbind(linenumber = seq_along(s08e02texto), get_nrc_sentiment(s08e02texto))
s08e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0803.html')
s08e03nrc <- cbind(linenumber = seq_along(s08e03texto), get_nrc_sentiment(s08e03texto))
s08e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0804.html')
s08e04nrc <- cbind(linenumber = seq_along(s08e04texto), get_nrc_sentiment(s08e04texto))
s08e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0805.html')
s08e05nrc <- cbind(linenumber = seq_along(s08e05texto), get_nrc_sentiment(s08e05texto))
s08e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0806.html')
s08e06nrc <- cbind(linenumber = seq_along(s08e06texto), get_nrc_sentiment(s08e06texto))
s08e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0807.html')
s08e07nrc <- cbind(linenumber = seq_along(s08e07texto), get_nrc_sentiment(s08e07texto))
s08e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0808.html')
s08e08nrc <- cbind(linenumber = seq_along(s08e08texto), get_nrc_sentiment(s08e08texto))
s08e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0809.html')
s08e09nrc <- cbind(linenumber = seq_along(s08e09texto), get_nrc_sentiment(s08e09texto))
s08e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0810.html')
s08e10nrc <- cbind(linenumber = seq_along(s08e10texto), get_nrc_sentiment(s08e10texto))
s08e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0811.html')
s08e11nrc <- cbind(linenumber = seq_along(s08e11texto), get_nrc_sentiment(s08e11texto))
s08e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0812.html')
s08e12nrc <- cbind(linenumber = seq_along(s08e12texto), get_nrc_sentiment(s08e12texto))
s08e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0813.html')
s08e13nrc <- cbind(linenumber = seq_along(s08e13texto), get_nrc_sentiment(s08e13texto))
s08e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0814.html')
s08e14nrc <- cbind(linenumber = seq_along(s08e14texto), get_nrc_sentiment(s08e14texto))
s08e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0815.html')
s08e15nrc <- cbind(linenumber = seq_along(s08e15texto), get_nrc_sentiment(s08e15texto))
s08e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0816.html')
s08e16nrc <- cbind(linenumber = seq_along(s08e16texto), get_nrc_sentiment(s08e16texto))
s08e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0817.html')
s08e17nrc <- cbind(linenumber = seq_along(s08e17texto), get_nrc_sentiment(s08e17texto))
s08e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0818.html')
s08e18nrc <- cbind(linenumber = seq_along(s08e18texto), get_nrc_sentiment(s08e18texto))
s08e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0819.html')
s08e19nrc <- cbind(linenumber = seq_along(s08e19texto), get_nrc_sentiment(s08e19texto))
s08e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0820.html')
s08e20nrc <- cbind(linenumber = seq_along(s08e20texto), get_nrc_sentiment(s08e20texto))
s08e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0821.html')
s08e21nrc <- cbind(linenumber = seq_along(s08e21texto), get_nrc_sentiment(s08e21texto))
s08e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0822.html')
s08e22nrc <- cbind(linenumber = seq_along(s08e22texto), get_nrc_sentiment(s08e22texto))
s08e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0823.html')
s08e23nrc <- cbind(linenumber = seq_along(s08e23texto), get_nrc_sentiment(s08e23texto))
s08e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0824.html')
s08e24nrc <- cbind(linenumber = seq_along(s08e24texto), get_nrc_sentiment(s08e24texto))

s08texto <- c("s08e01", s08e01texto, "s08e02", s08e02texto, "s08e03", s08e03texto, "s08e04", s08e04texto, "s08e05", s08e05texto, "s08e06", s08e06texto, "s08e07", s08e07texto, "s08e08", s08e08texto, "s08e09", s08e09texto, "s08e10", s08e10texto, "s08e11", s08e11texto, "s08e12", s08e12texto, "s08e13", s08e13texto, "s08e14", s08e14texto, "s08e15", s08e15texto, "s08e16", s08e16texto, "s08e17", s08e17texto, "s08e18", s08e18texto, "s08e19", s08e19texto, "s08e20", s08e20texto, "s08e21", s08e21texto, "s08e22", s08e22texto, "s08e23", s08e23texto, "s08e24", s08e24texto)
s08nrc <- cbind(linenumber = seq_along(s08texto), get_nrc_sentiment(s08texto))
s08annotatetext <- data.frame(x = c(14, 235), y = rep(2.2, 1), 
                           label = c("Ross sort of asks Rachel out", "Ross has a baby"))
s08annotatearrow <- data.frame(x = c(14, 235), 
                            y1 = rep(2, 1), y2 = c(1.2))
s08plot <- s08nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s08_rawsentiment <- ggplot(data = s08plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s08ft <- as.numeric(get_transformed_values(s08plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s08ft <- data.frame(cbind(linenumber = seq_along(s08ft), ft = s08ft))
s08_sentiment <- ggplot(data = s08ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 1")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s08annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s08annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s08_joy <- pintar_joy_temp(s08nrc, s08texto, "Season 8")

# para sacar dónde van las flechas
write_lines(x = s08texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/08.txt', append = FALSE)

episodioplot <- s08nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 1")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s08annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s08annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s08textocorto <- character()
for (i in seq_along(s08texto)) {
  if (i%%68 == 1) s08textocorto[ceiling(i/68)] <- str_c(s08texto[i], 
                                                        s08texto[i+1],
                                                        s08texto[i+2],
                                                        s08texto[i+3],
                                                        s08texto[i+4],
                                                        s08texto[i+5],
                                                        s08texto[i+6],
                                                        s08texto[i+7],
                                                        s08texto[i+8],
                                                        s08texto[i+9],
                                                        s08texto[i+10],
                                                        s08texto[i+11],
                                                        s08texto[i+12],
                                                        s08texto[i+13],
                                                        s08texto[i+14],
                                                        s08texto[i+15],
                                                        s08texto[i+16],
                                                        s08texto[i+17],
                                                        s08texto[i+18],
                                                        s08texto[i+19],
                                                        s08texto[i+20],
                                                        s08texto[i+21],
                                                        s08texto[i+22],
                                                        s08texto[i+23],
                                                        s08texto[i+24],
                                                        s08texto[i+25],
                                                        s08texto[i+26],
                                                        s08texto[i+27],
                                                        s08texto[i+28],
                                                        s08texto[i+29],
                                                        s08texto[i+30],
                                                        s08texto[i+31],
                                                        s08texto[i+32],
                                                        s08texto[i+33],
                                                        s08texto[i+34],
                                                        s08texto[i+35],
                                                        s08texto[i+36],
                                                        s08texto[i+37],
                                                        s08texto[i+38],
                                                        s08texto[i+39],                                                      
                                                        s08texto[i+40],
                                                        s08texto[i+41],
                                                        s08texto[i+42],
                                                        s08texto[i+43],
                                                        s08texto[i+44],
                                                        s08texto[i+45],
                                                        s08texto[i+46],
                                                        s08texto[i+47],
                                                        s08texto[i+48],
                                                        s08texto[i+49],                                                      
                                                        s08texto[i+50],
                                                        s08texto[i+51],
                                                        s08texto[i+52],
                                                        s08texto[i+53],
                                                        s08texto[i+54],
                                                        s08texto[i+55],
                                                        s08texto[i+56],
                                                        s08texto[i+57],
                                                        s08texto[i+58],
                                                        s08texto[i+59],                                                      
                                                        s08texto[i+60],
                                                        s08texto[i+61],
                                                        s08texto[i+62],
                                                        s08texto[i+63],
                                                        s08texto[i+64],
                                                        s08texto[i+65],
                                                        s08texto[i+66],
                                                        s08texto[i+67], sep = " ")
}

s08cortonrc <- cbind(linenumber = seq_along(s08textocorto), get_nrc_sentiment(s08textocorto))
episodioplot <- as.data.frame(s08cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s08_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 8")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s08annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s08annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s08_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s08annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s08annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s08cortonrc <- cbind(linenumber = seq_along(s08textocorto), get_nrc_sentiment(s08textocorto))
s08cortonrc$volume <- "Season 8"
emotions <- s08cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s08emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 8")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Novena temporada

s09e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0901.html")
s09e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0902.html")
s09e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0903.html")
s09e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0904.html")
s09e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0905.html")
s09e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0906.html")
s09e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0907.html")
s09e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0908.html")
s09e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0909.html")
s09e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0910.html")
s09e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0911.html")
s09e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0912.html")
s09e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0913.html")
s09e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0914.html")
s09e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0915.html")
s09e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0916.html")
s09e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0917.html")
s09e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0918.html")
s09e19 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0919.html")
s09e20 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0920.html")
s09e21 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0921.html")
s09e22 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0922.html")
s09e23 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0923.html")
s09e24 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0924.html")

## Sentimiento de la temporada 9

s09e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0901.html')
s09e01nrc <- cbind(linenumber = seq_along(s09e01texto), get_nrc_sentiment(s09e01texto))
s09e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0902.html')
s09e02nrc <- cbind(linenumber = seq_along(s09e02texto), get_nrc_sentiment(s09e02texto))
s09e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0903.html')
s09e03nrc <- cbind(linenumber = seq_along(s09e03texto), get_nrc_sentiment(s09e03texto))
s09e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0904.html')
s09e04nrc <- cbind(linenumber = seq_along(s09e04texto), get_nrc_sentiment(s09e04texto))
s09e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0905.html')
s09e05nrc <- cbind(linenumber = seq_along(s09e05texto), get_nrc_sentiment(s09e05texto))
s09e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0906.html')
s09e06nrc <- cbind(linenumber = seq_along(s09e06texto), get_nrc_sentiment(s09e06texto))
s09e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0907.html')
s09e07nrc <- cbind(linenumber = seq_along(s09e07texto), get_nrc_sentiment(s09e07texto))
s09e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0908.html')
s09e08nrc <- cbind(linenumber = seq_along(s09e08texto), get_nrc_sentiment(s09e08texto))
s09e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0909.html')
s09e09nrc <- cbind(linenumber = seq_along(s09e09texto), get_nrc_sentiment(s09e09texto))
s09e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0910.html')
s09e10nrc <- cbind(linenumber = seq_along(s09e10texto), get_nrc_sentiment(s09e10texto))
s09e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0911.html')
s09e11nrc <- cbind(linenumber = seq_along(s09e11texto), get_nrc_sentiment(s09e11texto))
s09e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0912.html')
s09e12nrc <- cbind(linenumber = seq_along(s09e12texto), get_nrc_sentiment(s09e12texto))
s09e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0913.html')
s09e13nrc <- cbind(linenumber = seq_along(s09e13texto), get_nrc_sentiment(s09e13texto))
s09e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0914.html')
s09e14nrc <- cbind(linenumber = seq_along(s09e14texto), get_nrc_sentiment(s09e14texto))
s09e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0915.html')
s09e15nrc <- cbind(linenumber = seq_along(s09e15texto), get_nrc_sentiment(s09e15texto))
s09e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0916.html')
s09e16nrc <- cbind(linenumber = seq_along(s09e16texto), get_nrc_sentiment(s09e16texto))
s09e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0917.html')
s09e17nrc <- cbind(linenumber = seq_along(s09e17texto), get_nrc_sentiment(s09e17texto))
s09e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0918.html')
s09e18nrc <- cbind(linenumber = seq_along(s09e18texto), get_nrc_sentiment(s09e18texto))
s09e19texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0919.html')
s09e19nrc <- cbind(linenumber = seq_along(s09e19texto), get_nrc_sentiment(s09e19texto))
s09e20texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0920.html')
s09e20nrc <- cbind(linenumber = seq_along(s09e20texto), get_nrc_sentiment(s09e20texto))
s09e21texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0921.html')
s09e21nrc <- cbind(linenumber = seq_along(s09e21texto), get_nrc_sentiment(s09e21texto))
s09e22texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0922.html')
s09e22nrc <- cbind(linenumber = seq_along(s09e22texto), get_nrc_sentiment(s09e22texto))
s09e23texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0923.html')
s09e23nrc <- cbind(linenumber = seq_along(s09e23texto), get_nrc_sentiment(s09e23texto))
s09e24texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/0924.html')
s09e24nrc <- cbind(linenumber = seq_along(s09e24texto), get_nrc_sentiment(s09e24texto))

s09texto <- c("s09e01", s09e01texto, "s09e02", s09e02texto, "s09e03", s09e03texto, "s09e04", s09e04texto, "s09e05", s09e05texto, "s09e06", s09e06texto, "s09e07", s09e07texto, "s09e08", s09e08texto, "s09e09", s09e09texto, "s09e10", s09e10texto, "s09e11", s09e11texto, "s09e12", s09e12texto, "s09e13", s09e13texto, "s09e14", s09e14texto, "s09e15", s09e15texto, "s09e16", s09e16texto, "s09e17", s09e17texto, "s09e18", s09e18texto, "s09e19", s09e19texto, "s09e20", s09e20texto, "s09e21", s09e21texto, "s09e22", s09e22texto, "s09e23", s09e23texto, "s09e24", s09e24texto)
s09nrc <- cbind(linenumber = seq_along(s09texto), get_nrc_sentiment(s09texto))
s09annotatetext <- data.frame(x = c(14, 235), y = rep(2.2, 1), 
                           label = c("Ross sort of asks Rachel out", "Ross has a baby"))
s09annotatearrow <- data.frame(x = c(14, 235), 
                            y1 = rep(2, 1), y2 = c(1.2))
s09plot <- s09nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s09_rawsentiment <- ggplot(data = s09plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s09ft <- as.numeric(get_transformed_values(s09plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s09ft <- data.frame(cbind(linenumber = seq_along(s09ft), ft = s09ft))
s09_sentiment <- ggplot(data = s09ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 9")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s09annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s09annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s09_joy <- pintar_joy_temp(s09nrc, s09texto, "Season 9")

# para sacar dónde van las flechas
write_lines(x = s09texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/09.txt', append = FALSE)

episodioplot <- s09nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 9")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s09annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s09annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s09textocorto <- character()
for (i in seq_along(s09texto)) {
  if (i%%68 == 1) s09textocorto[ceiling(i/68)] <- str_c(s09texto[i], 
                                                        s09texto[i+1],
                                                        s09texto[i+2],
                                                        s09texto[i+3],
                                                        s09texto[i+4],
                                                        s09texto[i+5],
                                                        s09texto[i+6],
                                                        s09texto[i+7],
                                                        s09texto[i+8],
                                                        s09texto[i+9],
                                                        s09texto[i+10],
                                                        s09texto[i+11],
                                                        s09texto[i+12],
                                                        s09texto[i+13],
                                                        s09texto[i+14],
                                                        s09texto[i+15],
                                                        s09texto[i+16],
                                                        s09texto[i+17],
                                                        s09texto[i+18],
                                                        s09texto[i+19],
                                                        s09texto[i+20],
                                                        s09texto[i+21],
                                                        s09texto[i+22],
                                                        s09texto[i+23],
                                                        s09texto[i+24],
                                                        s09texto[i+25],
                                                        s09texto[i+26],
                                                        s09texto[i+27],
                                                        s09texto[i+28],
                                                        s09texto[i+29],
                                                        s09texto[i+30],
                                                        s09texto[i+31],
                                                        s09texto[i+32],
                                                        s09texto[i+33],
                                                        s09texto[i+34],
                                                        s09texto[i+35],
                                                        s09texto[i+36],
                                                        s09texto[i+37],
                                                        s09texto[i+38],
                                                        s09texto[i+39],                                                      
                                                        s09texto[i+40],
                                                        s09texto[i+41],
                                                        s09texto[i+42],
                                                        s09texto[i+43],
                                                        s09texto[i+44],
                                                        s09texto[i+45],
                                                        s09texto[i+46],
                                                        s09texto[i+47],
                                                        s09texto[i+48],
                                                        s09texto[i+49],                                                      
                                                        s09texto[i+50],
                                                        s09texto[i+51],
                                                        s09texto[i+52],
                                                        s09texto[i+53],
                                                        s09texto[i+54],
                                                        s09texto[i+55],
                                                        s09texto[i+56],
                                                        s09texto[i+57],
                                                        s09texto[i+58],
                                                        s09texto[i+59],                                                      
                                                        s09texto[i+60],
                                                        s09texto[i+61],
                                                        s09texto[i+62],
                                                        s09texto[i+63],
                                                        s09texto[i+64],
                                                        s09texto[i+65],
                                                        s09texto[i+66],
                                                        s09texto[i+67], sep = " ")
}

s09cortonrc <- cbind(linenumber = seq_along(s09textocorto), get_nrc_sentiment(s09textocorto))
episodioplot <- as.data.frame(s09cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s09_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 9")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s09annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s09annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s09_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s09annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s09annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s09cortonrc <- cbind(linenumber = seq_along(s09textocorto), get_nrc_sentiment(s09textocorto))
s09cortonrc$volume <- "Season 9"
emotions <- s09cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s09emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 9")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Décima temporada

s10e01 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1001.html")
s10e02 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1002.html")
s10e03 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1003.html")
s10e04 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1004.html")
s10e05 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1005.html")
s10e06 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1006.html")
s10e07 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1007.html")
s10e08 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1008.html")
s10e09 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1009.html")
s10e10 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1010.html")
s10e11 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1011.html")
s10e12 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1012.html")
s10e13 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1013.html")
s10e14 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1014.html")
s10e15 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1015.html")
s10e16 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1016.html")
s10e17 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1017.html")
s10e18 <- analizar_ep("C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1018.html")

## Sentimiento de la temporada 10

s10e01texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1001.html')
s10e01nrc <- cbind(linenumber = seq_along(s10e01texto), get_nrc_sentiment(s10e01texto))
s10e02texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1002.html')
s10e02nrc <- cbind(linenumber = seq_along(s10e02texto), get_nrc_sentiment(s10e02texto))
s10e03texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1003.html')
s10e03nrc <- cbind(linenumber = seq_along(s10e03texto), get_nrc_sentiment(s10e03texto))
s10e04texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1004.html')
s10e04nrc <- cbind(linenumber = seq_along(s10e04texto), get_nrc_sentiment(s10e04texto))
s10e05texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1005.html')
s10e05nrc <- cbind(linenumber = seq_along(s10e05texto), get_nrc_sentiment(s10e05texto))
s10e06texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1006.html')
s10e06nrc <- cbind(linenumber = seq_along(s10e06texto), get_nrc_sentiment(s10e06texto))
s10e07texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1007.html')
s10e07nrc <- cbind(linenumber = seq_along(s10e07texto), get_nrc_sentiment(s10e07texto))
s10e08texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1008.html')
s10e08nrc <- cbind(linenumber = seq_along(s10e08texto), get_nrc_sentiment(s10e08texto))
s10e09texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1009.html')
s10e09nrc <- cbind(linenumber = seq_along(s10e09texto), get_nrc_sentiment(s10e09texto))
s10e10texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1010.html')
s10e10nrc <- cbind(linenumber = seq_along(s10e10texto), get_nrc_sentiment(s10e10texto))
s10e11texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1011.html')
s10e11nrc <- cbind(linenumber = seq_along(s10e11texto), get_nrc_sentiment(s10e11texto))
s10e12texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1012.html')
s10e12nrc <- cbind(linenumber = seq_along(s10e12texto), get_nrc_sentiment(s10e12texto))
s10e13texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1013.html')
s10e13nrc <- cbind(linenumber = seq_along(s10e13texto), get_nrc_sentiment(s10e13texto))
s10e14texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1014.html')
s10e14nrc <- cbind(linenumber = seq_along(s10e14texto), get_nrc_sentiment(s10e14texto))
s10e15texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1015.html')
s10e15nrc <- cbind(linenumber = seq_along(s10e15texto), get_nrc_sentiment(s10e15texto))
s10e16texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1016.html')
s10e16nrc <- cbind(linenumber = seq_along(s10e16texto), get_nrc_sentiment(s10e16texto))
s10e17texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1017.html')
s10e17nrc <- cbind(linenumber = seq_along(s10e17texto), get_nrc_sentiment(s10e17texto))
s10e18texto <- obtener_texto('C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/1018.html')
s10e18nrc <- cbind(linenumber = seq_along(s10e18texto), get_nrc_sentiment(s10e18texto))

s10texto <- c("s10e01", s10e01texto, "s10e02", s10e02texto, "s10e03", s10e03texto, "s10e04", s10e04texto, "s10e05", s10e05texto, "s10e06", s10e06texto, "s10e07", s10e07texto, "s10e08", s10e08texto, "s10e09", s10e09texto, "s10e10", s10e10texto, "s10e11", s10e11texto, "s10e12", s10e12texto, "s10e13", s10e13texto, "s10e14", s10e14texto, "s10e15", s10e15texto, "s10e16", s10e16texto, "s10e17", s10e17texto, "s10e18", s10e18texto)
s10nrc <- cbind(linenumber = seq_along(s10texto), get_nrc_sentiment(s10texto))
s10annotatetext <- data.frame(x = c(14, 235), y = rep(2.2, 1), 
                           label = c("Ross sort of asks Rachel out", "Ross has a baby"))
s10annotatearrow <- data.frame(x = c(14, 235), 
                            y1 = rep(2, 1), y2 = c(1.2))
s10plot <- s10nrc %>% select(linenumber, negative, positive) %>% mutate(sentiment = positive - negative)
s10_rawsentiment <- ggplot(data = s10plot, aes(x = linenumber, y = sentiment)) + geom_bar(stat = "identity", position = "identity", color = "midnightblue") +
  theme_minimal() +
  scale_x_discrete(expand=c(0.02,0)) +
  theme(axis.text.y=element_text(margin=margin(r=-10))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank())
s10ft <- as.numeric(get_transformed_values(s10plot$sentiment, 
                                           low_pass_size = 3,
                                           x_reverse_len = 240,
                                           scale_vals = TRUE,
                                           scale_range = FALSE))
s10ft <- data.frame(cbind(linenumber = seq_along(s10ft), ft = s10ft))
s10_sentiment <- ggplot(data = s10ft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in ", italic("Season 1")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s10annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s10annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s10_joy <- pintar_joy_temp(s10nrc, s10texto, "Season 10")

# para sacar dónde van las flechas
write_lines(x = s10texto, 'C:/Users/Leticia/Documents/GitHub/R/Friends/Transcripts/10.txt', append = FALSE)

episodioplot <- s10nrc %>% select(linenumber, joy)
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 3,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
episodio_joy <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 10")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s10annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s10annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# Joy

s10textocorto <- character()
for (i in seq_along(s10texto)) {
  if (i%%68 == 1) s10textocorto[ceiling(i/68)] <- str_c(s10texto[i], 
                                                        s10texto[i+1],
                                                        s10texto[i+2],
                                                        s10texto[i+3],
                                                        s10texto[i+4],
                                                        s10texto[i+5],
                                                        s10texto[i+6],
                                                        s10texto[i+7],
                                                        s10texto[i+8],
                                                        s10texto[i+9],
                                                        s10texto[i+10],
                                                        s10texto[i+11],
                                                        s10texto[i+12],
                                                        s10texto[i+13],
                                                        s10texto[i+14],
                                                        s10texto[i+15],
                                                        s10texto[i+16],
                                                        s10texto[i+17],
                                                        s10texto[i+18],
                                                        s10texto[i+19],
                                                        s10texto[i+20],
                                                        s10texto[i+21],
                                                        s10texto[i+22],
                                                        s10texto[i+23],
                                                        s10texto[i+24],
                                                        s10texto[i+25],
                                                        s10texto[i+26],
                                                        s10texto[i+27],
                                                        s10texto[i+28],
                                                        s10texto[i+29],
                                                        s10texto[i+30],
                                                        s10texto[i+31],
                                                        s10texto[i+32],
                                                        s10texto[i+33],
                                                        s10texto[i+34],
                                                        s10texto[i+35],
                                                        s10texto[i+36],
                                                        s10texto[i+37],
                                                        s10texto[i+38],
                                                        s10texto[i+39],                                                      
                                                        s10texto[i+40],
                                                        s10texto[i+41],
                                                        s10texto[i+42],
                                                        s10texto[i+43],
                                                        s10texto[i+44],
                                                        s10texto[i+45],
                                                        s10texto[i+46],
                                                        s10texto[i+47],
                                                        s10texto[i+48],
                                                        s10texto[i+49],                                                      
                                                        s10texto[i+50],
                                                        s10texto[i+51],
                                                        s10texto[i+52],
                                                        s10texto[i+53],
                                                        s10texto[i+54],
                                                        s10texto[i+55],
                                                        s10texto[i+56],
                                                        s10texto[i+57],
                                                        s10texto[i+58],
                                                        s10texto[i+59],                                                      
                                                        s10texto[i+60],
                                                        s10texto[i+61],
                                                        s10texto[i+62],
                                                        s10texto[i+63],
                                                        s10texto[i+64],
                                                        s10texto[i+65],
                                                        s10texto[i+66],
                                                        s10texto[i+67], sep = " ")
}

s10cortonrc <- cbind(linenumber = seq_along(s10textocorto), get_nrc_sentiment(s10textocorto))
episodioplot <- as.data.frame(s10cortonrc %>% select(linenumber, joy))
episodioft <- as.numeric(get_transformed_values(episodioplot$joy, 
                                                low_pass_size = 10,
                                                x_reverse_len = 240, 
                                                scale_vals = TRUE,
                                                scale_range = FALSE))
episodioft <- data.frame(cbind(linenumber = seq_along(episodioft), ft = episodioft))
s10_joy_bars <- ggplot(data = episodioft, aes(x = linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "darkgreen", fill = "darkgreen") +
  theme_minimal() +
  ylab("Joy") +
  ggtitle(expression(paste("Joy in ", italic("Season 10")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  geom_text(data = s10annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s10annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

s10_joy_line <- ggplot(episodioft, aes(linenumber, ft)) + 
  geom_path() +
  aes(colour = "green") +
  geom_text(data = s10annotatetext, aes(x,y,label=label), hjust = 0.5, 
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = s10annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE)

# All emotions

s10cortonrc <- cbind(linenumber = seq_along(s10textocorto), get_nrc_sentiment(s10textocorto))
s10cortonrc$volume <- "Season 10"
emotions <- s10cortonrc %>% select(linenumber, volume, anger, anticipation, disgust,
                                   fear, joy, sadness, surprise, trust) %>%
  melt(id = c("linenumber", "volume"))
names(emotions) <- c("linenumber", "volume", "sentiment", "value")
levels(emotions$sentiment) <- c("Anger", "Anticipation", "Disgust", "Fear", 
                                "Joy", "Sadness", "Surprise", "Trust")
emotions$sentiment = factor(emotions$sentiment, levels(emotions$sentiment)[c(5,8,2,7,6,3,4,1)])
s10emotions <- ggplot(data = emotions, aes(x = linenumber, y = sentiment, fill = value)) +
  geom_tile(color="white", size=0.1) +
  facet_wrap(~volume, nrow = 3) +
  scale_fill_viridis(name="Sentiment\nScore") +
  coord_equal() + 
  labs(x=NULL, y=NULL, 
       title=expression(paste("Sentiment in ", italic("Season 10")))) +
  theme(axis.ticks=element_blank(), axis.text.x=element_blank()) +
  scale_x_discrete(expand=c(0,0)) +
  theme(axis.text=element_text(size=6)) +
  theme(panel.border=element_blank()) +
  theme(legend.title=element_text(size=6)) + 
  theme(legend.title.align=1) + 
  theme(legend.text=element_text(size=6)) + 
  theme(legend.position="bottom") + 
  theme(legend.key.size=unit(0.2, "cm")) + 
  theme(legend.key.width=unit(1, "cm"))

# Para escoger las frases con más emoción

joy_items <- which(s05nrc$joy > 2)
s05texto[joy_items]
