library(rvest)
library(dplyr)
library(syuzhet)

s01e01 <- read_html("http://friends.tktv.net/Episodes1/summaries/1.html")
s01e01 %>%
  html_node('body') %>%
  html_text() -> s01e01_text

text_list <- strsplit(as.character(s01e01_text), '\n')
text_df <- data.frame(lines=unlist(text_list))

get_nrc_sentiment("SCENE 1: CENTRAL PERK. (ALL PRESENT EXCEPT RACHEL AND ROSS)")
