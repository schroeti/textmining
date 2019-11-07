library(tidyverse)
library(quanteda)
library(tidytext)
library(keras)
library(caret)
library(purrr)
library(readr)
library(data.table)
library(sentimentr)
library(RSentiment)
library(wordcloud)
library(ggwordcloud)
library(RColorBrewer)
library(ggforce)

setwd("../data")

sunscreen <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(.)) 

sunscreen[c(2:5,7:8,14, 16:23)] <- lapply(sunscreen[c(2:5,7:8,14, 16:23)], factor)

levels(sunscreen$productName) <- c(
  "Age Shield Sunblock SPF 45",
  "Anthelios Ultra Light SPF 60" , 
  "Anthelios XL, SPF 60+", 
  "Botanical Tinted Mineral SPF 50", 
  "Clear Face Break-Out SPF 30", 
  "RESIST Super-Light Daily SPF 30", 
  "Sunscreen Face Lotion SPF 50", 
  "Super City Block Oil-Free SPF 40", 
  "Super City Block SPF 25", 
  "Ultra Sheer Dry-Touch SPF 55",
  "Ultra Sheer Dry-Touch SPF45", 
  "UV Aqua Rich Watery SPF50+"    
)


#Change units of text to sentences ?
#try ngrams for example 2gram ? 

my_stop_words <- tibble(word = c("#", "s", "ve", "re", "skin", "sunscreen", "product"))

sunscreen_cleaned <- sunscreen %>%
  mutate(review2 = review) %>%
  as.tibble() %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  filter(is.na(as.numeric(word)))

#wordcloud
sunscreen_cleaned %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50, colors=brewer.pal(8, "Spectral")))


# plot_wordcloud <- function(x){
#   sunscreen_cleaned %>%
#   filter(productId == x) %>%
#   count(word) %>%
#   with(wordcloud(word, n, max.words = 50, colors=brewer.pal(8, "Spectral")))
# }


#sentiment with nrc
sentiment_by_brand <- sunscreen_cleaned %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(productName, sentiment) %>%
  count()

plot_sentiment_page <- function(p){
  ggplot(sentiment_by_brand, aes(x = sentiment, y = n, fill = sentiment)) + 
  geom_bar(stat = "identity") + 
  facet_wrap_paginate(facets = ~ productName, nrow = 2, ncol = 2, page = p) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Number of words", fill = "Sentiment")
}

for(i in 1:3){
  plot_sentiment_page(i) %>%
  print()
}

#sentiment with afinn
sentiment_by_brand <- sunscreen_cleaned %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(productName, sentiment) %>%
  count()

plot_sentiment_page <- function(p){
  ggplot(sentiment_by_brand, aes(x = sentiment, y = n, fill = sentiment)) + 
    geom_bar(stat = "identity") + 
    facet_wrap_paginate(facets = ~ productName, nrow = 2, ncol = 2, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

for(i in 1:3){
  plot_sentiment_page(i) %>%
    print()
}

  


