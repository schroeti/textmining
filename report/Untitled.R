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
library(ggplot2)
library(lexicon)
library(sentimentr)
library(sunscReen)
library(tidytext)
library(textstem)
library(quanteda)



#Loading the data set. Make sure you are in the right working repository 
setwd("../data")
sunscreen <-
  list.files(pattern = "*.csv") %>%
  map_df(~read_csv(.))

#preformating of the database

##lowercase
sunscreen$review<-sunscreen$review%>%tolower()
##factors
sunscreen[c(2:5,7:8,14, 16:23)] <- lapply(sunscreen[c(2:5,7:8,14, 16:23)], factor)
##renaming the levels
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

#vectors of brands and products that we will use
brands<-levels(sunscreen$brandName)%>%tolower()%>%c()
productnames<-levels(sunscreen$productName%>%as.factor())%>%tolower()%>%c()

#lemmatization based on dictionnary hash_lemmas
sunscreen$review<-lemmatize_strings(sunscreen$review, dictionary=hash_lemmas)
sunscreen$review<-tolower(sunscreen$review)
sunscreen$brandName<-tolower(sunscreen$brandName)
sunscreen$productName<-tolower(sunscreen$productName)

#We put as stopwords common english words as well as the names of the brands and products 
my_stop_words <- tibble(word = c("#", "s", "ve", "re", "skin", "sunscreen", "product","spf", brands, productnames, "shield",
                                 "sunscreen","sunscreens","t","it","It"))

#We create a review2 column that remains untouched and we tokenize review
#We then take away the stop words

sunscreen_cleaned <- sunscreen %>%
  mutate(review2 = review) %>%
  as.tibble() %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  filter(is.na(as.numeric(word)))

#we look at a general wordcloud
sunscreen_cleaned %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50, colors=brewer.pal(8, "Spectral")))

#tf-idf: we want to see if some words look specific to a review or to a product. 
tf_byreview<- sunscreen_cleaned %>%
  group_by(reviewId) %>%
  count(word) %>%
  ungroup()

tfidf_byreview<-tf%>%
  tidytext::bind_tf_idf(word, reviewId, n )

tf_byproduct <- sunscreen_cleaned %>%
  group_by(productName) %>%
  count(word) %>%
  ungroup()

tfidf_product<-tf %>%
  bind_tf_idf(word, productName, n )


#Interpretation: 
#Words that are specific to reviews we have but tfidf very little by product so we can't really caracterize
#tfidf maximum would be : 


#in order to perform a sentiment analysis, we use quanteda to do a corpus : by brand
crude.cp_bybrand<-corpus(sunscreen%>%group_by(productId), docid_field = "brandName", text_field = "review")
summary(crude.cp_bybrand)

crude.tk_bybrand <- quanteda::tokens(crude.cp_bybrand, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_separators=TRUE)
crude.tk_bybrand[[1]]

crude.tk_bybrand <- tokens_remove(crude.tk_bybrand, c("sunscreen","skin","product","t","s","quot",stopwords("english")))
crude.tk_bybrand <- tokens_remove(crude.tk_bybrand, my_stop_words)

crude.tk_bybrand[[1]]

crude.dfm <- dfm(crude.tk_bybrand)
View(crude.dfm)

crude.tfidf <- dfm_tfidf(crude.dfm)
View(crude.tfidf)



#by product:

crude.cp_byproduct <-corpus(sunscreen%>%group_by(productId), docid_field = "productName", text_field = "review")
summary(crude.cp_byproduct)

crude.tk_byproduct <- quanteda::tokens(crude.cp_bybrand, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, remove_separators=TRUE)
crude.tk_byproduct[[1]]

crude.tk_byproduct <- tokens_remove(crude.tk_bybrand, my_stop_words)
crude.tk_byproduct <- tokens_remove(crude.tk_bybrand, c("sunscreen","skin","product","t","s","quot",stopwords("english")))

crude.tk_byproduct[[1]]

crude.dfm <- dfm(crude.tk_byproduct)
View(crude.dfm)

crude.tfidf <- dfm_tfidf(crude.dfm)
View(crude.tfidf)

#Sentiments

for (i in c(1:length(sunscreen$review))){
sunscreen$sentiments[i]<-sentiment_by(sunscreen$review[i],hash_valence_shifters)
}

sunscreen$sentiments<-sunscreen$sentiments%>%as.numeric()
sentiment_valence_shifter<-sunscreen%>%group_by(brandName)%>%mutate(sentimentbybrand=mean(sentiment))
sentiment_valence_shifter<-sentiment_valence_shifter%>%group_by(productName)%>%mutate(sentimentbyproduct=mean(sentiment))


par(mfrow=c(3,3))

for (i in brands){
  x<-sentiment_valence_shifter%>%filter(brandName==i)
  #summary(x$sentiment)%>%print()
  boxplot(x$sentiments)
}

for (i in productnames){
  y<-sentiment_valence_shifter%>%filter(productName==i)
  #summary(x$sentiment)%>%print()
  boxplot(x$sentiments)
}


###FIRST ANALYSIS => PUT SPF out

spf<-str_extract_all(sunscreen$productName, "\\d+")%>%
  as.data.frame()%>%
  t()

sunscreen<-cbind(sunscreen, spf)

par(mfrow=c(1,1))
#wordcloud
sunscreen_cleaned %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 20, colors=brewer.pal(8, "Spectral")))

#Try per brand

sunscreen_cleaned_for_wordcloud <- sunscreen %>%
  dplyr::mutate(review2 = review) %>%
  dplyr::group_by(brandName)%>%
  as.tibble() %>%
  tidytext::unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  dplyr::filter(is.na(as.numeric(word)))

par(mfrow=c(3,3))

for(i in brands) {
  sunscreen_cleaned_for_wordcloud %>%
    dplyr::select(brandName, word)%>%
    filter(brandName == i )%>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Spectral")))
}

#We see some brands are more associated with makeup, with "primer", "makeup", others with
#ingredients like "zinc" , "niacinamide" .
#Some are more oily than others..

#Per product: 

sunscreen_cleaned_for_wordcloudperproduct <- sunscreen %>%
  dplyr::mutate(review2 = review) %>%
  dplyr::group_by(productName)%>%
  as.tibble() %>%
  tidytext::unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  dplyr::filter(is.na(as.numeric(word)))

par(mfrow=c(3,3))

for(i in productnames%>%c()) {
  sunscreen_cleaned_for_wordcloud %>%
    dplyr::select(productName, word)%>%
    filter(productName == i )%>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Spectral")))
}

#Sentiment analysis by brand

#sentiment with nrc: these are by products
sentiment_by_brand.2 <- sunscreen_cleaned_for_wordcloud %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(brandName, sentiment) %>%
  count()

sentiment_by_brand_sun<-sunscreen_cleaned_for_wordcloud %>%
  inner_join(sunscReen::get_sunsentiments("sunscReen"), by="word") %>%
  group_by(brandName, sentiment) %>%
  count()

sentiment_normalized_perbrand_sun<-sentiment_by_brand_sun%>%
  group_by(brandName)%>%
  mutate(norm=n/max(n))

sentiment_normalized_perbrand<-sentiment_by_brand.2%>%
  group_by(brandName)%>%
  mutate(norm=n/max(n))


plot_sentiment_page.2 <- function(p){
  ggplot(sentiment_by_brand.2, aes(x = sentiment, y = n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ggforce::facet_wrap_paginate(facets = ~ brandName, nrow = 3, ncol = 3, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

plot_sentiment_pagenormalized.2 <- function(p){
  ggplot(sentiment_normalized_perbrand, aes(x = sentiment, y = norm, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ggforce::facet_wrap_paginate(facets = ~ brandName, nrow = 3, ncol = 3, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

plot_sentiment_page_sun <- function(p){
  ggplot(sentiment_by_brand_sun, aes(x = sentiment, y = n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ggforce::facet_wrap_paginate(facets = ~ brandName, nrow = 3, ncol = 3, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

plot_sentiment_pagenormalized_sun<- function(p){
  ggplot(sentiment_normalized_perbrand_sun, aes(x = sentiment, y = norm, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ggforce::facet_wrap_paginate(facets = ~ brandName, nrow = 3, ncol = 3, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

plot_sentiment_page.2(1)%>%plot()
plot_sentiment_pagenormalized.2(1)%>%plot()

plot_sentiment_page_sun(1)%>%plot()
plot_sentiment_pagenormalized_sun(1)%>%plot()

###############################################################

#modify sentiments per product too with new

sunscreen_nostopword_corpus <- sunscreen %>%
  mutate(review2 = review) %>%
  as.tibble()

sunscreen_nostopword_corpusbrand <- sunscreen %>%
  mutate(review2 = review) %>%
  group_by(brandName)%>%
  as.tibble()


sunscreen_corpus<-corpus(sunscreen_nostopword_corpus$review)%>%
  quanteda::tokens( what = "word", remove_numbers = TRUE, remove_punct = TRUE,
                    remove_separators = TRUE,
                    remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                    ngrams = 1L, skip = 0L, concatenator = "_",
                    verbose = quanteda_options("verbose"), include_docvars = TRUE)%>%
  tokens_select( pattern =stopwords('en'), selection = 'remove')

sunscreen_corpus_perbrand<-corpus(sunscreen_nostopword_corpusbrand$review)%>%
  quanteda::tokens( what = "word", remove_numbers = TRUE, remove_punct = TRUE,
                    remove_separators = TRUE,
                    remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
                    ngrams = 1L, skip = 0L, concatenator = "_",
                    verbose = quanteda_options("verbose"), include_docvars = TRUE)%>%
  tokens_select( pattern =stopwords('en'), selection = 'remove')

sunscreen_corpus<- tokens_remove(sunscreen_corpus, pattern= my_stop_words, padding=FALSE)
sunscreen_corpus_perbrand<- tokens_remove(sunscreen_corpus_perbrand, pattern= my_stop_words, padding=FALSE)

## 1. method = jaccard

summary(sunscreen_corpus)
summary(sunscreen_nostopword_corpusbrand)
dfmat <- dfm(sunscreen_corpus,
             remove_punct = TRUE, remove = stopwords("english"))
View(dfmat)
dfmat[,1:5]
(tstat1 <- textstat_simil(dfmat, method = "jaccard", margin = "documents"))

sum((dfmat[1,] > 0) & (dfmat[2,] > 0)) # common words=25
sum((dfmat[1,] > 0) | (dfmat[2,] > 0)) # used words (unique)=221
tstat1[1,2]
## 33/220  = tstat1[1,2]
tstat1[20,30]

## 2. method = cosine
(tstat2 <- textstat_simil(dfmat, method = "cosine", margin = "documents"))
sum(dfmat[1,]*dfmat[2,])/sqrt(sum(dfmat[1,]^2)*sum(dfmat[2,]^2))

par(mfrow=c(1,1))
plot(hclust(as.dist(1-tstat2)))

## 3. For features (i.e. tokens or words)
(tstat3 <- textstat_simil(dfmat, method = "jaccard", margin = "features"))
as.matrix(tstat3)[1:2,1:2]
dfmat[,1:2]

## 4. with tf-idf
(tstat4 <- textstat_simil(dfm_tfidf(dfmat), method = "jaccard", margin = "documents"))



## LSA
## Running the LSA with quanteda
dfmat <- dfm(sunscreen_corpus,
             remove_punct = TRUE, remove = stopwords("english"))
tmod <- textmodel_lsa(dfmat, nd=58) # see also nd=58, 57, 10 etc.
head(tmod$docs)
head(tmod$features)

## some graphical representation
library(ggplot2)
df.doc <- data.frame(dim1=tmod$docs[,1], dim2=tmod$docs[,2])
rownames(df.doc) <- rownames(tmod$docs)
ggplot(df.doc, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(label=rownames(df.doc))

df.feat <- data.frame(dim1=tmod$features[,1], dim2=tmod$features[,2])
rownames(df.feat) <- rownames(tmod$features)
ggplot(df.feat, aes(x=dim1, y=dim2)) +
  geom_point() +
  geom_text(label=rownames(df.feat))

## Low rank matrix calculations
dfmat.test <- tmod$docs %*% diag(tmod$sk) %*% t(tmod$features)
range(dfmat.test - dfmat)
range(dfmat.test - tmod$matrix_low_rank )

## Look at the terms linked to topic 1 and 2
library(dplyr)
lsa.terms.tbl <- tibble(Term=rownames(tmod$features), topic1=tmod$features[,1],
                        topic2=tmod$features[,2])

topic1.tbl <- rbind(top_n(lsa.terms.tbl, wt=topic1, n=10), top_n(lsa.terms.tbl, wt=topic1, n=-10))
ggplot(topic1.tbl, aes(x = reorder(Term, -topic1), y = topic1))+
  geom_bar(stat="identity", color='skyblue',fill='steelblue') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

topic2.tbl <- rbind(top_n(lsa.terms.tbl, wt=topic2, n=10), top_n(lsa.terms.tbl, wt=topic2, n=-10))
ggplot(topic2.tbl, aes(x = reorder(Term, -topic2), y = topic2))+
  geom_bar(stat="identity", color='skyblue',fill='steelblue') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

## Look at the documents linked to topic 1 and 2
lsa.doc.tbl <- tibble(Doc=rownames(tmod$docs), topic1=tmod$docs[,1],
                      topic2=tmod$docs[,2])

topic1.tbl <- rbind(top_n(lsa.doc.tbl, wt=topic1, n=10), top_n(lsa.doc.tbl, wt=topic1, n=-10))
ggplot(topic1.tbl, aes(x = reorder(Doc, -topic1), y = topic1))+
  geom_bar(stat="identity", color='skyblue',fill='steelblue') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

topic2.tbl <- rbind(top_n(lsa.doc.tbl, wt=topic2, n=10), top_n(lsa.doc.tbl, wt=topic2, n=-10))
ggplot(topic2.tbl, aes(x = reorder(Doc, -topic2), y = topic2))+
  geom_bar(stat="identity", color='skyblue',fill='steelblue') +
  theme(axis.text.x=element_text(angle=45, hjust=1))



## ###########################
## LDA

library(topicmodels)
## convert quateda object to topicmodels object
dtm <- convert(dfmat, to = "topicmodels")
lda <- LDA(dtm, k = 10) # build 10 topics
terms(lda, 5) # see the 5 terms most associated with each topic
topics(lda, 5) # see the 5 topics most associated with each documents

## Extract the beta and gamma from topicmodels object
lda@beta[,1:10] # 1 first words
lda@gamma

## show the betas of each document
library(tidytext)
library(ggplot2)
library(dplyr)
beta.td <- tidy(lda, matrix = "beta")
beta.td

filter(beta.td, topic==1) ## all for topic 1
sum(filter(beta.td, topic==1)$beta)

## describes the topics with their most associated terms
beta.top.terms <- beta.td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

beta.top.terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

## describes the topics in each documents
gamma.td <- tidy(lda, matrix = "gamma")
gamma.td
filter(gamma.td, document=="1825-Adams") ## topics in this documents
sum(filter(gamma.td, document=="1825-Adams")$gamma)

gamma.td %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

## Assignment of topic to term in each document
augment(lda)

## New documents
lda2 <- LDA(dtm[-1,], k = 10) # set asside 1 document
posterior(lda2, newdata = dtm[1,])$topics





