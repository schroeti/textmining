#install.packages("RedditExtractoR")
library(RedditExtractoR)
library(dplyr)
library(tidytext)
library(readxl)
library(tibble)
library(ggplot2)

#extract the data from reddit
d1 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/craf60/selfie_after_a_long_time_of_search_i_have_finally/")

d2 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/btx79r/sun_care_dermatologist_told_me_to_ditch_sunscreen/")

d3 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/c7x8ke/product_question_30_minutes_after_applying/")

d4 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/c096h9/review_me_6_months_ago_sunscreen_is_so_greasy_and/")

d5 <- reddit_content((URL="https://www.reddit.com/r/SkincareAddiction/comments/dmif3o/review_2_skinceutical_sunscreens_and_2_elta_md/"))

d6 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/dcern5/review_the_10_sunscreens_ive_tried_in_my_hg/")

d7 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/dkppo4/sun_care_european_high_uva_sunscreens_for/")

d8 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/d5x4g0/11_sunscreens_for_sensitive_skin_at_low_price/")

d9 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/df7l2i/review_barisun_50_uvauvb_and_anessa_50_pa/")

#-----------------------------------------------------------------------#

dtotal <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)

## dtotal -- corpus
library(tm)
dtotal <- tibble(text=dtotal$comment)
dtot.tok <- dtotal%>% unnest_tokens(word, text, to_lower=TRUE)
dtot.cp <- VCorpus(VectorSource(dtot.tok$word))
dtot.cp <- tm_map(dtot.cp, removeWords, stopwords("english"))
dtot.cp <- tm_map(dtot.cp, removeWords, c("skin", "sunscreen"))
dtot.cp

inspect(dtot.cp)

#stemming + lemmatization
dtot.cp <- tm_map(dtot.cp, stemDocument)
library(lexicon)
library(textstem)
#install.packages("textstem")
lemmatize_words(dtot.cp, dictionary=hash_lemmas)

#document term matrix
dtot.dtm <- DocumentTermMatrix(dtot.cp)

#Frequencies + plot
dtot.fr <- colSums(as.matrix(dtot.dtm))
dtot.fr
dtot.df <- data.frame(word=names(dtot.fr), freq=dtot.fr)
require(ggplot2)
ggplot(top_n(dtot.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()

#tot 50
dtot.top50 <- top_n(dtot.df, n=50)
dtot.top502 <- top_n(dtot.tok, n=50) #fonctionne pas
dtot.top503 <- top_n(dtot.cp, n=50) #fonctionne pas

#Cloud of words
library(wordcloud)
dtot.counts <- count(dtot.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 100))

dtot.counts <- count(dtot.top502, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 100))

dtot.counts <- count(dtot.top503, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 100), margin(t=0, r=0, b=0, l=0, unit ="pt"))

###----------------------------------sentiment analysis
library(stringr)
library(textdata)

get_sentiments("bing")
filter(get_sentiments("bing"), word %in% dtot.tok)

get_sentiments("nrc")
filter(get_sentiments("nrc"), word %in% dtot.tok)

get_sentiments("afinn")
filter(get_sentiments("afinn"), word %in% dtot.tok)

get_sentiments("loughran")
filter(get_sentiments("loughran"), word %in% dtot.tok)


dtot.sentiment <- dtot.tok %>% right_join(get_sentiments("nrc")) %>%
  count(sentiment, sort=T) #le meilleur

dtot.sentiment <- dtot.tok %>% right_join(get_sentiments("bing")) %>%
  count(sentiment, sort=T) # NUL

dtot.sentiment <- dtot.tok %>% right_join(get_sentiments("loughran")) %>%
  count(sentiment, sort=T)


#plot sentiment
ggplot(dtot.sentiment, aes(sentiment, n)) + geom_bar(alpha=0.5, stat="identity", show.legend=F)

#word contributing to the sentiment
word.sent <- inner_join(dtot.tok, get_sentiments(("nrc")))
word.sent <- count(word.sent, word, sentiment, sort=T)
word.sent %>%
  top_n(n=10)%>%
  ggplot(aes(reorder(word,n),n, fill=sentiment)) +
  geom_bar(alpha=0.8, stat="identity")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#polarity score, sentiment par commentaire
library(sentimentr)
dtotal.pol <- sentiment_by(dtotal$comment)
dtotal.pol

get_sentiments("nrc")
filter(get_sentiments("nrc"), word %in% dtotal$comment)

dtotal.sent <- dtotal %>% right_join(get_sentiments("nrc"), by=NULL) %>%
  count(sentiment, sort=T) #le meilleur

?right_join

word.sent <- inner_join(dtotal.sent, get_sentiments(("nrc")))
word.sent <- count(dtotal.sent, comment, sentiment, sort=T)
word.sent %>%
  top_n(n=10)%>%
  ggplot(aes(reorder(word,n),n, fill=sentiment)) +
  geom_bar(alpha=0.8, stat="identity")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

##-----------------------------------------SIMILARITIES

#Analyse par review

library(quanteda)

#To know which review
reviews <- data.frame(num_comments = c(83,450,320,363,8,116,35,155,10), review = c(1,2,3,4,5,6,7,8,9))

ddtotal <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)

#Copié-collé de plus haut...
dd <- ddtotal %>%
  select(num_comments, comment)
dd <- left_join(dd,reviews)
dd.tok <- dd %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd.cp <- VCorpus(VectorSource(dd.tok$word))
dd.cp <- tm_map(dd.cp, removeWords, stopwords("english"))
dd.cp <- tm_map(dd.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really","skincareaddiction","www.reddit.com","https"))
dd.cp
#document term matrix
dd.dtm <- DocumentTermMatrix(dd.cp)
#Frequencies + plot
dd.fr <- colSums(as.matrix(dd.dtm))
dd.fr
dd.df <- data.frame(word=names(dd.fr), freq=dd.fr)
require(ggplot2)
ggplot(top_n(dd.df, n=20), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()


##---------------------------------------------### Function qui fonctionne pas...
freq.data <- function(data) {
dd.tok <- data %>% 
  unnest_tokens(word, comment, to_lower=TRUE)

dd.cp <- VCorpus(VectorSource(dd.tok$word))
dd.cp <- tm_map(dd.cp, removeWords, stopwords("english"))
dd.cp <- tm_map(dd.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd.dtm <- DocumentTermMatrix(dd.cp)
#Frequencies + plot
dd.fr <- colSums(as.matrix(dd.dtm))
dd.fr
dd.df <- data.frame(word=names(dd.fr), freq=dd.fr)
return(dd.df)
}

#--------------------------------------------------------------------------#

##Review 1
dd1 <- dd %>% 
  filter(review == 1)
dd1.tok <- dd1 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd1.cp <- VCorpus(VectorSource(dd1.tok$word))
dd1.cp <- tm_map(dd1.cp, removeWords, stopwords("english"))
dd1.cp <- tm_map(dd1.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd1.dtm <- DocumentTermMatrix(dd1.cp)
#Frequencies + plot
dd1.fr <- colSums(as.matrix(dd1.dtm))
dd1.fr
dd1.df <- data.frame(word=names(dd1.fr), freq=dd1.fr)
ggplot(top_n(dd1.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review1")

##Review 2
dd2 <- dd %>% 
  filter(review == 2)
dd2.tok <- dd2 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd2.cp <- VCorpus(VectorSource(dd2.tok$word))
dd2.cp <- tm_map(dd2.cp, removeWords, stopwords("english"))
dd2.cp <- tm_map(dd2.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd2.dtm <- DocumentTermMatrix(dd2.cp)
#Frequencies + plot
dd2.fr <- colSums(as.matrix(dd2.dtm))
dd2.fr
dd2.df <- data.frame(word=names(dd2.fr), freq=dd2.fr)
ggplot(top_n(dd2.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review2")

##Review 3
dd3 <- dd %>% 
  filter(review == 3)
dd3.tok <- dd3 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd3.cp <- VCorpus(VectorSource(dd3.tok$word))
dd3.cp <- tm_map(dd3.cp, removeWords, stopwords("english"))
dd3.cp <- tm_map(dd3.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd3.dtm <- DocumentTermMatrix(dd3.cp)
#Frequencies + plot
dd3.fr <- colSums(as.matrix(dd3.dtm))
dd3.fr
dd3.df <- data.frame(word=names(dd3.fr), freq=dd3.fr)
ggplot(top_n(dd3.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review3")

##Review 4
dd4 <- dd %>% 
  filter(review == 4)
dd4.tok <- dd4 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd4.cp <- VCorpus(VectorSource(dd4.tok$word))
dd4.cp <- tm_map(dd4.cp, removeWords, stopwords("english"))
dd4.cp <- tm_map(dd4.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd4.dtm <- DocumentTermMatrix(dd4.cp)
#Frequencies + plot
dd4.fr <- colSums(as.matrix(dd4.dtm))
dd4.fr
dd4.df <- data.frame(word=names(dd4.fr), freq=dd4.fr)
ggplot(top_n(dd4.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review4")

##Review 5
dd5 <- dd %>% 
  filter(review == 5)
dd5.tok <- dd5 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd5.cp <- VCorpus(VectorSource(dd5.tok$word))
dd5.cp <- tm_map(dd5.cp, removeWords, stopwords("english"))
dd5.cp <- tm_map(dd5.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd5.dtm <- DocumentTermMatrix(dd5.cp)
#Frequencies + plot
dd5.fr <- colSums(as.matrix(dd5.dtm))
dd5.fr
dd5.df <- data.frame(word=names(dd5.fr), freq=dd5.fr)
ggplot(top_n(dd5.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review5")

##Review 6
dd6 <- dd %>% 
  filter(review == 6)
dd6.tok <- dd6 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd6.cp <- VCorpus(VectorSource(dd6.tok$word))
dd6.cp <- tm_map(dd6.cp, removeWords, stopwords("english"))
dd6.cp <- tm_map(dd6.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd6.dtm <- DocumentTermMatrix(dd6.cp)
#Frequencies + plot
dd6.fr <- colSums(as.matrix(dd6.dtm))
dd6.fr
dd6.df <- data.frame(word=names(dd6.fr), freq=dd6.fr)
ggplot(top_n(dd6.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review6")

##Review 7
dd7 <- dd %>% 
  filter(review == 7)
dd7.tok <- dd7 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd7.cp <- VCorpus(VectorSource(dd7.tok$word))
dd7.cp <- tm_map(dd7.cp, removeWords, stopwords("english"))
dd7.cp <- tm_map(dd7.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd7.dtm <- DocumentTermMatrix(dd7.cp)
#Frequencies + plot
dd7.fr <- colSums(as.matrix(dd7.dtm))
dd7.fr
dd7.df <- data.frame(word=names(dd7.fr), freq=dd7.fr)
ggplot(top_n(dd7.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review7")

##Review 8
dd8 <- dd %>% 
  filter(review == 8)
dd8.tok <- dd8 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd8.cp <- VCorpus(VectorSource(dd8.tok$word))
dd8.cp <- tm_map(dd8.cp, removeWords, stopwords("english"))
dd8.cp <- tm_map(dd8.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd8.dtm <- DocumentTermMatrix(dd8.cp)
#Frequencies + plot
dd8.fr <- colSums(as.matrix(dd8.dtm))
dd8.fr
dd8.df <- data.frame(word=names(dd8.fr), freq=dd8.fr)
ggplot(top_n(dd8.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review8")

##Review 9
dd9 <- dd %>% 
  filter(review == 9)
dd9.tok <- dd9 %>% 
  unnest_tokens(word, comment, to_lower=TRUE)
dd9.cp <- VCorpus(VectorSource(dd9.tok$word))
dd9.cp <- tm_map(dd9.cp, removeWords, stopwords("english"))
dd9.cp <- tm_map(dd9.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
#document term matrix
dd9.dtm <- DocumentTermMatrix(dd9.cp)
#Frequencies + plot
dd9.fr <- colSums(as.matrix(dd9.dtm))
dd9.fr
dd9.df <- data.frame(word=names(dd9.fr), freq=dd9.fr)
ggplot(top_n(dd9.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review9")


a <- ggplot(top_n(dd1.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review1")+theme(legend.position = NULL)
b <- ggplot(top_n(dd2.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review2")+theme(legend.position = NULL)
c <- ggplot(top_n(dd3.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review3")+theme(legend.position = NULL)
d <- ggplot(top_n(dd4.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review4")+theme(legend.position = NULL)
e <- ggplot(top_n(dd5.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review5")+theme(legend.position = NULL)
f <- ggplot(top_n(dd6.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review6")+theme(legend.position = NULL)
g <- ggplot(top_n(dd7.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review7")+theme(legend.position = NULL)
h <- ggplot(top_n(dd8.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review8")+theme(legend.position = NULL)
i <- ggplot(top_n(dd9.df, n=5), aes(reorder(word,freq),freq, fill=freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review9")+theme(legend.position = NULL)

library(cowplot)
plot_grid(a,b,c,d,e,f,g,h,i, labels=c("1","2","3","4","5","6","7","8","9"), ncol = 3, nrow = 3)

#---------------------------------------------------------------------------------------#
###fonctionne pas non plus...

d1.top50 <- top_n(dd1.df, n=50)
d2.top50 <- top_n(dd2.df, n=50)
d3.top50 <- top_n(dd3.df, n=50)
d4.top50 <- top_n(dd4.df, n=50)
d5.top50 <- top_n(dd5.df, n=50)
d6.top50 <- top_n(dd6.df, n=50)
d7.top50 <- top_n(dd7.df, n=50)
d8.top50 <- top_n(dd8.df, n=50)
d9.top50 <- top_n(dd9.df, n=50)


library(wordcloud)
d1.counts <- count(d1.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))

dt2.counts <- count(d2.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))

d3.counts <- count(d3.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))

d4.counts <- count(d4.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))

d5.counts <- count(d5.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))

d6.counts <- count(d6.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))

d7.counts <- count(d7.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))

d8.counts <- count(d8.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))

d9.counts <- count(d9.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 10))


plot_grid(a,b,c,d,e,f,g,h,i, labels=c("1","2","3","4","5","6","7","8","9"), ncol = 3, nrow = 3)




##------------------------------------------- LSA
## Running the LSA with quanteda
dtot.cp2 <- corpus(dtotal$comment)
dfmat <- dfm(dtot.cp2, tolower = TRUE, remove = stopwords("english"), stem = FALSE, remove_punct = TRUE)
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
  geom_bar(stat="identity", color='red',fill='pink') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

topic2.tbl <- rbind(top_n(lsa.doc.tbl, wt=topic2, n=10), top_n(lsa.doc.tbl, wt=topic2, n=-10))
ggplot(topic2.tbl, aes(x = reorder(Doc, -topic2), y = topic2))+
  geom_bar(stat="identity", color='red',fill='pink') +
  theme(axis.text.x=element_text(angle=45, hjust=1))


##----------------------------------------------------- LDA

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
