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
dtotal.tib <- tibble(text=dtotal$comment, doc=c(1:1549))
dtot.tok <- dtotal.tib%>% unnest_tokens(word, text, to_lower=TRUE) %>%count(doc, word, sort=TRUE) %>% ungroup()
dtot.cp <- VCorpus(VectorSource(dtot.tok$word))
dtot.cp <- tm_map(dtot.cp, removeWords, stopwords("english"))
dtot.cp <- tm_map(dtot.cp, removeWords, c("skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really","skincareaddiction","www.reddit.com","https"))
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
ggplot(top_n(dtot.df, n=15), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Frequency of the most seen words in all reviews")

#tot 50
dtot.top50 <- top_n(dtot.df, n=50)
dtot.top502 <- top_n(dtot.tok, n=50) #fonctionne pas vraiment

#Cloud of words
library(wordcloud)
dtot.counts <- count(dtot.top50, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 100))

dtot.counts <- count(dtot.top502, word, sort=T)
with(dtot.counts, wordcloud(word, max.words = 100))

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


dtot.sentiment.nrc <- dtot.tok %>% right_join(get_sentiments("nrc")) %>%
  count(sentiment, sort=T) #le meilleur

dtot.sentiment.bing <- dtot.tok %>% right_join(get_sentiments("bing")) %>%
  count(sentiment, sort=T) # NUL

dtot.sentiment.loughran <- dtot.tok %>% right_join(get_sentiments("loughran")) %>%
  count(sentiment, sort=T)

#plot sentiment ---> POURQUOI ILS NE FONCTIONNENT PLUS CHEZ MOI? --> nn au lieu de n (nom des colonnes...)
ggplot(dtot.sentiment.nrc, aes(sentiment,nn)) + geom_bar(alpha=0.5, stat="identity", show.legend=F) + ggtitle("Sentiment using the nrc lexicon")
ggplot(dtot.sentiment.bing, aes(sentiment, nn)) + geom_bar(alpha=0.5, stat="identity", show.legend=F) +ggtitle("Sentiment using the bing lexicon")
ggplot(dtot.sentiment.loughran, aes(sentiment, nn)) + geom_bar(alpha=0.5, stat="identity", show.legend=F)+ggtitle("Sentiment using the loughran lexicon")


#word contributing to the sentiment --> pas vraiment utile ou juste (A voir ce qu'on peut faire d'autre)
word.sent <- inner_join(dtot.tok, get_sentiments(("nrc")))
word.sent <- count(word.sent, word, sentiment, sort=T)
word.sent %>%
  top_n(n=10)%>%
  ggplot(aes(reorder(word,nn),nn, fill=sentiment)) +
  geom_bar(alpha=0.8, stat="identity")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#polarity score, sentiment par commentaire
library(sentimentr)
dtotal.pol <- sentiment_by(dtotal$comment)
dtotal.pol


##-----------------------------------------SIMILARITIES

#Analyse par review

library(quanteda)

#To know which review (bien contrôler que le nombre de commentaires ne change pas, sinon à changer manuellement)
reviews <- data.frame(num_comments = c(83,450,320,363,8,116,35,154,10), review = c(1,2,3,4,5,6,7,8,9))

dtotal.rev <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)

#Copié-collé de plus haut...
dd <- dtotal.rev %>%
  select(num_comments, comment) %>% 
  mutate(doc=c(1:1549))
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
ggplot(top_n(dd.df, n=20), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Frequency of the most used words - dd.df / dtot.rev")


##---------------------------------------------### Function pour chaque poste
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

d1.df <- freq.data(d1)
d2.df <- freq.data(d2)
d3.df <- freq.data(d3)
d4.df <- freq.data(d4)
d5.df <- freq.data(d5)
d6.df <- freq.data(d6)
d7.df <- freq.data(d7)
d8.df <- freq.data(d8)
d9.df <- freq.data(d9)

#Plots of the most frequent words in each review (We can choose how many we want to show)
freq1 <- ggplot(top_n(d1.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 1")
freq2 <- ggplot(top_n(d2.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 2")
freq3 <- ggplot(top_n(d3.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 3")
freq4 <- ggplot(top_n(d4.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 4")
freq5 <- ggplot(top_n(d5.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 5")
freq6 <- ggplot(top_n(d6.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 6")
freq7 <- ggplot(top_n(d7.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 7")
freq8 <- ggplot(top_n(d8.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 8")
freq9 <- ggplot(top_n(d9.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Most frequent words in review 9")

freq1
freq2
freq3
freq4
freq5
freq6
freq7
freq8
freq9

#Show the plots in the same window
library(cowplot)
plot_grid(freq1,freq2,freq3,freq4,freq5,freq6,freq6,freq7,freq8,freq9, labels=c("1","2","3","4","5","6","7","8","9"), ncol = 3, nrow = 3)

#---------------------------------------------------------------------------------------#
#Wordclouds for each review --< stylé mais pas très utile car les mots sont plus ou moins tous de la même grandeur (fréqence proche)

d1.top10 <- top_n(d1.df, n=10)
d2.top10 <- top_n(d2.df, n=10)
d3.top10 <- top_n(d3.df, n=10)
d4.top10 <- top_n(d4.df, n=10)
d5.top10 <- top_n(d5.df, n=10)
d6.top10 <- top_n(d6.df, n=10)
d7.top10 <- top_n(d7.df, n=10)
d8.top10 <- top_n(d8.df, n=10)
d9.top10 <- top_n(d9.df, n=10)

d1.counts <- count(d1.top10, word, sort=T)
with(d1.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens"), vfont=c("serif","plain")))

d2.counts <- count(d2.top10, word, sort=T)
with(d2.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens")))

d3.counts <- count(d3.top10, word, sort=T)
with(d3.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens")))

d4.counts <- count(d4.top10, word, sort=T)
with(d4.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens")))

d5.counts <- count(d5.top10, word, sort=T)
with(d5.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens")))

d6.counts <- count(d6.top10, word, sort=T)
with(d6.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens")))

d7.counts <- count(d7.top10, word, sort=T)
with(d7.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens")))

d8.counts <- count(d8.top10, word, sort=T)
with(d8.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens")))

d9.counts <- count(d9.top10, word, sort=T)
with(d9.counts, wordcloud(word, max.words = 10,colors=brewer.pal(5, "Greens")))

#A voir si on garde ou pas, si oui les mettre dans la même fenêtre

##------------------------------------------------ LSA

## Running the LSA with quanteda
dtot.cp2 <- corpus(dtotal$comment)
dfmat <- dfm(dtot.cp2, tolower = TRUE, remove = c(stopwords("english"), c("m","s","t","skin", "sunscreen", "sunscreens", "like", "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https")), stem = FALSE, remove_punct = TRUE)
tmod <- textmodel_lsa(dfmat, nd=5) # on peut changer nd=5, 10, etc. --> nombre de noeuds, plus le noeud est faible plus les groupes sont gros --> gros clusters
head(tmod$docs)
head(tmod$features)

#celui qui génère le plus de traffic, --> comment score (méga long commentaire --> sûrement le commentaire du poste)
dtotal[905,13]

## some graphical representation - chaque texte correspond à une commentaire.
library(ggplot2)
df.doc <- data.frame(dim1=tmod$docs[,1], dim2=tmod$docs[,2])
rownames(df.doc) <- rownames(tmod$docs)
ggplot(df.doc, aes(x=dim1, y=dim2)) +
  geom_point() + 
  geom_text(label=rownames(df.doc)) +
  ggtitle("Association of the texts (comments) to dim1 and dim2")

df.feat <- data.frame(dim1=tmod$features[,1], dim2=tmod$features[,2], rownames(tmod$features))
rownames(df.feat) <- rownames(tmod$features)
colnames(df.feat)

dtot.tok.sent <- dtot.tok %>% right_join(get_sentiments("bing"))

df.feat <- left_join(df.feat, dtot.tok.sent, by=c("rownames.tmod.features."="word"))

ggplot(df.feat, aes(x=dim1, y=dim2, col=sentiment.y)) +
  geom_point() + 
  geom_text(label=rownames(df.feat))+
  ggtitle("Association of the words to dim1 and dim2 - lexicon bing")

ggplot(df.feat, aes(x=dim1, y=dim2, col=sentiment.x)) +
  geom_point() + 
  geom_text(label=rownames(df.feat))+
  ggtitle("Association of the words to dim1 and dim2 - lexicon nrc")# pas terrible

### mettre en couleur les mots positifs et négatifs -- see above

## Low rank matrix calculations
dfmat.test <- tmod$docs %*% diag(tmod$sk) %*% t(tmod$features)
range(dfmat.test - dfmat)
range(dfmat.test - tmod$matrix_low_rank )

## Look at the terms linked to topic 1 and 2

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
filter(gamma.td, document=="") ## topics in this documents
sum(filter(gamma.td, document=="")$gamma) 

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

#--------------------------------------------------------------#
## Boxplot
boxplot(dtotal$comment_score, xlab="Traffic following a post")


#que les positifs et on enlève les outliers
dtotal_pos <- dtotal %>% 
  filter(dtotal$comment_score < 10 & dtotal$comment_score > 0)

boxplot(dtotal_pos$comment_score, xlab="Number of interactions")

#truc interactif pour montrer les négatifs --> plotly ne fonctionne pas

##----------------------------------------------------------##

#Analyse sur les dates -- Quand y a-t-il le plus de posts et reviews?
library(dplyr)
library(magrittr)
library(readr)
library(lubridate)

#Par comment
dtotal$comm_date <-  as_date(dtotal$comm_date)

dtotal <- dtotal %>%
  mutate(mois = month(dtotal$comm_date))

plot(table(dtotal$mois))

ggplot(data=dtotal, aes(x=mois, y=mois)) +
  geom_bar(stat="identity") + labs(x="Month", y="Comments") + 
  ggtitle("Comments per month")

##---------------------------------------------------Brand Analysis

###sortir une marque
neutrogena <- dtot.tok %>% 
  filter(word=="neutrogena")

biore <- dtot.tok %>% 
  filter(word=="biore")

cerave <- dtot.tok %>% 
  filter(word=="cerave")

paula.choice <- dtot.tok %>% #pas trouvé
  filter(word=="paula s choice")

australian.god <- dtot.tok %>% 
  filter(word=="australian god")

australian.god <- dtotal %>%
  str_extract_all("australian god")
australian.god

roche.posay <- dtot.tok %>% 
  filter(word=="posay")

roche.posay <- dtotal %>% 
  str_extract_all("roche posay")
roche.posay

clinique <- dtot.tok %>% 
  filter(word=="clinique")

brands <- rbind(neutrogena,biore,cerave,clinique, roche.posay, australian.god)

brands$doc <- as.factor(brands$doc)

ggplot(data=brands, aes(x=word, y=n)) +
  geom_bar(stat="identity") + labs(x="Brand", y="Number") + 
  ggtitle("Number of time a brand's name appear in a comment")

#Brands/comments
ggplot(data=brands, aes(x=word,y=doc)) + geom_tile(aes(fill=n))+
  ggtitle("Appearance of brands in comments")

#Brands/Review(doc)
dd$doc <- as.factor(dd$doc)
brands <- left_join(brands, dd)

brands <- brands %>% 
  group_by(review, word) %>% 
  mutate(n_rev = sum(n)) %>% 
  ungroup()


#Number of time a brand appears in a review --> manque australian god et possiblement des roche posay
ggplot(data=brands, aes(x=word,y=review)) + geom_tile(aes(fill=n_rev))+
  ggtitle("Appearance of brands in reviews") + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9)) +
  scale_fill_gradient(low="gray", high="red") + labs(x="Brands", y="Reviews")

##-----------------------------------------------------------------------##

## HEATMAPS --> PAS ENCORE REUSSI

#TF-IDF
dtot.top10 <- top_n(dtot.df, n=10)
top10 <- unique(dtot.top10$word)
token10 <- left_join(dtot.top10, dtot.tok)

dtot.tfidf <- bind_tf_idf(token10, word, doc, n)
dtot.tfidf

tfidf <- dtot.tfidf %>% 
  select(word,tf_idf) %>% 
  group_by(word) %>% 
  mutate(tfidf = mean(tf_idf)) %>% 
  select(word,tfidf) %>% 
  group_by(word,tfidf) %>% 
  na.omit() %>% 
  summarise()

#install.packages("rdist")
library(rdist)
dist <- rdist(tfidf$tfidf, metric = "euclidean", p = 2)
dist <-  as.matrix(dist)

heatmap(dist,Colv = NA, Rowv = NA, scale="column")


#Try with ggplot --> pas ce qu'on veut...
ggplot(dtot.tfidf, aes(x=word,y=word)) + geom_tile(aes(fill=tf_idf)) + labs(X=" ", y=" ") +
  ggtitle("tf-idf pour chaque mots...")
