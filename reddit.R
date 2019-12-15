#----------------------------Code for the REDDIT analysis

##Libraries
install.packages("RedditExtractoR")
library(RedditExtractoR)
library(dplyr)
library(tidytext)
library(readxl)
library(tibble)
library(ggplot2)
library(tm)
library(lexicon)
library(textstem)
library(stringr)
library(textdata)
library(sentimentr)
library(quanteda)
library(topicmodels)
library(tidytext)
library(magrittr)
library(readr)
library(lubridate)

##Extraction of the databases from reddit using the package RedditExtractoR
d1 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/craf60/selfie_after_a_long_time_of_search_i_have_finally/")
d2 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/btx79r/sun_care_dermatologist_told_me_to_ditch_sunscreen/")
d3 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/c7x8ke/product_question_30_minutes_after_applying/")
d4 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/c096h9/review_me_6_months_ago_sunscreen_is_so_greasy_and/")
d5 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/dmif3o/review_2_skinceutical_sunscreens_and_2_elta_md/")
d6 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/dcern5/review_the_10_sunscreens_ive_tried_in_my_hg/")
d7 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/dkppo4/sun_care_european_high_uva_sunscreens_for/")
d8 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/d5x4g0/11_sunscreens_for_sensitive_skin_at_low_price/")
d9 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/df7l2i/review_barisun_50_uvauvb_and_anessa_50_pa/")

##Creation of a dataframe containing all databases, adding a column "doc" and a column indicating the review
dtotal <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)

##Pretreatment - tokenization, removing stopwords, creation of a corpus
dtotal.tib <- tibble(text=dtotal$comment, doc=c(1:nrow(dtotal)))

dtot.tok <- dtotal.tib%>% 
  unnest_tokens(word, text, to_lower=TRUE) %>%
  count(doc, word, sort=TRUE) %>% 
  ungroup()

dtot.cp <- VCorpus(VectorSource(dtot.tok$word))
dtot.cp <- tm_map(dtot.cp, removeWords, stopwords("english"))
dtot.cp <- tm_map(dtot.cp, 
                  removeWords,
                  c("m","s","t","skin", "sunscreen", "sunscreens", "like", 
                    "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
inspect(dtot.cp)

##Stemming and lemmatization
dtot.cp <- tm_map(dtot.cp, stemDocument)
lemmatize_words(dtot.cp, dictionary=hash_lemmas)

##Document term matrix
dtot.dtm <- DocumentTermMatrix(dtot.cp)

#TF-IDF
dtot.tfidf <- bind_tf_idf(dtot.tok, word, doc, n)
dtot.tfidf

tfidf <- dtot.tfidf %>% 
  select(word,tf_idf) %>% 
  group_by(word) %>% 
  mutate(tfidf = mean(tf_idf)) %>% 
  select(word,tfidf) %>% 
  group_by(word,tfidf) %>% 
  na.omit() %>% 
  summarise()

#Frequencies and plot of frequencies for the total database
dtot.fr <- colSums(as.matrix(dtot.dtm))
dtot.df <- data.frame(word=names(dtot.fr), freq=dtot.fr)
ggplot(top_n(dtot.df, n=15), aes(reorder(word,freq),freq))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  ggtitle("Frequency of the most seen words in all reviews")+
  labs(x="", y="Frequency")

#----------------------------Sentiment Analysis

get_sunsentiments <- function(lexicon = c("sunscReen")) {
  lexicon <- match.arg(lexicon)
  sunscReen = lexicon_sunscReen()
}

lexicon_sunscReen <- function() {
  readRDS("data/sunscReen.rds")
}

##Get sentiment per token with sunscReen lexicon
dtot.sentiment.sunscReen <- dtot.tok %>%
  right_join(get_sunsentiments("sunscReen")) %>%
  count(sentiment)

##Plot of the sentiment in the total database using the sunscReen lexicon
ggplot(dtot.sentiment.sunscReen, aes(sentiment,nn)) +
  geom_bar(alpha=0.5, stat="identity", show.legend=F) +
  ggtitle("Sentiment using the sunscReen lexicon") +
  labs(x="Sentiment", y="Frequency")

##Polarity score
dtotal.pol <- sentiment_by(dtotal$comment)
dtotal.pol

#----------------------------Similarity Analysis

##Function we do the same pretreatment for each database
freq.data <- function(data) {
dd.tok <- data %>% 
  unnest_tokens(word, comment, to_lower=TRUE)

dd.cp <- VCorpus(VectorSource(dd.tok$word))
dd.cp <- tm_map(dd.cp, removeWords, stopwords("english"))
dd.cp <- tm_map(dd.cp, removeWords,
                c("m","s","t","skin", "sunscreen", "sunscreens", "like", "get", 
                  "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https"))
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

#Plots of the most frequent words in each review
freq1 <- ggplot(top_n(d1.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 1")
freq2 <- ggplot(top_n(d2.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 2")
freq3 <- ggplot(top_n(d3.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 3")
freq4 <- ggplot(top_n(d4.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 4")
freq5 <- ggplot(top_n(d5.df, n=4), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 5")
freq6 <- ggplot(top_n(d6.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 6")
freq7 <- ggplot(top_n(d7.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 7")
freq8 <- ggplot(top_n(d8.df, n=10), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 8")
freq9 <- ggplot(top_n(d9.df, n=9), aes(reorder(word,freq),freq))+geom_col()+xlab(NULL)+coord_flip()+ggtitle("Review 9")

#Show the plots in the same window
library(cowplot)
plot_grid(freq1,freq2,freq3,freq4,freq5,freq6,freq7,freq8,freq9, labels=c("1","2","3","4","5","6","7","8","9"), ncol = 3, nrow = 3)

#----------------------------LSA

##LSA with the quanteda package
dtot.cp2 <- corpus(dtotal$comment)
dfmat <- dfm(dtot.cp2, tolower = TRUE, 
             remove = c(stopwords("english"), 
                        c("m","s","t","skin", "sunscreen", "sunscreens", "like", 
                          "get", "one", "just","can", "really", "skincareaddiction", "www.reddit.com","https")), stem = FALSE, remove_punct = TRUE)
tmod <- textmodel_lsa(dfmat, nd=5) #we chose 5 nodes
head(tmod$docs)
head(tmod$features)

##Plot of the comments (texts) and dimension 1 and 2
df.doc <- data.frame(dim1=tmod$docs[,1], dim2=tmod$docs[,2])
rownames(df.doc) <- rownames(tmod$docs)
ggplot(df.doc, aes(x=dim1, y=dim2)) +
  geom_point() + 
  geom_text(label=rownames(df.doc)) +
  ggtitle("Association of the comments to dimension 1 and dimension 2")+
  labs(x="Dimension 1", y="Dimension 2")

##Plot of the terms (words) and dimension 1 and 2
df.feat <- data.frame(dim1=tmod$features[,1], dim2=tmod$features[,2], rownames(tmod$features))
rownames(df.feat) <- rownames(tmod$features)
colnames(df.feat)
dtot.tok.sent <- dtot.tok %>% right_join(get_sunsentiments("sunscReen"))
df.feat <- left_join(df.feat, dtot.tok.sent, by=c("rownames.tmod.features."="word"))

ggplot(df.feat, aes(x=dim1, y=dim2, col=sentiment)) +
  geom_point() + 
  geom_text(label=df.feat$rownames.tmod.features.) +
  ggtitle("Association of the words to dimension 1 and dimension 2")+
  labs(x="Dimension 1", y="Dimension 2")


##Low rank matrix calculations
dfmat.test <- tmod$docs %*% diag(tmod$sk) %*% t(tmod$features)
range(dfmat.test - dfmat)
range(dfmat.test - tmod$matrix_low_rank )

##Terms associated to topic 1 and topic 2
lsa.terms.tbl <- tibble(Term=rownames(tmod$features), topic1=tmod$features[,1],
                        topic2=tmod$features[,2])

topic1.tbl <- rbind(top_n(lsa.terms.tbl, wt=topic1, n=10), top_n(lsa.terms.tbl, wt=topic1, n=-10))
ggplot(topic1.tbl, aes(x = reorder(Term, -topic1), y = topic1))+
  geom_bar(stat="identity", color='skyblue',fill='steelblue') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Terms associated to topic 1 (dimension 1)") +
  labs(x="", y="Topic 1")

topic2.tbl <- rbind(top_n(lsa.terms.tbl, wt=topic2, n=10), top_n(lsa.terms.tbl, wt=topic2, n=-10))
ggplot(topic2.tbl, aes(x = reorder(Term, -topic2), y = topic2))+
  geom_bar(stat="identity", color='skyblue',fill='steelblue') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Terms associated to topic 2 (dimension2)")+
  labs(x="", y="Topic 2")

##Documents (texts/comments) associated to topic 1 and 2
lsa.doc.tbl <- tibble(Doc=rownames(tmod$docs), topic1=tmod$docs[,1],
                      topic2=tmod$docs[,2])

topic1.tbl <- rbind(top_n(lsa.doc.tbl, wt=topic1, n=10), top_n(lsa.doc.tbl, wt=topic1, n=-10))
ggplot(topic1.tbl, aes(x = reorder(Doc, -topic1), y = topic1))+
  geom_bar(stat="identity", color='red',fill='pink') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Texts (comments) associated to topic 1") +
  labs(x=" ", y="Topic 1")
  

topic2.tbl <- rbind(top_n(lsa.doc.tbl, wt=topic2, n=10), top_n(lsa.doc.tbl, wt=topic2, n=-10))
ggplot(topic2.tbl, aes(x = reorder(Doc, -topic2), y = topic2))+
  geom_bar(stat="identity", color='red',fill='pink') +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Texts (comments) associated to topic 2") +
  labs(x=" ", y="Topic 2")

#----------------------------LDA

##Creation of a topicmodel object
dtm <- convert(dfmat, to = "topicmodels")
lda <- LDA(dtm, k = 10) #10 topics
terms(lda, 5) 
topics(lda, 5)

##Extraction of the beta and gamma
lda@beta[,1:10]
lda@gamma 

##Betas of each document
beta.td <- tidy(lda, matrix = "beta")
beta.td

##most associated terms to each topic
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

##most associated documents to each topic
gamma.td <- tidy(lda, matrix = "gamma")

gamma.top10 <- gamma.td %>%
  group_by(topic) %>%
  top_n(10, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)

gamma.top10 %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

##Assignment of topic to term in each document
augment(lda)

#----------------------------Complementary Analysis

##Boxplot of the comment score
boxplot(dtotal$comment_score, xlab="Traffic following a post")

#Filtering on the positive values and revoving the outliers
dtotal_positive <- dtotal %>% 
  filter(dtotal$comment_score < 10 & dtotal$comment_score > 0)

boxplot(dtotal_positive$comment_score, xlab="Number of interactions")

#Time/Season Analysis
#Conversion of the variable comm_date into a date variable
dtotal$comm_date <-  as_date(dtotal$comm_date)

#Creation of a new column containing the month of the post
dtotal <- dtotal %>%
  mutate(mois = month(dtotal$comm_date))

#Plot of the posting dates of the comments
ggplot(data=dtotal, aes(x=mois, y=mois)) +
  geom_bar(stat="identity") + labs(x="Month", y="Number of comments") + 
  ggtitle("Comments per month")

#----------------------------Brand Analysis

#We try to find if the brands are cited in comments
neutrogena <- dtot.tok %>% 
  filter(word=="neutrogena")

biore <- dtot.tok %>% 
  filter(word=="biore")

cerave <- dtot.tok %>% 
  filter(word=="cerave")

paula.choice <- dtot.tok %>% 
  filter(word=="paula s choice")
#There is no values found

australian.god <- dtot.tok %>% 
  filter(word=="australian god")
#None found either

roche.posay <- dtot.tok %>% 
  filter(word=="posay")

clinique <- dtot.tok %>% 
  filter(word=="clinique")

#Dataframe containing all brands
brands <- rbind(neutrogena,biore,cerave,clinique, roche.posay, australian.god)
brands

#We create a column indicating the review number
reviews <- data.frame(num_comments = c(d1[1,]$num_comments,d2[1,]$num_comments,d3[1,]$num_comments,d4[1,]$num_comments,
                                       d5[1,]$num_comments,d6[1,]$num_comments,d7[1,]$num_comments,d8[1,]$num_comments,d9[1,]$num_comments),
                      review = c(1,2,3,4,5,6,7,8,9))

dtotal <- dtotal %>% 
  mutate(doc=1:nrow(dtotal))

brands <- left_join(brands, dtotal)
brands <- left_join(brands,reviews)
brands$doc <- as.factor(brands$doc)

#plot of the appearance of brands in reviews
ggplot(data=brands, aes(x=word, y=n)) +
  geom_bar(stat="identity") + labs(x="Brand", y="Number of times") + 
  ggtitle("Appearance of brands' names in reviews")

#Plot of the appearance of brands in comments
ggplot(data=brands, aes(x=word,y=doc)) + geom_tile(aes(fill=n))+
  ggtitle("Appearance of brands in comments")

#Number of time a brand appears in a review - which brand appears in which review
brands <- brands %>% 
  group_by(review, word) %>% 
  mutate(n_rev = sum(n)) %>% 
  ungroup()

ggplot(data=brands, aes(x=word,y=review)) + geom_tile(aes(fill=n_rev))+
  ggtitle("Appearance of brands in reviews") + scale_y_continuous(breaks=c(1,2,3,4,5,6,7,8,9)) +
  scale_fill_gradient(low="gray", high="red") + labs(x="Brand", y="Reviews")

#sentiment analysis of brands
brands.tib <- tibble(text=brands$comment, doc=c(1:nrow(brands)))
brands.tok <- brands.tib%>% unnest_tokens(word, text, to_lower=TRUE) %>%count(doc, word, sort=TRUE) %>% ungroup()

brands.sentiment.sunscReen <- brands.tok %>%
  right_join(get_sunsentiments("sunscReen"))

brands.sentiment.doc <- brands.sentiment.sunscReen %>% 
  group_by(doc) %>% 
  count(sentiment) %>% 
  na.omit()

brands.sentiment.doc <- left_join(brands.sentiment.doc,brands.tib, by="doc") 
brands.sentiment.doc <- left_join(brands.sentiment.doc, brands, by=c("text"="comment"))

brands.sentiment.brand <- brands.sentiment.doc %>% 
  group_by(word) %>% 
  count(sentiment)

ggplot(brands.sentiment.brand, aes(x = word, y = nnn, fill=sentiment))+
  geom_bar(stat="identity") + 
  ggtitle("Sentiment associated per brand - lexicon sunscReen") + 
  labs(x="Brand", y="Frequency")

