#similarities

library(quanteda)

sunscreen_nostopword_corpus <- sunscreen %>%
  mutate(review2 = review) %>%
  as.tibble()


sunscreen_corpus<-corpus(sunscreen_nostopword_corpus$review2)%>%
  quanteda::tokens( what = "word", remove_numbers = TRUE, remove_punct = TRUE,
          remove_separators = TRUE,
         remove_twitter = TRUE, remove_hyphens = TRUE, remove_url = TRUE,
         ngrams = 1L, skip = 0L, concatenator = "_",
         verbose = quanteda_options("verbose"), include_docvars = TRUE)%>%
  tokens_select( pattern =stopwords('en'), selection = 'remove')

my_stop_words <-  my_stop_words <- list("#", "s", "ve", "re", "skin", "sunscreen", "product","spf", "sunblock","quot", "sunscreens", "didn","doesn","don" ,"makes","i","m","t","clinique")
 sunscreen_corpus<- tokens_remove(sunscreen_corpus, pattern= my_stop_words, padding=FALSE)


## 1. method = jaccard

summary(sunscreen_corpus)
dfmat <- dfm(sunscreen_corpus,
             remove_punct = TRUE, remove = stopwords("english"))
View(dfmat)
dfmat[,1:5]
(tstat1 <- textstat_simil(dfmat, method = "jaccard", margin = "documents"))

sum((dfmat[1,] > 0) & (dfmat[2,] > 0)) # common words=25
sum((dfmat[1,] > 0) | (dfmat[2,] > 0)) # used words (unique)=221
tstat1[1,2]
## 25/221 = tstat1[1,2]

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

## #####################
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
