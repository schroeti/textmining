library(caret)
library(data.table)
library(ggforce)
library(ggplot2)
library(ggwordcloud)
library(kableExtra)
library(keras)
library(lexicon)
library(magrittr)
library(purrr)
library(quanteda)
library(RColorBrewer)
library(readr)
library(RSentiment)
library(rword2vec)
library(sentimentr)
library(sunscReen)
library(textdata)
library(textstem)
library(text2vec)
library(tidytext)
library(tidyverse)
library(topicmodels)
library(wordcloud)


##########Sentiments:

get_sunsentiments <- function(lexicon = c("sunscReen")) {
  lexicon <- match.arg(lexicon)
  
  sunscReen = lexicon_sunscReen()
}


lexicon_sunscReen <- function() {
  readRDS("../data/sunscReen.rds") 
}

#Loading the data set. Make sure you are in the right working repository 
setwd("data")
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
my_stop_words <- c(word = c("#", "s", "ve", "re", "skin", "sunscreen", "product","spf",
                            "sunscreen","sunscreens","t","it","It","use"), c(stopwords::stopwords("en")))%>%as_tibble()
my_stop_words$word<-my_stop_words$value

#We create a review2 column that remains untouched and we tokenize review
#We then take away the stop words

sunscreen_cleaned <- sunscreen %>%
  mutate(review2 = review) %>%
  as.tibble() %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  filter(is.na(as.numeric(word)))

#tf-idf: we want to see if some words look specific to a review or to a product. 
tf_byreview<- sunscreen_cleaned %>%
  group_by(reviewId) %>%
  count(word) %>%
  ungroup()

tfidf_byreview<-tf_byreview%>%
  tidytext::bind_tf_idf(word, reviewId, n )

tfidf_byreview%>%
  head()%>%
  kable()

tf_byproduct <- sunscreen_cleaned %>%
  group_by(productName) %>%
  count(word) %>%
  ungroup()

tfidf_product<-tf_byproduct%>%
  bind_tf_idf(word, productName, n )


#Interpretation: 
#Words that are specific to reviews we have but tfidf very little by product so we can't really caracterize
#tfidf maximum would be : 


#Sentiments wuth valence shifters

for (i in c(1:length(sunscreen$review))){
  sunscreen$sentiments[i]<-sentiment_by(sunscreen$review[i],
                                        hash_valence_shifters)
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


#Try per brand

my_stop_words<-as_tibble(my_stop_words)
my_stop_words$word<-my_stop_words$value

sunscreen_cleaned_for_wordcloud <- sunscreen %>%
  dplyr::mutate(review2 = review) %>%
  dplyr::group_by(brandName)%>%
  as.tibble() %>%
  tidytext::unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  dplyr::filter(is.na(as.numeric(word)))

#####################################################

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
  mutate(norm=n/sum(n))

sentiment_normalized_perbrand<-sentiment_by_brand.2%>%
  group_by(brandName)%>%
  mutate(norm=n/sum(n))


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

sunscreen_cleaned_for_wordcloud_product <- sunscreen %>%
  dplyr::mutate(review2 = review) %>%
  dplyr::group_by(productName)%>%
  as.tibble() %>%
  tidytext::unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  dplyr::filter(is.na(as.numeric(word)))

sentiment_by_brand_product.2 <- sunscreen_cleaned_for_wordcloud_product %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(productName, sentiment) %>%
  count()

sentiment_by_brand_sun_product<-sunscreen_cleaned_for_wordcloud_product %>%
  inner_join(sunscReen::get_sunsentiments("sunscReen"), by="word") %>%
  group_by(productName, sentiment) %>%
  count()

sentiment_normalized_perbrand_sun_product<-sentiment_by_brand_sun_product%>%
  group_by(productName)%>%
  mutate(norm=n/sum(n))

sentiment_normalized_perbrand_product<-sentiment_by_brand_product.2%>%
  group_by(productName)%>%
  mutate(norm=n/sum(n))


plot_sentiment_page_product.2 <- function(p){
  ggplot(sentiment_by_brand_product.2, aes(x = reorder(productName, -n), y = n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ggforce::facet_wrap_paginate(facets = ~ sentiment, nrow = 3, ncol = 3, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

plot_sentiment_pagenormalized_product.2 <- function(p){
  ggplot(sentiment_normalized_perbrand_product, aes(x = reorder(productName, -n), y = norm, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ggforce::facet_wrap_paginate(facets = ~ sentiment, nrow = 3, ncol = 3, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

plot_sentiment_page_sun_product <- function(p){
  ggplot(sentiment_by_brand_sun_product, aes(x = reorder(productName, -n), y = n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ggforce::facet_wrap_paginate(facets = ~ sentiment, nrow = 3, ncol = 3, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

plot_sentiment_pagenormalized_sun_product<- function(p){
  ggplot(sentiment_normalized_perbrand_sun_product, aes(x = reorder(productName, -n), y = norm, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ggforce::facet_wrap_paginate(facets = ~ sentiment, nrow = 3, ncol = 3, page = p) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "", y = "Number of words", fill = "Sentiment")
}

plot_sentiment_page_product.2(1)%>%plot()
plot_sentiment_pagenormalized_product.2(1)%>%plot()

plot_sentiment_page_sun_product(1)%>%plot()
plot_sentiment_pagenormalized_sun_product(1)%>%plot()


#########################################################
##SIMILARITIES:

##STILL WANT TO ADD A TAG FOR BRAND / PRODUCT 


dfmat <- dfm(sunscreen_corpus,
             remove_punct = TRUE, tolower=TRUE, remove = stopwords("english"))
dfmat<-dfm(sunscreen_corpus,
           remove_punct = TRUE, remove = my_stop_words$word)

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

 #method = cosine
(tstat2 <- textstat_simil(dfmat, method = "cosine", margin = "documents"))
sum(dfmat[1,]*dfmat[2,])/sqrt(sum(dfmat[1,]^2)*sum(dfmat[2,]^2))


par(mfrow=c(1,1))

plot(hclust(as.dist(1-tstat2)))


#######################################################
# LDA



## convert quateda object to topicmodels object
dtm <- convert(dfmat, to = "topicmodels")
lda <- LDA(dtm, k = 10) # build 10 topics
terms(lda, 5) # see the 5 terms most associated with each topic
topics(lda, 5) # see the 5 topics most associated with each documents

## Extract the beta and gamma from topicmodels object
lda@beta[,1:10] # 1 first words
lda@gamma

# Save beta and gamma results and do matrix multiplication between the 2 matrices

beta <- lda@beta[,1:10]
gam <- lda@gamma

mlda <- gam %*% beta

## show the betas of each document

beta.td <- tidy(lda, matrix = "beta")
beta.td

filter(beta.td, topic==1) ## all for topic 1

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

gamma.td %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

## Assignment of topic to term in each document
augment(lda)

#####################################################

### WORD EMBEDDING :


#GloVe

sunscreen.fcm <- fcm(sunscreen_corpus, 
                     context = "window",
                     count = "weighted",
                     weights = 1/(1:5),
                     tri = TRUE)

get_similar <- function(w, df, k = 10) {
  if(is.character(w)) {
    v <- as.numeric(df[w,])
  } else {
    v <- as.numeric(w)
  }
  nv <- sqrt(sum(v^2))
  m <- as.matrix(df)
  rownorms <- sqrt(rowSums(m^2))
  sims <- m %*% v / (nv * rownorms)
  names(sims) <- rownames(df)
  return(head(sort(sims, decreasing = TRUE), n = k))
}

glove <- GlobalVectors$new(word_vectors_size = 50, vocabulary = featnames(sunscreen.fcm), x_max = 10)
sunscreen.glove <- as.dfm(fit_transform(sunscreen.fcm, glove, n_iter = 100) + t(glove$components))

get_similar(sunscreen.glove["lotion",] - sunscreen.glove["liquid",] + sunscreen.glove["cream",], sunscreen.glove)

top50 <- names(get_similar("grail", sunscreen.glove, 50))[2:50]
distances <- textstat_dist(sunscreen.glove[top50,])
clustering <- hclust(as.dist(distances))
cluster.assignments = cutree(clustering, k = 3)
plot(clustering)
#pca 
pca <- prcomp(sunscreen.glove[top50,])
plot(pca$x[,1:2], col='white')
text(pca$x[,1:2], labels = rownames(pca$x))

word_vectors <- glove$components

sun2.doc <- matrix(nr=nrow(word_vectors), nc=length(sunscreen_corpus))
colnames(sun2.doc) <- names(sunscreen_corpus)
for (i in 1:length(sunscreen_corpus)){
  sun2.doc[,i] <- apply(word_vectors[,sunscreen_corpus[[i]], drop = F], 1, mean)
}

sun2.doc <- as.matrix(sun2.doc)

sun2.doc <- t(sun2.doc)

sun2.doc <- as.data.frame(sun2.doc) 

sun2.doc <- as.data.frame(sun2.doc) %>% mutate(text = 1:nrow(sun2.doc))





################# 

# FOR SUPERVISED LEARNING FEATURE

sentiment_review <- sunscreen_cleaned_for_wordcloud %>%
  inner_join(sunscReen::get_sunsentiments("sunscReen"), by = "word") %>%
  group_by(reviewId, sentiment) %>%
  count()

positive <- c("comfort", "joy", "trust")
negative <- c("anger", "disgust", "fear", "sadness", "discomfort")


sentiment_review <- sentiment_review %>%
  mutate(senplus = ifelse(sentiment %in% positive, n, 0)) %>%
  mutate(senmoins = ifelse(sentiment %in% negative,-n, 0))

sentiment_per_text <-
  sentiment_review %>% select(reviewId, senmoins, senplus) %>%
  group_by(reviewId) %>%
  summarize(sentimenttext = sum(senmoins + senplus))


saveRDS(sentiment_per_text, file="../data/sentiment_per_text.RData")

#########################################



# Join sun2.doc and sunscreen

brands <- c("australian", "biore", "bioré", "cerave", "clinique", "roche", "posay", "neutrogena", "paula")


brands_dummy <- data.frame(matrix(NA, nrow = nrow(sunscreen), ncol = length(brands)))

for(i in 1:length(brands)){
  brands_dummy[,i] <- ifelse(str_detect(sunscreen$review, brands[i]),1,0)
  names(brands_dummy)[i] <- paste("d_", brands[i])
}

sunscreen.numbered <- sunscreen %>% cbind(brands_dummy) %>%  mutate(text = 1:nrow(sunscreen))  




mlda.numbered <- mlda %>% as.data.frame() %>%  mutate(text = 1:nrow(mlda)) %>% 
  rename_at(vars(starts_with("V")), 
            funs(str_replace(., "V", "G")))

sun2.mlda <- inner_join(sun2.doc, mlda.numbered, by = "text")



sunscreen.sl.rating <- inner_join(sunscreen.numbered, sun2.mlda, by = "text") %>%
  mutate(nwords = sapply(strsplit(review, " "), length)) %>%
  select(helpful, reviewId, votes, skinType, skinTone, brandId, starts_with("V"),starts_with("G"), starts_with("d_"),nwords, rating) %>%
  mutate(rating = as.factor(rating))

sunscreen.sl.rating <- inner_join(sentiment_per_text,sunscreen.sl.rating, by = "reviewId") %>%
  select(-reviewId)


sunscreen.sl.brand <- inner_join(sunscreen.numbered, sun2.mlda, by = "text") %>% 
  mutate(nwords = sapply(strsplit(review, " "), length)) %>% 
  select(reviewId, sentiments,  starts_with("V"),starts_with("G"),starts_with("d_"),nwords, brandId) %>% 
  mutate(brandId = as.factor(brandId))

sunscreen.sl.brand <- inner_join(sentiment_per_text,sunscreen.sl.brand, by = "reviewId") %>% 
  select(-reviewId) 


# 
# #############################################################
# # SUPERVISED LEARNING
# # Predict brand
# 
# ggplot(sunscreen.sl.rating, aes(x = rating)) +
#  geom_histogram(stat = "count")
# 
# 
# # Keras
# 
# 
# index<-createDataPartition(sunscreen.sl.rating$rating,p=0.75,list=F)
# Train_Features <- data.matrix(sunscreen.sl.rating[index,-ncol(sunscreen.sl.rating)])
# Train_Labels <- sunscreen.sl.rating[index,ncol(sunscreen.sl.rating)]
# Test_Features <- data.matrix(sunscreen.sl.rating[-index,-ncol(sunscreen.sl.rating)])
# Test_Labels <- sunscreen.sl.rating[-index,ncol(sunscreen.sl.rating)]
# 
# to_categorical(Train_Labels)[,c(-1)] -> Train_Labels
# to_categorical(Test_Labels)[,c(-1)] -> Test_Labels
# 
# # 
# # Train_Features <- as.matrix(apply(Train_Features, 2, function(x) (x-min(x))/(max(x) - min(x)))) 
# # Test_Features <- as.matrix(apply(Test_Features, 2, function(x) (x-min(x))/(max(x) - min(x))))
# # 
# 
# 
# model <- keras_model_sequential()
# model %>%
#   layer_dense(units= 106, activation = "relu",input_shape = ncol(Train_Features)) %>%
#   layer_dense(units = 50, activation = "relu") %>%
#   layer_dense(units = 30, activation = "relu") %>%
#   layer_dense(units = 10, activation = "relu") %>%
#   layer_dense(units = 5, activation = "softmax")
# 
# 
# model %>% compile(loss = "categorical_crossentropy",
#                   optimizer = 'adam',
#                   metrics = c('accuracy'))
# history <-
#   model %>% keras::fit(
#     Train_Features,
#     Train_Labels,
#     validation_split = 0.15,
#     epochs = 200,
#     batch_size = 100,
#     shuffle = T
#   )
# 
# 
# score <- model %>% evaluate(Test_Features, Test_Labels, batch_size = 25)
# 
# # Print the loss and accuracy metrics
# print(score)
# summary(model)
# 
# 
# #RF
# 
# library(randomForest)
# index<-createDataPartition(sunscreen.sl.rating$rating,p=0.75,list=F)
# train.data  <- sunscreen.sl.rating[index,]
# trainClasses <- sunscreen.sl.rating[index, ncol(train.data)]
# test.data <- sunscreen.sl.rating[-index, ]  
# testClasses <- sunscreen.sl.rating[-index, ncol(test.data)]
# 
# set.seed(9560)
# 
#      
# 
# table(up_train$Class)
# 
# 
# 
# #From the results, we see that 500 trees yield the same result as 3000. Therefore, having Occam's razor in mind, we decide to go for the simpler model. 
# system.time(rf_500 <- caret::train(Class ~ ., data = up_train, method = "rf", ntree = 500,preProcess = "range"))
# cm.rf_500 <- confusionMatrix(predict(rf_500, test.data[,-ncol(test.data)]), test.data$rating)
# print(cm.rf_500)
# 
# 

## Brand prediction
library(randomForest)

set.seed(1000)

index_b<-createDataPartition(sunscreen.sl.brand$brandId,p=0.75,list=F)
train.data  <- sunscreen.sl.brand[index_b,]
trainClasses <- sunscreen.sl.brand[index_b, ncol(train.data)]
test.data <- sunscreen.sl.brand[-index_b, ]  
testClasses <- sunscreen.sl.brand[-index_b, ncol(test.data)]



#From the results, we see that 500 trees yield the same result as 3000. Therefore, having Occam's razor in mind, we decide to go for the simpler model. 
system.time(rf_500_brand <- caret::train(brandId ~ ., data = train.data, method = "rf", ntree = 100,preProcess = "range"))
cm.rf_500_brand <- confusionMatrix(predict(rf_500_brand, test.data[,-ncol(test.data)]), test.data$brandId)
print(cm.rf_500_brand)



Train_Features <- data.matrix(sunscreen.sl.brand[index_b,-ncol(sunscreen.sl.brand)])
Test_Features <- data.matrix(sunscreen.sl.brand[-index_b,-ncol(sunscreen.sl.brand)])
keras.dum <- model.matrix(~0+sunscreen.sl.brand$brandId)

Train_Labels <- keras.dum[index_b,]
Test_Labels <- keras.dum[-index_b,]
  


# 
# Train_Features <- as.matrix(apply(Train_Features, 2, function(x) (x-min(x))/(max(x) - min(x)))) 
# Test_Features <- as.matrix(apply(Test_Features, 2, function(x) (x-min(x))/(max(x) - min(x))))
# 


model <- keras_model_sequential()
model %>%
  layer_dense(units = 38, activation = "relu") %>%
  layer_dense(units = 20, activation = "relu") %>%
  layer_dense(units = 15, activation = "relu") %>%
  layer_dense(units = 7, activation = "softmax")


model %>% compile(loss = "categorical_crossentropy",
                  optimizer = 'rmsprop',
                  metrics = c('accuracy'))
history <-
  model %>% keras::fit(
    Train_Features,
    Train_Labels,
    validation_split = 0.15,
    epochs = 200,
    batch_size = 100,
    shuffle = T
  )


score <- model %>% evaluate(Test_Features, Test_Labels, batch_size = 25)

# Print the loss and accuracy metrics
print(score)
summary(model)





