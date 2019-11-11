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

dtotal <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)
#-----------------------------------------------------------------------#

## dtotal -- corpus
library(tm)
dtotal <- tibble(text=dtotal$comment)
dtot.tok <- dtotal%>% unnest_tokens(word, text, to_lower=TRUE)
dtot.cp <- VCorpus(VectorSource(dtot.tok$word))
dtot.cp <- tm_map(dtot.cp, removeWords, stopwords("english"))
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
dtot.top502 <- top_n(dtot.tok, n=50)
dtot.top503 <- top_n(dtot.cp, n=50)

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

#dfmat
library(quanteda)

n=1

dd <- dtotal %>%
  select(title, comment)%>%
  for (title in dtotal) {
    if (title[i] = title[i-1])
    {mutate(review = n)}
    else
    {mutate(review = n+1)}
  }

dfmat <- quanteda::dfm(dtotal$comment)


