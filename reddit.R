#install.packages("RedditExtractoR")
library(RedditExtractoR)
library(dplyr)
library(tidytext)
library(readxl)
library(tibble)
library(ggplot2)

#extract the data from reddit
d8 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/d5x4g0/11_sunscreens_for_sensitive_skin_at_low_price/")

d7 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/dkppo4/sun_care_european_high_uva_sunscreens_for/")

d1 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/craf60/selfie_after_a_long_time_of_search_i_have_finally/")

d2 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/btx79r/sun_care_dermatologist_told_me_to_ditch_sunscreen/")

d3 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/c7x8ke/product_question_30_minutes_after_applying/")

d4 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/c096h9/review_me_6_months_ago_sunscreen_is_so_greasy_and/")

d5 <- reddit_content((URL="https://www.reddit.com/r/SkincareAddiction/comments/dmif3o/review_2_skinceutical_sunscreens_and_2_elta_md/"))

d6 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/dcern5/review_the_10_sunscreens_ive_tried_in_my_hg/")

d9 <- reddit_content(URL="https://www.reddit.com/r/SkincareAddiction/comments/c096h9/review_me_6_months_ago_sunscreen_is_so_greasy_and/")

#possible pages-comments that I found:
## https://www.reddit.com/r/SkincareAddiction/comments/craf60/selfie_after_a_long_time_of_search_i_have_finally/
## https://www.reddit.com/r/SkincareAddiction/comments/btx79r/sun_care_dermatologist_told_me_to_ditch_sunscreen/
## https://www.reddit.com/r/SkincareAddiction/comments/c7x8ke/product_question_30_minutes_after_applying/
## https://www.reddit.com/r/SkincareAddiction/comments/c096h9/review_me_6_months_ago_sunscreen_is_so_greasy_and/

d11 <- d1 %>%
  select(id, comment)
d1[3:13, 2]

words <- d1 %>% unnest_tokens(word, comment, to_lower=TRUE)

word.counts <- count(words, word, sort=TRUE)
word.counts

barplot(word.counts$n[1:10], main="Hallelujah word counts", horiz=TRUE,
        names.arg=word.counts$word[1:10]) # simple barplot of the 10 most frequent words
ggplot(word.counts[1:10,], aes(word,n)) + geom_col() + xlab(NULL) + coord_flip() # with ggplot

library(wordcloud)
with(filter(word.counts), wordcloud(word, freq=n, max.words = 100))

####------------ same but with second dataset

d22 <- d2 %>%
  select(id, comment)

#encore ? travailler... stopword et tout
library(tm)
words2 <- d22 %>% unnest_tokens(word, comment, to_lower=TRUE)
words2 <- as.tibble(words2)
words2.cp <- VCorpus(VectorSource(words2$word))
words2.cp <- tm_map(words2.cp, removeWords, stopwords("english"))

word.counts2 <- count(words2, word, sort=TRUE)
word.counts2


#barplot
barplot(word.counts2$n[1:10], main="Hallelujah word counts", horiz=TRUE,
        names.arg=word.counts2$word[1:10]) # simple barplot of the 10 most frequent words
ggplot(word.counts2[1:10,], aes(word,n)) + geom_col() + xlab(NULL) + coord_flip() # with ggplot

#wordcloud
library(wordcloud)
with(filter(word.counts2), wordcloud(word, freq=n, max.words = 100))


