#Rita's tries...
#We have 12 products from MakeupAlley from 7 brands

sunscreen$brandName%>%levels()
sunscreen$productName%>%levels()
#1 We want to add a column with the SPF 
# See if link between SPF and reviews

sunscreen$userName%>%levels()
# 1015 reviewers and 1062 entries so some of the reviewers did many reviews but not enough to analyse
sunscreen$ageRange%>%plyr::count()
# a good variety of ages, might be interesting to predict.
sunscreen$skinType%>%plyr::count()
# interesting to see repartition in each product 
# can we predict grades for each type 

#Sentiment analysis> per product
                #  > Per brand
                # modify the dictionnary if necessary

###FIRST ANALYSIS => PUT SPF out 
spf<-str_extract_all(sunscreen$productName, "\\d+")%>%
  as.data.frame()%>%
  t()

sunscreen<-cbind(sunscreen, spf)
#                => Take off some variables
sunscreen<- subset(sunscreen, 
                   select = -c(brandId, 
                               categoryId,
                               categoryName,
                               thumbnail,
                               hairColor,
                               hairType,
                               hairTexture,
                               eyeColor) )

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

head(sunscreen)
sunscreen[,c(2,4,5,9,10,12,13,14,15)] <- lapply(sunscreen[,c(2,4,5,9,10,12,13,14,15)], factor)

#first word cloud to decide stop words
sunscreen_before_cleaning <- sunscreen %>%
  mutate(review2 = review) %>%
  as.tibble() %>%
  unnest_tokens(word, review) %>%
  filter(is.na(as.numeric(word)))

sunscreen_before_cleaning %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50, colors=brewer.pal(8, "Spectral")))

my_stop_words <- tibble(word = c("#", "s", "ve", "re", "skin", "sunscreen", "product","spf", "sunblock","quot", "sunscreens", "didn","doesn","don" ,"makes","i"))

sunscreen_cleaned_for_wordcloud <- sunscreen %>%
  mutate(review2 = review) %>%
  as.tibble() %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  filter(is.na(as.numeric(word)))

#wordcloud
sunscreen_cleaned_for_wordcloud %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50, colors=brewer.pal(8, "Spectral")))

#Try per brand

sunscreen_cleaned_for_wordcloud <- sunscreen %>%
  mutate(review2 = review) %>%
  dplyr::group_by(brandName)%>%
  as.tibble() %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(my_stop_words, by = "word") %>%
  filter(is.na(as.numeric(word)))

par(mfrow=c(3,3))

brands<-levels(sunscreen_cleaned_for_wordcloud$brandName)%>%as.tibble()

for( i in c(1:nrow(brands))) {
  sunscreen_cleaned_for_wordcloud %>%
    dplyr::select(brandName, review2, word)%>%
    filter(brandName%>%as.character() == brands$value[i] )%>%
    count(word) %>%
    with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Spectral")))
}

#We see some brands are more associated with makeup, with "primer", "makeup", others with 
#ingredients like "zinc" , "niacinamide" .
#Some are more oily than others.. 

  
#Sentiment analysis by brand

#sentiment with nrc: these are by products
sentiment_by_brand.2 <- sunscreen_cleaned_for_wordcloud %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  group_by(brandName, sentiment) %>%
  count()

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

plot_sentiment_page.2(1)%>%plot()
plot_sentiment_pagenormalized.2(1)%>%plot()


get_nrc_sentiment()

mydictionnary <- data.frame("moist", 
                   "dry",
                   "dries",
                   "dried",
                   "oily",
                   "oil",
                   "oilier",
                   "creamy", 
                   "light", 
                   "heavy",
                   "tight",
                   "moisturized",
                   "moisture",
                   "smell",
                   "smelly",
                   "yuck",
                   "scent",
                   "tacky",
                   "tuggy",
                   "grisp",
                   "makeupy", 
                   "gentle", 
                   "gently", 
                   "artificial",
                   
                   )



