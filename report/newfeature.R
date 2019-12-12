sentiment_review<-sunscreen_cleaned_for_wordcloud %>%
  inner_join(sunscReen::get_sunsentiments("sunscReen"), by="word") %>%
  group_by(reviewId, sentiment) %>%
  count()

levels(sentiment_review$sentiment)
positive<-c("comfort","joy","trust")
negative<-c("anger", "disgust","fear","sadness","discomfort")


sentiment_review<-sentiment_review%>%
  mutate(senplus=ifelse(sentiment %in% positive, n, 0 ))%>%
  mutate(senmoins=ifelse(sentiment %in% negative, -n, 0 ))

sentiment_per_text<-sentiment_review%>%select(reviewId, senmoins, senplus)%>%
  group_by(reviewId)%>%
  summarize(sentimenttext= sum(senmoins+senplus))


