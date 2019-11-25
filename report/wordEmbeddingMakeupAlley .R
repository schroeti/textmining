## document centroid representation
library(text2vec)
library(quanteda)

sunscreen.tok <- tokens(sunscreen$review, remove_numbers=TRUE, remove_punct=TRUE)
sunscreen.tok <- tokens_remove(sunscreen.tok, stopwords("english"))
my_stop_words <-  my_stop_words <- list("#", "s", "ve", "re", "skin", "sunscreen", "product","spf", "sunblock","quot", "sunscreens", "didn","doesn","don" ,"makes","i","m","t","clinique")
sunscreen.tok<- tokens_remove(sunscreen.tok, pattern= my_stop_words, padding=FALSE)

sunscreen.dat <- paste(unlist(sunscreen_corpus), collapse=" ")

tokens <- space_tokenizer(sunscreen.dat)
it = itoken(tokens, progressbar = TRUE)
vocab <- create_vocabulary(it)

vocab <- prune_vocabulary(vocab)

# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
glove$fit_transform(tcm, n_iter = 20)
word_vectors <- glove$components

## looking for the dov2vec representation, by hand
sunscreen.doc <- matrix(nr=nrow(word_vectors), nc=length(sunscreen.tok))

colnames(sunscreen.doc) <- names(sunscreen.tok)

#Somehow sometimes out of bound sometimes not wtf 

for (i in c(1:1062)){
  sunscreen.doc[,i]<-apply(word_vectors[,sunscreen.tok[[i]]],1,mean(na.rm=T) )
}
# sunscreen.doc




sunscreen_dfm<-sunscreen.tok%>%dfm()
sunscreen.tfidf <- dfm_tfidf(sunscreen_dfm)

num_topics <- 15
sunscreen.lsa <- textmodel_lsa(sunscreen.tfidf, nd = num_topics, margin = "documents")

# scree plot
plot(sunscreen.lsa$sk / sum(sunscreen.lsa$sk) * 100, ylab = "Percentage of variance", xlab = "Components")

#plausible k are 2,3 and 4
cos_sim <- function(x, y) {
  return(t(as.numeric(x))%*%as.numeric(y)/(sqrt(sum(x^2))*sqrt(sum(y^2))))
}

# This function searches for similar words to the one provided,
# by computing the cosine distance in the embedded space.
# Note that `df` should be a matrix with word vectors as rows
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

get_similar("pricy", sunscreen.lsa$features)

get_similar("japanese", df = sunscreen.lsa$features)

get_similar("asian", sunscreen.lsa$features)


top50 <- names(get_similar("grail", sunscreen.lsa$features, 50))[2:50]
distances <- textstat_dist(as.dfm(sunscreen.lsa$features)[top50,])
clustering <- hclust(as.dist(distances))
cluster.assignments = cutree(clustering, k = 3)
clustering%>%plot()

##Word2vec

library(rword2vec)

# save to file


fn <- paste0("w2v_train_", format(Sys.time(), "%Y-%m-%d_%H:%M"))
cat(paste(sunscreen_corpus, collapse = ' '), file = paste0(fn, '.tmp'))

# fit word2vec -- this can take a long time depending on your machine
word.size <- 50

w2v.model <- word2vec(
  train_file = paste0(fn, '.tmp'),
  output_file = paste0(fn, '.bin'),
  binary = 1,
  num_threads = 4,
  debug = 1,
  cbow = FALSE,
  layer1_size = word.size
)


# Retrieve the word vectors
library(readr)
invisible(bin_to_txt(paste0(fn, '.bin'), paste0(fn, '.wv')))
sunscreen.w2v <- read.csv(paste0(fn, '.wv'), skip = 1, sep = ' ',
                        stringsAsFactors = FALSE,
                        col.names = c("word", paste0("X", 1:word.size)))
sunscreen.w2v <- sunscreen.w2v[complete.cases(sunscreen.w2v),]
rownames(sunscreen.w2v) <- sunscreen.w2v$word
sunscreen.w2v$word <- NULL

#GloVe

sunscreen.fcm <- fcm(sunscreen_corpus, 
                   context = "window",
                   count = "weighted",
                   weights = 1/(1:5),
                   tri = TRUE)

library(text2vec)
glove <- GlobalVectors$new(word_vectors_size = 25, vocabulary = featnames(sunscreen.fcm), x_max = 10)
sunscreen.glove <- as.dfm(fit_transform(sunscreen.fcm, glove, n_iter = 10) + t(glove$components))

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
