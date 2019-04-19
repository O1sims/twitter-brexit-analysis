library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")



prebrexit.tweets.df <- news.tweets.df %>% 
  subset(ymd.datetime. >= "2016-06-23")


text <- ""
for (i in 1:nrow(prebrexit.tweets.df)) {
  text %<>% paste0(" ", prebrexit.tweets.df$text[i])
}

docs <- Corpus(VectorSource(text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c(
  "tco", "now", "can", "https", "http", "still", "say", "said", "issueone", "one", "newtonemerson", "just", "minister", "also", "amp", "will", "says")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(
  words = d$word, 
  freq = d$freq, 
  min.freq = 1,
  max.words = 100, 
  random.order = FALSE, 
  rot.per = 0.35, 
  colors = brewer.pal(8, "Dark2"))
