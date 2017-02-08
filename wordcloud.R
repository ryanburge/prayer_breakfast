library(wordcloud2)
library(tm)

trump <- filter(prayer, speaker == "Trump")

wordCorpus <- Corpus(VectorSource(trump$text))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)

dtm <- DocumentTermMatrix(wordCorpus,
                          control=list(wordLengths=c(1, Inf),
                                       bounds=list(global=c(floor(length(wordCorpus)*0.005), Inf))))
dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)


trump <- as.data.frame(as.table(frequency))

trump <- trump[-c(1, 4, 41, 59, 60, 104, 105 ),]

trump$word <- trump$Var1
trump$freq <- trump$Freq

trump <- filter(trump, freq >=3)

wordcloud2(data = trump)