review_text <- paste(trump$text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, removeWords, c("applause", "laughter"))

dtm <- DocumentTermMatrix(wordCorpus,
                          control=list(wordLengths=c(1, Inf),
                                       bounds=list(global=c(floor(length(wordCorpus)*0.01), Inf))))

dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 25)
trump <- as.data.frame(as.table(frequency))
trump$speaker <- c("Trump")
trump$pct <- trump$Freq/1142

review_text <- paste(obama$text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, removeWords, c("applause", "laughter"))

dtm <- DocumentTermMatrix(wordCorpus,
                          control=list(wordLengths=c(1, Inf),
                                       bounds=list(global=c(floor(length(wordCorpus)*0.01), Inf))))

dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 25)

obama <- as.data.frame(as.table(frequency))
obama$speaker <- c("Obama")
obama$pct <- obama$Freq/9185

review_text <- paste(bush43$text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, removeWords, c("applause", "laughter"))

dtm <- DocumentTermMatrix(wordCorpus,
                          control=list(wordLengths=c(1, Inf),
                                       bounds=list(global=c(floor(length(wordCorpus)*0.01), Inf))))

dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 25)

bush43 <- as.data.frame(as.table(frequency))
bush43$speaker <- c("Bush 43")
bush43$pct <- bush43$Freq/4111

review_text <- paste(clinton$text, collapse=" ")
review_source <- VectorSource(review_text)
wordCorpus <- Corpus(review_source)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, removeWords, c("applause", "laughter"))

dtm <- DocumentTermMatrix(wordCorpus,
                          control=list(wordLengths=c(1, Inf),
                                       bounds=list(global=c(floor(length(wordCorpus)*0.01), Inf))))

dtm2 <- as.matrix(dtm)
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency, 25)

clinton <- as.data.frame(as.table(frequency))
clinton$speaker <- c("Clinton")
clinton$pct <- clinton$Freq/5043


freq <- read.csv("freq.csv")
freq$speaker <- factor(freq$speaker, levels=c("Trump", "Obama", "Bush 43", "Clinton"))
ggplot(freq, aes(x=word, y=freq)) + geom_bar(aes(fill=speaker),stat="identity", position= "dodge")

ggplot(freq, aes(x=word, y=freq, group = speaker)) + geom_bar(aes(fill=speaker),stat="identity", position= "dodge")  + 
scale_fill_manual("speaker", values = c("firebrick1","dodgerblue", "firebrick4", "dodgerblue4")) +
ggtitle("       Religious Words Used by Each President at the Prayer Breakfast") +
xlab("Word") + ylab("Percentage of Total Words Spoken") + 
  theme(legend.title=element_blank()) + 
  annotate("text", x = 1.5, y = .035, label = "religioninpublic.blog", size = 5) + 
  theme(text=element_text(size=16, family="KerkisSans"))



