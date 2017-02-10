#### Found the syntax here: 
#### http://rstudio-pubs-static.s3.amazonaws.com/149291_bafd9f23bdee4a75a5726ad93394ec64.html


library(tm)
library(dplyr)
library(xtable)
library(wordcloud)


compare <- head(prayer, 2)
docs <- Corpus(VectorSource(compare$text)) %>%
tm_map(removePunctuation) %>%
tm_map(removeNumbers) %>%
tm_map(tolower)  %>%
tm_map(removeWords, stopwords("english")) %>%
tm_map(stripWhitespace) %>%
tm_map(PlainTextDocument)
tdm <- TermDocumentMatrix(docs) %>%
as.matrix()
colnames(tdm) <- c("Trump","Obama")
print(xtable(head(tdm)), type="html")
par(mfrow=c(1,1))
comparison.cloud(tdm, random.order=FALSE, colors = c("indianred3","lightsteelblue3"),
title.size=1.5, max.words=200)


commonality.cloud(tdm, random.order=FALSE, scale=c(5, .5),colors = brewer.pal(4, "Dark2"), max.words=200)