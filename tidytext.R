library(tidytext)
library(tidyr)
prayer$text <- as.character(prayer$text)


tidy_speech <- prayer %>% 
  unnest_tokens(word, text)

data("stop_words")
cleaned_speech <- tidy_speech %>%
  anti_join(stop_words)

cleaned_speech %>%
  count(word, sort = TRUE) 


bing <- get_sentiments("bing")

speech_sentiment <- tidy_speech %>%
  inner_join(bing) %>%
  count(year,  sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(speech_sentiment, aes(x=year, y=sentiment)) + geom_line(colour = "black", size = 1) + geom_point(colour = "black") + 
  annotate("rect", xmin = 1969, xmax = 1975, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  +
  annotate("rect", xmin = 1975, xmax = 1975.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  +
  annotate("rect", xmin = 1975.45, xmax = 1979.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "blue")  +
  annotate("rect", xmin = 1979.45, xmax = 1989, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  +
  annotate("rect", xmin = 1989, xmax = 1992.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  +
  annotate("rect", xmin = 1992.45, xmax = 2000.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "blue")  + 
  annotate("rect", xmin = 2000.5, xmax = 2008.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  + 
  annotate("rect", xmin = 2008.45, xmax = 2016.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "blue")  + 
  annotate("rect", xmin = 2016.45, xmax = 2017.5, ymin = 0, ymax = Inf,  alpha = .2, fill = "red") + 
  ggtitle("                        Sentiment in Annual Prayer Breakfast Addresses") + 
  xlab("Year") + ylab("Total Positive Sentiment")