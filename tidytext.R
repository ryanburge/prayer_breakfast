library(tidytext)

prayer$text <- as.character(prayer$text)


tidy_speech <- prayer %>% 
  unnest_tokens(word, text)

data("stop_words")
cleaned_speech <- tidy_speech %>%
  anti_join(stop_words)

cleaned_speech %>%
  count(word, sort = TRUE) 

library(tidyr)
bing <- get_sentiments("bing")

speech_sentiment <- tidy_speech %>%
  inner_join(bing) %>%
  count(year,  sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
