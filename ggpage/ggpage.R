library(tidyverse)
library(ggpage)
library(tidytext)
library(extrafont)


# prayer <- read.csv("https://raw.githubusercontent.com/ryanburge/prayer_breakfast/master/prayer_breakfast.csv")
# prayer_breakfast <- read_csv("prayer_breakfast.csv")
prayer <- read_csv("bkfast.csv")

sent <- prayer %>% 
  filter(year == "2018") %>% 
  unnest_tokens(text, text, token = "sentences") 

cols <- c("All Other" = "grey", "Jesus" = "darkblue", "Christ" = "darkred", "God or Lord" = "darkorchid", "Prayer" = "darkgreen")

sent %>%
  ggpage_build() %>%
  mutate(highlight = case_when(word %in% c("jesus") ~ "Jesus",
                               word %in% c("christ") ~ "Christ",
                               word %in% c("god", "God", "lord") ~ "God or Lord",
                               word %in% c("pray", "praying", "prayer") ~ "Prayer",
                               TRUE ~ "All Other")) %>%
  ggpage_plot(mapping = aes(fill = highlight)) +
  scale_fill_manual(values = cols) +
  labs(title = "Trump's Speech in 2018", caption = "Data: American Presidency Project", 
       fill = NULL) + bar_rb() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.title = element_text(size = 44, hjust = 0.45)) +
  theme(plot.caption = element_text(hjust = 2.2))


ggsave(file="D://prayer_breakfast/ggpage_trump_final_18.png", type = "cairo-png", width = 20, height =12)

##Obama

obama <- prayer %>% filter(year == 2016)
sent <- obama %>% unnest_tokens(text, text, token = "sentences") 


obama <- obama %>% mutate(text = as.character(text))
test <- sent$text
test <- as.tibble(test) %>% mutate(speech = c("Obama")) %>% rename(text = value)

sent %>%
  ggpage_build() %>%
  mutate(highlight = case_when(word %in% c("jesus") ~ "Jesus",
                               word %in% c("christ") ~ "Christ",
                               word %in% c("god", "God", "lord") ~ "God or Lord",
                               word %in% c("pray", "praying", "prayer") ~ "Prayer",
                               TRUE ~ "All Other")) %>%
  ggpage_plot(mapping = aes(fill = highlight)) +
  scale_fill_manual(values = cols) +
  labs(title = "Obama's Speech in 2016", caption = "Data: American Presidency Project", 
       fill = NULL) + bar_rb() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.title = element_text(size = 44, hjust = 0.45)) +
  theme(plot.caption = element_text(hjust = 2.2))


ggsave(file="D://prayer_breakfast/ggpage_obama_16.png", type = "cairo-png", width = 20, height =12)


