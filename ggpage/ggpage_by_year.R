prayer <- read_csv("bkfast.csv")

sent <- prayer %>% 
  filter(year == "2017") %>% 
  unnest_tokens(text, text, token = "sentences") 

cols <- c("All Other" = "grey", "Jesus" = "darkblue", "Christ" = "darkred", "God or Lord" = "darkorchid", "Prayer" = "darkgreen")

sent %>%
  ggpage_build(wtl = FALSE, y_space_pages = 0, ncol = 3) %>%
  mutate(highlight = case_when(word %in% c("jesus") ~ "Jesus",
                               word %in% c("christ") ~ "Christ",
                               word %in% c("god", "God", "lord") ~ "God or Lord",
                               word %in% c("pray", "praying", "prayer") ~ "Prayer",
                               TRUE ~ "All Other")) %>%
  ggpage_plot(mapping = aes(fill = highlight)) +
  scale_fill_manual(values = cols) +
  labs(title = "Obama's Speech in 2009", caption = "Data: American Presidency Project", 
       fill = NULL) + bar_rb() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.title = element_text(size = 44, hjust = 0.45)) +
  theme(plot.caption = element_text(hjust = 2.2))


ggsave(file="D://prayer_breakfast/ggpage_obama_final_09.png", type = "cairo-png", width = 20, height =12)