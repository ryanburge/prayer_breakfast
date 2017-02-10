long <- read.csv("long.csv")

ggplot(long, aes(x=year, y=words)) + geom_line(colour = "black", size = 1) + geom_point(colour = "black") + 
  annotate("rect", xmin = 1969, xmax = 1975, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  +
  annotate("rect", xmin = 1975, xmax = 1975.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  +
  annotate("rect", xmin = 1975.45, xmax = 1979.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "blue")  +
  annotate("rect", xmin = 1979.45, xmax = 1989, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  +
  annotate("rect", xmin = 1989, xmax = 1992.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  +
  annotate("rect", xmin = 1992.45, xmax = 2000.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "blue")  + 
  annotate("rect", xmin = 2000.5, xmax = 2008.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "red")  + 
  annotate("rect", xmin = 2008.45, xmax = 2016.4, ymin = 0, ymax = Inf,  alpha = .2, fill = "blue")  + 
  annotate("rect", xmin = 2016.45, xmax = 2017.5, ymin = 0, ymax = Inf,  alpha = .2, fill = "red") + 
  ggtitle("                        Number of Words in Annual Prayer Breakfast Addresses") + 
  xlab("Year") + ylab("Number of Words") + 
  annotate("text", x = 2010, y = 50, label = "religioninpublic.blog", size = 5) + 
  theme(text=element_text(size=16, family="KerkisSans"))

long %>% group_by(party) %>% summarize(mean = mean(words, na.rm= TRUE))

bar <- long %>% group_by(speaker) %>% summarize(mean = mean(words, na.rm= TRUE), sum = sum(words, na.rm = TRUE))

ggplot(bar, aes(x=speaker, y=mean, fill = party)) + geom_col()

