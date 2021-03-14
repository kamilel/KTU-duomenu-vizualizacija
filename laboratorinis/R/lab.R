library(tidyverse)

data <- read_csv("data/lab_sodra.csv")
mydata <- data %>% filter(ecoActCode == 494100)
str(mydata)
summary(mydata)

### 1 uzduotis

mydata %>%
    ggplot(aes(x = avgWage)) +
    geom_histogram(binwidth = 100,
                   col = "white",
                   fill = "coral") +
    theme_light() +
    labs(x = "Average wage", y = "Count", title = "Average wage distribution")

ggsave("img/plot1.png")
dev.off()

### 2 uzduotis

top5names <- mydata %>% # saugo 5-iu imoniu names su max avgWage
    group_by(name) %>%
    arrange(desc(avgWage)) %>% 
    slice_head(n = 1)%>% 
    ungroup %>% 
    arrange(desc(avgWage)) %>%
    slice_head(n = 5) %>%
    pull(name)

top5 <- mydata %>% subset(name %in% top5names)

top5 %>%
    mutate(month = as.numeric(substr(month, 5, 7))) %>%
    filter(!is.na(avgWage)) %>%
    ggplot(aes(x = month, y = avgWage, group = name, color = name)) +
    geom_line(size = 1) +
    geom_point() +
    scale_x_continuous(breaks = 1:12) +
    theme_light() +
    labs(x = "Month", y = "Average wage", color = "Name", 
         title = "Average wage in 2020")

ggsave("img/plot2.png")
dev.off()

### 3 uzduotis

top5 %>%
    group_by(name) %>%
    arrange(desc(numInsured)) %>%
    slice_head(n = 1) %>%
    ggplot(aes(x = name, y = numInsured, fill = name)) +
    geom_bar(stat = 'identity', show.legend = FALSE) +
    geom_text(aes(label = numInsured), vjust = -0.3, size = 3.5) +
    theme_classic() +
    labs(x = element_blank(), y = "Number insured", 
         title = "Maximum number of insured employees")

ggsave("img/plot3.png")
dev.off()