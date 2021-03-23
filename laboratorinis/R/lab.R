library(tidyverse)

data <- read_csv("../data/lab_sodra.csv")
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
    scale_x_continuous(breaks = seq(0, 6000, 1000)) +
    labs(x = "Vidutinis atlyginimas", y = "Daznis", title = "Vidutinio atlyginimo pasiskirstymas")

ggsave("../img/plot1.png")
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

top5 <- mydata %>% subset(name %in% top5names) # saugo 5-iu imoniu duomenis su max avgWage

top5 %>%
    mutate(month = as.numeric(substr(month, 5, 7))) %>%
    filter(!is.na(avgWage)) %>%
    ggplot(aes(x = month, y = avgWage, group = name, color = name)) +
    geom_line(size = 1) +
    geom_point() +
    scale_x_continuous(breaks = 1:12) +
    theme_light() +
    labs(x = "Menuo", y = "Vidutinis atlyginimas", color = "Pavadinimas", 
         title = "Vidutinio atlyginimo pokytis")

ggsave("../img/plot2.png")
dev.off()

### 3 uzduotis

top5 %>%
    group_by(name) %>%
    arrange(desc(numInsured)) %>%
    slice_head(n = 1) %>%
    ggplot(aes(x = (reorder(name,-numInsured)), y = numInsured, fill = name)) +
    geom_bar(stat = 'identity', show.legend = FALSE) +
    geom_text(aes(label = numInsured), vjust = -0.3, size = 3.5) +
    theme_classic() +
    labs(x = "Pavadinimas", y = "Apdraustuju skaicius", 
         title = "Didziausias apdraustu darbuotoju skaicius")

ggsave("../img/plot3.png")
dev.off()