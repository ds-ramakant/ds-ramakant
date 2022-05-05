library(httr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(scales)
library(tidyr)
theme_set(theme_light())


url <- "https://www.nytimes.com/games/wordle/main.18637ca1.js"
wordle_script_text <- GET(url) %>% 
  content(as = "text", encoding = "UTF-8")
word_list = substr(
  wordle_script_text,
  # cigar is the first word
  str_locate(wordle_script_text, "cigar")[,"start"],
  # shave is the last word
  str_locate(wordle_script_text, "shave")[,"end"]) %>%
  str_remove_all("\"") %>%
  str_split(",") %>%
  data.frame() %>%
  select(word = 1) %>%
  mutate(word = toupper(word))


letter_list <- word_list %>%
  as.character() %>%
  str_split("") %>% 
  as.data.frame() %>% 
  select(w_letter = 1) %>% 
  filter(row_number()!=1) %>%
  filter(w_letter %in% LETTERS) %>% 
  mutate(type = case_when(w_letter %in% c("A","E","I","O","U") ~ "vowel",
                          T ~ "consonant")) %>% 
  group_by(w_letter, type) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

#appearance of all letters 
letter_list %>% ungroup() %>% 
  ggplot(aes(x = reorder(w_letter, -freq), y = freq))+
  geom_col(aes(fill = type))+
  scale_y_continuous(labels = comma)+
  geom_text(aes(label = freq), 
            #position = position_dodge(width = 0.3),
            size = 3)+
  labs(x = "Letter", y = "Frequency",
       title = "Frequency of words in Official Wordle list")


position_word_list <- word_list %>% 
  separate(word, 
           sep = "", 
           into = c("x","p1","p2","p3","p4","p5")) %>% 
  select(-x)
  
