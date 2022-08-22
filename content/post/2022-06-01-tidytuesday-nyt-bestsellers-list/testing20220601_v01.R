library(tidyverse)
library(scales)
library(knitr)
library(tidytuesdayR)
library(forcats)
library(lubridate)

data <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')

glimpse(data)

data %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  filter(best_rank==1, year<2020) %>% 
  group_by(decade) %>% 
  summarise(n = n_distinct(title)) %>% 
  ggplot(aes(x = decade, y = n))+
  geom_bar(stat = "identity")

data %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  filter(year<2020, best_rank<6) %>% 
  group_by(decade) %>% 
  ggplot(aes(x = decade, y = total_weeks))+
  geom_boxplot()


data %>% 
  #mutate(decade = factor(10*year %/% 10)) %>% 
  filter(year> 1989) %>% 
  group_by(year) %>% 
  summarise(n = n_distinct(title)) %>% 
  ggplot(aes(x = year, y = n))+
  geom_line()

rank_all <- data %>% 
  #filter(year> 1989) %>% 
  group_by(year) %>% 
  summarise(all_titles = n_distinct(title))

rank_best <-data %>% 
  filter(best_rank ==1) %>% 
  group_by(year) %>% 
  summarise(best_rank_is_1 = n_distinct(title))

table01 <- rank_all %>% 
  left_join(rank_best, by = "year") %>% 
  mutate(success_ratio = best_rank_is_1/all_titles)

#comparing number of titles to the ones that made it top of the charts
table01 %>% 
  ggplot(aes(x = year))+
  geom_line(aes(y = all_titles), color = "red")+
  geom_line(aes(y = best_rank_is_1), color = "blue")+
  scale_x_continuous(breaks = (seq(from = 1930, to = 2020, by = 10)))

#number of best sellers jumped up significantly in the 2000s
#this means that in the 90s there were an avg of 10 best sellers in each each
#but that has changed to 22 during the 2000s and 31 in the 2010s
table01 %>% 
  filter(year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(x = decade, y = best_rank_is_1))+
  geom_boxplot()

##draft table
table01 %>% 
  filter(year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  group_by(decade) %>% 
  summarise(avg_titles = mean(all_titles),
            avg_best = mean(best_rank_is_1))


#success ratio
table01 %>% 
  ggplot(aes(x = year, y = success_ratio))+
  geom_line()+
  scale_x_continuous(breaks = (seq(from = 1930, to = 2020, by = 10)))

#has this had an impact on the shelf life of books?
data %>%
  mutate(decade = factor(10*year %/% 10)) %>%
  summary()


#what is the shelflife of the books? 
data %>% 
  filter(best_rank<6, year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  group_by(decade, best_rank) %>% 
  summarise(n = n_distinct(title)) %>% 
  ggplot(aes(x=decade, y = n))+
  geom_bar(stat = "identity", aes(fill = factor(best_rank)))+
  facet_wrap(.~best_rank)# this plot is not working

#somekind of scatter graph
data %>% 
  filter(best_rank==1, year>1989,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(y = debut_rank, x  = total_weeks))+
  geom_point(aes(color = decade), position = "jitter")
  


data %>% 
  filter(best_rank==1, year>1989,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(x = debut_rank, y  = total_weeks))+
  geom_boxplot(aes(color = decade, group = debut_rank))+
  facet_grid(~decade)

#best graph yet
data %>% 
  filter(best_rank==1,year<2020, total_weeks<150) %>% 
  mutate(decade = factor(10*year %/% 10)) %>% 
  ggplot(aes(x = debut_rank, y  = total_weeks))+
  geom_point(aes(color = decade, group = debut_rank))+
  facet_grid(~decade)

#seasonality

data %>% 
  filter(best_rank==1, year>1989,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10),
         week = week(first_week)) %>% 
  group_by(decade,week) %>% 
  summarise(n = n_distinct(title)) %>% 
  ggplot(aes(x = week, y = n))+
  geom_point(aes(color = decade))

data %>% 
  filter(best_rank<10, year> 1949,year<2020) %>% 
  mutate(decade = factor(10*year %/% 10),
         month = month(first_week, label = T),
         stage = case_when(year<1970 ~ "pre 1970",
                           year>=1970 & year<2000 ~ "1970-1990s",
                           year>= 2000 ~ "2000 onwards",
                           T ~ "x")) %>% 
  group_by(stage,decade,month) %>% 
  summarise(n = n_distinct(title)) %>% 
  mutate(all_titles = ave(n, decade, FUN = sum),
         pct = n/all_titles) %>%  #nifty way to find sum in a seperate grouping
  ggplot(aes(x = month, y = pct))+
  geom_line(aes(color = decade, group = decade), size = 1.5)+
  facet_wrap(~stage)

#almost there
data %>% 
  filter(best_rank<11, year> 2010) %>% 
  mutate(month = month(first_week, label = T),
         stage = case_when(year<=2015 ~ "2011-2015",
                           year> 2015 ~ "2016-2020",
                           T ~ "x")) %>% 
  group_by(stage,year,month) %>% 
  summarise(n = n_distinct(title)) %>% 
  mutate(all_titles = ave(n, year, FUN = sum),
         pct = n/all_titles) %>%  #nifty way to find sum in a seperate grouping
  ggplot(aes(x = month, y = pct))+
  geom_point(aes(color = factor(stage), group = year), 
            size = 2, alpha = 1,
            position = "jitter")


#adding geom_smooth. This works just fine!
data %>% 
  filter(best_rank<11, year> 2010) %>% 
  mutate(month = month(first_week, label = T),
         stage = case_when(year<=2015 ~ "2011-2015",
                           year> 2015 ~ "2016-2020",
                           T ~ "x")) %>% 
  group_by(stage,year,month) %>% 
  summarise(n = n_distinct(title)) %>% 
  mutate(all_titles = ave(n, year, FUN = sum),
         pct = n/all_titles) %>%  #nifty way to find sum in a seperate grouping
  ggplot(aes(x = month, y = pct, group = 1))+
  geom_point(size = 2, 
            alpha = 0.5, position = "jitter")+
  # geom_boxplot(aes(x = month, y = pct, group = month), 
  #              inherit.aes = F ,
  #              alpha = 0.5)+
  #for geom_smooth to work, grouping must be done in the ggplot function
  #https://stackoverflow.com/a/40600861/7938068
  geom_smooth(se = T) 

