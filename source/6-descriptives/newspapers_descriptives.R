rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(ggplot2)

setwd(dir = '~/misinformation_socialmedia/data/')

### Load and prepare data
load('5-analysis/1-input_data/data_reg_newspapers.Rda', verbose = TRUE)

input_reg <- input_reg %>%
  mutate(growth_likes = ifelse(growth_likes == Inf, 0, growth_likes), 
         growth_shares = ifelse(growth_shares == Inf, 0, growth_shares), 
         growth_comments = ifelse(growth_comments == Inf, 0, growth_comments), 
         growth_reactions = ifelse(growth_reactions == Inf, 0, growth_reactions))

######

gg_data <- input_reg %>%
  group_by(n_days_since_factcheck) %>%
  count()

ggplot(gg_data, aes(x = n_days_since_factcheck, y = n)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  labs(title = 'Observations per period', 
       subtitle = 'Newspapers Data', 
       x = 'days since fact check', 
       y = 'number observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') 


gg_data2 <- input_reg %>%
  group_by(n_days_since_factcheck)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments =mean(growth_comments, na.rm = TRUE))  %>%
  gather(likes:comments, key = 'interaction', value = 'value')

ggplot(gg_data2, aes(x = n_days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(title = 'Change in growth of interactions since publication of fact check in Facebook', 
       subtitle = 'Newspapers Data')



gg_data3 <- input_reg %>%
  group_by(n_days_since_publication)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments =mean(growth_comments, na.rm = TRUE))  %>%
  gather(likes:comments, key = 'interaction', value = 'value')

ggplot(gg_data3, aes(x = n_days_since_publication, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted') +
  labs(title = 'Change in growth of interactions since publication of post', 
       subtitle = 'Newspapers Data')



counts <- clean_input_reg %>%
  left_join(panel, by = 'id_post') %>%
  group_by(id_desinformacion ) %>%
  count() 

length(counts$id_desinformacion)

ggplot(counts, aes(x = n)) + 
  geom_density() + 
  labs(title = 'Number of posts by desinformation', 
       subtitle = 'Max posts: 13,128\nMin posts: 5\nMean: 950\nMisinformations: 128')


