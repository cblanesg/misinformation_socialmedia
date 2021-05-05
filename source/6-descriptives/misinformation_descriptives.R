rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(ggplot2)

setwd(dir = '~/misinformation_socialmedia/data/')

### Load and prepare data
load('5-analysis/1-input_data/misinformation/data_reg_misinformation.Rda', verbose = TRUE)

popular_measure = read_excel('1-factchecks/2-clean_factchecks/factchecks_engagements.xlsx')  %>%
  select(popular, id_factcheck, engagements_complete)

unique_ids_poynter <- input_reg %>%
  select(id_post_desinformacion, poynter) %>%
  distinct()%>%
  group_by(id_post_desinformacion, poynter) %>%
  count()%>% ungroup() %>%
  group_by(id_post_desinformacion)%>%
  count()%>% filter( n == 1)

input_reg <- input_reg %>%
  filter(id_post_desinformacion %in% unique_ids_poynter$id_post_desinformacion) %>%
  select(-above_median)%>%
  left_join(popular_measure, 'id_factcheck')

######

input_reg %>%
  group_by(id_post_desinformacion, id_factcheck) %>%
  count() %>%
  ungroup()%>%
  summarise(mean(n))

input_reg %>%
  select(id_post_desinformacion, poynter, label_desinformacion) %>%
  group_by(poynter, label_desinformacion) %>%
  count()

# input_reg %>%
#   select(id_post_desinformacion, above_median) %>%
#   distinct()  %>%
#   group_by(above_median) %>%
#   count()

nrow(distinct(select(input_reg, id_post_desinformacion, id_factcheck)))

###### Observations around Fact Check

stat1 = input_reg %>%
  filter(days_since_factcheck == 0) %>%
  select(id_post_desinformacion, days_since_publication) %>%
  distinct() 


stat1.1 = input_reg %>%
  filter(days_since_factcheck == 0) %>%
  select(id_post_desinformacion, days_since_publication) %>%
  distinct() %>%
  group_by(days_since_publication) %>%
  count() %>%
  mutate(pct_observations = n /length(unique(input_reg$id_post_desinformacion)))

stat2 = input_reg %>%
  filter(days_since_factcheck == 0) %>%
  select(id_desinformacion, days_since_publication) %>%
  distinct()  %>%
  group_by(id_desinformacion) %>%
  mutate(min_date = min(days_since_publication)) %>%
  filter(days_since_publication == min_date) %>%
  ungroup() %>%
  distinct()

stat2.1 = input_reg %>%
  filter(days_since_factcheck == 0) %>%
  select(id_desinformacion, days_since_publication) %>%
  distinct()  %>%
  group_by(id_desinformacion) %>%
  mutate(min_date = min(days_since_publication)) %>%
  filter(days_since_publication == min_date) %>%
  ungroup() %>%
  distinct() %>%
  group_by(days_since_publication) %>%
  count()  %>%
  mutate(pct_observations = n /length(unique(input_reg$id_desinformacion)))



ggplot(stat1.1, aes(x = as.factor(days_since_publication), )) +
  geom_bar() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = 'Date of Post Relative to Fact Check', 
       y = 'Post Misinformation') + 
  ggsave('6-descriptives/misinformation/dop_relative_fc.png')


ggplot(stat1, aes(x = days_since_publication)) + 
  geom_histogram(bins = 80) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = 'Date of Post Relative to Fact Check', 
       y = 'Post Misinformation') +
  ggsave('6-descriptives/misinformation/dop_relative_fc_histogram.png')

ggplot(stat1.1, aes(x = as.factor(days_since_publication), y = pct_observations)) +
  geom_bar(stat = 'identity') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = 'Date of Post Relative to Fact Check', 
       y = 'Percent of Observations Post Misinformation') + 
  ggsave('6-descriptives/misinformation/dop_relative_fc_pct.png')

ggplot(stat2, aes(x = as.factor(days_since_publication))) +
  geom_bar() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = 'Date of Post Relative to Fact Check', 
       y = 'Misinformation') + 
  ggsave('6-descriptives/misinformation/dom_relative_fc.png')

ggplot(stat2.1, aes(x = as.factor(days_since_publication), y = pct_observations)) +
  geom_bar(stat = 'identity') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = 'Date of Post Relative to Fact Check', 
       y = 'Percent of Observations Misinformation') + 
  ggsave('6-descriptives/misinformation/dom_relative_fc_pct.png')


###### Observations around Fact Check
gg_data <- input_reg %>%
  group_by(days_since_factcheck) %>%
  count() %>%
  mutate(pct_observations = n/12957)

ggplot(gg_data, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  labs(
       # title = 'Observations by period', 
       # subtitle = 'Newspapers + Social Media Data', 
       x = 'days since fact check', 
       y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw()+
  ggsave('6-descriptives/misinformation/observations_pct.png')

input_reg %>%
  select(poynter, id_post_desinformacion) %>%
  distinct() %>%
  group_by(poynter)%>%
  count()

gg_data_poynter <- input_reg %>%
  group_by(days_since_factcheck, poynter) %>%
  count() %>%
  ungroup() %>%
  mutate(pct_observations = ifelse(poynter == 0, n/7374, n/5715)) %>%
  mutate(poynter = ifelse(poynter == 0, 'No-Poynter', 'Poynter'))

ggplot(gg_data_poynter, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  facet_grid(.~poynter)+ 
  #theme(text = element_text(size=15)) + 
    # title = 'Observations by period', 
    # subtitle = 'Newspapers + Social Media Data', 
   labs( x = 'days since fact check', 
    y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() +
  ggsave('6-descriptives/misinformation/observations_pct_poynter.png')


input_reg %>%
  select(label_desinformacion, id_post_desinformacion) %>%
  distinct() %>%
  group_by(label_desinformacion)%>%
  count()

gg_data_label <- input_reg %>%
  group_by(days_since_factcheck, label_desinformacion) %>%
  count() %>%
  ungroup() %>%
  mutate(pct_observations = ifelse(label_desinformacion == 'true', n/5084, 
                                   ifelse(label_desinformacion == 'fake', n/4831, n/3042)))

ggplot(gg_data_label, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  facet_grid(.~label_desinformacion)+ 
  #theme(text = element_text(size=15)) + 
  # title = 'Observations by period', 
  # subtitle = 'Newspapers + Social Media Data', 
  labs( x = 'days since fact check', 
        y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() +
  ggsave('6-descriptives/misinformation/observations_pct_label.png')


###### Growth since publication and since fact check
gg_data2 <- input_reg %>%
  group_by(days_since_factcheck)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE), 
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
    theme(text = element_text(size=15),
          #strip.background = element_rect(colour="white", fill="white"),
          legend.position=c(.9,.75), 
          legend.title =  element_blank()
    ) +
  ggsave('6-descriptives/misinformation/growth_factcheck.png')


gg_data2_poynter <- input_reg %>%
  group_by(days_since_factcheck, poynter)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')


ggplot(gg_data2_poynter, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~poynter) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_poynter.png')

gg_data2_label <- input_reg %>%
  group_by(days_since_factcheck, label_desinformacion)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2_label, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~label_desinformacion) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_label.png')


gg_data2poynter_label <- input_reg %>%
  group_by(days_since_factcheck, label_desinformacion, poynter)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2poynter_label, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  facet_wrap(.~poynter + label_desinformacion) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=12),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.92,.87), 
        legend.title =  element_blank()
  ) +
  ggsave(width = 10,
         height = 8,
         '6-descriptives/misinformation/growth_factcheck_label_poynter.png')


### Growth since Publication

gg_data3 <- input_reg %>%
  group_by(days_since_publication)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments =mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) +
  ggsave('6-descriptives/misinformation/growth_factcheck.png')


################
### Subset 10 days 
################

subset_ids <- input_reg %>%
  filter(days_since_factcheck == 0)  %>%
  filter(days_since_publication <= 10)

length(unique(subset_ids$id_post_desinformacion))

subset_input_reg <- input_reg %>%
  filter(id_post_desinformacion %in% subset_ids$id_post_desinformacion)

###### Observations around Fact Check

subset_input_reg %>%
  group_by(id_post_desinformacion, days_since_factcheck) %>%
  count() %>%
  filter(n > 1)
  

gg_data <- subset_input_reg %>%
  select(id_post_desinformacion, days_since_factcheck)%>% 
  distinct() %>%
  group_by(days_since_factcheck) %>%
  count() %>%
  mutate(pct_observations = n/length(unique(subset_input_reg$id_post_desinformacion)))

ggplot(gg_data, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  labs(
    # title = 'Observations by period', 
    # subtitle = 'Newspapers + Social Media Data', 
    x = 'days since fact check', 
    y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw()+
  ggsave('6-descriptives/misinformation/observations_pct_subset.png')

subset_input_reg %>%
  select(poynter, id_post_desinformacion) %>%
  distinct() %>%
  group_by(poynter)%>%
  count()

gg_data_poynter <- subset_input_reg %>%
  select(id_post_desinformacion, days_since_factcheck, poynter)%>% 
  distinct() %>%
  group_by(days_since_factcheck, poynter) %>%
  count() %>%
  ungroup() %>%
  mutate(pct_observations = ifelse(poynter == 0, n/3306, n/2137)) %>%
  mutate(poynter = ifelse(poynter == 0, 'No-Poynter', 'Poynter'))

ggplot(gg_data_poynter, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  facet_grid(.~poynter)+ 
  #theme(text = element_text(size=15)) + 
  # title = 'Observations by period', 
  # subtitle = 'Newspapers + Social Media Data', 
  labs( x = 'days since fact check', 
        y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() +
  ggsave('6-descriptives/misinformation/observations_pct_poynter_subset.png')


subset_input_reg %>%
  select(label_desinformacion, id_post_desinformacion) %>%
  distinct() %>%
  group_by(label_desinformacion)%>%
  count()

gg_data_label <- subset_input_reg %>%
  select(label_desinformacion, days_since_factcheck, id_post_desinformacion)%>% 
  distinct() %>%
  group_by(days_since_factcheck, label_desinformacion) %>%
  count() %>%
  ungroup() %>%
  mutate(pct_observations = ifelse(label_desinformacion == 'true', n/2444, 
                                   ifelse(label_desinformacion == 'fake', n/1794, n/1170)))

ggplot(gg_data_label, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  facet_grid(.~label_desinformacion)+ 
  #theme(text = element_text(size=15)) + 
  # title = 'Observations by period', 
  # subtitle = 'Newspapers + Social Media Data', 
  labs( x = 'days since fact check', 
        y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() +
  ggsave('6-descriptives/misinformation/observations_pct_label_subset.png')


###### Growth since publication and since fact check
gg_data2 <- subset_input_reg %>%
  group_by(days_since_factcheck)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE), 
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) +
  ggsave('6-descriptives/misinformation/growth_factcheck_subset.png')


gg_data2_poynter <- subset_input_reg %>%
  group_by(days_since_factcheck, poynter)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')


ggplot(gg_data2_poynter, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~poynter) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_poynter_subset.png')

gg_data2_label <- subset_input_reg %>%
  group_by(days_since_factcheck, label_desinformacion)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2_label, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~label_desinformacion) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.78), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_label_subset.png')


gg_data2poynter_label <- subset_input_reg %>%
  group_by(days_since_factcheck, label_desinformacion, poynter)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2poynter_label, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  facet_wrap(.~poynter + label_desinformacion ) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.92,.86), 
        legend.title =  element_blank()
  ) +
  ggsave(width = 10,
         height = 8,
         '6-descriptives/misinformation/growth_factcheck_label_poynter_subset.png')


### Growth since Publication

gg_data3 <- subset_input_reg %>%
  group_by(days_since_publication)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments =mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data3, aes(x = days_since_publication, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) +
  ggsave('6-descriptives/misinformation/growth_publication_subset.png')

#### #### #### #### #### #### 
#### Stats day at Fact Check
#### #### #### #### #### #### 

comparison_factcheck_day = subset_input_reg %>%
  filter(!is.na(label_desinformacion_pool))%>%
  filter(days_since_factcheck == 0) %>%
  ungroup() %>%
  group_by(poynter) %>%
  summarise(days_since_publication = median(days_since_publication), 
            
            growth_comments = round(mean(growth_comments, na.rm = TRUE), 2), 
            comments= round(mean(approx_comments, na.rm = TRUE), 2), 
            
            growth_likes = round(mean(growth_likes, na.rm = TRUE), 2), 
            likes = round(mean(approx_likes, na.rm = TRUE), 2), 
            
            growth_shares = round(mean(growth_shares, na.rm = TRUE), 2), 
            shares = round(mean(approx_shares, na.rm = TRUE), 2), 
            
            growth_reactions = round(mean(growth_reactions, na.rm = TRUE), 2), 
            reactions= round(mean(approx_reactions, na.rm = TRUE), 2), 
            
            growth_interactions = round(mean(growth_interactions, na.rm = TRUE), 2), 
            interactions= round(mean(approx_interactions, na.rm = TRUE), 2), 
  ) %>%
  gather(days_since_publication:interactions, key = 'Variable', value = 'value') %>%
  spread(key = 'poynter', value = value) %>%
  rename('No Poynter' = `0`, 
         'Poynter' = `1`)

stargazer::stargazer(comparison_factcheck_day, summary=FALSE, rownames=FALSE, 
                     title = 'statistics at the day of the fact check')
  
comparison_factcheck_day = input_reg %>%
  filter(!is.na(label_desinformacion_pool))%>%
  filter(days_since_factcheck == 0) %>%
  ungroup() %>%
  group_by(poynter) %>%
  summarise(days_since_publication = median(days_since_publication), 
            growth_comments = round(mean(growth_comments, na.rm = TRUE), 2), 
            comments= round(mean(approx_comments, na.rm = TRUE), 2), 
            
            growth_likes = round(mean(growth_likes, na.rm = TRUE), 2), 
            likes = round(mean(approx_likes, na.rm = TRUE), 2), 
            
            growth_shares = round(mean(growth_shares, na.rm = TRUE), 2), 
            shares = round(mean(approx_shares, na.rm = TRUE), 2), 
            
            growth_reactions = round(mean(growth_reactions, na.rm = TRUE), 2), 
            reactions= round(mean(approx_reactions, na.rm = TRUE), 2), 
            
            growth_interactions = round(mean(growth_interactions, na.rm = TRUE), 2), 
            interactions= round(mean(approx_interactions, na.rm = TRUE), 2), 
  ) %>%
  gather(days_since_publication:interactions, key = 'Variable', value = 'value') %>%
  spread(key = 'poynter', value = value) %>%
  rename('No Poynter' = `0`, 
         'Poynter' = `1`)

stargazer::stargazer(comparison_factcheck_day, summary=FALSE, rownames=FALSE, 
                     title = 'statistics at the day of the fact check')


length(unique(input_reg$id_post_desinformacion))
input_reg %>%
  filter(days_since_factcheck == 0) %>%
  ungroup() %>%
  group_by(poynter)%>%
  summarise(median(days_since_publication))

input_reg %>%
  filter(days_since_factcheck == 0) %>%
  ungroup() %>%
  group_by(poynter)%>%
  summarise(mean(days_since_publication))


############
############

gg_data4 <- input_reg %>%
  group_by(days_since_factcheck, poynter) %>%
  count() %>% 
  mutate(poynter = ifelse(poynter == 0, 'No-Poynter', 'Poynter'))

ggplot(gg_data4, aes(x = days_since_factcheck, y = n)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  facet_grid(.~poynter) + 
  labs(title = 'Observations per period', 
       subtitle = 'Newspapers Data', 
       x = 'days since fact check', 
       y = 'number observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() + 
  ggsave('6-descriptives/newspapers/observations_poynter_counts.png')


gg_data4.2 <- input_reg %>%
  group_by(days_since_factcheck, label_desinformacion) %>%
  count() 

ggplot(gg_data4.2, aes(x = days_since_factcheck, y = n)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  facet_grid(.~label_desinformacion) + 
  labs(title = 'Observations per period', 
       subtitle = 'Newspapers Data', 
       x = 'days since fact check', 
       y = 'number observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() + 
  ggsave('6-descriptives/newspapers/observations_label_counts.png')
### Days since Fact Check

input_reg %>%
  filter(days_since_factcheck == 0) %>% 
  group_by(poynter) %>% 
  summarise(median(days_since_publication))
  mutate(distance_days = date_timestep - date_factcheck ) %>%
  group_by(poynter)%>%
  summarise(mean(distance_days))

input_reg %>%
  select(id_post_desinformacion, date_desinformacion, date_factcheck, date_timestep, days_since_publication) %>%
  filter(days_since_publication == 0) %>%
  mutate(distance_days = date_timestep - date_factcheck ) %>%
  summarise(mean(distance_days))






###### Descriptions {-10, 10}


rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(ggplot2)

# install.packages("remotes")
# remotes::install_github("ChandlerLutz/starpolishr")

setwd(dir = '~/misinformation_socialmedia/data/')

### Load and prepare data
load('5-analysis/1-input_data/misinformation/data_reg_misinformation.Rda', verbose = TRUE)

unique_ids_poynter <- input_reg %>%
  select(id_post_desinformacion, poynter) %>%
  distinct()%>%
  group_by(id_post_desinformacion, poynter) %>%
  count()%>% ungroup() %>%
  group_by(id_post_desinformacion)%>%
  count()%>% filter( n == 1)

input_reg <- input_reg %>%
  filter(id_post_desinformacion %in% unique_ids_poynter$id_post_desinformacion)

##### subset difference between fact check and misinformation less or equal to 10 days
subset_ids <- input_reg %>%
  filter(days_since_factcheck == 0)  %>%
  filter(days_since_publication <= 10)

subset_input_reg <- input_reg %>%
  filter(id_post_desinformacion %in% subset_ids$id_post_desinformacion)

##### same number of observations {-10, 10}
subset_ids10 <- input_reg %>%
  filter(days_since_factcheck <= 10) %>%
  filter(days_since_factcheck >= -10) %>%
  group_by(id_post_desinformacion) %>%
  count() %>%
  filter(n == 21)

subset10_input_reg <- input_reg %>%
  filter(id_post_desinformacion %in% subset_ids10$id_post_desinformacion) %>%
  filter(days_since_factcheck <= 10) %>%
  filter(days_since_factcheck >= -10) 


##### same number of observations {-5, 5}
subset_ids5 <- input_reg %>%
  filter(days_since_factcheck <= 5) %>%
  filter(days_since_factcheck >= -5) %>%
  group_by(id_post_desinformacion) %>%
  count() %>%
  filter(n == 11)

subset5_input_reg <- input_reg %>%
  filter(id_post_desinformacion %in% subset_ids5$id_post_desinformacion) %>%
  filter(days_since_factcheck <= 5) %>%
  filter(days_since_factcheck >= -5) 

## observations




gg_data <- subset10_input_reg %>%
  select(id_post_desinformacion, days_since_factcheck)%>% 
  distinct() %>%
  group_by(days_since_factcheck) %>%
  count() %>%
  mutate(pct_observations = n/length(unique(subset10_input_reg$id_post_desinformacion)))

ggplot(gg_data, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  labs(
    # title = 'Observations by period', 
    # subtitle = 'Newspapers + Social Media Data', 
    x = 'days since fact check', 
    y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() +
  ggsave('6-descriptives/misinformation/observations_pct_subset10.png')



gg_data2 <- subset10_input_reg %>%
  group_by(days_since_factcheck)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE), 
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) +
  ggsave('6-descriptives/misinformation/growth_factcheck_subset10.png')


gg_data2_poynter <- subset10_input_reg %>%
  group_by(days_since_factcheck, poynter)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')


ggplot(gg_data2_poynter, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~poynter) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_poynter_subset10.png')

gg_data2_label <- subset10_input_reg %>%
  group_by(days_since_factcheck, label_desinformacion)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2_label, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~label_desinformacion) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.78), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_label_subset10.png')


gg_data2poynter_label <- subset10_input_reg %>%
  group_by(days_since_factcheck, label_desinformacion, poynter)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2poynter_label, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  facet_wrap(.~poynter + label_desinformacion ) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.92,.86), 
        legend.title =  element_blank()
  ) +
  ggsave(width = 10,
         height = 8,
         '6-descriptives/misinformation/growth_factcheck_label_poynter_subset10.png')

####### Subset {-5, 5}


gg_data <- subset5_input_reg %>%
  select(id_post_desinformacion, days_since_factcheck)%>% 
  distinct() %>%
  group_by(days_since_factcheck) %>%
  count() %>%
  mutate(pct_observations = n/length(unique(subset5_input_reg$id_post_desinformacion)))

ggplot(gg_data, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  labs(
    # title = 'Observations by period', 
    # subtitle = 'Newspapers + Social Media Data', 
    x = 'days since fact check', 
    y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() +
  ggsave('6-descriptives/misinformation/observations_pct_subset5.png')



gg_data2 <- subset5_input_reg %>%
  group_by(days_since_factcheck)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE), 
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) +
  ggsave('6-descriptives/misinformation/growth_factcheck_subset5.png')


gg_data2_poynter <- subset5_input_reg %>%
  group_by(days_since_factcheck, poynter)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')


ggplot(gg_data2_poynter, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~poynter) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_poynter_subset5.png')

gg_data2_label <- subset5_input_reg %>%
  group_by(days_since_factcheck, label_desinformacion)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2_label, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~label_desinformacion) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.78), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_label_subset5.png')


gg_data2poynter_label <- subset5_input_reg %>%
  group_by(days_since_factcheck, label_desinformacion, poynter)  %>%
  mutate(poynter = ifelse(poynter == 1, 'Poynter', 'No-Poynter')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data2poynter_label, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  facet_wrap(.~poynter + label_desinformacion ) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.92,.86), 
        legend.title =  element_blank()
  ) +
  ggsave(width = 10,
         height = 8,
         '6-descriptives/misinformation/growth_factcheck_label_poynter_subset5.png')

