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

input_reg %>%
  select(id_post_desinformacion, popular) %>%
  distinct()  %>%
  group_by(popular) %>%
  count() 

###### statistics popular factchecks 

stats_all = read_excel('1-factchecks/2-clean_factchecks/factchecks_engagements.xlsx')  %>%
  select(reaction_count, 
         comment_count, 
         share_count, 
         engagements_complete, 
         popular) %>%
  summarise(n_factchecks = n(), 
            
            comments_mean = round(mean(comment_count, na.rm = TRUE), 2), 
            comments_median = round(median(comment_count, na.rm = TRUE), 2), 
            
            reactions_mean = round(mean(reaction_count, na.rm = TRUE), 2), 
            reactions_median = round(median(reaction_count, na.rm = TRUE), 2), 
            
            share_mean = round(mean(share_count, na.rm = TRUE), 2), 
            share_median = round(median(share_count, na.rm = TRUE), 2), 
            
            engagement_mean = round(mean(engagements_complete, na.rm = TRUE), 2), 
            engagement_median = round(median(engagements_complete, na.rm = TRUE), 2), 
  ) %>%
  gather(n_factchecks:engagement_median, key = 'Variable', value = 'value')  %>%
  rename('All Fact Checks' = 'value')


stats_group = read_excel('1-factchecks/2-clean_factchecks/factchecks_engagements.xlsx')  %>%
  select(reaction_count, 
         comment_count, 
         share_count, 
         engagements_complete, 
         popular) %>%
  group_by(popular)%>%
  summarise(n_factchecks = n(), 
            
            comments_mean = round(mean(comment_count, na.rm = TRUE), 2), 
            comments_median = round(median(comment_count, na.rm = TRUE), 2), 
            
            reactions_mean = round(mean(reaction_count, na.rm = TRUE), 2), 
            reactions_median = round(median(reaction_count, na.rm = TRUE), 2), 
            
            share_mean = round(mean(share_count, na.rm = TRUE), 2), 
            share_median = round(median(share_count, na.rm = TRUE), 2), 
            
            engagement_mean = round(mean(engagements_complete, na.rm = TRUE), 2), 
            engagement_median = round(median(engagements_complete, na.rm = TRUE), 2), 
  ) %>%
  gather(n_factchecks:engagement_median, key = 'Variable', value = 'value') %>%
  spread(key = 'popular', value = value) %>%
  rename('No-Popular' = `0`, 
         'Popular' = `1`)


stargazer::stargazer(left_join(stats_all, stats_group, 'Variable'), summary=FALSE, rownames=FALSE, 
                     title = 'Statistics Fact Checks')

input_reg %>%
  select(id_post_desinformacion, popular) %>%
  distinct()%>%
  group_by(popular)%>%
  count()

###### Observations around Fact Check
input_reg %>%
  select(popular, id_post_desinformacion) %>%
  distinct() %>%
  group_by(popular)%>%
  count()

gg_data_popular_fc <- input_reg %>%
  group_by(days_since_factcheck, popular) %>%
  count() %>%
  ungroup() %>%
  mutate(pct_observations = ifelse(popular == 0, n/5351, n/7478)) %>%
  mutate(popular = ifelse(popular == 0, 'No-Popular Fact Check', 'Popular Fact Check'))

ggplot(gg_data_popular_fc, aes(x = days_since_factcheck, y = pct_observations)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  facet_grid(.~popular)+ 
  #theme(text = element_text(size=15)) + 
  # title = 'Observations by period', 
  # subtitle = 'Newspapers + Social Media Data', 
  labs( x = 'days since fact check', 
        y = 'pct of observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted')  + 
  theme_bw() +
  ggsave('6-descriptives/misinformation/observations_pct_popular_fc.png')

###### Growth since publication and since fact check
gg_data2 <- input_reg %>%
  group_by(days_since_factcheck)  %>%
  summarise(likes =  mean(growth_likes, na.rm = TRUE), 
            shares = mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE), 
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

gg_data2_popular_fc <- input_reg %>%
  group_by(days_since_factcheck, popular)  %>%
  mutate(popular = ifelse(popular == 1, 'Popular Fact Check', 'No-Popular Fact Check')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')


ggplot(gg_data2_popular_fc, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~popular) + 
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
  ggsave('6-descriptives/misinformation/growth_factcheck_popular_fc.png')


gg_data3_popular_fc <- input_reg %>%
  group_by(days_since_factcheck, popular, label_desinformacion)  %>%
  mutate(popular = ifelse(popular == 1, 'Popular Fact Check', 'No-Popular Fact Check')) %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares = mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments = mean(growth_comments, na.rm = TRUE),
            interactions =  mean(growth_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')

ggplot(gg_data3_popular_fc, aes(x = days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  facet_wrap(.~popular + label_desinformacion) + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Growth') +
  theme_bw() + 
  theme(text = element_text(size=10),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.92,.85), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/growth_factcheck_popular_label.png')

comparison_factcheck_day = input_reg %>%
  filter(!is.na(label_desinformacion_pool))%>%
  filter(days_since_factcheck == 0) %>%
  ungroup() %>%
  group_by(popular) %>%
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
  spread(key = 'popular', value = value) %>%
  rename('No-Popular Fact Check' = `0`, 
         'Popular Fact Check' = `1`)

stargazer::stargazer(comparison_factcheck_day, summary=FALSE, rownames=FALSE, 
                     title = 'statistics at the day of the fact check')




