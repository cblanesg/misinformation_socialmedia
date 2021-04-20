rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)

setwd(dir = '~/misinformation_socialmedia/data/')

### Prepare Input to Analysis

load('4-panel_data/misinformation/panel_interpolate.RData', verbose = TRUE)

input_reg <- misinformation_panel %>%
  rename('date_desinformacion' = 'date_publication') %>%
  mutate(treatment = ifelse(days_since_factcheck < 0,0, 1), 
         facebook_partnership_date = as.Date(facebook_partnership_date, '%Y-%m-%d'), 
         date_desinformacion = as.Date(date_desinformacion, '%Y-%m-%d'), 
         date_factcheck = as.Date(date_factcheck, '%Y-%m-%d'), 
         days_since_poynter  = date_factcheck - facebook_partnership_date,
         poynter = ifelse(is.na(days_since_poynter), 0,
                          ifelse(days_since_poynter <= 0, 0, 1))) %>%
  ungroup()  %>%
  mutate(label_desinformacion = ifelse(label_desinformacion == 'fake', 'fake',
                                       ifelse(label_desinformacion == 'misleading', 'misleading', 'true')), 
         label_desinformacion_pool = ifelse(label_desinformacion == 'fake', 'fake', 'true'), 
         label_desinformacion_pool = relevel(factor(label_desinformacion_pool , ordered = FALSE ), ref = 'true'), 
         label_desinformacion = relevel(factor(label_desinformacion , ordered = FALSE ), ref = 'true')) %>%
  arrange(id_desinformacion, days_since_publication) %>%
  mutate(growth_likes = ifelse(growth_likes == Inf, 0, growth_likes), 
         growth_shares = ifelse(growth_shares == Inf, 0, growth_shares),
         growth_comments = ifelse(growth_comments == Inf, 0, growth_comments),
         growth_reactions = ifelse(growth_reactions == Inf, 0, growth_reactions),
         growth_interactions = ifelse(growth_interactions == Inf, 0, growth_interactions),
         
         growth_likes = ifelse(lag(approx_likes) == 0 & is.na(approx_likes), 0, growth_likes),
         growth_shares = ifelse(lag(approx_shares) == 0 & is.na(approx_shares), 0, growth_shares),
         growth_comments = ifelse(lag(approx_comments) == 0 & is.na(approx_comments), 0, growth_comments),
         growth_reactions = ifelse(lag(approx_reactions) == 0 & is.na(approx_reactions), 0, growth_reactions),
         growth_interactions = ifelse(lag(approx_interactions) == 0 & is.na(approx_interactions), 0, growth_interactions),
         
         growth_likes = ifelse(lag(approx_likes) == 0 & approx_likes == 0, 0, growth_likes),
         growth_shares = ifelse(lag(approx_shares) == 0 & approx_shares== 0, 0, growth_shares),
         growth_comments = ifelse(lag(approx_comments) == 0 & approx_comments== 0, 0, growth_comments),
         growth_reactions = ifelse(lag(approx_reactions) == 0 & approx_reactions== 0, 0, growth_reactions),
         growth_interactions = ifelse(lag(approx_interactions) == 0 & approx_interactions== 0, 0, growth_interactions)
  ) %>%
  filter(!is.na(growth_likes))

## load fact checks virality

factchecks = read_excel('1-factchecks/2-clean_factchecks/factchecks_engagements.xlsx')  %>%
  select(id_factcheck, 'factcheck_engagement' = 'total_engagement', above_median)

input_reg = left_join(input_reg, factchecks, 'id_factcheck') 

save(input_reg, file = '5-analysis/1-input_data/misinformation/data_reg_misinformation.Rda')
