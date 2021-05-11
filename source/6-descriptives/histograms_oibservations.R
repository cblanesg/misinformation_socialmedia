


rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)
library(lfe)
library(coefplot)

# install.packages("remotes")
# remotes::install_github("ChandlerLutz/starpolishr")
library(starpolishr)

setwd(dir = '~/misinformation_socialmedia/data/')

### Load and prepare data
load('5-analysis/1-input_data/misinformation/data_reg_misinformation.Rda', verbose = TRUE)

input_reg <- input_reg %>%
  mutate(label_desinformacion  = as.factor(label_desinformacion), 
         treatment = as.factor(treatment),
         days_since_misinformation_relative_fc = as.factor(days_since_misinformation_relative_fc),
         poynter = as.factor(poynter),
         id_desinformacion = as.factor(id_desinformacion) #, 
         #days_since_factcheck = as.factor(days_since_factcheck)
         ) %>%
  rename('label' = 'label_desinformacion', 
         'dsm_relative_fc'  = 'days_since_misinformation_relative_fc') 
# %>%
#   mutate(days_since_factcheck = relevel(days_since_factcheck, ref = '0'))

count_observations = input_reg %>%
  filter(!is.na(approx_likes)) %>%
  group_by(dsm_relative_fc, label) %>%
  count() %>%
  filter(!is.na(dsm_relative_fc))

ggplot(data = count_observations, aes(x = dsm_relative_fc, y = n, fill = label)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() + 
  labs(y = 'number of observations', 
       x = 'day of misinformation since relative to fact check') +
  ggsave('6-descriptives/misinformation/dsm_histogram_label.png')

count_observations = input_reg %>%
  filter(!is.na(approx_likes)) %>%
  group_by(dsm_relative_fc, poynter) %>%
  count() %>%
  filter(!is.na(dsm_relative_fc))

ggplot(data = count_observations, aes(x = dsm_relative_fc, y = n, fill = poynter)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() + 
  labs(y = 'number of observations', 
       x = 'day of misinformation since relative to fact check') +
  ggsave('6-descriptives/misinformation/dsm_histogram_poynter.png')


count_observations = input_reg %>%
  filter(!is.na(approx_likes)) %>%
  group_by(dsm_relative_fc, popular) %>%
  count() %>%
  filter(!is.na(dsm_relative_fc))

ggplot(data = count_observations, aes(x = dsm_relative_fc, y = n, fill = as.factor(popular))) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() + 
  labs(y = 'number of observations', 
       x = 'day of misinformation since relative to fact check') +
  ggsave('6-descriptives/misinformation/dsm_histogram_popular.png')


count_observations = input_reg %>%
  filter(!is.na(approx_likes)) %>%
  group_by(days_since_factcheck, label) %>%
  count() %>%
  mutate(days_since_factcheck = as.factor(days_since_factcheck))

ggplot(data = count_observations, aes(x = days_since_factcheck, y = n, fill = label)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() + 
  labs(y = 'number of observations') 
# + 
#   ggsave('6-descriptives/misinformation/observations_levels.png')

counts_poynter <- input_reg %>%
  filter(!is.na(approx_likes)) %>%
  group_by(days_since_factcheck, poynter) %>%
  count() %>%
  mutate(days_since_factcheck = as.factor(days_since_factcheck))

ggplot(data = counts_poynter, aes(x = days_since_factcheck, y = n, fill = poynter)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() + 
  labs(y = 'number of observations') 


counts_popular <- input_reg %>%
  filter(!is.na(approx_likes)) %>%
  group_by(days_since_factcheck, popular) %>%
  count() %>%
  mutate(days_since_factcheck = as.factor(days_since_factcheck))

ggplot(data = counts_poynter, aes(x = days_since_factcheck, y = n, fill = poynter)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_bw() + 
  labs(y = 'number of observations') 


count_observations_growth = input_reg %>%
  filter(!is.na(growth_likes)) %>%
  group_by(days_since_factcheck) %>%
  count() %>%
  mutate(days_since_factcheck = as.factor(days_since_factcheck)) + 
  labs(y = 'number of observations')
  

ggplot(data = count_observations_growth, aes(x = days_since_factcheck, y = n)) + 
  geom_bar(stat = 'identity') +
  theme_bw()  + 
  ggsave('6-descriptives/misinformation/observations_growth.png')


