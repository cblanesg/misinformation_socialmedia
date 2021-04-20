rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)
library(progress)

setwd(dir = '~/misinformation_socialmedia/data/')


############
## Include total interactions and index
############

### Load Input

input_interpolate <- read_csv('4-panel_data/newspapers/panel_interpolate.csv')

temp_interactions <- input_interpolate %>%
  #ungroup()  %>%
  filter(abs(days_since_factcheck) <= 15) %>%
  mutate(approx_likes = as.numeric(approx_likes), 
         approx_shares = as.numeric(approx_shares), 
         approx_comments = as.numeric(approx_comments), 
         approx_reactions = as.numeric(approx_reactions), 
         approx_interactions = approx_likes + approx_shares + approx_comments + approx_reactions)

id_data <- read_excel('2-misinformation/newspapers/0-misinformation/misinformation_newspapers.xlsx') %>%
  select(id_desinformacion, label_desinformacion)  %>% distinct()

input_reg <- temp_interactions %>%
  group_by(id_post_desinformacion) %>%
  arrange(days_since_publication) %>%
  mutate(growth_likes = round((approx_likes - dplyr::lag(approx_likes))/dplyr::lag(approx_likes), digits = 4),
         growth_shares = round((approx_shares - dplyr::lag(approx_shares))/dplyr::lag(approx_shares), digits = 4),
         growth_comments = round((approx_comments - dplyr::lag(approx_comments))/dplyr::lag(approx_comments), digits = 4),
         growth_reactions = round((approx_reactions - dplyr::lag(approx_reactions))/dplyr::lag(approx_reactions), digits = 4), 
         growth_interactions = round((approx_interactions - dplyr::lag(approx_interactions))/dplyr::lag(approx_interactions), digits = 4)) %>%
  left_join(id_data, 'id_desinformacion')

input_reg <- input_reg %>%
  #filter(label_desinformacion != 'misleading') %>%
  rename('date_desinformacion' = 'date_publication') %>%
  mutate(treatment = ifelse(days_since_factcheck < 0,0, 1), 
         facebook_partnership_date = as.Date(facebook_partnership_date, '%Y-%m-%d'), 
         date_desinformacion = as.Date(date_desinformacion, '%Y-%m-%d'), 
         days_since_poynter  = date_desinformacion - facebook_partnership_date,
         poynter = ifelse(is.na(days_since_poynter), 0,
                          ifelse(days_since_poynter <= 0, 1, 0))) %>%
  ungroup()  %>%
  mutate(label_desinformacion = ifelse(label_desinformacion == 'fake', 'fake',
                                       ifelse(label_desinformacion == 'misleading', 'misleading', 'true')), 
         label_desinformacion_pool =  ifelse(label_desinformacion == 'fake', 'fake', 'true'),
         label_desinformacion_pool = relevel(factor(label_desinformacion_pool , ordered = FALSE ), ref = 'true'), 
         label_desinformacion =  relevel(factor(label_desinformacion , ordered = FALSE ), ref = 'true')) %>%
  mutate(growth_likes = ifelse(growth_likes == Inf, 0, growth_likes), 
         growth_shares = ifelse(growth_shares == Inf, 0, growth_shares),
         growth_comments = ifelse(growth_comments == Inf, 0, growth_comments),
         growth_reactions = ifelse(growth_reactions == Inf, 0, growth_reactions),
         growth_interactions = ifelse(growth_interactions == Inf, 0, growth_interactions))

input_reg <- input_reg %>%
  group_by(id_post_desinformacion)%>%
  mutate(growth_likes = ifelse(lag(approx_likes) == 0 & is.na(approx_likes), 0, growth_likes),
         growth_shares = ifelse(lag(approx_shares) == 0 & is.na(approx_shares), 0, growth_shares),
         growth_comments = ifelse(lag(approx_comments) == 0 & is.na(approx_comments), 0, growth_comments),
         growth_reactions = ifelse(lag(approx_reactions) == 0 & is.na(approx_reactions), 0, growth_reactions),
         growth_interactions = ifelse(lag(approx_interactions) == 0 & is.na(approx_interactions), 0, growth_interactions),
         growth_likes = ifelse(days_since_publication == 0, NA, growth_likes),
         
         growth_likes = ifelse(lag(approx_likes) == 0 & approx_likes == 0, 0, growth_likes),
         growth_shares = ifelse(lag(approx_shares) == 0 & approx_shares== 0, 0, growth_shares),
         growth_comments = ifelse(lag(approx_comments) == 0 & approx_comments== 0, 0, growth_comments),
         growth_reactions = ifelse(lag(approx_reactions) == 0 & approx_reactions== 0, 0, growth_reactions),
         growth_interactions = ifelse(lag(approx_interactions) == 0 & approx_interactions== 0, 0, growth_interactions)) %>%
  filter(!is.na(growth_likes))

remove_duplicates <- input_reg %>%
  select(id_post_desinformacion, label_desinformacion) %>%
  distinct()  %>%
  group_by( id_post_desinformacion) %>%
  count()%>%
  filter( n > 1)
`%notin%` <- negate(`%in%`)

input_reg <- input_reg %>%
  filter(id_post_desinformacion %notin% remove_duplicates$id_post_desinformacion)

save(input_reg, file = '5-analysis/1-input_data/data_reg_newspapers.Rda')

############

id_subset <- input_reg %>%
  filter(days_since_factcheck == 0) %>%
  filter(days_since_publication <= 20) 


subset_input_reg <- input_reg %>%
  #filter(abs(days_since_factcheck) <= 10) %>%
  filter(id_post_desinformacion %in% id_subset$id_post_desinformacion)

save(subset_input_reg, file = '5-analysis/1-input_data/data_reg_newspapers_subset.Rda')


length(unique(input_reg$id_post_desinformacion))
length(unique(input_reg$id_desinformacion))

input_reg %>%
  select(id_desinformacion, poynter, label_desinformacion)  %>%
  distinct()  %>%
  group_by(poynter, label_desinformacion) %>%
  count() %>%
  ungroup() %>%
  summarise(sum(n))

input_reg %>%
  select(id_desinformacion, poynter)  %>%
  distinct()  %>%
  group_by(id_desinformacion) %>%
  count() %>%
  filter(n > 1)

input_reg %>%
  select(id_post_desinformacion, poynter, label_desinformacion)  %>%
  distinct()  %>%
  group_by( poynter, label_desinformacion) %>%
  count() %>%
  ungroup() %>%
  summarise(sum(n))


input_reg %>%
  group_by(label_desinformacion, poynter) %>%
  count()

input_reg %>%
  group_by(label_desinformacion, poynter) %>%
  count()%>%
  ungroup() %>%
  summarise(sum(n))

input_reg %>%
  group_by(id_post_desinformacion) %>%
  count() %>%
  ungroup() %>%
  summarise(mean(n))

