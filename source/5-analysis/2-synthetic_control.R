
library(tjbal)
library(kbal)
library(panelView)
library(tidyverse)
library(readxl)

setwd('~/cam.blanes Dropbox/Camila Blanes/Bolivia_Project/data/04-fakenews-Repository/06-virality/synthetic_control/social/')

setwd(dir = '~/misinformation_socialmedia/data/')

load('5-analysis/1-input_data/data_reg_social.Rda', verbose = TRUE)

data_desinformacion <- input_reg %>%
  select('id_post' =id_desinformacion,'change_likes'  = growth_likes,treatment, 'n_days_since_publicacion'  = n_days_since_publication, label_desinformacion)

data_control <- read_excel('~/cam.blanes Dropbox/Camila Blanes/Bolivia_Project/data/04-fakenews-Repository/06-virality/synthetic_control/social/0-input_data/input_synthetic_control.xlsx') %>%
  filter(n_days_since_publicacion <= 30) %>%
  filter(label_desinformacion == 'fake')  %>%
  filter(id_desinformacion %in% c(unique(data_desinformacion$id_post))) %>%
  select(id_post,  change_likes,treatment,
         n_days_since_publicacion,label_desinformacion)

data_all = rbind(data_desinformacion, data_control)

data_all <- data_all %>%
  filter(n_days_since_publication <= 15)


## make balanced panel <- 
panel_dataset <- data.frame()

for (i in unique(data_all$id_post)){
  n_days_since_publicacion <- c(1:20)
  temp <- as.data.frame(n_days_since_publicacion)
  temp$id_post  <- i
  panel_dataset <- rbind(panel_dataset, temp)
}


panel_dataset <- left_join(panel_dataset, data_all, by = c('id_post', 
                                                           'n_days_since_publicacion')) %>%
  group_by(id_post)%>%
  mutate(change_likes = ifelse(n_days_since_publicacion ==1 & is.na(change_likes), 0, change_likes),
         treatment = ifelse(n_days_since_publicacion ==1 & is.na(treatment), 0, treatment),
         treatment = ifelse(is.na(treatment), lag(treatment), 
                            ifelse(is.na(treatment), lag(treatment, n = 2), 
                                   ifelse(is.na(treatment), lag(treatment, n = 3), treatment))),
         change_likes = ifelse(is.na(change_likes), lag(change_likes), 
                               ifelse(is.na(change_likes), lag(change_likes, n = 2), 
                                      ifelse(is.na(change_likes), lag(change_likes, n = 3), change_likes))),
         treatment = ifelse(is.na(treatment), lag(treatment), 
                            ifelse(is.na(treatment), lag(treatment, n = 2), 
                                   ifelse(is.na(treatment), lag(treatment, n = 3), treatment))),
         change_likes = ifelse(is.na(change_likes), lag(change_likes), 
                               ifelse(is.na(change_likes), lag(change_likes, n = 2), 
                                      ifelse(is.na(change_likes), lag(change_likes, n = 3), change_likes))),
         treatment = ifelse(is.na(treatment), lag(treatment), 
                            ifelse(is.na(treatment), lag(treatment, n = 2), 
                                   ifelse(is.na(treatment), lag(treatment, n = 3), treatment))),
         change_likes = ifelse(is.na(change_likes), lag(change_likes), 
                               ifelse(is.na(change_likes), lag(change_likes, n = 2), 
                                      ifelse(is.na(change_likes), lag(change_likes, n = 3), change_likes))),
         change_likes = ifelse(id_post == '952a20c8-6c01-3ab4-9476-0e6b7ca0d3b2' & is.na(change_likes), 0, change_likes),
         treatment = ifelse(id_post == '952a20c8-6c01-3ab4-9476-0e6b7ca0d3b2' & is.na(treatment), 1, treatment),
         change_likes = ifelse(is.na(change_likes), 0, change_likes),
         treatment = ifelse(is.na(treatment), 0, treatment), 
         n_days_since_publicacion = as.integer(n_days_since_publicacion) , 
         treatment = ifelse(lag(treatment) == 1, 1, treatment),
         treatment = ifelse(lag(treatment) == 1, 1, treatment),
         treatment = ifelse(lag(treatment) == 1, 1, treatment),
         treatment = ifelse(lag(treatment) == 1, 1, treatment),
         treatment = ifelse(lag(treatment) == 1, 1, treatment)
         
  )

### Check 

panelView(change_likes ~ treatment , data = panel_dataset, #show.id = c(1:50), 
          index = c("id_post","n_days_since_publicacion"), xlab = "n_days_since_publicacion", ylab = "id_post",
          axis.lab.gap = c(0,1), by.timing = TRUE)

tjbal(data = data_all, Y = 'change_likes', 
      D = 'treatment',
      index = c("id_post","n_days_since_publicacion"), Y.match.npre = 0, 
      demean = TRUE, vce = "boot", nsims = 200)








