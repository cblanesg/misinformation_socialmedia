rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)
library(progress)

setwd(dir = '~/misinformation_socialmedia/data/')

### Load Input 

load('2-misinformation/newspapers/3-clean_misinformation/newspapers.RData', verbose = TRUE)

## Prepare Data: Newspapers

data <- misinformation_newspapers_panel %>%
  mutate(factcheck.var = ifelse(days_since_factcheck < 0, 0, 1))  %>%
  left_join(distinct(select(read_excel('2-misinformation/newspapers/0-misinformation/misinformation_newspapers.xlsx'), id_desinformacion, label_desinformacion)), 
            'id_desinformacion') %>%
  filter(label_desinformacion != 'misleading')

### Make panel

data <- data %>%
  group_by(id_desinformacion)  %>% 
  mutate(treatment = ifelse(days_since_factcheck < 0, 0, 1), 
         reactions = sum(loveCount:careCount)) %>%
  select(id_desinformacion, id_post_desinformacion, date_publication, date_timestep, days_since_publication, treatment, date_factcheck, days_since_factcheck, 
        'likes' = 'likeCount', 'shares' = 'shareCount', reactions, 'comments' = 'commentCount') 

# #### panel
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(unique(data$id_post_desinformacion)), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar


panel_structure <- data.frame()
counter = 1
for (i in unique(data$id_post_desinformacion)){

  id_data <- data %>%
    filter(id_post_desinformacion == i)

  df <- data.frame(c(-15:15))
  colnames(df) <- 'days_since_factcheck'
  df$id_post_desinformacion <- i

  panel_structure <- rbind(panel_structure, df)
  setTxtProgressBar(pb,counter)
  counter = counter + 1
}

colnames(panel_structure) <-c('days_since_factcheck', 'id_post_desinformacion')

input_intrapolation <- full_join(panel_structure, select(ungroup(data), id_post_desinformacion, days_since_factcheck, 
                                    likes, comments, reactions, shares), by = c('id_post_desinformacion','days_since_factcheck')) %>%
  left_join(distinct(select(data, 
  id_post_desinformacion, 
  date_factcheck, 
  date_publication)), by = c('id_post_desinformacion')) %>%
  as_tibble() %>%
  mutate(date_factcheck = as.Date(date_factcheck, '%Y-%m-%d'),
         date_publication = as.Date(date_publication, '%Y-%m-%d'),
         date_timestep = date_factcheck + as.difftime(days_since_factcheck, unit="days"), 
         days_since_publication = lubridate::time_length(difftime(date_timestep, date_publication), "days")) %>%
  filter(days_since_publication >= 0) %>%
  select(id_desinformacion, id_post_desinformacion, date_factcheck, 
         'date_desinformacion' = 'date_publication', 
         'date_timestep', 
         days_since_factcheck, days_since_publication, 
         likes, comments, shares, reactions)  %>%
  mutate(likes = ifelse(days_since_publication == 0, dplyr::lead(likes, order_by=id_post_desinformacion), likes), 
         comments = ifelse(days_since_publication == 0, dplyr::lead(comments, order_by=id_post_desinformacion), comments),
         shares = ifelse(days_since_publication == 0, dplyr::lead(shares, order_by=id_post_desinformacion), shares),
         reactions = ifelse(days_since_publication == 0, dplyr::lead(reactions, order_by=id_post_desinformacion), reactions))

panel_data <- data.frame()
counter = 1
for (i in 1:length(unique(input_intrapolation$id_post_desinformacion))){
  temp <-  input_intrapolation %>%
    filter(id_post_desinformacion == unique(input_intrapolation$id_post_desinformacion)[[i]])  
  if (is.na(temp$likes[1])){
    setTxtProgressBar(pb,counter)
    counter = counter + 1
  }else{
    out <- distinct(temp, days_since_publication, .keep_all = TRUE)%>%
      ungroup() %>%
      group_by(id_post_desinformacion)  %>%
      arrange(days_since_publication) %>%
      mutate(likes = ifelse(days_since_publication == 0, dplyr::lead(likes, order_by=id_post_desinformacion), likes),
             likes = ifelse(days_since_publication == 0 & is.na(likes), 0, likes),
             
             shares = ifelse(days_since_publication == 0, dplyr::lead(shares, order_by=id_post_desinformacion), shares),
             shares = ifelse(days_since_publication == 0 & is.na(shares), 0, shares),
             
             comments = ifelse(days_since_publication == 0, dplyr::lead(comments, order_by=id_post_desinformacion), comments),
             comments = ifelse(days_since_publication == 0 & is.na(comments), 0, comments),
             
             reactions = ifelse(days_since_publication == 0, dplyr::lead(reactions, order_by=id_post_desinformacion), reactions),
             reactions = ifelse(days_since_publication == 0 & is.na(reactions), 0, reactions)
             
             
             # approx_likes = na.approx(likes ,days_since_publication), 
             # approx_shares = na.approx(ifelse(days_since_publication == 0, dplyr::lead(shares, order_by=id_post_desinformacion), shares),days_since_publication), 
             # approx_comments = na.approx(ifelse(days_since_publication == 0, dplyr::lead(comments, order_by=id_post_desinformacion), comments),days_since_publication), 
             # approx_reactions = na.approx(ifelse(days_since_publication == 0, dplyr::lead(reactions, order_by=id_post_desinformacion), reactions),days_since_publication)
            ) 
    
    aprox_likes <- na.approx(out$likes)
    length(aprox_likes) <- nrow(out)
    out$approx_likes = aprox_likes
    
    approx_comments <- na.approx(out$comments)
    length(approx_comments) <- nrow(out)
    out$approx_comments = approx_comments
    
    aprox_reactions <- na.approx(out$reactions)
    length(aprox_reactions) <- nrow(out)
    out$approx_reactions = aprox_reactions
    
    aprox_shares <- na.approx(out$shares)
    length(aprox_shares) <- nrow(out)
    out$approx_shares = aprox_shares
    
    panel_data <- rbind(panel_data, out)
    setTxtProgressBar(pb,counter)
    counter = counter + 1
  }
}

############
## Include total interactions and index
############

matStand <- function(x, sgroup = rep(TRUE, nrow(x))){
  for(j in 1:ncol(x)){
    x[,j] <- (x[,j] - mean(x[sgroup,j]))/sd(x[sgroup,j])
  }
  return(x)
}

icwIndex <- function(	xmat,
                      wgts=rep(1, nrow(xmat)),
                      revcols = NULL,
                      sgroup = rep(TRUE, nrow(xmat))){
  X <- matStand(xmat, sgroup)
  if(length(revcols)>0){
    X[,revcols] <-  -1*X[,revcols]
  }
  i.vec <- as.matrix(rep(1,ncol(xmat)))
  Sx <- cov.wt(X, wt=wgts)[[1]]
  weights <- solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)
  index <- t(solve(t(i.vec)%*%solve(Sx)%*%i.vec)%*%t(i.vec)%*%solve(Sx)%*%t(X))
  return(list(weights = weights, index = index))
}

temp_interactions <- panel_data %>%
  ungroup()  %>%
  filter(abs(days_since_factcheck) <= 15) %>%
  mutate(total_interactions = approx_likes + approx_shares + approx_comments + approx_reactions)

# interactions_panel <- data.frame()
# counter = 1 
# for (i in 1:length(unique(temp_interactions$id_post_desinformacion))){
#   X.raw <- temp_interactions %>%
#     filter(id_post_desinformacion == unique(temp_interactions$id_post_desinformacion)[[4]]) %>%
#     select(approx_likes, aprox_shares,approx_comments,  aprox_reactions) %>%
#     as.matrix()
# 
#   
#   icwX <- icwIndex(X.raw)
#   z <- icwX$index
#   
#   temp$index_interactions <- z
#   
#   panel_data <- rbind(interactions_panel, temp)
#   setTxtProgressBar(pb,i)
#   
# }

id_data <- read_excel('2-misinformation/newspapers/0-misinformation/misinformation_newspapers.xlsx') %>%
  select(id_desinformacion, label_desinformacion, facebook_partnership_date, organizacion)  %>% distinct()

input_reg <- temp_interactions %>%
  group_by(id_post_desinformacion) %>%
  arrange(days_since_publication) %>%
  mutate(growth_likes = round((approx_likes - dplyr::lag(approx_likes))/dplyr::lag(approx_likes), digits = 4),
         growth_shares = round((approx_shares - dplyr::lag(approx_shares))/dplyr::lag(approx_shares), digits = 4),
         growth_comments = round((approx_comments - dplyr::lag(approx_comments))/dplyr::lag(approx_comments), digits = 4),
         growth_reactions = round((approx_reactions - dplyr::lag(approx_reactions))/dplyr::lag(approx_reactions), digits = 4), 
         growth_interactions = round((total_interactions - dplyr::lag(total_interactions))/dplyr::lag(total_interactions), digits = 4)) %>%
  left_join(id_data, 'id_desinformacion') %>%
  mutate(treatment = ifelse(days_since_factcheck < 0,0, 1), 
         facebook_partnership_date = as.Date(facebook_partnership_date, '%Y-%m-%d'), 
         days_since_poynter  = date_desinformacion - facebook_partnership_date,
         poynter = ifelse(is.na(days_since_poynter), 0,
                          ifelse(days_since_poynter <= 0, 1, 0))) %>%
  filter(label_desinformacion != 'misleading') %>%
  #mutate(label_desinformacion = ifelse(label_desinformacion == 'fake', 'fake', 'true_misinformation'))  %>%
  ungroup()  %>%
  mutate(label_desinformacion = relevel(factor(label_desinformacion , ordered = FALSE ), ref = 'true')) %>%
  mutate(growth_likes = ifelse(growth_likes == Inf, 0, growth_likes), 
         growth_shares = ifelse(growth_shares == Inf, 0, growth_shares),
         growth_comments = ifelse(growth_comments == Inf, 0, growth_comments),
         growth_reactions = ifelse(growth_reactions == Inf, 0, growth_reactions),
         growth_interactions = ifelse(growth_interactions == Inf, 0, growth_interactions)
  )


length(unique(input_reg$id_post_desinformacion))
input_reg %>%
  select(label_desinformacion,id_desinformacion ) %>%
  distinct()  %>%
  group_by(label_desinformacion) %>%
  count()

input_reg %>%
  select(label_desinformacion,id_post_desinformacion ) %>%
  distinct()  %>%
  group_by(label_desinformacion) %>%
  count()

save(input_reg, file = '5-analysis/1-input_data/data_reg_newspapers.Rda')

stargazer::stargazer(lm(growth_likes~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
           data = input_reg), 
           lm(growth_shares~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
              data = input_reg), 
           lm(growth_comments~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
              data = input_reg), 
           lm(growth_reactions~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
              data = input_reg), #type = 'text,'
           
           covariate.labels   = c('treatment',
                              'fake label',
                              'treatment:fake'),
           omit.stat = c("ser", "rsq","f")
           )


stargazer::stargazer(lm(growth_likes~as.factor(treatment) + as.factor(poynter_facebook) + as.factor(poynter_facebook)*as.factor(treatment), 
                        data = subset(input_reg, label_desinformacion == 'fake')), 
                     lm(growth_shares~as.factor(treatment) + as.factor(poynter_facebook) + as.factor(poynter_facebook)*as.factor(treatment), 
                        data = subset(input_reg, label_desinformacion == 'fake')), 
                     lm(growth_comments~as.factor(treatment) + as.factor(poynter_facebook) + as.factor(poynter_facebook)*as.factor(treatment), 
                        data = subset(input_reg, label_desinformacion == 'fake')), 
                     lm(growth_reactions~as.factor(treatment) + as.factor(poynter_facebook) + as.factor(poynter_facebook)*as.factor(treatment), 
                        data = subset(input_reg, label_desinformacion == 'fake')), #type = 'text', 
                     covariate.labels   = c('treatment', 
                                            'poynter facebook', 
                                            'treatment:poynter'), 
                     
                     omit.stat = c("ser", "rsq","f"))


##### With fixed effects

dep.var <- c('growth_likes', 
             'growth_shares', 
             'growth_comments', 
             'growth_reactions')

reg_fe_label <- list()

for (i in 1: length(dep.var)){
  formula <- as.formula(paste0(dep.var[i], 
                               '~',
                               'as.factor(label_desinformacion)*as.factor(treatment)', 
                               '+', 
                               'id_desinformacion', 
                               '+', 
                               'n_days_since_factcheck'))
  reg_fe_label[[i]] <- lm(formula, 
                    data =input_reg)
}


stargazer::stargazer(reg_fe_label[1], 
                     reg_fe_label[2], 
                     reg_fe_label[3], 
                     reg_fe_label[4], 
                     omit = 'id_desinformacion|n_days_since_factcheck',
                     type = 'text', 
                     # covariate.labels = c('treatment',
                     #                      'fake',
                     #                      'treatment:fake'),
                     omit.stat = c("ser", "rsq","f"), 
                     add.lines = list(c('\\textbf{Minsinformation FE}', rep('Yes', 4)), 
                                      c('\\textbf{Time FE}', rep('Yes', 4))))

reg_fe <- list()

for (i in 1: length(dep.var)){
  formula <- as.formula(paste0(dep.var[i], 
                               '~',
                               'as.factor(treatment)',
                               '+', 
                               'as.factor(poynter_facebook)', 
                               '+', 
                               'as.factor(poynter_facebook)*as.factor(treatment)', 
                               '+', 
                               'id_desinformacion', 
                               '+', 
                               'as.factor(n_days_since_factcheck)'))
  reg_fe[[i]] <- lm(formula, 
                    data = subset(input_reg, label_desinformacion == 'fake'))
}


stargazer::stargazer(reg_fe[1], 
                     reg_fe[2], 
                     reg_fe[3], 
                     reg_fe[4], 
                     omit = 'id_desinformacion|n_days_since_factcheck',
                     #type = 'text', 
                     covariate.labels = c('treatment', 
                                          'poynter', 
                                          'treatment:poynter'), 
                     omit.stat = c("ser", "rsq","f"), 
                     add.lines = list(c('\\textbf{Minsinformation FE}', rep('Yes', 4)), 
                                      c('\\textbf{Time FE}', rep('Yes', 4))))

library(jtools)
plot_summs(reg_fe[[1]], scale = TRUE)


df_coeficients <- as.data.frame(coef(reg_fe[[1]])) %>%
  mutate(variable = rownames(as.data.frame(coef(reg_fe[[1]]))))%>%
  filter(grepl("n_days_since_factcheck",variable)) %>%
  mutate(days_since_factcheck = as.integer(str_replace(string = variable, pattern = 'as.factor[(]n_days_since_factcheck[)]', replacement = ''))) %>%
  as_tibble()
df_coeficients
colnames(df_coeficients) <- c('coefficient', 'variable', 'days_since_factcheck') 


ggplot(df_coeficients, aes(x = days_since_factcheck, y = coefficient)) + 
  geom_point() +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  labs(title = 'Social Media: Coefficients of Days Since Fact Check', 
       subtitle = 'Group Variable: Poynter Misinformation', 
       x = 'days since fact check')


df_coeficients <- as.data.frame(coef(reg_fe_label[[1]])) %>%
  mutate(variable = rownames(as.data.frame(coef(reg_fe_label[[1]]))))%>%
  filter(grepl("n_days_since_factcheck",variable)) %>%
  mutate(days_since_factcheck = as.integer(str_replace(string = variable, pattern = 'as.factor[(]n_days_since_factcheck[)]', replacement = ''))) %>%
  as_tibble()
df_coeficients
colnames(df_coeficients) <- c('coefficient', 'variable', 'days_since_factcheck') 


ggplot(df_coeficients, aes(x = days_since_factcheck, y = coefficient)) + 
  geom_point() +
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') + 
  labs(title = 'Social Media: Coefficients of Days Since Fact Check', 
       subtitle = 'Group Variable: Label Misinformation', 
       x = 'days since fact check')

