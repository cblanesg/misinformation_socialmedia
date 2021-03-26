
rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)

setwd(dir = '~/misinformation_socialmedia/data/')

### Prepare Input to Analysis

data <- read_excel('4-panel_data/newspapers/input_intrapolation.xlsx')
panel <- read_excel('4-panel_data/newspapers/panel_newspapers.xlsx')

input_intrapolation <- data %>%
  mutate(likes_facebook = ifelse(n_days_since_publication == 0, dplyr::lead(likes_facebook, order_by=id_post), likes_facebook), 
         comments_facebook = ifelse(n_days_since_publication == 0, dplyr::lead(comments_facebook, order_by=id_post), comments_facebook),
         shares_facebook = ifelse(n_days_since_publication == 0, dplyr::lead(shares_facebook, order_by=id_post), shares_facebook),
         reactions_facebook = ifelse(n_days_since_publication == 0, dplyr::lead(reactions_facebook, order_by=id_post), reactions_facebook))

panel_data <- data.frame()
for (i in 1:length(unique(input_intrapolation$id_post))){
  temp <-  input_intrapolation %>%
    filter(id_post == unique(input_intrapolation$id_post)[i])  
  if (is.na(temp$likes_facebook[1]) | is.na(subset(temp, n_days_since_publication == max(n_days_since_publication))$likes_facebook)){
    print(i/length(unique(input_intrapolation$id_post))*100) 
  }else{
    out <- temp %>%
      group_by(id_post)  %>%
      arrange(n_days_since_publication) %>%
      mutate(likes_facebook = ifelse(n_days_since_publication == 0, dplyr::lead(likes_facebook, order_by=id_post), likes_facebook),
             approx_likes = na.approx(likes_facebook,n_days_since_publication), 
             approx_shares = na.approx(ifelse(n_days_since_publication == 0, dplyr::lead(shares_facebook, order_by=id_post), shares_facebook),n_days_since_publication), 
             approx_comments = na.approx(ifelse(n_days_since_publication == 0, dplyr::lead(comments_facebook, order_by=id_post), comments_facebook),n_days_since_publication), 
             approx_reactions = na.approx(ifelse(n_days_since_publication == 0, dplyr::lead(reactions_facebook, order_by=id_post), reactions_facebook),n_days_since_publication)
      ) 
    
    panel_data <- rbind(panel_data, out)
    print(i/length(unique(input_intrapolation$id_post))*100) 
  }
}


input_reg <- panel_data %>%
  filter(abs(n_days_since_factcheck) <= 15) %>%
  ungroup() %>%
  group_by(id_post) %>%
  arrange(n_days_since_publication) %>%
  mutate(growth_likes = round((approx_likes - dplyr::lag(approx_likes))/dplyr::lag(approx_likes), digits = 4),
         growth_shares = round((approx_shares - dplyr::lag(approx_shares))/dplyr::lag(approx_shares), digits = 4),
         growth_comments = round((approx_comments - dplyr::lag(approx_comments))/dplyr::lag(approx_comments), digits = 4),
         growth_reactions = round((approx_reactions - dplyr::lag(approx_reactions))/dplyr::lag(approx_reactions), digits = 4)) %>%
  left_join(distinct(select(panel, id_post, label_desinformacion, poynter_facebook)), 'id_post') %>%
  mutate(treatment = ifelse(n_days_since_factcheck < 0,0, 1)) %>%
  mutate(label_desinformacion = ifelse(label_desinformacion == 'fake', 'fake', 'true_misinformation'))  %>%
  ungroup()  %>%
  mutate(label_desinformacion = relevel(factor( label_desinformacion , ordered = FALSE ), ref = 'true_misinformation'))

save(input_reg, file = '5-analysis/1-input_data/data_reg_newspapers.Rda')

input_reg %>%
  select(label_desinformacion,id_post ) %>%
  distinct()  %>%
  group_by(label_desinformacion) %>%
  count()


panel %>%
  select(id_desinformacion, id_factcheck)  %>%
  distinct() %>% 
  group_by(id_desinformacion) %>% 
  count() 

clean_input_reg <- input_reg %>%
  mutate(growth_likes = ifelse(growth_likes == Inf, 0, growth_likes), 
         growth_shares = ifelse(growth_shares == Inf, 0, growth_shares), 
         growth_comments = ifelse(growth_comments == Inf, 0, growth_comments), 
         growth_reactions = ifelse(growth_reactions == Inf, 0, growth_reactions))

stargazer::stargazer(lm(growth_likes~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
                        data = clean_input_reg), 
                     lm(growth_shares~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
                        data = clean_input_reg), 
                     lm(growth_comments~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
                        data = clean_input_reg), 
                     lm(growth_reactions~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
                        data = clean_input_reg), 
                     #type = 'text',
                     covariate.labels   = c('treatment',
                                            'fake label',
                                            'treatment:fake'),
                     omit.stat = c("ser", "rsq","f")
)


stargazer::stargazer(lm(growth_likes~as.factor(treatment) + as.factor(poynter_facebook) + as.factor(poynter_facebook)*as.factor(treatment), 
                        data = subset(clean_input_reg, label_desinformacion == 'fake')), 
                     lm(growth_shares~as.factor(treatment) + as.factor(poynter_facebook) + as.factor(poynter_facebook)*as.factor(treatment), 
                        data = subset(clean_input_reg, label_desinformacion == 'fake')), 
                     lm(growth_comments~as.factor(treatment) + as.factor(poynter_facebook) + as.factor(poynter_facebook)*as.factor(treatment), 
                        data = subset(clean_input_reg, label_desinformacion == 'fake')), 
                     lm(growth_reactions~as.factor(treatment) + as.factor(poynter_facebook) + as.factor(poynter_facebook)*as.factor(treatment), 
                        data = subset(clean_input_reg, label_desinformacion == 'fake')), 
                     #type = 'text', 
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
                               'as.factor(treatment)',
                               '+', 
                               'as.factor(label_desinformacion)', 
                               '+', 
                               'as.factor(label_desinformacion)*as.factor(treatment)', 
                               '+', 
                               'id_post', 
                               '+', 
                               'as.factor(n_days_since_factcheck)'))
  reg_fe_label[[i]] <- lm(formula, 
                          data =clean_input_reg)
}


stargazer::stargazer(reg_fe_label[1], 
                     reg_fe_label[2], 
                     reg_fe_label[3], 
                     reg_fe_label[4], 
                     omit = 'id_ost|n_days_since_factcheck',
                     #type = 'text', 
                     covariate.labels = c('treatment',
                                          'fake',
                                          'treatment:fake'),
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
