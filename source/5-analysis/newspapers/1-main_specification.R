

rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)

setwd(dir = '~/misinformation_socialmedia/data/')

### Load and prepare data
load('5-analysis/1-input_data/data_reg_newspapers.Rda', verbose = TRUE)

clean_input_reg <- input_reg %>%
  mutate(growth_likes = ifelse(growth_likes == Inf, 0, growth_likes), 
         growth_shares = ifelse(growth_shares == Inf, 0, growth_shares), 
         growth_comments = ifelse(growth_comments == Inf, 0, growth_comments), 
         growth_reactions = ifelse(growth_reactions == Inf, 0, growth_reactions))


## Run regressions
stargazer::stargazer(lm(growth_likes~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
                        data = clean_input_reg), 
                     lm(growth_shares~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
                        data = clean_input_reg), 
                     lm(growth_comments~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
                        data = clean_input_reg), 
                     lm(growth_reactions~as.factor(treatment) + label_desinformacion + label_desinformacion*as.factor(treatment), 
                        data = clean_input_reg), 
                     type = 'text',
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
                               # 'as.factor(treatment)',
                               # '+', 
                               # 'as.factor(label_desinformacion)', 
                               # '+', 
                               'as.factor(label_desinformacion)*as.factor(treatment)',
                               '+',
                               'id_post' ,
                               '+',
                               'as.factor(n_days_since_factcheck)'
                               ))
  reg_fe_label[[i]] <- lm(formula, 
                          data =clean_input_reg)
  print(paste0('Finish: ', i))
}

stargazer::stargazer(reg_fe_label[1], 
                     reg_fe_label[2], 
                     reg_fe_label[3], 
                     reg_fe_label[4], 
                     omit = 'id_post|n_days_since_factcheck',
                     #type = 'text', 
                     covariate.labels = c('treatment',
                                          'fake',
                                          'treatment:fake'),
                     omit.stat = c("ser", "rsq","f"), 
                     add.lines = list(c('\\textbf{Post FE}', rep('Yes', 4)), 
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
                               'id_post', 
                               '+', 
                               'as.factor(n_days_since_factcheck)'))
  reg_fe[[i]] <- lm(formula, 
                    data = subset(clean_input_reg, label_desinformacion == 'fake'))
}


stargazer::stargazer(reg_fe[1], 
                     reg_fe[2], 
                     reg_fe[3], 
                     reg_fe[4], 
                     omit = 'id_post|n_days_since_factcheck',
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
