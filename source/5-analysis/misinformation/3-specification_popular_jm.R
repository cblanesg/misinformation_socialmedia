
rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)
library(lfe)

# install.packages("remotes")
# remotes::install_github("ChandlerLutz/starpolishr")
library(starpolishr)

setwd(dir = '~/misinformation_socialmedia/data/')

### Load and prepare data
load('5-analysis/1-input_data/misinformation/data_reg_misinformation.Rda', verbose = TRUE)

popular_measure = read_excel('1-factchecks/2-clean_factchecks/factchecks_engagements.xlsx')  %>%
  select(popular, id_factcheck)

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

## Run regressions

##### ##### ##### ##### ##### 
##### Analysis II: Poynter
##### ##### ##### ##### ##### 

dep.var <- c('growth_likes',
             'growth_shares',
             'growth_comments',
             'growth_reactions',
             'growth_interactions'
)

## 

reg_fe_popular <- list()
for (i in 1: length(dep.var)){

  
  formula_reg <- as.formula(paste0(dep.var[i], 
                                   '~',
                                   'as.factor(popular)*as.factor(treatment)*label_desinformacion', 
                                   '+',
                                   'as.factor(popular)*as.factor(treatment)', 
                                   '+', 
                                   'as.factor(treatment)*label_desinformacion', 
                                   '|', 
                                   'as.factor(days_since_factcheck)',  ## days since fact check fe
                                   '+',
                                   'as.factor(id_post_desinformacion)', ### misinformation fe
                                   '+',
                                   'as.factor(days_since_publication)', ## days since post fe
                                   '+',
                                   'as.factor(treatment)*as.factor(days_since_publication)'
  ))
  
  reg_fe_popular[[i]] <- felm(formula = formula_reg, 
                              data = input_reg)
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_popular <- stargazer::stargazer(reg_fe_popular[1], 
                                         reg_fe_popular[2],
                                         reg_fe_popular[3],
                                         reg_fe_popular[4],
                                         reg_fe_popular[5],
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         title = '\\textbf{Analysis II: }Group Variable: Poynter Facebook (Poynter, No-Poynter)',
                                         #type = 'text', 
                                         font.size = "small",
                                         no.space = TRUE,
                                         keep = c('as.factor[(]popular[)]1:as.factor[(]treatment[)]1', 
                                                  'as.factor[(]treatment[)]1:label_desinformacionfake', 
                                                  'as.factor[(]treatment[)]1:label_desinformacionmisleading', 
                                                  'as.factor[(]popular[)]1:as.factor[(]treatment[)]1:label_desinformacionfake', 
                                                  'as.factor[(]popular[)]1:as.factor[(]treatment[)]1:label_desinformacionmisleading'),
                                         covariate.labels = c('treatment:popular',
                                                              'treatment:fake', 
                                                              'treatment:misleading', 
                                                              'treatment:popular:fake', 
                                                              'treatment:popular:misleading'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "adj.rsq","f"), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsP FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('DsP: days since publication of post of misinformation. DsF: days since fact check'))


## label 10 days since misinformation
reg_fe_popular_subset <- list()
for (i in 1: length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i], 
                                   '~',
                                   'as.factor(popular)*as.factor(treatment)*label_desinformacion', 
                                   '+',
                                   'as.factor(popular)*as.factor(treatment)', 
                                   '+', 
                                   'as.factor(treatment)*label_desinformacion', 
                                   '|', 
                                   'as.factor(days_since_factcheck)',  ## days since fact check fe
                                   '+',
                                   'as.factor(id_post_desinformacion)', ### misinformation fe
                                   '+',
                                   'as.factor(days_since_publication)', ## days since post fe
                                   '+',
                                   'as.factor(treatment)*as.factor(days_since_publication)'
  ))
  
  reg_fe_popular_subset[[i]] <- felm(formula = formula_reg, 
                                     data = subset_input_reg)
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_popular_subset <- stargazer::stargazer(reg_fe_popular_subset[1], 
                                                reg_fe_popular_subset[2],
                                                reg_fe_popular_subset[3],
                                                reg_fe_popular_subset[4],
                                                reg_fe_popular_subset[5],
                                                omit = 'id_desinformacion|days_since_factcheck',
                                                #type = 'text', 
                                                font.size = "small",
                                                no.space = TRUE,
                                                keep = c('as.factor[(]popular[)]1:as.factor[(]treatment[)]1', 
                                                         'as.factor[(]treatment[)]1:label_desinformacionfake', 
                                                         'as.factor[(]treatment[)]1:label_desinformacionmisleading', 
                                                         'as.factor[(]popular[)]1:as.factor[(]treatment[)]1:label_desinformacionfake', 
                                                         'as.factor[(]popular[)]1:as.factor[(]treatment[)]1:label_desinformacionmisleading'),
                                                covariate.labels = c('treatment:popular',
                                                                     'treatment:fake', 
                                                                     'treatment:misleading', 
                                                                     'treatment:popular:fake', 
                                                                     'treatment:popular:misleading'),
                                                omit.stat = c("ser", "rsq","f"), 
                                                add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                                 c('\\textbf{DsP FE}', rep('Yes', 5)),
                                                                 c('\\textbf{DsF FE}', rep('Yes', 5))),
                                                notes = c('DsP: days since publication of post of misinformation. DsF: days since fact check'))


## label subset -10, 10
reg_fe_popular_subset10 <- list()
for (i in 1: length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i], 
                                   '~',
                                   'as.factor(popular)*as.factor(treatment)*label_desinformacion', 
                                   '+',
                                   'as.factor(popular)*as.factor(treatment)', 
                                   '+', 
                                   'as.factor(treatment)*label_desinformacion', 
                                   '|', 
                                   'as.factor(days_since_factcheck)',  ## days since fact check fe
                                   '+',
                                   'as.factor(id_post_desinformacion)', ### misinformation fe
                                   '+',
                                   'as.factor(days_since_publication)', ## days since post fe
                                   '+',
                                   'as.factor(treatment)*as.factor(days_since_publication)'
  ))
  
  reg_fe_popular_subset10[[i]] <- felm(formula = formula_reg, 
                                       data = subset10_input_reg)
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_popular_subset10 <- stargazer::stargazer(reg_fe_popular_subset10[1], 
                                                  reg_fe_popular_subset10[2],
                                                  reg_fe_popular_subset10[3],
                                                  reg_fe_popular_subset10[4],
                                                  reg_fe_popular_subset10[5],
                                                  omit = 'id_desinformacion|days_since_factcheck',
                                                  #type = 'text',
                                                  font.size = "small",
                                                  no.space = TRUE,
                                                  keep = c('as.factor[(]popular[)]1:as.factor[(]treatment[)]1', 
                                                           'as.factor[(]treatment[)]1:label_desinformacionfake', 
                                                           'as.factor[(]treatment[)]1:label_desinformacionmisleading', 
                                                           'as.factor[(]popular[)]1:as.factor[(]treatment[)]1:label_desinformacionfake', 
                                                           'as.factor[(]popular[)]1:as.factor[(]treatment[)]1:label_desinformacionmisleading'),
                                                  covariate.labels = c('treatment:popular',
                                                                       'treatment:fake', 
                                                                       'treatment:misleading', 
                                                                       'treatment:popular:fake', 
                                                                       'treatment:popular:misleading'),
                                                  omit.stat = c("ser", "rsq","f"), 
                                                  add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                                   c('\\textbf{DsP FE}', rep('Yes', 5)),
                                                                   c('\\textbf{DsF FE}', rep('Yes', 5))))


## label subset -5, 5
reg_fe_popular_subset5 <- list()
for (i in 1: length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i], 
                                   '~',
                                   'as.factor(popular)*as.factor(treatment)*label_desinformacion', 
                                   '+',
                                   'as.factor(popular)*as.factor(treatment)', 
                                   '+', 
                                   'as.factor(treatment)*label_desinformacion', 
                                   '|', 
                                   'as.factor(days_since_factcheck)',  ## days since fact check fe
                                   '+',
                                   'as.factor(id_post_desinformacion)', ### misinformation fe
                                   '+',
                                   'as.factor(days_since_publication)', ## days since post fe
                                   '+',
                                   'as.factor(treatment)*as.factor(days_since_publication)'
  ))
  
  reg_fe_popular_subset5[[i]] <- felm(formula = formula_reg, 
                                      data = subset5_input_reg)
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_popular_subset5 <- stargazer::stargazer(reg_fe_popular_subset5[1], 
                                                 reg_fe_popular_subset5[2],
                                                 reg_fe_popular_subset5[3],
                                                 reg_fe_popular_subset5[4],
                                                 reg_fe_popular_subset5[5],
                                                 omit = 'id_desinformacion|days_since_factcheck',
                                                 #type = 'text', 
                                                 font.size = "small",
                                                 no.space = TRUE,
                                                 keep = c('as.factor[(]popular[)]1:as.factor[(]treatment[)]1', 
                                                          'as.factor[(]treatment[)]1:label_desinformacionfake', 
                                                          'as.factor[(]treatment[)]1:label_desinformacionmisleading', 
                                                          'as.factor[(]popular[)]1:as.factor[(]treatment[)]1:label_desinformacionfake', 
                                                          'as.factor[(]popular[)]1:as.factor[(]treatment[)]1:label_desinformacionmisleading'),
                                                 covariate.labels = c('treatment:popular',
                                                                      'treatment:fake', 
                                                                      'treatment:misleading', 
                                                                      'treatment:popular:fake', 
                                                                      'treatment:popular:misleading'),
                                                 omit.stat = c("ser", "rsq","f"), 
                                                 add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                                  c('\\textbf{DsP FE}', rep('Yes', 5)),
                                                                  c('\\textbf{DsF FE}', rep('Yes', 5))))


star.panel.out <- star_panel(table_fe_popular, 
                             table_fe_popular_subset,
                             table_fe_popular_subset10, 
                             table_fe_popular_subset5,
                             panel.names = c("\\textbf{All data}",
                                             "\\textbf{fact check 10 days or less since misinformation}",
                                             "\\textbf{-10 and 10 days since fact check}", 
                                             "\\textbf{-5 and 5 days since fact check}"))


save(reg_fe_popular, file = '5-analysis/4-reg_out/popular/popular.Rda')
save(reg_fe_popular_subset, file = '5-analysis/4-reg_out/popular/popular_subset.Rda')
save(reg_fe_popular_subset10, file = '5-analysis/4-reg_out/popular/popular_subset10.Rda')
save(reg_fe_popular_subset5, file = '5-analysis/4-reg_out/popular/popular_subset5.Rda')

write.table(star.panel.out, 
            '5-analysis/2-reg_tables/misinformation/panel_popular_jm.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)
