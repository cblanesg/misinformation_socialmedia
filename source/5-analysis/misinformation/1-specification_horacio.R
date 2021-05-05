
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

## Run regressions

##### ##### ##### ##### 
##### Analysis I: FE
##### ##### ##### ##### 

input_reg <- input_reg %>%
  mutate(label_desinformacion  = as.factor(label_desinformacion), 
         treatment = as.factor(treatment),
         days_since_misinformation_relative_fc = as.factor(days_since_misinformation_relative_fc),
         poynter = as.factor(poynter))%>%
  rename('dsm_relative_fc' = 'days_since_misinformation_relative_fc', 
         'label' = 'label_desinformacion')


###### LABELS
#### Horacio's version

dep.var <- c('approx_likes' ,
             'approx_comments',
             'approx_shares',
             'approx_reactions',
             'approx_interactions',
             'growth_likes' ,
             'growth_comments',
             'growth_shares',
             'growth_reactions',
             'growth_interactions'
)

reg_fe_label2 <- list()
for (i in 1: length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i], ## dep variable
                                   '~',
                                   'treatment*label',  ##  beta * post_t * X_i
                                   '+',
                                   'treatment*label*dsm_relative_fc', ## delta * post_t * X_i * lambda_i
                                   '+',
                                   'treatment*dsm_relative_fc', ## gamma * date of post relative to fc * treatment 
                                   '|',
                                   'id_desinformacion', ## mu_i: misinformation fe
                                   '+',
                                   'days_since_factcheck'  ## omega_t: days since fc fe
  ))
  
  
  reg_fe_label2[[i]] <- felm(formula = formula_reg, 
                             data = input_reg, 
                             clustervar = 'id_desinformacion')
  
  
  
  print(paste0('Finish: ', i))
}
#####

table_fe_label1 <- stargazer::stargazer(reg_fe_label2[1], 
                                       reg_fe_label2[2],
                                       reg_fe_label2[3],
                                       reg_fe_label2[4],
                                       reg_fe_label2[5],
                                       omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       title = '\\textbf{Analysis I}. Group Variable: Label Misinformation (True, Fake). Dep.var: levels',
                                       #type = 'text', 
                                       font.size = "small",
                                       no.space = TRUE,
                                       keep = c('treatment1:labelfake',
                                                'treatment1:labelmisleading',
                                                'treatment1:labelfake:dsm_relative_fc',
                                                'treatment1:labelmisleading:dsm_relative_fc'),
                                       covariate.labels = c('treatment:fake',
                                                            'treatment:misleading'),
                                       dep.var.labels = c('likes', 
                                                          'shares', 
                                                          'comments', 
                                                          'reactions', 
                                                          'interactions'),
                                       omit.stat = c("ser", "adj.rsq","f"), 
                                       add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)), 
                                                        c('\\textbf{DsF FE}', rep('Yes', 5))), 
                                       notes = c('dsm_relative_fc: days since misinformation relative to fact check. DsF: days since fact check'))

table_fe_label2 <- stargazer::stargazer(reg_fe_label2[6], 
                                        reg_fe_label2[7],
                                        reg_fe_label2[8],
                                        reg_fe_label2[9],
                                        reg_fe_label2[10],
                                        omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                        title = '\\textbf{Analysis I}. Group Variable: Label Misinformation (True, Fake). Dep.var: growth',
                                        #type = 'text', 
                                        font.size = "small",
                                        no.space = TRUE,
                                        keep = c('treatment1:labelfake',
                                                 'treatment1:labelmisleading',
                                                 'treatment1:labelfake:dsm_relative_fc',
                                                 'treatment1:labelmisleading:dsm_relative_fc'),
                                        covariate.labels = c('treatment:fake',
                                                             'treatment:misleading'),
                                        dep.var.labels = c('likes', 
                                                           'shares', 
                                                           'comments', 
                                                           'reactions', 
                                                           'interactions'),
                                        omit.stat = c("ser", "adj.rsq","f"), 
                                        add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)), 
                                                         c('\\textbf{DsF FE}', rep('Yes', 5))), 
                                        notes = c('dsm_relative_fc: days since misinformation relative to fact check. DsF: days since fact check'))


write.table(table_fe_label1, 
            '5-analysis/2-reg_tables/misinformation/panel_label_levels_hl.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)
write.table(table_fe_label2, 
            '5-analysis/2-reg_tables/misinformation/panel_label_growth_hl.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

#### POYNTER

reg_fe_poynter2 <- list()
for (i in 1: length(dep.var)){
  
  
  formula_reg <- as.formula(paste0(dep.var[i], ## dep variable
                                   '~',
                                   'treatment*poynter',  ##  beta * post_t * X_i
                                   '+',
                                   'treatment*poynter*dsm_relative_fc', ## delta * post_t * X_i * lambda_i
                                   '+',
                                   'treatment*dsm_relative_fc', ## gamma * date of post relative to fc * treatment 
                                   '|',
                                   'id_desinformacion', ## mu_i: misinformation fe
                                   '+',
                                   'days_since_factcheck'  ## omega_t: days since fc fe
  ))
  
  reg_fe_poynter2[[i]] <- felm(formula = formula_reg, 
                               data = input_reg, 
                               clustervar = 'id_desinformacion')
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_poynter <- stargazer::stargazer(reg_fe_poynter2[1], 
                                         reg_fe_poynter2[2],
                                         reg_fe_poynter2[3],
                                         reg_fe_poynter2[4],
                                         reg_fe_poynter2[5],
                                         font.size = "small",
                                         no.space = TRUE,
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         title = '\\textbf{Analysis II: }Group Variable: Poynter Facebook (Poynter, No-Poynter)',
                                         #type = 'text', 
                                         keep = c('treatment1:poynter1',
                                                  'treatment1:poynter1:dsm_relative_fc'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "f", 'adj.rsq'), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('dsm\\_relative\\_fc: days since misinformation relative to fact check. DsF: days since fact check'))

table_fe_poynter2 <- stargazer::stargazer(reg_fe_poynter2[6], 
                                         reg_fe_poynter2[7],
                                         reg_fe_poynter2[8],
                                         reg_fe_poynter2[9],
                                         reg_fe_poynter2[10],
                                         font.size = "small",
                                         no.space = TRUE,
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         #title = '\\textbf{Analysis II: }Group Variable: Poynter Facebook (Poynter, No-Poynter). Dep Var: growth',
                                         #type = 'text', 
                                         keep = c('treatment1:poynter1',
                                                  'treatment1:poynter1:dsm_relative_fc'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "f", 'adj.rsq'), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('dsm\\_relative\\_fc: days since misinformation relative to fact check. DsF: days since fact check'))



star.panel.out <- star_panel(table_fe_poynter1, 
                             table_fe_poynter2,
                             panel.names = c("\\textbf{Levels}",
                                             "\\textbf{Growth}"))

write.table(star.panel.out, 
            '5-analysis/2-reg_tables/misinformation/panel_poynter_hl.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)



######### POPULAR

reg_fe_popular2 <- list()
for (i in 1: length(dep.var)){
  
  
  formula_reg <- as.formula(paste0(dep.var[i], ## dep variable
                                   '~',
                                   'treatment*popular',  ##  beta * post_t * X_i
                                   '+',
                                   'treatment*popular*dsm_relative_fc', ## delta * post_t * X_i * lambda_i
                                   '+',
                                   'treatment*dsm_relative_fc', ## gamma * date of post relative to fc * treatment 
                                   '|',
                                   'id_desinformacion', ## mu_i: misinformation fe
                                   '+',
                                   'days_since_factcheck'  ## omega_t: days since fc fe
  ))
  
  reg_fe_popular2[[i]] <- felm(formula = formula_reg, 
                               data = input_reg, 
                               clustervar = 'id_desinformacion')
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_popular1 <- stargazer::stargazer(reg_fe_popular2[1], 
                                         reg_fe_popular2[2],
                                         reg_fe_popular2[3],
                                         reg_fe_popular2[4],
                                         reg_fe_popular2[5],
                                         font.size = "small",
                                         no.space = TRUE,
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         title = '\\textbf{Analysis II: }Group Variable: Popular Facebook (Popular, No-Popular)',
                                         #type = 'text', 
                                         keep = c('treatment1:popular',
                                                  'treatment1:popular:dsm_relative_fc'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "f", 'adj.rsq'), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('dsm\\_relative\\_fc: days since misinformation relative to fact check. DsF: days since fact check'))

table_fe_popular2 <- stargazer::stargazer(reg_fe_popular2[6], 
                                         reg_fe_popular2[7],
                                         reg_fe_popular2[8],
                                         reg_fe_popular2[9],
                                         reg_fe_popular2[10],
                                         font.size = "small",
                                         no.space = TRUE,
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         #title = '\\textbf{Analysis II: }Group Variable: Popular Facebook (Popular, No-Popular)',
                                         #type = 'text', 
                                         keep = c('treatment1:popular',
                                                  'treatment1:popular:dsm_relative_fc'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "f", 'adj.rsq'), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('dsm\\_relative\\_fc: days since misinformation relative to fact check. DsF: days since fact check'))

star.panel.out <- star_panel(table_fe_popular1, 
                             table_fe_popular2,
                             panel.names = c("\\textbf{Levels}",
                                             "\\textbf{Growth}"))

write.table(star.panel.out, 
            '5-analysis/2-reg_tables/misinformation/panel_popular_hl.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)
