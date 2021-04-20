
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

unique_ids_poynter <- input_reg %>%
  select(id_post_desinformacion, poynter) %>%
  distinct()%>%
  group_by(id_post_desinformacion, poynter) %>%
  count()%>% ungroup() %>%
  group_by(id_post_desinformacion)%>%
  count()%>% filter( n == 1)

input_reg <- input_reg %>%
  filter(id_post_desinformacion %in% unique_ids_poynter$id_post_desinformacion)

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

##### ##### ##### ##### 
##### Analysis I: FE
##### ##### ##### ##### 


dep.var <- c('growth_likes'
             'growth_shares',
             'growth_comments',
             'growth_reactions',
             'growth_interactions'
             )

##### label 
reg_fe_label <- list()
for (i in 1: length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i], 
                                   '~',
                                   'as.factor(label_desinformacion)*as.factor(treatment)', 
                                   '|', 
                                   'as.factor(days_since_factcheck) + id_post_desinformacion', 
                                   '+',
                                   'as.factor(days_since_publication)'))
  
  reg_fe_label[[i]] <- felm(formula = formula_reg, 
                            data = input_reg)
  
  
  
  print(paste0('Finish: ', i))
}
#####

table_fe_label <- stargazer::stargazer(reg_fe_label[1], 
                                       reg_fe_label[2],
                                       reg_fe_label[3],
                                       reg_fe_label[4],
                                       reg_fe_label[5],
                                       omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)',
                                       #type = 'text', 
                                       keep = c('as.factor[(]label_desinformacion[)]fake:as.factor[(]treatment[)]1', 
                                                'as.factor[(]label_desinformacion[)]misleading:as.factor[(]treatment[)]1'),
                                       covariate.labels = c('treatment:fake', 
                                                            'treatment:misleading'),
                                       dep.var.labels = c('likes', 
                                                          'shares', 
                                                          'comments', 
                                                          'reactions', 
                                                          'interactions'),
                                       omit.stat = c("ser", "rsq","f"), 
                                       add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)), 
                                                        c('\\textbf{DsP FE}', rep('Yes', 5)), 
                                                        c('\\textbf{DsF FE}', rep('Yes', 5))), 
                                       notes = c('DsP: days since publication of post of misinformation. DsF: days since fact check'))


##### label subset 10 days since Fake News
reg_fe_label_subset <- list()
for (i in 1: length(dep.var)){

  formula_reg <- as.formula(paste0(dep.var[i],
                                   '~',
                                   'as.factor(label_desinformacion)*as.factor(treatment)',
                                   '|',
                                   'as.factor(days_since_factcheck) + id_post_desinformacion',
                                   '+',
                                   'as.factor(days_since_publication)'))

  reg_fe_label_subset[[i]] <- felm(formula = formula_reg,
                            data = subset_input_reg)



  print(paste0('Finish: ', i))
}

table_fe_label_subset <- stargazer::stargazer(reg_fe_label_subset[1],
                                              reg_fe_label_subset[2],
                                              reg_fe_label_subset[3],
                                              reg_fe_label_subset[4],
                                              reg_fe_label_subset[5],
                                       omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake) \\\\ \\textbf{Subset 10}',
                                       #type = 'text',
                                       keep = c('as.factor[(]label_desinformacion[)]fake:as.factor[(]treatment[)]1', 
                                                'as.factor[(]label_desinformacion[)]misleading:as.factor[(]treatment[)]1'),
                                       covariate.labels = c('treatment:fake', 
                                                            'treatment:misleading'),
                                       omit.stat = c("ser","f"),
                                       add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                        c('\\textbf{DsP FE}', rep('Yes', 5)),
                                                        c('\\textbf{DsF FE}', rep('Yes', 5))))

##### label subset {-10, 10}

reg_fe_label_subset10 <- list()
for (i in 1: length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i],
                                   '~',
                                   'as.factor(label_desinformacion)*as.factor(treatment)',
                                   '|',
                                   'as.factor(days_since_factcheck) + id_post_desinformacion',
                                   '+',
                                   'as.factor(days_since_publication)'))
  
  reg_fe_label_subset10[[i]] <- felm(formula = formula_reg,
                                   data = subset10_input_reg)
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_label_subset10 <- stargazer::stargazer(reg_fe_label_subset10[1],
                                              reg_fe_label_subset10[2],
                                              reg_fe_label_subset10[3],
                                              reg_fe_label_subset10[4],
                                              reg_fe_label_subset10[5],
                                              omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                              #type = 'text',
                                              keep = c('as.factor[(]label_desinformacion[)]fake:as.factor[(]treatment[)]1', 
                                                       'as.factor[(]label_desinformacion[)]misleading:as.factor[(]treatment[)]1'),
                                              covariate.labels = c('treatment:fake', 
                                                                   'treatment:misleading'),
                                              omit.stat = c("ser","f"),
                                              add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                               c('\\textbf{DsP FE}', rep('Yes', 5)),
                                                               c('\\textbf{DsF FE}', rep('Yes', 5))))
##### label subset {-5, 5}

reg_fe_label_subset5 <- list()
for (i in 1: length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i],
                                   '~',
                                   'as.factor(label_desinformacion)*as.factor(treatment)',
                                   '|',
                                   'as.factor(days_since_factcheck) + id_post_desinformacion',
                                   '+',
                                   'as.factor(days_since_publication)'))
  
  reg_fe_label_subset5[[i]] <- felm(formula = formula_reg,
                                     data = subset5_input_reg)
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_label_subset5 <- stargazer::stargazer(reg_fe_label_subset5[1],
                                                reg_fe_label_subset5[2],
                                                reg_fe_label_subset5[3],
                                                reg_fe_label_subset5[4],
                                                reg_fe_label_subset5[5],
                                                omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                                #type = 'text',
                                                keep = c('as.factor[(]label_desinformacion[)]fake:as.factor[(]treatment[)]1', 
                                                         'as.factor[(]label_desinformacion[)]misleading:as.factor[(]treatment[)]1'),
                                                covariate.labels = c('treatment:fake', 
                                                                     'treatment:misleading'),
                                                omit.stat = c("ser","f"),
                                                add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                                 c('\\textbf{DsP FE}', rep('Yes', 5)),
                                                                 c('\\textbf{DsF FE}', rep('Yes', 5))))


star.panel.out <- star_panel(table_fe_label, 
                             table_fe_label_subset,
                             table_fe_label_subset10, 
                             table_fe_label_subset5,
                             panel.names = c("\\textbf{Main Specification}",
                                             "\\textbf{fact check 10 days or less since misinformation}",
                                             "\\textbf{-10 and 10 days since fact check}", 
                                             "\\textbf{-5 and 5 days since fact check}"))


save(reg_fe_label, file = '5-analysis/4-reg_out/label/label.Rda')
save(reg_fe_label_subset, file = '5-analysis/4-reg_out/label/label_subset.Rda')
save(reg_fe_label_subset10, file = '5-analysis/4-reg_out/label/label_subset10.Rda')
save(reg_fe_label_subset5, file = '5-analysis/4-reg_out/label/label_subset5.Rda')

write.table(star.panel.out, 
            '5-analysis/2-reg_tables/misinformation/panel_labels.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

