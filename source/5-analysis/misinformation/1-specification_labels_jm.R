
rm(list=ls()) ## clean enviornment

list.packages <- c('tidyverse', 
                   'plm', 
                   'miceadds', 
                   'stats', 
                   'lmtest', 
                   'multiwayvcov', 
                   'car', 
                   'regrrr', 
                   'readxl', 
                   'zoo', 
                   'lfe')

lapply(list.packages,
       require,
       character.only = TRUE)

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
         poynter = as.factor(poynter), 
         id_desinformacion = as.factor(id_desinformacion), 
         days_since_factcheck = as.factor(days_since_factcheck)) %>%
  rename('dsm_relative_fc' = 'days_since_misinformation_relative_fc', 
         'label' = 'label_desinformacion')

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

reg_fe_label <- list()
coef.labels <- list()
se.lables <- list()
for (i in 1:length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i], ## dep variable
                                   '~',
                                   'label*treatment',  ## beta * post_t * X_i
                                   '+', 
                                   'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                   '|',
                                   'id_desinformacion', ## mu_i
                                   '+',
                                   'days_since_factcheck'  ## omega_t
  ))
  
  reg_fe_label[[i]] <- felm(formula = formula_reg, 
                            data = input_reg, 
                          clustervar = 'id_desinformacion')
  
  print(paste0('Finish: ', i))
}


table_fe_label1 <- stargazer::stargazer(reg_fe_label[1], 
                                      reg_fe_label[2],
                                       reg_fe_label[3],
                                       reg_fe_label[4],
                                       reg_fe_label[5],
                                      font.size = "small",
                                      no.space = TRUE,
                                       #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)',
                                       #type = 'text', 
                                       keep = c('labelfake:treatment1',
                                                'labelmisleading:treatment1',
                                                'treatment1:dsm_relative_fc'),
                                       covariate.labels = c('treatment1:fake',
                                                            'treatment1:misleading'
                                                            ),
                                       dep.var.labels = c('likes', 
                                                          'shares', 
                                                          'comments', 
                                                          'reactions', 
                                                          'interactions'),
                                       omit.stat = c("ser", "rsq","f"), 
                                       add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)), 
                                                        c('\\textbf{DsF FE}', rep('Yes', 5))), 
                                       notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))

table_fe_label2 <- stargazer::stargazer(reg_fe_label[6], 
                                       reg_fe_label[7],
                                       reg_fe_label[8],
                                       reg_fe_label[9],
                                       reg_fe_label[10],
                                       font.size = "small",
                                       no.space = TRUE,
                                       #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)\\\\ Dep Variable: growth interactions',
                                       #type = 'text', 
                                       keep = c('labelfake:treatment1',
                                                'labelmisleading:treatment1',
                                                'treatment1:dsm_relative_fc'),
                                       covariate.labels = c('treatment1:fake',
                                                            'treatment1:misleading'
                                       ),
                                       dep.var.labels = c('likes', 
                                                          'shares', 
                                                          'comments', 
                                                          'reactions', 
                                                          'interactions'),
                                       omit.stat = c("ser", "rsq","f"), 
                                       add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)), 
                                                        c('\\textbf{DsF FE}', rep('Yes', 5))), 
                                       notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))


star.panel.out <- star_panel(table_fe_label1, 
                             table_fe_label2,
                             panel.names = c("\\textbf{Levels}",
                                             "\\textbf{Growth}"))


write.table(star.panel.out, 
            '5-analysis/2-reg_tables/misinformation/panel_labels_jm.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)


###### POYNTER
######### john specification

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

reg_fe_poynter <- list()
for (i in 1: length(dep.var)){
  
  
  formula_reg <- as.formula(paste0(dep.var[i], ## dep variable
                                   '~',
                                   'poynter*treatment',  ## beta * post_t * X_i
                                   '+', 
                                   'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                   '|',
                                   'id_desinformacion', ## mu_i
                                   '+',
                                   'days_since_factcheck'  ## omega_t
  ))
  
  reg_fe_poynter[[i]] <- felm(formula = formula_reg, 
                              data = input_reg, 
                              clustervar = 'id_desinformacion')
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_poynter1 <- stargazer::stargazer(reg_fe_poynter[1], 
                                         reg_fe_poynter[2],
                                         reg_fe_poynter[3],
                                         reg_fe_poynter[4],
                                         reg_fe_poynter[5],
                                         font.size = "small",
                                         no.space = TRUE,
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         title = '\\textbf{Analysis II: }Group Variable: Poynter Facebook (Poynter, No-Poynter)',
                                         #type = 'text', 
                                         keep = c('poynter1:treatment1',
                                                  'treatment1:dsm_relative_fc'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "f", 'adj.rsq'), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))

table_fe_poynter2 <- stargazer::stargazer(reg_fe_poynter[6], 
                                         reg_fe_poynter[7],
                                         reg_fe_poynter[8],
                                         reg_fe_poynter[9],
                                         reg_fe_poynter[10],
                                         font.size = "small",
                                         no.space = TRUE,
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         title = '\\textbf{Analysis II: }Group Variable: Poynter Facebook (Poynter, No-Poynter)',
                                         #type = 'text', 
                                         keep = c('poynter1:treatment1',
                                                  'treatment1:dsm_relative_fc'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "f", 'adj.rsq'), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))


star.panel.out <- star_panel(table_fe_poynter1, 
                             table_fe_poynter2,
                             panel.names = c("\\textbf{Levels}",
                                             "\\textbf{Growth}"))

write.table(star.panel.out, 
            '5-analysis/2-reg_tables/misinformation/panel_poynter_jm.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)


######### POPULAR
############# john


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



reg_fe_popular <- list()
for (i in 1: length(dep.var)){
  
  
  formula_reg <- as.formula(paste0(dep.var[i], ## dep variable
                                   '~',
                                   'popular*treatment',  ## beta * post_t * X_i
                                   '+', 
                                   'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                   '|',
                                   'id_desinformacion', ## mu_i
                                   '+',
                                   'days_since_factcheck'  ## omega_t
  ))
  
  reg_fe_popular[[i]] <- felm(formula = formula_reg, 
                              data = input_reg, 
                              clustervar = 'id_desinformacion')
  
  
  
  print(paste0('Finish: ', i))
}

table_fe_popular1 <- stargazer::stargazer(reg_fe_popular[1], 
                                         reg_fe_popular[2],
                                         reg_fe_popular[3],
                                         reg_fe_popular[4],
                                         reg_fe_popular[5],
                                         font.size = "small",
                                         no.space = TRUE,
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         title = '\\textbf{Analysis II: }Group Variable: Popular Fact Check (Popular, No-Popular)',
                                         #type = 'text', 
                                         keep = c('popular:treatment1',
                                                  'treatment1:dsm_relative_fc'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "f", 'adj.rsq'), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))

table_fe_popular2 <- stargazer::stargazer(reg_fe_popular[6], 
                                         reg_fe_popular[7],
                                         reg_fe_popular[8],
                                         reg_fe_popular[9],
                                         reg_fe_popular[10],
                                         font.size = "small",
                                         no.space = TRUE,
                                         omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                         #title = '\\textbf{Analysis II: }Group Variable: Popular Fact Check (Popular, No-Popular)',
                                         #type = 'text', 
                                         keep = c('popular:treatment1',
                                                  'treatment1:dsm_relative_fc'),
                                         dep.var.labels = c('likes', 
                                                            'shares', 
                                                            'comments', 
                                                            'reactions', 
                                                            'interactions'),
                                         omit.stat = c("ser", "f", 'adj.rsq'), 
                                         add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)),
                                                          c('\\textbf{DsF FE}', rep('Yes', 5))),
                                         notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))
star.panel.out <- star_panel(table_fe_popular1, 
                             table_fe_popular2,
                             panel.names = c("\\textbf{Levels}",
                                             "\\textbf{Growth}"))


write.table(star.panel.out, 
            '5-analysis/2-reg_tables/misinformation/panel_popular_jm.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

