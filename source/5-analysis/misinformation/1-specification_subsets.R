
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

reg_fe_label_fake <- list()
for (i in 1:length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[1], ## dep variable
                                   '~',
                                   'treatment',  ## beta * post_t 
                                   '+', 
                                   'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                   '|',
                                   'id_desinformacion', ## mu_i
                                   '+',
                                   'date_timestep'  ## omega_t
  ))
  
  reg_fe_label_fake[[1]] <- felm(formula = formula_reg, 
                            data = subset(input_reg, label == 'fake'), 
                          clustervar = 'id_desinformacion')
  
  print(paste0('Finish: ', i))
}


fake_subset = subset(input_reg, label == 'fake')

stargazer::stargazer(reg_fe_label_fake[[1]], type = 'text')

table_fe_label1 <- stargazer::stargazer(reg_fe_label_fake[1], 
                                        reg_fe_label_fake[2],
                                        reg_fe_label_fake[3],
                                        reg_fe_label_fake[4],
                                        reg_fe_label_fake[5],
                                      font.size = "small",
                                      no.space = TRUE,
                                       #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)',
                                       #type = 'text', 
                                       keep = c('treatment1:dsm_relative_fc'),
                                       # covariate.labels = c('treatment1:fake',
                                       #                      'treatment1:misleading'
                                       #                      ),
                                       dep.var.labels = c('likes', 
                                                          'shares', 
                                                          'comments', 
                                                          'reactions', 
                                                          'interactions'),
                                       omit.stat = c("ser", "rsq","f"), 
                                       add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)), 
                                                        c('\\textbf{DsF FE}', rep('Yes', 5))), 
                                       notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))

table_fe_label2 <- stargazer::stargazer(reg_fe_label_fake[6], 
                                        reg_fe_label_fake[7],
                                        reg_fe_label_fake[8],
                                        reg_fe_label_fake[9],
                                        reg_fe_label_fake[10],
                                       font.size = "small",
                                       no.space = TRUE,
                                       #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)\\\\ Dep Variable: growth interactions',
                                       #type = 'text', 
                                       keep = c('treatment1:dsm_relative_fc'),
                                       # covariate.labels = c('treatment1:fake',
                                       #                      'treatment1:misleading'
                                       #                      ),
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
            '5-analysis/2-reg_tables/misinformation/subset_fake.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)


## misleading

reg_fe_label_fake <- list()
for (i in 1:length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i], ## dep variable
                                   '~',
                                   'treatment',  ## beta * post_t 
                                   '+', 
                                   'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                   '|',
                                   'id_desinformacion', ## mu_i
                                   '+',
                                   'days_since_factcheck'  ## omega_t
  ))
  
  reg_fe_label_fake[[i]] <- felm(formula = formula_reg, 
                                 data = subset(input_reg, label == 'misleading'), 
                                 clustervar = 'id_desinformacion')
  
  print(paste0('Finish: ', i))
}


table_fe_label1 <- stargazer::stargazer(reg_fe_label_fake[1], 
                                        reg_fe_label_fake[2],
                                        reg_fe_label_fake[3],
                                        reg_fe_label_fake[4],
                                        reg_fe_label_fake[5],
                                        font.size = "small",
                                        no.space = TRUE,
                                        #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                        #title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)',
                                        #type = 'text', 
                                        keep = c('treatment1:dsm_relative_fc'),
                                        # covariate.labels = c('treatment1:fake',
                                        #                      'treatment1:misleading'
                                        #                      ),
                                        dep.var.labels = c('likes', 
                                                           'shares', 
                                                           'comments', 
                                                           'reactions', 
                                                           'interactions'),
                                        omit.stat = c("ser", "rsq","f"), 
                                        add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)), 
                                                         c('\\textbf{DsF FE}', rep('Yes', 5))), 
                                        notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))

table_fe_label2 <- stargazer::stargazer(reg_fe_label_fake[6], 
                                        reg_fe_label_fake[7],
                                        reg_fe_label_fake[8],
                                        reg_fe_label_fake[9],
                                        reg_fe_label_fake[10],
                                        font.size = "small",
                                        no.space = TRUE,
                                        #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                    #    title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)\\\\ Dep Variable: growth interactions',
                                        #type = 'text', 
                                        keep = c('treatment1:dsm_relative_fc'),
                                        # covariate.labels = c('treatment1:fake',
                                        #                      'treatment1:misleading'
                                        #                      ),
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
            '5-analysis/2-reg_tables/misinformation/subset_misleading.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

### true

reg_fe_label_fake <- list()
for (i in 1:length(dep.var)){
  
  formula_reg <- as.formula(paste0(dep.var[i], ## dep variable
                                   '~',
                                   'treatment',  ## beta * post_t * X_i
                                   '+', 
                                   'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                   '|',
                                   'id_desinformacion', ## mu_i
                                   '+',
                                   'days_since_factcheck'  ## omega_t
  ))
  
  reg_fe_label_fake[[i]] <- felm(formula = formula_reg, 
                                 data = subset(input_reg, label == 'true'), 
                                 clustervar = 'id_desinformacion')
  
  print(paste0('Finish: ', i))
}


table_fe_label1 <- stargazer::stargazer(reg_fe_label_fake[1], 
                                        reg_fe_label_fake[2],
                                        reg_fe_label_fake[3],
                                        reg_fe_label_fake[4],
                                        reg_fe_label_fake[5],
                                        font.size = "small",
                                        no.space = TRUE,
                                        #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                        #title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)',
                                        #type = 'text', 
                                        keep = c('treatment1:dsm_relative_fc'),
                                        # covariate.labels = c('treatment1:fake',
                                        #                      'treatment1:misleading'
                                        #                      ),
                                        dep.var.labels = c('likes', 
                                                           'shares', 
                                                           'comments', 
                                                           'reactions', 
                                                           'interactions'),
                                        omit.stat = c("ser", "rsq","f"), 
                                        add.lines = list(c('\\textbf{Misinformation FE}', rep('Yes', 5)), 
                                                         c('\\textbf{DsF FE}', rep('Yes', 5))), 
                                        notes = c('dsm_relative_fc: days since misinf relative to fact check. DsF: days since fact check'))

table_fe_label2 <- stargazer::stargazer(reg_fe_label_fake[6], 
                                        reg_fe_label_fake[7],
                                        reg_fe_label_fake[8],
                                        reg_fe_label_fake[9],
                                        reg_fe_label_fake[10],
                                        font.size = "small",
                                        no.space = TRUE,
                                        #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                        #    title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (True, Fake)\\\\ Dep Variable: growth interactions',
                                        #type = 'text', 
                                        keep = c('treatment1:dsm_relative_fc'),
                                        # covariate.labels = c('treatment1:fake',
                                        #                      'treatment1:misleading'
                                        #                      ),
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
            '5-analysis/2-reg_tables/misinformation/subset_true.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

