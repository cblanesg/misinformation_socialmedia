
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
load('5-analysis/1-input_data/misinformation/data_reg_misinformation_placebo.Rda', verbose = TRUE)

input_reg_placebo <- input_reg_placebo %>%
  mutate(category = label_desinformacion,
         label_desinformacion  = ifelse(placebo == 1, 'placebo', label_desinformacion),
         label_desinformacion = as.factor(label_desinformacion),
         treatment = as.factor(treatment),
         id_desinformacion = as.factor(id_desinformacion), 
         days_since_factcheck = as.factor(days_since_factcheck)) %>%
  rename('label' = 'label_desinformacion')

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
  
  reg_fe_label_fake[[i]] <- felm(formula = formula_reg, 
                            data = subset(input_reg_placebo, category == 'fake'), 
                          clustervar = 'id_desinformacion')
  
  print(paste0('Finish: ', i))
}

reg_fe_label_misleading <- list()
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
  
  reg_fe_label_misleading[[i]] <- felm(formula = formula_reg, 
                                 data = subset(input_reg_placebo, category == 'misleading'), 
                                 clustervar = 'id_desinformacion')
  
  print(paste0('Finish: ', i))
}

reg_fe_label_true <- list()
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
  
  reg_fe_label_true[[i]] <- felm(formula = formula_reg, 
                                 data = subset(input_reg_placebo, category == 'true'), 
                                 clustervar = 'id_desinformacion')
  
  print(paste0('Finish: ', i))
}

## fake 

table_fe_label_fake <- stargazer::stargazer(reg_fe_label_fake[1], 
                                            reg_fe_label_fake[2],
                                            reg_fe_label_fake[3],
                                            reg_fe_label_fake[4],
                                            reg_fe_label_fake[5],
                                      font.size = "small",
                                      no.space = TRUE,
                                       #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (Placebo, Fake)',
                                       #type = 'text', 
                                       keep = c('labelplacebo:treatment1',
                                                'treatment1:dsm_relative_fc'),
                                       covariate.labels = c('treatment1:placebo'
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

table_fe_label_fake2 <- stargazer::stargazer(reg_fe_label_fake[6], 
                                             reg_fe_label_fake[7],
                                             reg_fe_label_fake[8],
                                             reg_fe_label_fake[9],
                                             reg_fe_label_fake[10],
                                       font.size = "small",
                                       no.space = TRUE,
                                       #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                       #type = 'text', 
                                       keep = c('labelplacebo:treatment1',
                                                'treatment1:dsm_relative_fc'),
                                       covariate.labels = c('treatment1:placebo'
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


star.panel.out <- star_panel(table_fe_label_fake, 
                             table_fe_label_fake2,
                             panel.names = c("\\textbf{Levels}",
                                             "\\textbf{Growth}"))


write.table(star.panel.out, 
            '5-analysis/5-placebo/reg_tables/placebo_fake.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

## misleading

table_fe_label_mideleading <- stargazer::stargazer(reg_fe_label_misleading[1], 
                                            reg_fe_label_misleading[2],
                                            reg_fe_label_misleading[3],
                                            reg_fe_label_misleading[4],
                                            reg_fe_label_misleading[5],
                                            font.size = "small",
                                            no.space = TRUE,
                                            #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                            title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (Placebo, Misleading)',
                                            #type = 'text', 
                                            keep = c('labelplacebo:treatment1',
                                                     'treatment1:dsm_relative_fc'),
                                            covariate.labels = c('treatment1:placebo'
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

table_fe_label_mideleading2 <- stargazer::stargazer(reg_fe_label_misleading[6], 
                                             reg_fe_label_misleading[7],
                                             reg_fe_label_misleading[8],
                                             reg_fe_label_misleading[9],
                                             reg_fe_label_misleading[10],
                                             font.size = "small",
                                             no.space = TRUE,
                                             #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                             #type = 'text', 
                                             keep = c('labelplacebo:treatment1',
                                                      'treatment1:dsm_relative_fc'),
                                             covariate.labels = c('treatment1:placebo'
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


star.panel.out <- star_panel(table_fe_label_mideleading, 
                             table_fe_label_mideleading2,
                             panel.names = c("\\textbf{Levels}",
                                             "\\textbf{Growth}"))


write.table(star.panel.out, 
            '5-analysis/5-placebo/reg_tables/placebo_misleading.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)



## true

table_fe_label_true <- stargazer::stargazer(reg_fe_label_true[1], 
                                                   reg_fe_label_true[2],
                                                   reg_fe_label_true[3],
                                                   reg_fe_label_true[4],
                                                   reg_fe_label_true[5],
                                                   font.size = "small",
                                                   no.space = TRUE,
                                                   #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                                   title = '\\textbf{Analysis I} \\\\ Group Variable: Label Misinformation (Placebo, True)',
                                                   #type = 'text', 
                                                   keep = c('labelplacebo:treatment1',
                                                            'treatment1:dsm_relative_fc'),
                                                   covariate.labels = c('treatment1:placebo'
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

table_fe_label_true2 <- stargazer::stargazer(reg_fe_label_true[6], 
                                             reg_fe_label_true[7],
                                             reg_fe_label_true[8],
                                             reg_fe_label_true[9],
                                             reg_fe_label_true[10],
                                                    font.size = "small",
                                                    no.space = TRUE,
                                                    #omit = 'id_desinformacion|days_since_factcheck|days_since_publication',
                                                    #type = 'text', 
                                                    keep = c('labelplacebo:treatment1',
                                                             'treatment1:dsm_relative_fc'),
                                                    covariate.labels = c('treatment1:placebo'
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


star.panel.out <- star_panel(table_fe_label_true, 
                             table_fe_label_true2,
                             panel.names = c("\\textbf{Levels}",
                                             "\\textbf{Growth}"))


write.table(star.panel.out, 
            '5-analysis/5-placebo/reg_tables/placebo_true.txt',sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

