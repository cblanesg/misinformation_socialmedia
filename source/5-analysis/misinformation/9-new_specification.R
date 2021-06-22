
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

input_reg <- input_reg %>%
  filter(!is.na(days_since_misinformation_relative_fc))



input_reg <- input_reg %>%
  mutate(label_desinformacion  = as.factor(label_desinformacion), 
         treatment = as.factor(treatment),
         days_since_misinformation_relative_fc = as.factor(days_since_misinformation_relative_fc),
         poynter = as.factor(poynter), 
         id_desinformacion = as.factor(id_desinformacion), 
         days_since_factcheck = as.factor(days_since_factcheck), 
         time_stamp = as.factor(days_since_publication)) %>%
  rename('dsm_relative_fc' = 'days_since_misinformation_relative_fc', 
         'label' = 'label_desinformacion')

## Specification regression
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
                                   'treatment',  ## beta * post_t * X_i
                                   '|',
                                   'id_desinformacion', ## mu_i
                                   '+',
                                   'days_since_factcheck'  ## omega_t
  ))
  
  reg_fe_label[[i]] <- felm(formula = formula_reg, 
                            data = subset(input_reg, label_desinformacion == 'misleading'), 
                            clustervar = 'id_desinformacion')
  
  print(paste0('Finish: ', i))
}


#####

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


stargazer::stargazer(reg_fe_label[[2]], type = 'text')


