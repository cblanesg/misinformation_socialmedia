

rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)
library(lfe)
library(coefplot)

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
  rename('label' = 'label_desinformacion', 
         'dsm_relative_fc'  = 'days_since_misinformation_relative_fc') %>%
  mutate(days_since_factcheck = relevel(days_since_factcheck, ref = '0'))

##### label 

formula_reg <- as.formula(paste0('growth_likes', ## dep variable
                                 '~',
                                 'label*days_since_factcheck',  ## beta * post_t * X_i
                                 '+', 
                                 'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                 '|',
                                 'id_desinformacion', ## mu_i
                                 '+',
                                 'days_since_factcheck'  ## omega_t
))
  
reg_labels <- felm(formula = formula_reg, 
                            data = input_reg, 
                   clustervar = 'id_desinformacion')


df<- as.data.frame(coef(reg_labels))
df$predictors <- rownames(df)

df_fake <- df[grepl("labelfake", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_labels, coefficients = c(df_fake$predictors), innerCI = 1, outerCI = 0) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_jm/label_coeff_fake.png')


df<- as.data.frame(coef(reg_labels))
df$predictors <- rownames(df)

df_fake <- df[grepl("labelmisleading", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_labels, coefficients = c(df_fake$predictors), innerCI = 1, outerCI = 0) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_jm/label_coeff_misleading.png')


#### levels

formula_reg <- as.formula(paste0('approx_likes', ## dep variable
                                 '~',
                                 'label*days_since_factcheck',  ## beta * post_t * X_i
                                 '+', 
                                 'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                 '|',
                                 'id_desinformacion', ## mu_i
                                 '+',
                                 'days_since_factcheck'  ## omega_t
))

reg_labels <- felm(formula = formula_reg, 
                   data = input_reg, 
                   clustervar = 'id_desinformacion')


df <- as.data.frame(coef(reg_labels))
df$predictors <- rownames(df)

df_fake <- df[grepl("labelfake", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_labels, coefficients = c(df_fake$predictors), innerCI = 1, outerCI = 0) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_jm/label_coeff_fake_levels.png')


df<- as.data.frame(coef(reg_labels))
df$predictors <- rownames(df)

df_fake <- df[grepl("labelmisleading", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_labels, coefficients = c(df_fake$predictors), innerCI = 1, outerCI = 0) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_jm/label_coeff_misleading_levels.png')


######### POYNTER
######### john specification

formula_reg <- as.formula(paste0('growth_likes', ## dep variable
                                   '~',
                                   'poynter*days_since_factcheck',  ## beta * post_t * X_i
                                   '+', 
                                   'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                   '|',
                                   'id_desinformacion', ## mu_i
                                   '+',
                                   'days_since_factcheck'  ## omega_t
  ))
  

reg_fe_poynter <- felm(formula = formula_reg, 
                              data = input_reg, 
                       clustervar = 'id_desinformacion')
  
df<- as.data.frame(coef(reg_fe_poynter))
df$predictors <- rownames(df)

df_fake <- df[grepl("poynter1", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_fe_poynter, coefficients = c(df_fake$predictors), innerCI = 1, outerCI = 0) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggsave('5-analysis/3-descriptives_reg/coefficients_jm/poynter_coeff.png')

formula_reg <- as.formula(paste0('approx_likes', ## dep variable
                                 '~',
                                 'poynter*days_since_factcheck',  ## beta * post_t * X_i
                                 '+', 
                                 'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                 '|',
                                 'id_desinformacion', ## mu_i
                                 '+',
                                 'days_since_factcheck'  ## omega_t
))


reg_fe_poynter <- felm(formula = formula_reg, 
                       data = input_reg, 
                       clustervar = 'id_desinformacion')

df<- as.data.frame(coef(reg_fe_poynter))
df$predictors <- rownames(df)

df_fake <- df[grepl("poynter1", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_fe_poynter, coefficients = c(df_fake$predictors)) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggsave('5-analysis/3-descriptives_reg/coefficients_jm/poynter_coeff_levels.png')


######### POPULAR
######### john specification

formula_reg <- as.formula(paste0('growth_likes', ## dep variable
                                 '~',
                                 'popular*days_since_factcheck',  ## beta * post_t * X_i
                                 '+', 
                                 'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                 '|',
                                 'id_desinformacion', ## mu_i
                                 '+',
                                 'days_since_factcheck'  ## omega_t
))


reg_fe_popular <- felm(formula = formula_reg, 
                       data = input_reg, clustervar = 'id_desinformacion')


df<- as.data.frame(coef(reg_fe_popular))
df$predictors <- rownames(df)

df_fake <- df[grepl("popular", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_fe_popular, coefficients = c(df_fake$predictors)) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggsave('5-analysis/3-descriptives_reg/coefficients_jm/popular_coeff.png')

formula_reg <- as.formula(paste0('approx_likes', ## dep variable
                                 '~',
                                 'popular*days_since_factcheck',  ## beta * post_t * X_i
                                 '+', 
                                 'treatment*dsm_relative_fc', ## delta * treatment_t * lambda_i
                                 '|',
                                 'id_desinformacion', ## mu_i
                                 '+',
                                 'days_since_factcheck'  ## omega_t
))


reg_fe_popular <- felm(formula = formula_reg, 
                       data = input_reg, 
                       clustervar = 'id_desinformacion')


df<- as.data.frame(coef(reg_fe_popular))
df$predictors <- rownames(df)

df_fake <- df[grepl("popular", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_fe_popular, coefficients = c(df_fake$predictors), innerCI = 1, outerCI = 0) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggsave('5-analysis/3-descriptives_reg/coefficients_jm/popular_coeff_levels.png')


