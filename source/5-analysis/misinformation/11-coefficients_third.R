
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
                   'lfe', 
                   'coefplot', 
                   'starpolishr')

lapply(list.packages,
       require,
       character.only = TRUE)

setwd(dir = '~/misinformation_socialmedia/data/')

### Load and prepare data
load('5-analysis/1-input_data/misinformation/data_reg_misinformation.Rda', verbose = TRUE)

## Run regressions

input_reg <- input_reg %>%
  mutate(label_desinformacion  = as.factor(label_desinformacion), 
         treatment = as.factor(treatment),
         days_since_misinformation_relative_fc = as.factor(days_since_misinformation_relative_fc),
         poynter = as.factor(poynter), 
         id_desinformacion = as.factor(id_desinformacion), 
         days_since_factcheck = as.factor(days_since_factcheck)) %>%
  rename('dsm_relative_fc' = 'days_since_misinformation_relative_fc', 
         'label' = 'label_desinformacion') %>%
  mutate(relevel(factor(days_since_factcheck , ordered = FALSE ), ref = '0'))

##### label 

formula_reg <- as.formula(paste0('growth_interactions', ## dep variable
                                 '~',
                                 'treatment',  ## beta * post_t * X_i
                                 '+',
                                 'days_since_publication*days_since_factcheck',
                                 '+',
                                 'id_desinformacion', ## mu_i
                                 '+',
                                 'days_since_factcheck'  ## omega_t
))

reg_labels <- felm(formula = formula_reg, 
                   data = input_reg, 
                   clustervar = 'id_desinformacion')

df<- as.data.frame(coef(reg_labels))
df$predictors <- rownames(df)

df_intercept <- df[grepl("[(]Intercept[)]", df[["predictors"]]), ]$`coef(reg_labels)`
df_coefficients <- df[grepl("^days_since_factcheck", df[["predictors"]]), ]


coefplot(reg_labels, coefficients = c(df_coefficients$predictors), innerCI = 1, outerCI = 0) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggsave('5-analysis/3-descriptives_reg/coefficients_third/growth_likes.png')


formula_reg <- as.formula(paste0('approx_interactions', ## dep variable
                                 '~',
                                 'treatment',  ## beta * post_t * X_i
                                 '+',
                                 'days_since_publication*days_since_factcheck',
                                 '+',
                                 'id_desinformacion', ## mu_i
                                 '+',
                                 'days_since_factcheck'  ## omega_t
))

reg_labels <- felm(formula = formula_reg, 
                   data = input_reg, 
                   clustervar = 'id_desinformacion')

df<- as.data.frame(coef(reg_labels))
df$predictors <- rownames(df)

df_intercept <- df[grepl("[(]Intercept[)]", df[["predictors"]]), ]$`coef(reg_labels)`
df_coefficients <- df[grepl("^days_since_factcheck", df[["predictors"]]), ]


coefplot(reg_labels, coefficients = c(df_coefficients$predictors), innerCI = 1, outerCI = 0) + 
  #scale_y_discrete(labels=c(-15:-1, 1:15)) + 
  geom_hline(yintercept = 16, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ggsave('5-analysis/3-descriptives_reg/coefficients_third/accumulated_likes.png')

