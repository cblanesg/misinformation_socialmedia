
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



#### #### #### 
#### LABELS
#### #### #### 

######## horacio's
library(foreign)
library(multcomp)

formula_reg <- as.formula(paste0('growth_likes', ## dep variable
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

reg_label2 <- felm(formula = formula_reg, 
                   data = input_reg, 
                   clustervar = 'id_desinformacion')

vcov.matrix <- vcov(reg_label2)
df = as.data.frame(coef(reg_label2))
df$predictor = rownames(df)
df$counter = c(1:nrow(df))

out <- summary(reg_label2)
out$coefficients[,2][13]

data_errors = as.data.frame(c('treatment1:labelfake'))
colnames(data_errors) = c('variable')
data_errors$s.error = out$coefficients[,2][13]
data_errors$coefficient =  subset(df, predictor == 'treatment1:labelfake')$`coef(reg_label2)`

for (i in c(1:8, 15)){
  
  position1 = subset(df, predictor == 'treatment1:labelfake')$counter
  position2 = subset(df, predictor == paste0('treatment1:labelfake:dsm_relative_fc', i))$counter
  coefficient2 = subset(df, predictor == paste0('treatment1:labelfake:dsm_relative_fc', i))$`coef(reg_label2)`
  sigma2Q = vcov.matrix[position1, position1]
  sigma2W = vcov.matrix[position2, position2]
  sigmaQW <- vcov.matrix[position1,position2]
  
  s.error = sqrt(sigma2Q + sigma2W + 2*sigmaQW)
  
  
  temp = as.data.frame(c(paste0('treatment1:labelfake + treatment1:labelfake:dsm_relative_fc', i)))
  colnames(temp) = c('variable')
  temp$s.error = s.error
  temp$coefficient = coefficient2 + out$coefficients[,2][13]
  data_errors = rbind(data_errors, temp)
  
}

ggplot(data = data_errors, aes(x = variable, y = coefficient)) + 
  geom_point() + 
  theme_bw() + 
  geom_pointrange(aes(ymin = coefficient - s.error, ymax = coefficient + s.error)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_hl/label_coeff_fake.png')



data_errors = as.data.frame(c('treatment1:labelmisleading'))
colnames(data_errors) = c('variable')
data_errors$s.error = out$coefficients[,2][13]
data_errors$coefficient =  subset(df, predictor == 'treatment1:labelmisleading')$`coef(reg_label2)`

for (i in c(1:8, 15)){
  
  position1 = subset(df, predictor == 'treatment1:labelmisleading')$counter
  position2 = subset(df, predictor == paste0('treatment1:labelmisleading:dsm_relative_fc', i))$counter
  coefficient2 = subset(df, predictor == paste0('treatment1:labelmisleading:dsm_relative_fc', i))$`coef(reg_label2)`
  sigma2Q = vcov.matrix[position1, position1]
  sigma2W = vcov.matrix[position2, position2]
  sigmaQW <- vcov.matrix[position1,position2]
  
  s.error = sqrt(sigma2Q + sigma2W + 2*sigmaQW)
  
  
  temp = as.data.frame(c(paste0('treatment1:labelmisleading + treatment1:labelmisleading:dsm_relative_fc', i)))
  colnames(temp) = c('variable')
  temp$s.error = s.error
  temp$coefficient = coefficient2 + out$coefficients[,2][13]
  data_errors = rbind(data_errors, temp)
  
}

ggplot(data = data_errors, aes(x = variable, y = coefficient)) + 
  geom_point() + 
  theme_bw() + 
  geom_pointrange(aes(ymin = coefficient - s.error, ymax = coefficient + s.error)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_hl/label_coeff_misleading.png')


#### #### #### #### 
#### LABELS: Levels 
#### #### #### #### 

formula_reg <- as.formula(paste0('approx_likes', ## dep variable
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

reg_label2 <- felm(formula = formula_reg, 
                   data = input_reg, 
                   clustervar = 'id_desinformacion')

vcov.matrix <- vcov(reg_label2)
df = as.data.frame(coef(reg_label2))
df$predictor = rownames(df)
df$counter = c(1:nrow(df))

out <- summary(reg_label2)
out$coefficients[,2][13]

data_errors = as.data.frame(c('treatment1:labelfake'))
colnames(data_errors) = c('variable')
data_errors$s.error = out$coefficients[,2][13]
data_errors$coefficient =  subset(df, predictor == 'treatment1:labelfake')$`coef(reg_label2)`

for (i in c(1:8, 15)){
  
  position1 = subset(df, predictor == 'treatment1:labelfake')$counter
  position2 = subset(df, predictor == paste0('treatment1:labelfake:dsm_relative_fc', i))$counter
  coefficient2 = subset(df, predictor == paste0('treatment1:labelfake:dsm_relative_fc', i))$`coef(reg_label2)`
  sigma2Q = vcov.matrix[position1, position1]
  sigma2W = vcov.matrix[position2, position2]
  sigmaQW <- vcov.matrix[position1,position2]
  
  s.error = sqrt(sigma2Q + sigma2W + 2*sigmaQW)
  
  
  temp = as.data.frame(c(paste0('treatment1:labelfake + treatment1:labelfake:dsm_relative_fc', i)))
  colnames(temp) = c('variable')
  temp$s.error = s.error
  temp$coefficient = coefficient2 + out$coefficients[,2][13]
  data_errors = rbind(data_errors, temp)
  
}

ggplot(data = data_errors, aes(x = variable, y = coefficient)) + 
  geom_point() + 
  theme_bw() + 
  geom_pointrange(aes(ymin = coefficient - s.error, ymax = coefficient + s.error)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_hl/label_coeff_fake_levels.png')



data_errors = as.data.frame(c('treatment1:labelmisleading'))
colnames(data_errors) = c('variable')
data_errors$s.error = out$coefficients[,2][13]
data_errors$coefficient =  subset(df, predictor == 'treatment1:labelmisleading')$`coef(reg_label2)`

for (i in c(1:8, 15)){
  
  position1 = subset(df, predictor == 'treatment1:labelmisleading')$counter
  position2 = subset(df, predictor == paste0('treatment1:labelmisleading:dsm_relative_fc', i))$counter
  coefficient2 = subset(df, predictor == paste0('treatment1:labelmisleading:dsm_relative_fc', i))$`coef(reg_label2)`
  sigma2Q = vcov.matrix[position1, position1]
  sigma2W = vcov.matrix[position2, position2]
  sigmaQW <- vcov.matrix[position1,position2]
  
  s.error = sqrt(sigma2Q + sigma2W + 2*sigmaQW)
  
  
  temp = as.data.frame(c(paste0('treatment1:labelmisleading + treatment1:labelmisleading:dsm_relative_fc', i)))
  colnames(temp) = c('variable')
  temp$s.error = s.error
  temp$coefficient = coefficient2 + out$coefficients[,2][13]
  data_errors = rbind(data_errors, temp)
  
}

ggplot(data = data_errors, aes(x = variable, y = coefficient)) + 
  geom_point() + 
  theme_bw() + 
  geom_pointrange(aes(ymin = coefficient - s.error, ymax = coefficient + s.error)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_hl/label_coeff_misleading_levels.png')

#### #### #### 
### POYNTER
#### #### #### 

formula_reg <- as.formula(paste0('growth_likes', ## dep variable
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

reg_fe_poynter2 <- felm(formula = formula_reg, 
                             data = input_reg, 
                             clustervar = 'id_desinformacion')

vcov.matrix <- vcov(reg_fe_poynter2)
df = as.data.frame(coef(reg_fe_poynter2))
df$predictor = rownames(df)
df$counter = c(1:nrow(df))

out <- summary(reg_fe_poynter2)
out$coefficients[,2][13]


data_errors = as.data.frame(c('treatment1:poynter1'))
colnames(data_errors) = c('variable')
data_errors$s.error = out$coefficients[,2][13]
data_errors$coefficient =  subset(df, predictor == 'treatment1:poynter1')$`coef(reg_fe_poynter2)`

for (i in c(1:8, 15)){
  
  position1 = subset(df, predictor == 'treatment1:poynter1')$counter
  position2 = subset(df, predictor == paste0('treatment1:poynter1:dsm_relative_fc', i))$counter
  coefficient2 = subset(df, predictor == paste0('treatment1:poynter1:dsm_relative_fc', i))$`coef(reg_fe_poynter2)`
  sigma2Q = vcov.matrix[position1, position1]
  sigma2W = vcov.matrix[position2, position2]
  sigmaQW <- vcov.matrix[position1,position2]
  
  s.error = sqrt(sigma2Q + sigma2W + 2*sigmaQW)
  
  
  temp = as.data.frame(c(paste0('treatment1:poynter1 + treatment1:poynter1:dsm_relative_fc', i)))
  colnames(temp) = c('variable')
  temp$s.error = s.error
  temp$coefficient = coefficient2 + out$coefficients[,2][13]
  data_errors = rbind(data_errors, temp)
  
}

ggplot(data = data_errors, aes(x = variable, y = coefficient)) + 
  geom_point() + 
  theme_bw() + 
  geom_pointrange(aes(ymin = coefficient - s.error, ymax = coefficient + s.error)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_hl/poynter_coeff.png')


#### levels

formula_reg <- as.formula(paste0('approx_likes', ## dep variable
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

reg_fe_poynter2 <- felm(formula = formula_reg, 
                        data = input_reg, 
                        clustervar = 'id_desinformacion')

vcov.matrix <- vcov(reg_fe_poynter2)
df = as.data.frame(coef(reg_fe_poynter2))
df$predictor = rownames(df)
df$counter = c(1:nrow(df))

out <- summary(reg_fe_poynter2)
out$coefficients[,2][13]


data_errors = as.data.frame(c('treatment1:poynter1'))
colnames(data_errors) = c('variable')
data_errors$s.error = out$coefficients[,2][13]
data_errors$coefficient =  subset(df, predictor == 'treatment1:poynter1')$`coef(reg_fe_poynter2)`

for (i in c(1:8, 15)){
  
  position1 = subset(df, predictor == 'treatment1:poynter1')$counter
  position2 = subset(df, predictor == paste0('treatment1:poynter1:dsm_relative_fc', i))$counter
  coefficient2 = subset(df, predictor == paste0('treatment1:poynter1:dsm_relative_fc', i))$`coef(reg_fe_poynter2)`
  sigma2Q = vcov.matrix[position1, position1]
  sigma2W = vcov.matrix[position2, position2]
  sigmaQW <- vcov.matrix[position1,position2]
  
  s.error = sqrt(sigma2Q + sigma2W + 2*sigmaQW)
  
  
  temp = as.data.frame(c(paste0('treatment1:poynter1 + treatment1:poynter1:dsm_relative_fc', i)))
  colnames(temp) = c('variable')
  temp$s.error = s.error
  temp$coefficient = coefficient2 + out$coefficients[,2][13]
  data_errors = rbind(data_errors, temp)
  
}

ggplot(data = data_errors, aes(x = variable, y = coefficient)) + 
  geom_point() + 
  theme_bw() + 
  geom_pointrange(aes(ymin = coefficient - s.error, ymax = coefficient + s.error)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_hl/poynter_coeff_levels.png')

#### #### #### 
#### POPULAR
#### #### #### 

formula_reg <- as.formula(paste0('growth_likes', ## dep variable
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

reg_fe_popular2 <- felm(formula = formula_reg, 
                             data = input_reg, 
                             clustervar = 'id_desinformacion')

vcov.matrix <- vcov(reg_fe_popular2)
df = as.data.frame(coef(reg_fe_popular2))
df$predictor = rownames(df)
df$counter = c(1:nrow(df))

out <- summary(reg_fe_popular2)
out$coefficients[,2][13]


data_errors = as.data.frame(c('treatment1:popular'))
colnames(data_errors) = c('variable')
data_errors$s.error = out$coefficients[,2][13]
data_errors$coefficient =  subset(df, predictor == 'treatment1:popular')$`coef(reg_fe_popular2)`

for (i in c(1:8, 15)){
  
  position1 = subset(df, predictor == 'treatment1:popular')$counter
  position2 = subset(df, predictor == paste0('treatment1:popular:dsm_relative_fc', i))$counter
  coefficient2 = subset(df, predictor == paste0('treatment1:popular:dsm_relative_fc', i))$`coef(reg_fe_popular2)`
  sigma2Q = vcov.matrix[position1, position1]
  sigma2W = vcov.matrix[position2, position2]
  sigmaQW <- vcov.matrix[position1,position2]
  
  s.error = sqrt(sigma2Q + sigma2W + 2*sigmaQW)
  
  
  temp = as.data.frame(c(paste0('treatment1:popular + treatment1:popular:dsm_relative_fc', i)))
  colnames(temp) = c('variable')
  temp$s.error = s.error
  temp$coefficient = coefficient2 + out$coefficients[,2][13]
  data_errors = rbind(data_errors, temp)
  
}

ggplot(data = data_errors, aes(x = variable, y = coefficient)) + 
  geom_point() + 
  theme_bw() + 
  geom_pointrange(aes(ymin = coefficient - s.error, ymax = coefficient + s.error)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_hl/popular_coeff.png')

### Levels

formula_reg <- as.formula(paste0('approx_likes', ## dep variable
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

reg_fe_popular2 <- felm(formula = formula_reg, 
                        data = input_reg, 
                        clustervar = 'id_desinformacion')

vcov.matrix <- vcov(reg_fe_popular2)
df = as.data.frame(coef(reg_fe_popular2))
df$predictor = rownames(df)
df$counter = c(1:nrow(df))

out <- summary(reg_fe_popular2)
out$coefficients[,2][13]

data_errors = as.data.frame(c('treatment1:popular'))
colnames(data_errors) = c('variable')
data_errors$s.error = out$coefficients[,2][13]
data_errors$coefficient =  subset(df, predictor == 'treatment1:popular')$`coef(reg_fe_popular2)`

for (i in c(1:8, 15)){
  
  position1 = subset(df, predictor == 'treatment1:popular')$counter
  position2 = subset(df, predictor == paste0('treatment1:popular:dsm_relative_fc', i))$counter
  coefficient2 = subset(df, predictor == paste0('treatment1:popular:dsm_relative_fc', i))$`coef(reg_fe_popular2)`
  sigma2Q = vcov.matrix[position1, position1]
  sigma2W = vcov.matrix[position2, position2]
  sigmaQW <- vcov.matrix[position1,position2]
  
  s.error = sqrt(sigma2Q + sigma2W + 2*sigmaQW)
  
  
  temp = as.data.frame(c(paste0('treatment1:popular + treatment1:popular:dsm_relative_fc', i)))
  colnames(temp) = c('variable')
  temp$s.error = s.error
  temp$coefficient = coefficient2 + out$coefficients[,2][13]
  data_errors = rbind(data_errors, temp)
  
}

ggplot(data = data_errors, aes(x = variable, y = coefficient)) + 
  geom_point() + 
  theme_bw() + 
  geom_pointrange(aes(ymin = coefficient - s.error, ymax = coefficient + s.error)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggsave('5-analysis/3-descriptives_reg/coefficients_hl/popular_coeff_levels.png')
