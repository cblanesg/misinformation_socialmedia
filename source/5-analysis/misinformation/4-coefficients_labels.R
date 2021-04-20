
rm(list=ls()) ## clean enviornment

library(tidyverse)
library(readxl)
library(zoo)
library(lfe)
library(coefplot)

setwd(dir = '~/misinformation_socialmedia/data/')

### Load data

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


################# Plot coefficients

#### panel A: total interactions
reg_label <- felm(formula = growth_interactions ~ as.factor(days_since_factcheck) * label_desinformacion | 
                            as.factor(days_since_factcheck) + id_post_desinformacion + 
                            as.factor(days_since_publication), 
                          data = input_reg)


df<- as.data.frame(coef(reg_label))
df$predictors <- rownames(df)

df_fake <- df[grepl("label_desinformacionfake", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_label, coefficients = c(df_fake$predictors)) + 
  scale_y_discrete(labels=c(-14:15)) + 
  geom_hline(yintercept = 15, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient')  + 
  ggsave('5-analysis/3-descriptives_reg/label/coeff_panel_a_fake.png')



df_misleading <- df[grepl("label_desinformacionmisleading", df[["predictors"]]), ]
df_misleading <- df_misleading[grepl("days_since_factcheck", df_misleading[["predictors"]]), ]

coefplot(reg_label, coefficients = c(df_misleading$predictors)) + 
  scale_y_discrete(labels=c(-14:15)) + 
  geom_hline(yintercept = 15, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient')  + 
  ggsave('5-analysis/3-descriptives_reg/label/coeff_panel_a_misleading.png')


### subset

reg_label_subset <- felm(formula = growth_interactions ~ as.factor(days_since_factcheck) * label_desinformacion | 
                    as.factor(days_since_factcheck) + id_post_desinformacion + 
                    as.factor(days_since_publication), 
                  data = subset_input_reg)


df<- as.data.frame(coef(reg_label_subset))
df$predictors <- rownames(df)

df_fake <- df[grepl("label_desinformacionfake", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_label, coefficients = c(df_fake$predictors)) + 
  scale_y_discrete(labels=c(-14:15)) + 
  geom_hline(yintercept = 15, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient')  + 
  ggsave('5-analysis/3-descriptives_reg/label/coeff_panel_b_fake.png')



df_misleading <- df[grepl("label_desinformacionmisleading", df[["predictors"]]), ]
df_misleading <- df_misleading[grepl("days_since_factcheck", df_misleading[["predictors"]]), ]

coefplot(reg_label, coefficients = c(df_misleading$predictors)) + 
  scale_y_discrete(labels=c(-14:15)) + 
  geom_hline(yintercept = 15, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient')  + 
  ggsave('5-analysis/3-descriptives_reg/label/coeff_panel_b_misleading.png')

#### subset 10

reg_label_subset10 <- felm(formula = growth_interactions ~ as.factor(days_since_factcheck) * label_desinformacion | 
                           as.factor(days_since_factcheck) + id_post_desinformacion + 
                           as.factor(days_since_publication), 
                         data = subset10_input_reg)


df<- as.data.frame(coef(reg_label_subset10))
df$predictors <- rownames(df)

df_fake <- df[grepl("label_desinformacionfake", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_label, coefficients = c(df_fake$predictors)) + 
  scale_y_discrete(labels=c(-9:10)) + 
  geom_hline(yintercept = 10, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient')  + 
  ggsave('5-analysis/3-descriptives_reg/label/coeff_panel_c_fake.png')



df_misleading <- df[grepl("label_desinformacionmisleading", df[["predictors"]]), ]
df_misleading <- df_misleading[grepl("days_since_factcheck", df_misleading[["predictors"]]), ]

coefplot(reg_label, coefficients = c(df_misleading$predictors)) + 
  scale_y_discrete(labels=c(-9:10)) + 
  geom_hline(yintercept = 10, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient')  + 
  ggsave('5-analysis/3-descriptives_reg/label/coeff_panel_c_misleading.png')


#### subset 5

reg_label_subset5 <- felm(formula = growth_interactions ~ as.factor(days_since_factcheck) * label_desinformacion | 
                             as.factor(days_since_factcheck) + id_post_desinformacion + 
                             as.factor(days_since_publication), 
                           data = subset5_input_reg)


df<- as.data.frame(coef(reg_label_subset5))
df$predictors <- rownames(df)

df_fake <- df[grepl("label_desinformacionfake", df[["predictors"]]), ]
df_fake <- df_fake[grepl("days_since_factcheck", df_fake[["predictors"]]), ]

coefplot(reg_label, coefficients = c(df_fake$predictors)) + 
  scale_y_discrete(labels=c(-4:5)) + 
  geom_hline(yintercept = 5, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient')  + 
  ggsave('5-analysis/3-descriptives_reg/label/coeff_panel_d_fake.png')



df_misleading <- df[grepl("label_desinformacionmisleading", df[["predictors"]]), ]
df_misleading <- df_misleading[grepl("days_since_factcheck", df_misleading[["predictors"]]), ]

coefplot(reg_label, coefficients = c(df_misleading$predictors)) + 
  scale_y_discrete(labels=c(-4:5)) + 
  geom_hline(yintercept = 5, linetype = 'dashed', color = 'red') +
  coord_flip() +
  theme_bw() + 
  labs(title = '', 
       y = 'Days since Fact Check', 
       x = 'Coefficient')  + 
  ggsave('5-analysis/3-descriptives_reg/label/coeff_panel_d_misleading.png')
