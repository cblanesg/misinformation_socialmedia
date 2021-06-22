input_reg


gg_data3 <- input_reg %>%
  group_by(days_since_publication, label, dsm_relative_fc)  %>%
  summarise(likes = mean(approx_likes, na.rm = TRUE), 
            shares =mean(approx_shares, na.rm = TRUE),
            reactions = mean(approx_reactions, na.rm = TRUE), 
            comments =mean(approx_comments, na.rm = TRUE),
            interactions =  mean(approx_interactions, na.rm = TRUE))  %>%
  gather(likes:interactions, key = 'interaction', value = 'value')


for (i in unique(gg_data3$dsm_relative_fc)){
  ggplot(subset(gg_data3, dsm_relative_fc == '3'), aes(x = days_since_publication, y = value, colour = interaction)) + 
    geom_point()  + 
    geom_line() + 
    xlim(0, 30)+
    ylim(0, 400) + 
    facet_grid(.~label, scales = 'free')+
    geom_vline(xintercept  = 0, color = 'red', 
               linetype = 'dotted')  +
    labs(x = 'Days since Fact Check',
         y = 'Accumulated') +
    theme_bw() + 
    theme(text = element_text(size=15),
          #strip.background = element_rect(colour="white", fill="white"),
          legend.position=c(.9,.75), 
          legend.title =  element_blank()
    ) + 
    ggsave(paste0('6-descriptives/misinformation/accumulated_factcheck_dsm_relative_fc_', i, '.png'))
}


ggplot(gg_data3, aes(x = days_since_publication, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  xlim(0, 30)+
  ylim(0, 1000) + 
  facet_grid(.~label, scales = 'free')+
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(x = 'Days since Fact Check',
       y = 'Accumulated') +
  theme_bw() + 
  theme(text = element_text(size=15),
        #strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.9,.75), 
        legend.title =  element_blank()
  ) + 
  ggsave('6-descriptives/misinformation/levels_factcheck_label.png')