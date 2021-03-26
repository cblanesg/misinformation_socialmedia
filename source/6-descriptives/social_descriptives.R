


######

gg_data <- input_reg %>%
  group_by(n_days_since_factcheck) %>%
  count()

ggplot(gg_data, aes(x = n_days_since_factcheck, y = n)) + 
  geom_bar(stat = 'identity', color="black", fill="white") + 
  labs(title = 'Observations per period', 
       subtitle = 'Social Media Data', 
       x = 'days since fact check', 
       y = 'number observations') + 
  geom_vline(xintercept = 0, color = 'red', linetype = 'dotted') 


gg_data2 <- input_reg %>%
  group_by(n_days_since_factcheck)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments =mean(growth_comments, na.rm = TRUE))  %>%
  gather(likes:comments, key = 'interaction', value = 'value')

ggplot(gg_data2, aes(x = n_days_since_factcheck, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted')  +
  labs(title = 'Change in growth of interactions since publication of fact check in Facebook', 
       subtitle = 'Social Media Data')



gg_data3 <- input_reg %>%
  group_by(n_days_since_publication)  %>%
  summarise(likes = mean(growth_likes, na.rm = TRUE), 
            shares =mean(growth_shares, na.rm = TRUE),
            reactions = mean(growth_reactions, na.rm = TRUE), 
            comments =mean(growth_comments, na.rm = TRUE))  %>%
  gather(likes:comments, key = 'interaction', value = 'value')

ggplot(gg_data3, aes(x = n_days_since_publication, y = value, colour = interaction)) + 
  geom_point()  + 
  geom_line() + 
  geom_vline(xintercept  = 0, color = 'red', 
             linetype = 'dotted') +
  labs(title = 'Change in growth of interactions since publication of post', 
       subtitle = 'Social Media Data')
  
