ggplot(df, aes(WeeklyKM_base))+
  geom_histogram(aes(y=..density..), colour ="black", fill="white", bins = 20)+
  labs(x = "Weekly KM Baseline", y = NULL)+
scale_y_continuous(guide = "none")+
  stat_function(fun = dnorm, args=list(mean =mean (df$WeeklyKM_base, na.rm = T ), sd= sd(df$WeeklyKM_base, na.rm=T)), colour="black", linewidth =1)

df_long <- df %>%
  pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value")


ggplot(df_long, aes(value)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 20) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(guide = "none") +
  facet_wrap(~variable, scales = "free") + # Create separate panels for each variable
  stat_function(fun = dnorm,
                args = list(mean = mean(df_long$value, na.rm = TRUE),
                            sd = sd(df_long$value, na.rm = TRUE)),
                colour = "black", linewidth = 1)
df %>% sum(is.finite(df))
