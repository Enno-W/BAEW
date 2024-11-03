print_all_histograms <- function(df, bins_n=20) {
  df_long <- df %>%
    pivot_longer(cols = where(is.numeric), names_to = "variable", values_to = "value") %>% filter(!is.na(value))
  
 plot<- ggplot(df_long, aes(value)) +
    geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white", bins = bins_n) +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(guide = "none") +
    facet_wrap(~variable, scales = "free") + # Create separate panels for each variable
    stat_function(fun = dnorm,
                  args = list(mean = mean(df_long$value, na.rm = TRUE),
                              sd = sd(df_long$value, na.rm = TRUE)),
                  colour = "black", linewidth = 1)
 
 print (plot)
}


print_all_histograms(df, bins_n = 30)



