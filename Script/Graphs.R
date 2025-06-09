print_all_histograms(df, bins_n = 30)
print_all_histograms(df[correlation_variables])
custom_labels<-c(
  Age = "Alter",
  Locus = "Lokus",
  Dynamics = "Variabilität",
  completed_count = "Abgeschlossene\nTrainingseinheiten",
  Goal_ave = "Durchschnitt\nZiellerreichung",
  NA_ave = "Durchschnitt\nNegativer Affekt",
  NA_base = "Negativer Affekt\n(Trait)"
)

violin_plots<-print_all_violin_boxplots(df[correlation_variables], facet_ncol = 4, facet_nrow = NULL, custom_labels = custom_labels
)

### Plots zum Überprüfen der Voraussetzungen

# ohne Rangtransformation
diag_plots_no_ranktransform <-plot_model(goal_model5_no_ranktransform, 
                                         type = "diag") 
cleaned_diag_plots_no_ranktransform <- lapply(diag_plots_no_ranktransform, function(p) {
  p +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank()
    )+
    theme_blank()
})


plot_resid_vs_fitted_no_ranktransform <- cleaned_diag_plots_no_ranktransform[[1]]+
  labs(x = "Theoretische Quantile",
       y = "Residuen")
plot_norm_no_ranktransform<- cleaned_diag_plots_no_ranktransform[[2]]+
  labs(x = "Residuen", 
       y = "Dichte")
plot_heteroscedasticity_diag_no_ranktransform <- cleaned_diag_plots_no_ranktransform[[3]]+
  labs(x = "Vorhergesagte Werte",
       y = "Residuen")


# mit Rangtransformation
diag_plots <-plot_model(goal_model5, 
                        type = "diag") 
cleaned_diag_plots <- lapply(diag_plots, function(p) {
  p +
    labs(title = NULL, subtitle = NULL, caption = NULL) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank()
    )+
    theme_blank()
})

plot_resid_vs_fitted <- cleaned_diag_plots[[1]]+
  labs(x = "Theoretische Quantile",
       y = "Residuen")
plot_norm <- cleaned_diag_plots[[2]]+
  labs(x = "Residuen", 
       y = "Dichte")
plot_heteroscedasticity_diag <- cleaned_diag_plots[[3]]+
  labs(x = "Vorhergesagte Werte",
       y = "Residuen")


long_df$fitted_attribution <- fitted(goal_model5)

hlm_plot<-ggplot(long_df, aes(x = Time, y = fitted_attribution, group = ID, color = as.factor(ID))) +
  geom_line(show.legend = F)+
  geom_point(aes(x= Time, y= Goal), show.legend = F)+
  labs(x = "Time", y = "Fitted Goal") +
  theme_minimal()+
  labs(
    x = "Trainingseinheit Nr.",
    y = "Wahrgenommene Zielerreichung",
    color = "ID"
  )

pred_plot_NA_base<-plot_model(goal_model5, type = "pred", terms = "NA_base_centered", show.data = T, jitter = .2, grid = T ,axis.title = c("Negativer Affekt (Prä-Test)", "Wahrgenommene Zielerreichung"))
Plot_across_time <- hlm_plot + pred_plot_NA_base

# Vorhersagen für Interaktion berechnen
pred <- ggpredict(goal_model5, terms = c("Dynamics_centered", "NA_base_centered"))
plot(pred)

# Plotten
interaction_plot_NA.Dynamics <-ggplot(pred, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "Stabilität",
    y = "Vorhergesagte Zielerreichung",
    color = "Negativer Affekt",
    fill = "Negativer Affekt"
  ) +
  theme_minimal()


# Vorhersagen für Interaktion berechnen
pred2 <- ggpredict(goal_model5, terms = c("NA_base_centered", "NegativeAffect_cm_centered"))
plot(pred2)
# Plotten
interaction_plot_NAbase.NAsession <- ggplot(pred2, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "Negativer Affekt (Prä-Test)",
    y = "Vorhergesagte Zielerreichung",
    color = "Negativer Affekt (situational)",
    fill = "Negativer Affekt (situational)"
  ) +
  theme_minimal()

