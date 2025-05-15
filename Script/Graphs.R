print_all_histograms(df, bins_n = 30)
print_all_histograms(df[correlation_variables])
custom_labels<-c(
  Age = "Alter",
  Locus = "Locus",
  Dynamics = "Variabilität",
  completed_count = "Abgeschlossene\nTrainingseinheiten",
  Goal_ave = "Durchschnitt\nZiellerreichung",
  NA_ave = "Durchschnitt\nNegativer Affekt",
  NA_base = "Baseline\nNegativer Affekt"
)

violin_plots<-print_all_violin_boxplots(df[correlation_variables], facet_ncol = 4, facet_nrow = NULL, custom_labels = custom_labels
)

long_df$fitted_attribution <- fitted(goal_model4)

hlm_plot<-ggplot(long_df, aes(x = Time, y = fitted_attribution, group = ID, color = as.factor(ID))) +
  geom_line(show.legend = F)+
  geom_point(aes(x= Time, y= Goal), show.legend = F)+
  labs(x = "Time", y = "Fitted Goal", title = "Vorhergesagte Ziellerreichung nach ID") +
  theme_minimal()+
  labs(
    x = "Trainingseinheit Nr.",
    y = "Wahrgenommene Zielerreichung",
    color = "ID"
  )

pred_plot_NA_base<-plot_model(goal_model5, type = "pred", terms = "NA_base_centered", show.data = T, jitter = .2, grid = T ,axis.title = c("Trainingseinheit Nr.", "Wahrgenommene Zielerreichung"), title = "Vorhergesagte Ziellerreichung")

interaction_plot_NA.Dynamics<-plot_model(goal_model5, type = "int", terms = c("Dynamics_centered", "NA_base_centered"), mdrt.values = "meansd", show.data = F, jitter = .2, grid = T ,axis.title = c("Stufen des Negativen Affekts", "Zeilerreichung"))

Plot_across_time <- hlm_plot+pred_plot_NA_base

negative_affect_plot<-plot_model(goal_model3, type = "pred", terms = "NegativeAffect_cm_centered", show.data = T, jitter = .2, grid = T, axis.title = c("Negative Affect", "Wahrgenommene Zielerreichung"), title = "Vorhergesagte Ziellerreichung durch NA")

Homoscedasticity_plot <-plot_model(goal_model3, type = "diag") 

## Gegenüberstellung Abgeschlossen vs. nicht abgeschlossen
