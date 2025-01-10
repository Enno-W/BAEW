print_all_histograms(df, bins_n = 30)
print_all_histograms(df[correlation_variables])
custom_labels<-c(
  Age = "Alter",
  Locus = "Locus",
  Dynamics = "Variabilität",
  completed_count = "Abgeschlossene\nTrainingseinheiten",
  Goal_ave = "Durchschnitt\nZiellerreichung",
  SessionKM_ave = "Durchschnitt km pro Einheit",
  SesseionH_ave = "Durchschnitt\nh pro Einheit",
  SessionRPE_ave = "Durchschnitt\nSessionRPE",
  NA_ave = "Durchschnitt\nNegativer Affekt",
  WeeklyKM_base = "Baseline\nWöchentliche KM",
  WeeklyH_base = "Baseline\nWöchentliche H",
  WeeklyRPE_base = "Baseline\nWöchentliche RPE",
  NA_base = "Baseline\nNegativer Affekt"
)

violin_plots<-print_all_violin_boxplots(df[correlation_variables], facet_ncol = 3, facet_nrow = NULL, custom_labels = custom_labels
)

long_df$fitted_attribution <- fitted(goal_model_basic_pa)

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

time_plot<-plot_model(goal_model_basic_pa, type = "pred", terms = "Time", show.data = T, jitter = .2, grid = T ,axis.title = c("Trainingseinheit Nr.", "Wahrgenommene Zielerreichung"), title = "Vorhergesagte Ziellerreichung")

Plot_across_time <- hlm_plot+time_plot

negative_affect_plot<-plot_model(goal_model_basic_pa, type = "pred", terms = "NegativeAffect", show.data = T, jitter = .2, grid = T, axis.title = c("Negative Affect", "Wahrgenommene Zielerreichung"), title = "Vorhergesagte Ziellerreichung durch NA")
positive_affect_plot<-plot_model(goal_model_basic_pa, type = "pred", terms = "PositiveAffect", show.data = T, jitter = .2, grid = T, axis.title = c("Positive Affect", "Wahrgenommene Zielerreichung"), title = "Vorhergesagte Ziellerreichung durch PA")

PA_NA_plot <-negative_affect_plot+positive_affect_plot
plot_model(goal_model_basic_pa, type = "diag") 
