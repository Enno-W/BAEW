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

violin_plots<-print_all_violin_boxplots(df[correlation_variables], facet_ncol = 2, facet_nrow = NULL, custom_labels = custom_labels
)
