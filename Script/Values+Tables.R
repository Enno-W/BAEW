#### Creating a list with all commonly used descriptive statistics + other descriptive values ###############################################
descriptives_list<-mean_sd_median_min_max(df)
vars_not_normal<-which_var_not_normal(df)
vars_not_normal_with_p_values<-df %>% select(-ends_with("_1"), -ends_with("_2"), -ends_with("_3"), -ends_with("_4"), -ends_with("_5"), -ends_with("_6"), -"completed_count", -"Missings_amount", -"SesseionH_ave", -"WeeklyKM_base", -"WeeklyH_base", -"SessionKM_ave") %>% which_var_not_normal_p() %>% mutate(across(where(is.numeric), ~ ifelse(. < 0.001, "< .001", as.character(round(.,3)))))
stat.desc(df$Locus, basic = F, norm = T)
stat.desc(df$Dynamics, basic = F, norm = T)
hist(df$Locus, breaks = 15)

####demographics table ###############################################
demographicsdata<- select(df, Age, Gender, Sport)  %>% tbl_summary( percent = "column", by = Gender) %>%  add_p() %>% add_overall() %>% as.data.frame()

data <- demographicsdata %>%
  rename_with(~ gsub("\\*\\*", "", .)) %>% # To remove the asterisks from the headers. 
  mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

demographicstable<- data %>% flextable() %>% flextable::theme_apa() %>% autofit()

#### correlation table ####
average_variables<-df %>% select(ends_with("_ave")) %>% names()
base_variables<-df %>% select(ends_with("_base")) %>% names()
correlation_variables<-c("Age", "Locus", "Dynamics", "completed_count", "Goal_ave", "NA_ave", "NA_base")
ave_corr_table<-df[,correlation_variables] %>% 
  generate_correlation_table2(c(
    "1. Alter", 
    "2. Locus", 
    "3. Variabilität", 
    "4. n (Trainingseinheiten)", 
    "5. M (Ziellerreichung)", 
    "6. M (Negativer Affekt)", 
    "7. Negativer Affekt bei Baseline Messung"
  ))

#### Skewness, Kurtosis and min-max range table###############################################

df_stat<-get_descriptive_table(df[, correlation_variables], language = "German")

df_stat$Variable <- c(
  "Alter", 
  "Locus", 
  "Variabilität", 
  "n (Trainingseinheiten)", 
  "M (Ziellerreichung)", 
  "M (Negativer Affekt)", 
  "Baseline negativer Affekt"
)

table_stat<-df_stat %>% flextable() %>% flextable::theme_apa() %>% autofit()
 


#### Power-Analyse ####
pwr_result <- pwr.r.test(n = NULL,         
                     r = 0.5,           
                     sig.level = 0.05,  
                     power = 0.95,      
                     alternative = "greater") 


#### Regression Analyses ###############################################

###General Linear Models####
#Hypothese 1.1 Ein internal - variabler Attributionsstil sagt weniger Trainingsausfälle voraus; # Hypothesis 2.1 Ein negatives Affekterleben sagt mehr Trainingsausfälle voraus
count_model <- glmer(completed_count ~ 1 + Time + 
                       Locus_centered + 
                       Dynamics_centered + 
                       NA_base_centered+
                       NegativeAffect_cm_centered + 
                       PositiveAffect_cm_centered+ 
                       SessionRPE_cm_centered+ 
                       PA_base_centered+
                       (1 | ID), 
                    data = long_df, 
                    family = "poisson")
tidy_model<-tidy(count_model, effects = "fixed", p.value=TRUE ,conf.int = TRUE) # see huxreg documentation as for why this is necessary. This is defining a "tidy()-function"
summary(count_model)

### Table Output ####
glmtable<-huxreg("Modell 1" = count_model ,
                 statistics = c("Anzahl der Beobachtungen im Modell" ="nobs", 
                                "Freiheitsgrade" = "df.residual"), 
                 number_format = 3, 
                 bold_signif = 0.05, 
                 error_pos = "right",
                 coefs = c("Interzept"                      = "(Intercept)",
                           "Attributionsstil: Lokus"        = "Locus_centered",
                           "Attributionsstil: Variabilität" = "Dynamics_centered",
                           "Negativer Affekt (Trait)"       = "NA_base_centered",
                           "Negativer Affekt (State)"       = "NegativeAffect_cm_centered",
                           "Positiver Affekt (Trait)"       = "PA_base_centered",
                           "Positiver Affekt (State)"       = "PositiveAffect_cm_centered",
                           "Wahrgenommene Erschöpfung"      = "SessionRPE_cm_centered",
                           "Zeit"                           = "Time"))


####Binomiales Modell ###
long_df<-long_df%>% mutate(Status = ifelse(completed_count == 6, 1, 0))

long_df <- long_df %>%
  mutate(
    Locus_z = scale(Locus),
    Dynamics_z = scale(Dynamics),
    NA_base_z = scale(NA_base),
    NegativeAffect_z = scale(NegativeAffect)
  )
count_model_binomial <- glmer(Status ~ 1 +
                       Locus_z + 
                       Dynamics_z + 
                       NA_base_z +
                       NegativeAffect_z +
                       (1 | ID), 
                     data = long_df, 
                     family = "binomial")

binomialglmm_table<-huxreg("Modell 1" = count_model_binomial ,
                 statistics = c("Anzahl der Beobachtungen im Modell" ="nobs", 
                                "Freiheitsgrade" = "df.residual"), 
                 number_format = 3, 
                 bold_signif = 0.05, 
                 error_pos = "right",
                 coefs = c("Interzept"                      = "(Intercept)",
                           "Attributionsstil: Lokus"        = "Locus_z",
                           "Attributionsstil: Variabilität" = "Dynamics_z",
                           "Negativer Affekt (Trait)"       = "NA_base_z",
                           "Negativer Affekt (State)"       = "NegativeAffect_z"))

### Direkter Vergleich der Gruppenunterschiede

df <- df %>%
  mutate(Status = if_else(completed_count == 6, "abgeschlossen", "nicht abgeschlossen"))

vergleich_plot <- function(data, y_var_name, y_label = NULL) {
  ggplot(data, aes(x = Status, y = .data[[y_var_name]], group = Status, fill = Status)) +
    geom_violin(trim = FALSE, color = NA, alpha = 0.5) +
    geom_boxplot(width = 0.1, outlier.shape = NA, color = "black", fill="grey") +
    stat_boxplot(geom="errorbar", width=.5)+
    geom_point(position = position_jitter(width = 0.15), shape = 21, color = "black") +
    scale_fill_manual(values = c("abgeschlossen" = "black", "nicht abgeschlossen" = "grey")) +
    labs(
      x = NULL,
      y = if (!is.null(y_label)) y_label else y_var_name
    ) +
    theme_blank()+
    theme(legend.position = "none")
}

A<-vergleich_plot(df, "NA_ave", "M negativer Affekt")
B<-vergleich_plot(df, "NA_base", "Negativer Affekt bei Baseline")
C<-vergleich_plot(df, "Dynamics", "Variabilität")
D<-vergleich_plot(df, "Locus", "Locus")

Status_vergleich_plot<-((A+B)/(C+D))

df<-df%>% mutate(Status = ifelse(completed_count == 6, 1, 0))
t.test(NA_ave ~ Status, data = df)
t.test(Dynamics ~ Status, data = df)
t.test(NA_base ~ Status, data = df)
t.test(Locus ~ Status, data = df)
## Ausgabe der Werte in einer Tabelle ##
# Load required packages
# Run t-tests
test_results <- list(
  NA_ave = t.test(NA_ave ~ Status, data = df),
  Dynamics = t.test(Dynamics ~ Status, data = df),
  NA_base = t.test(NA_base ~ Status, data = long_df),
  Locus = t.test(Locus ~ Status, data = long_df)
)

# Sample sizes per group (assuming consistent across vars)
n1 <- table(df$Status)[["1"]]
n0 <- table(df$Status)[["0"]]

# Build result table with Cohen's d, 95% CI, and Power
results_df <- bind_rows(lapply(names(test_results), function(var) {
  test <- test_results[[var]]
  data_used <- if (var %in% c("NA_base", "Locus")) long_df else df
  
  # Calculate effect size
  effsize <- cohens_d(as.formula(paste(var, "~ Status")), 
                      data = data_used, ci = 0.95)
  d_val <- round(effsize$Cohens_d, 2)
  ci_lower <- round(effsize$CI_low, 2)
  ci_upper <- round(effsize$CI_high, 2)
  d_ci <- paste0(d_val, " [", ci_lower, ", ", ci_upper, "]")
  
  # Power calculation
  power_val <- round(pwr.t.test(d = d_val, 
                                n = min(n1, n0), 
                                sig.level = 0.05, 
                                type = "two.sample", 
                                alternative = "two.sided")$power, 2)
  
  data.frame(
    Prädiktor = var,
    t_Wert = round(test$statistic, 2),
    df = round(test$parameter, 0),
    p_Wert = format.pval(test$p.value, digits = 3),
    `Mittelwert vollständig` = round(test$estimate[["mean in group 1"]], 2),
    `Mittelwert unvollständig` = round(test$estimate[["mean in group 0"]], 2),
    `Cohen’s d (95% CI)` = d_ci,
    Power = power_val
  )
}))

# Create formatted flextable
ttesttable<-flextable(results_df) %>%
  set_header_labels(
    Prädiktor = "Prädiktor",
    t_Wert = "t-Wert",
    df = "df",
    p_Wert = "p-Wert",
    `Mittelwert vollständig` = "Mittelwert\nvollständig",
    `Mittelwert unvollständig` = "Mittelwert\nunvollständig",
    `Cohen’s d (95% CI)` = "Cohen’s d (95%-KI)",
    Power = "Power"
  ) %>% 
  autofit()



  #################Hierarchical linear models #################


long_df_z$Goal_rank <-rank(long_df_z$Goal) # Identisch: rank(long_df$Goal)==rank(long_df_z$Goal)

null_model_goal<-lme(
  Goal_rank~1,
  data = long_df_z,
  random = ~ 1| ID,
  method = "REML",
  na.action = na.omit,
  correlation = corAR1(form = ~ Time | ID)
)
icc_goal<-icc(null_model_goal)

goal_model1 <- update(null_model_goal, . ~ . + Locus_centered + Dynamics_centered)

goal_model2 <- update(goal_model1, . ~ . + Locus_centered + Dynamics_centered+ NA_base_centered + NegativeAffect_cm_centered)

goal_model3 <- update(goal_model2, . ~ . + Locus_centered * NegativeAffect_cm_centered + 
                        Dynamics_centered * NegativeAffect_cm_centered)

goal_model4 <- update(goal_model3, . ~ . + Locus_centered * NA_base_centered + 
                        Dynamics_centered * NA_base_centered)

goal_model5 <- update(goal_model4, . ~ . +NegativeAffect_cm_centered * NA_base_centered + 
                        Dynamics_centered * Locus_centered)

null_model_goal_no_ranktransform<-lme(
  Goal~1,
  data = long_df_z,
  random = ~ 1| ID,
  method = "REML",
  na.action = na.omit,
  correlation = corAR1(form = ~ Time | ID)
)

goal_model1_no_ranktransform <- update(null_model_goal_no_ranktransform, . ~ . + Locus_centered + Dynamics_centered)

goal_model2_no_ranktransform <- update(goal_model1_no_ranktransform, . ~ . + Locus_centered + Dynamics_centered+ NA_base_centered + NegativeAffect_cm_centered)

goal_model3_no_ranktransform <- update(goal_model2_no_ranktransform, . ~ . + Locus_centered * NegativeAffect_cm_centered + 
                        Dynamics_centered * NegativeAffect_cm_centered)

goal_model4_no_ranktransform <- update(goal_model3_no_ranktransform, . ~ . + Locus_centered * NA_base_centered + 
                        Dynamics_centered * NA_base_centered)

goal_model5_no_ranktransform <- update(goal_model4_no_ranktransform, . ~ . +NegativeAffect_cm_centered * NA_base_centered + 
                        Dynamics_centered * Locus_centered)



### Table Output #####
hlmtable<-huxreg("Nullmodell" = null_model_goal, 
                 "Modell 1" = goal_model1 , 
                 "Modell 2" = goal_model2 ,
                 "Modell 3" = goal_model3 ,
                 "Modell 4" = goal_model4 ,
                 "Modell 5" = goal_model5 ,
                 statistics = c("AIC", "BIC"), 
                 number_format = 3, 
                 bold_signif = 0.1, 
                 tidy_args =  list(effects = "fixed"), error_pos="right",
                 coefs = c("Interzept"                                = "(Intercept)",
                           " Lokus"                  = "Locus_centered",
                           "Variabilität"           = "Dynamics_centered",
                           "NA (Trait)"                 = "NA_base_centered",
                           "NA (State)"                 = "NegativeAffect_cm_centered",
                           "Lokus x NA (State)"          = "Locus_centered:NegativeAffect_cm_centered",
                           "Variabilität x NA (State)"   = "Dynamics_centered:NegativeAffect_cm_centered",
                           "Lokus x NA (Trait)"          = "Locus_centered:NA_base_centered",
                           "Variabilität x NA (Trait)"   = "Dynamics_centered:NA_base_centered",
                           "NA (Trait) x NA (State)" = "NA_base_centered:NegativeAffect_cm_centered",
                           "Lokus x Variabilität"                     = "Locus_centered:Dynamics_centered")
)

#####Überprüfen der Voraussetzungen  http://www.regorz-statistik.de/inhalte/r_HLM_2.html ###############################################
# Extrahieren der Residuen
Goal_residuals <- hlm_resid(goal_model5, level=1, include.ls = T) 
########Normality Plots #####
# Normality Plot with least square residuals
ndist_plot_km_ls <- ggplot(data = Goal_residuals  , aes(.ls.resid)) +
  geom_histogram(aes(y = after_stat(density)), bins=30) +
  stat_function(fun = dnorm,
                args = list(mean = mean(Goal_residuals  $.ls.resid),
                            sd = sd(Goal_residuals  $.ls.resid)), linewidth=2) # The "groups" (measurement points) are small, thus some are "rank deficient" - the regular residuals provide a less biased estimate

# Normality Plot with residuals
ndist_plot_km<- ggplot(data = Goal_residuals, aes(.resid)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "gray80",
                 color = "black") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(Goal_residuals$.resid),
      sd = sd(Goal_residuals$.resid)
    ),
    linewidth = 1,
    color = "black",
    linetype = "dashed"
  ) +
  labs(
    x = "Residuals",
    y = "Density"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black")
  )

# QQ line
qqplot_km<-ggplot(Goal_residuals, aes(sample = .resid)) +
  stat_qq(shape = 21, color = "black", fill = "gray80", size = 2) +
  stat_qq_line(color = "black", linetype = "dashed") +
  labs(
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black")
  )



###### Tests of normality ###############################################

#### Test for the Attribution Model #####
ntest_shapiro_km <-shapiro.test(Goal_residuals $.resid)  
ntest_ks_km <-ks.test(Goal_residuals  $.resid, "pnorm", mean(Goal_residuals  $.resid), sd(Goal_residuals  $.resid), exact = T)

######Variance Incluence Factor ####
# model_SessionKM_2_lmer <- lmer(
#   SessionKM_lead1 ~ 
#     Pride_cm_centered * PA_cm_centered +
#     Pride_cm_centered * Dynamics_centered + 
#     Pride_cm_centered * Locus_centered + 
#     Pride_cm_centered * Globality_centered +
#     (1 | ID),
#   data = long_df,
#   na.action = na.omit,
#   REML = TRUE
# )
# vif(model_SessionKM_2_lmer) 


##### Testing for homoscedasticity (homogeneity of variance of residuals), and outliers in the residuals####

resid_plot_km<-ggplot(data = Goal_residuals, aes(x = .fitted, y = .resid)) +
  geom_point(shape = 21, color = "black", fill = "gray80", size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted Values",
    y = "Residuals",
    title = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black")
  )

#resid_plot_km+resid_plot_km_no_logtransform

###### Shapiro test of normality ###############################################

#### Test for the Attribution Model #####
shaptest_goal <-shapiro.test(Goal_residuals $.ls.resid)

### Markdown Output of p value ####
shaptest_goal_p <-if (shaptest_goal$p.value < 0.001) {
  "< .001"
} else {
  round( shaptest_goal$p.value, 3)
}