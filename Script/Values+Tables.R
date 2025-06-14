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
correlation_variables<-c("Age", "Locus", "Dynamics", "completed_count", "Goal_ave", "NA_ave", "NA_base", "SessionRPE_ave", "WeeklyRPE_base")
ave_corr_table<-df[,correlation_variables] %>% 
  generate_correlation_table2(c(
    "1. Alter", 
    "2. Locus", 
    "3. Stabilität", 
    "4. n (Trainingseinheiten)", 
    "5. M (Ziellerreichung)", 
    "6. M (Negativer Affekt)", 
    "7. Negativer Affekt im Prä-Test",
    "8. M Session RPE",
    "9. Session RPE im Prä-Test"
  ))

#### Skewness, Kurtosis and min-max range table###############################################

df_stat<-get_descriptive_table(df[, correlation_variables], language = "German")

df_stat$Variable <- c(
  "Alter", 
  "Locus", 
  "Stabilität", 
  "n (Trainingseinheiten)", 
  "M (Ziellerreichung)", 
  "M (Negativer Affekt)", 
  "Negativer Affekt im Prä-Test",
  "M Session RPE",
  "Session RPE im Prä-Test"
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
count_model_log <- glmer(completed_count ~ 1 + Time + 
                       Locus_centered + 
                       Dynamics_centered + 
                       NA_base_centered+
                       NegativeAffect_cm_centered + 
                       SessionRPE_cm_centered+ 
                       (1 | ID), 
                    data = long_df_z, 
                    family = poisson(link="log"))
count_model_sqrt <- glmer(completed_count ~ 1 + Time + 
                       Locus_centered + 
                       Dynamics_centered + 
                       NA_base_centered+
                       NegativeAffect_cm_centered + 
                       SessionRPE_cm_centered+ 
                       (1 | ID), 
                     data = long_df_z, 
                     family = poisson(link="sqrt"))
count_model_identity <- glmer(completed_count ~ 1 + Time + 
                       Locus_centered + 
                       Dynamics_centered + 
                       NA_base_centered+
                       NegativeAffect_cm_centered + 
                       SessionRPE_cm_centered+ 
                       (1 | ID), 
                     data = long_df_z, 
                     family = poisson(link="identity"))

anova(count_model_log, count_model_sqrt, count_model_identity)
### Table Output ####
glmtable<-huxreg("Modell 1 (Logarithmische Link-Funktion)" = count_model_log ,
                 "Modell 2 (Quadratwurzel-Link-Funktion )" = count_model_sqrt ,
                 "Modell 1 (Identitäts-Link-Funktion)" = count_model_identity ,
                 statistics = c("Anzahl Fälle im Modell" ="nobs", 
                                "Freiheitsgrade" = "df.residual",
                                "AIC", "BIC"), 
                 number_format = 3, 
                 bold_signif = 0.05, 
                 error_pos = "right",
                 coefs = c("Interzept"                      = "(Intercept)",
                           "Attributionsstil: Lokus"        = "Locus_centered",
                           "Attributionsstil: Stabilität" = "Dynamics_centered",
                           "Negativer Affekt im Prä-Test"       = "NA_base_centered",
                           "Situationaler Negativer Affekt"       = "NegativeAffect_cm_centered",
                           "Wahrgenommene Erschöpfung"      = "SessionRPE_cm_centered",
                           "Zeit"                           = "Time"))


####Binomiales Modell ###
long_df_z<-long_df_z%>% mutate(Status = ifelse(completed_count == 6, 1, 0))

count_model_binomial_logit <- glmer(Status ~ 1 +
                                Locus + 
                                Dynamics + 
                                NA_base +
                                NegativeAffect +
                                (1 | ID), 
                              data = long_df_z, 
                              family = binomial(link= "logit"))

count_model_binomial_probit <- glmer(Status ~ 1 +
                                Locus + 
                                Dynamics + 
                                NA_base +
                                NegativeAffect +
                                (1 | ID), 
                              data = long_df_z, 
                              family = binomial(link= "probit"))
count_model_binomial_cloglog <- glmer(Status ~ 1 +
                                Locus + 
                                Dynamics + 
                                NA_base +
                                NegativeAffect +
                                (1 | ID), 
                              data = long_df_z, 
                              family = binomial(link= "cloglog"))
binomialglmm_table<-huxreg(
                "Modell 1 (Logit-Link-Funktion)"    = count_model_binomial_logit,
                "Modell 2 (Probit-Link-Funktion)"   = count_model_binomial_probit,
                "Modell 3 (Complementary Log-Log-Link-Funktion)" = count_model_binomial_cloglog,
                statistics = c("Anzahl Fälle im Modell" ="nobs", 
                               "Freiheitsgrade" = "df.residual",
                               "AIC", "BIC"),
                 number_format = 3, 
                 bold_signif = 0.05, 
                 error_pos = "right",
                 coefs = c("Interzept"                      = "(Intercept)",
                           "Attributionsstil: Lokus"        = "Locus",
                           "Attributionsstil: Stabilität" = "Dynamics",
                           "Negativer Affekt im Prä-Test"       = "NA_base",
                           "Situationaler Negativer Affekt"       = "NegativeAffect"))

### Direkter Vergleich der Gruppenunterschiede

df_z <- df_z %>%
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

A<-vergleich_plot(df_z, "NA_ave", "M negativer Affekt")
B<-vergleich_plot(df_z, "NA_base", "Negativer Affekt im Prä-Test")
C<-vergleich_plot(df_z, "Dynamics", "Stabilität")
D<-vergleich_plot(df_z, "Locus", "Lokus")

Status_vergleich_plot<-((A+B)/(C+D))

df_z<-df_z%>% mutate(Status = ifelse(completed_count == 6, 1, 0))
t.test(NA_ave ~ Status, data = df_z)
t.test(Dynamics ~ Status, data = df_z)
t.test(NA_base ~ Status, data = df_z)
t.test(Locus ~ Status, data = df_z)
## Ausgabe der Werte in einer Tabelle ##
# Load required packages
# Run t-tests
test_results <- list(
  NA_ave = t.test(NA_ave ~ Status, data = df_z),
  Dynamics = t.test(Dynamics ~ Status, data = df_z),
  NA_base = t.test(NA_base ~ Status, data = long_df_z),
  Locus = t.test(Locus ~ Status, data = long_df_z)
)

# Sample sizes per group (assuming consistent across vars)
n1 <- table(df_z$Status)[["1"]]
n0 <- table(df_z$Status)[["0"]]

# Build result table with Cohen's d, 95% CI, and Power
results_df <- bind_rows(lapply(names(test_results), function(var) {
  test <- test_results[[var]]
  data_used <- if (var %in% c("NA_base", "Locus")) long_df_z else df_z
  
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



  #################HLMs #################


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

#### Linearität ####

ggplot(data=long_df_z, aes(x=Locus_centered, y=Goal_rank)) +
  geom_point() + geom_smooth()
ggplot(data=long_df_z, aes(x=Dynamics_centered, y=Goal_rank)) +
  geom_point() + geom_smooth()
ggplot(data=long_df_z, aes(x=NA_base_centered, y=Goal_rank)) +
  geom_point() + geom_smooth()
ggplot(data=long_df_z, aes(x=NegativeAffect_cm_centered, y=Goal_rank)) +
  geom_point() + geom_smooth()

# Als facet plot (ChatGPT generiert)
# Auswahl der interessierenden Prädiktoren
facet_df <- long_df_z %>%
  select(Goal_rank,
         Locus_centered,
         Dynamics_centered,
         NA_base_centered,
         NegativeAffect_cm_centered) %>%
  rename(
    "Lokus" = Locus_centered,
    "Stabilität" = Dynamics_centered,
    "Negativer Affekt (Prä-Test)" = NA_base_centered,
    "Negativer Affekt (situational)" = NegativeAffect_cm_centered
  ) %>%
  tidyr::pivot_longer(
    cols = c("Lokus", "Stabilität", "Negativer Affekt (Prä-Test)", "Negativer Affekt (situational)"),
    names_to = "Prädiktor",
    values_to = "Wert"
  )

# Facet-Plot 
linearity_check_graph<-ggplot(facet_df, aes(x = Wert, y = Goal_rank)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "firebrick") +
  facet_wrap(~Prädiktor, scales = "free_x") +
  labs(x = "Prädiktorwert", y = "Wahrgenommene Zielerreichung") +
  theme_blank(base_size = 20)


#linearity_check_model <-  lm(Goal_rank ~ Locus_centered + Dynamics_centered + 
#                          NA_base_centered + NegativeAffect_cm_centered,
#                          data = long_df_z)
#crPlots(lm_check_simple)

## VIF ###
goal_model5_lme4 <- lmer(
  Goal_rank ~ 
    Locus_centered * NegativeAffect_cm_centered +
    Dynamics_centered * NegativeAffect_cm_centered +
    Locus_centered * NA_base_centered +
    Dynamics_centered * NA_base_centered +
    NegativeAffect_cm_centered * NA_base_centered +
    Dynamics_centered * Locus_centered +
    (1 | ID),
  data = long_df_z,
  REML = TRUE,
  na.action = na.omit
)
vif_goal_model<-vif(goal_model5_lme4)

#### Post-Hoc-Poweranalyse mit simr ###

#power_result_nabase_nasession <- powerSim(goal_model5_lme4, fixed("NA_base_centered:NegativeAffect_cm_centered", "t"), nsim = 100)
#power_result_nabase_dynamics <- powerSim(goal_model5_lme4, fixed("Dynamics_centered:NA_base_centered", "t"), nsim = 100) 
# Zu komplex, Simulation konvergiert nicht

goal_model5_reduced_NA <- lmer(
  Goal_rank ~ NA_base_centered * NegativeAffect_cm_centered+
    (1 | ID),
  data = long_df_z,
  REML = FALSE
)

goal_model5_reduced_dyn_NA <- lmer(
  Goal_rank ~ 
    Dynamics_centered* NA_base_centered+
    (1 | ID),
  data = long_df_z,
  REML = FALSE
)
#powerSim(goal_model5_reduced_NA, fixed("NA_base_centered:NegativeAffect_cm_centered", "t"), nsim = 100)
#powerSim(goal_model5_reduced_dyn_NA, fixed("Dynamics_centered:NA_base_centered", "t"), nsim = 100)

  ### Vergleich mit dem Modell ohne Rangtransformation

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
                           "Stabilität"           = "Dynamics_centered",
                           "NA (Trait)"                 = "NA_base_centered",
                           "NA (State)"                 = "NegativeAffect_cm_centered",
                           "Lokus x NA (State)"          = "Locus_centered:NegativeAffect_cm_centered",
                           "Stabilität x NA (State)"   = "Dynamics_centered:NegativeAffect_cm_centered",
                           "Lokus x NA (Trait)"          = "Locus_centered:NA_base_centered",
                           "Stabilität x NA (Trait)"   = "Dynamics_centered:NA_base_centered",
                           "NA (Trait) x NA (State)" = "NA_base_centered:NegativeAffect_cm_centered",
                           "Lokus x Stabilität"                     = "Locus_centered:Dynamics_centered")
)
# R Quadrat-Werte

# R²-Werte extrahieren
r2_goal_model1 <- r2(goal_model1)
r2_goal_model2 <- r2(goal_model2)
r2_goal_model3 <- r2(goal_model3)
r2_goal_model4 <- r2(goal_model4)
r2_goal_model5 <- r2(goal_model5)

# Werte in ein data.frame schreiben
r2_tabelle <- data.frame(
  "Modell 1" = c(r2_goal_model1$R2_marginal, r2_goal_model1$R2_conditional),
  "Modell 2" = c(r2_goal_model2$R2_marginal, r2_goal_model2$R2_conditional),
  "Modell 3" = c(r2_goal_model3$R2_marginal, r2_goal_model3$R2_conditional),
  "Modell 4" = c(r2_goal_model4$R2_marginal, r2_goal_model4$R2_conditional),
  "Modell 5" = c(r2_goal_model5$R2_marginal, r2_goal_model5$R2_conditional)
)
rownames(r2_tabelle) <- c("Marginales R²", "Konditionales R²")

### Berechnen von standardisierten Regressionskoeffizienten für diskutierte Prädiktoren



#Manuell R2-Werte prüfen


### Berichten der P-Werte

# Zusammenfassung des Modells speichern
summary_model5 <- summary(goal_model5)
summary_model3 <- summary(goal_model3)
# Zugriff auf P-Werte

p_nabase <- summary_model3$tTable["NA_base_centered", "p-value"]

p_nabase_dynamics_interaction <- summary_model5$tTable["Dynamics_centered:NA_base_centered", "p-value"]

p_nabase_nasession <- summary_model5$tTable["NA_base_centered:NegativeAffect_cm_centered", "p-value"]


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

# Ausreißer 
ausreißer_überblick<-ggplot(data = Goal_residuals, aes(y= .ls.resid)) + theme_blank(base_size = 20) + geom_boxplot() 
ausreißer.je.ID<- ggplot(data = Goal_residuals, aes( x= .ls.resid, y= as.factor(ID))) + theme_blank(base_size = 20) + geom_boxplot() + xlab("Residuen")  + ylab( "Versuchsperson-ID")

## Vergleich des Modells ohne Ausreißer

long_df_z_sensitiv <- subset(long_df_z, ID != 6 & ID != 17 & ID != 23 & ID != 12 & ID != 13 & ID != 14& ID != 15 & ID != 16)
goal_model5_sensitiv <- update(goal_model5, data = long_df_z_sensitiv)
summary_goal_model5<-summary(goal_model5)
summary_goal_model5_sensitiv<-summary(goal_model5_sensitiv)
###### Tests of normality ###############################################

#### Für das Modell zur Vorhersage von Zielerreichung, transformiert #####
shaptest_goal<-shapiro.test(Goal_residuals $.resid)  
ntest_ks_km <-ks.test(Goal_residuals  $.resid, "pnorm", mean(Goal_residuals  $.resid), sd(Goal_residuals  $.resid), exact = F)
## Nicht rangtransformiert##
Goal_residuals_no_ranktransform <- hlm_resid(goal_model5_no_ranktransform, level=1, include.ls = T) 
shaptest_goal_no_ranktransform<-shapiro.test(Goal_residuals_no_ranktransform  $.resid)  

### Markdown Output of p value ####
shaptest_goal_p <-if (shaptest_goal$p.value < 0.001) {
  "< .001"
} else {
  round( shaptest_goal$p.value, 3)
}
shaptest_goal_p_no_rank <-if (shaptest_goal_no_ranktransform$p.value < 0.001) {
  "< .001"
} else {
  round( shaptest_goal$p.value, 3)
}


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


#### Testen der Voraussetzungen auf Level 2 #####
Goal_residuals_level2 <- hlm_resid(goal_model5, level="ID", include.ls = F)
ggplot(data = Goal_residuals_level2, aes(.ranef.intercept)) +
  
  geom_histogram(aes(y = ..density..), bins=10) + stat_function(fun = dnorm,
                                                                args = list(mean = mean(Goal_residuals_level2$.ranef.intercept),
                                                                            sd = sd(Goal_residuals_level2$.ranef.intercept)), size=2)
level2.resids.shapirotest<-shapiro.test(Goal_residuals_level2$.ranef.intercept)


l2_residuen <- ranef(goal_model5) %>%
  tibble::rownames_to_column(var = "ID") %>%
  rename(ranef_intercept = `(Intercept)`)

### ChatGPT generiert: 
# Mittelwerte der Level-2-Prädiktoren auf Personenebene
l2_preds <- long_df_z %>%
  group_by(ID) %>%
  dplyr::summarize(
    Locus = mean(Locus_centered, na.rm = TRUE),
    Dynamics = mean(Dynamics_centered, na.rm = TRUE),
    NA_base = mean(NA_base_centered, na.rm = TRUE),
    NegAff = mean(NegativeAffect_cm_centered, na.rm = TRUE)
  )


# Fixeffekte manuell einfügen (aus summary(goal_model5))
fixefs <- fixef(goal_model5)

# Vorhersage berechnen (z. B. auf Basis von Locus & Dynamics)
l2_preds <- l2_preds %>%
  mutate(pred = fixefs["(Intercept)"] +
           fixefs["Locus_centered"] * Locus +
           fixefs["Dynamics_centered"] * Dynamics)

# Stelle sicher, dass beide IDs als character vorliegen:
l2_preds <- l2_preds %>%
  mutate(ID = as.character(ID))

l2_residuen <- l2_residuen %>%
  mutate(ID = as.character(ID))
# Zusammenführen
plot_df <- left_join(l2_preds, l2_residuen, by = "ID")

# Plot
ggplot(plot_df, aes(x = pred, y = ranef_intercept)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal() +
  labs(x = "Vorhergesagte Level-2-Werte", y = "Random Intercepts",
       title = "Diagnose: Homoskedastizität auf Level 2")



