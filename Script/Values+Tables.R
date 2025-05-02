#### Creating a list with all commonly used descriptive statistics + other descriptive values ###############################################
descriptives_list<-mean_sd_median_min_max(df)
vars_not_normal<-which_var_not_normal(df)
vars_not_normal_with_p_values<-which_var_not_normal_p(df) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0.001, "< .001", as.character(round(.,3)))))
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
correlation_variables<-c("Age", "Locus", "Dynamics", "completed_count", average_variables, base_variables)
correlation_variables <- correlation_variables[!correlation_variables %in% c("PA_ave", "PA_base", "Commit_ave")]# Excluding not needed variables
ave_corr_table<-df[,correlation_variables] %>% 
  generate_correlation_table2(c(
    "1. Alter", 
    "2. Locus", 
    "3. Variabilität", 
    "4. n (Trainingseinheiten)", 
    "5. M (Ziellerreichung)", 
    "6. M (km pro Einheit)", 
    "7. M (h pro Einheit)", 
    "8. M (SessionRPE)", 
    "9. M (Negativer Affekt)", 
    "10. Baseline wöchentliche KM", 
    "11. Baseline wöchentliche H", 
    "12. Baseline wöchentliche RPE", 
    "13. Baseline negativer Affekt"
  ))

#### Skewness, Kurtosis and min-max range table###############################################

df_stat<-get_descriptive_table(df[, correlation_variables], language = "German")

df_stat$Variable <- c(
  "Alter", 
  "Locus", 
  "Variabilität", 
  "n (Trainingseinheiten)", 
  "M (Ziellerreichung)", 
  "M (km pro Einheit)", 
  "M (h pro Einheit)", 
  "M (SessionRPE)", 
  "M (Negativer Affekt)", 
  "Baseline wöchentliche KM", 
  "Baseline wöchentliche H", 
  "Baseline wöchentliche RPE", 
  "Baseline negativer Affekt"
)

table_stat<-df_stat %>% flextable() %>% flextable::theme_apa() %>% autofit()
 


#### Power analysis ####
pwr_result <- pwr.r.test(n = NULL,         
                     r = 0.5,           
                     sig.level = 0.05,  
                     power = 0.95,      
                     alternative = "greater") 

#
#### Regression Analyses ###############################################

###ICC####
# Nullmodell
null_model_goal<- lme(Goal ~ 1, 
                      data=long_df, 
                      random= ~1|ID, 
                      method="ML", 
                      na.action = na.omit) # See the documentation for lme: ?lme --> other options: na.exclude, na.pass...#
# The "1" stands for the "intercept"
#The formula means: Fixed effects: for "Goal", only the intercepts are estimated. Random effects: "The intercept varies between participants". 
summary(null_model_goal)
icc_goal<--icc(null_model_goal) # The ICC is a lot lower with multiple imputation


###General Linear Models####
#Hypothese 1.1 Ein internal - variabler Attributionsstil sagt weniger Trainingsausfälle voraus
h1.1_model <- glmer(completed_count ~ 1 + Time + Locus_centered + Dynamics_centered + (1 | ID), 
                    data = long_df, 
                    family = "poisson")
tidy(h1.1_model, effects = "fixed", conf.int = TRUE) # see huxreg documentation as for why this is necessary. This is defining a "tidy()-function"
summary(h1.1_model)

# Hypothesis 2.1
h2.1_model <- glmer(completed_count ~ 1 + Time + NA_base_centered + NegativeAffect_cm_centered + (1 | ID), 
                    data = long_df, 
                    family = "poisson")
tidy(h2.1_model, effects = "fixed", conf.int = TRUE)
summary(h2.1_model)

### Table Output ####
glmtable<-huxreg("Attributions-Modell (H 1.1)" = h1.1_model, "Affekt-Modell (H 2.1)" = h2.1_model ,statistics = NULL, number_format = 3, bold_signif = 0.05, omit_coefs = "Time" , error_pos = "right")


#################Hierarchical linear models #################
long_df$Goal_log <-log(long_df$Goal)
long_df$Goal_log[is.infinite(long_df$Goal_log)] <- 0.000001

null_model_goal<-lme(
  Goal_log ~1,
  data = long_df,
  random = ~ 1| ID,
  method = "ML",
  na.action = na.omit,
  correlation = corAR1(form = ~ Time | ID)
)

goal_model1 <- lme(
  Goal_log ~ 1 + Time + Locus_centered + Dynamics_centered ,
  data = long_df,
  random = ~ 1| ID,
  method = "ML",
  na.action = na.omit,
  correlation = corAR1(form = ~ Time | ID)
)

goal_model2 <- lme(
  Goal_log ~ 1 + Time + Locus_centered + Dynamics_centered +  NegativeAffect_cm_centered,
  data = long_df,
  random = ~ 1| ID,
  method = "ML",
  na.action = na.omit,
  correlation = corAR1(form = ~ Time | ID)
)
goal_model3 <- lme(
  Goal_log ~ 1 + Time + Locus_centered + Dynamics_centered +  NegativeAffect_cm_centered + NA_base_centered, 
  data = long_df,
  random = ~ 1| ID,
  method = "ML",
  na.action = na.omit,
  correlation = corAR1(form = ~ Time | ID)
)



### Table Output #####
hlmtable<-huxreg("Nullmodell" = null_model_goal, 
                 "Modell 1" = goal_model1 , 
                 "Modell 2" = goal_model2 ,
                 "Modell 3" = goal_model3 ,
                 statistics = NULL, 
                 number_format = 3, 
                 bold_signif = 0.05, 
                 tidy_args =  list(effects = "fixed"), error_pos="right")

#####Überprüfen der Voraussetzungen  http://www.regorz-statistik.de/inhalte/r_HLM_2.html ###############################################
# Extrahieren der Residuen
Goal_residuals <- hlm_resid(goal_model3, level=1, include.ls = T) 
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
model_SessionKM_2_lmer <- lmer(
  SessionKM_lead1 ~ 
    Pride_cm_centered * PA_cm_centered +
    Pride_cm_centered * Dynamics_centered + 
    Pride_cm_centered * Locus_centered + 
    Pride_cm_centered * Globality_centered +
    (1 | ID),
  data = long_df,
  na.action = na.omit,
  REML = TRUE
)
vif(model_SessionKM_2_lmer) 


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
# Outliers (All)
ggplot(data = Goal_residuals , aes(y= .resid)) + theme_gray() + geom_boxplot()

#Outliers per individual
outl_plot_km <- ggplot(Goal_residuals, aes(x = .resid, y = ID)) +
  geom_boxplot(outlier.shape = 21, outlier.fill = "gray80", outlier.color = "black", outlier.size = 2) +
  labs(
    y = "Individual No.",
    x = "Residuals"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.border = element_rect(fill = NA, color = "black")
  )



goal_model_pa_residuals  <- hlm_resid(goal_model_basic_pa, level=1) # Funktion aus HLMdiag-Package
#Now, I use the "..._residuals" to make a graph. these are the "Least squares residuals", and they have the advantage that influences from level 2 and 1 are not mixed up. 
ggplot(data = goal_model_pa_residuals , aes(.ls.resid)) +
  geom_histogram(aes(y = after_stat(density)), bins=10) +
  stat_function(fun = dnorm,
                args = list(mean = mean(goal_model_pa_residuals $.ls.resid),
                            sd = sd(goal_model_pa_residuals $.ls.resid)), linewidth=2) 

###### Shapiro test of normality ###############################################

#### Test for the Attribution Model #####
shaptest_goal_attrib <-shapiro.test(goal_model_pa_residuals $.ls.resid)

### Markdown Output of p value ####
shaptest_goal_attrib_p <-if (shaptest_goal_attrib$p.value < 0.001) {
  "< .001"
} else {
  round( shaptest_goal_affect$p.value, 3)
}

##### Testing for homoscedasticity and Outliers: The variance of residuals must be constant for all values####
ggplot(data=goal_model_pa_residuals , aes(x=.ls.fitted, y=.ls.resid)) +
  geom_point() # Weird linear patterns emerge: This means, homoscedasticity is violated. 
# Outliers
ggplot(data = goal_model_pa_residuals , aes(y= .ls.resid)) + theme_gray() + geom_boxplot() #overall
ggplot(data = goal_model_pa_residuals , aes( x= .ls.resid, y= as.factor(ID))) + theme_gray() + geom_boxplot() # per group, the outliers are pretty random and the width varies substantially between groups


# Ausreißer
ggplot(data = goal_model_pa_residuals , aes(y= .resid)) + theme_gray() + geom_boxplot()
ggplot(data = goal_model_pa_residuals , aes(x= .resid, y= as.factor(ID))) + theme_gray() + geom_boxplot()
# Assumptions of normality and homoscedasticity are violated. Possible solutions could come from "robustlmm" package - to deal with "contamination", or using robust standard errors with "clubSandwich" and lme4


# Ausreißer
ggplot(data = goal_model_pa_residuals , aes(y= .resid)) + theme_gray() + geom_boxplot()
ggplot(data = goal_model_pa_residuals , aes(x= .resid, y= as.factor(ID))) + theme_gray() + geom_boxplot()
# Assumptions of normality and homoscedasticity are violated. Possible solutions could come from "robustlmm" package - to deal with "contamination", or using robust standard errors with "clubSandwich" and lme4

