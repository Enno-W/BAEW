#### Creating a list with all commonly used descriptive statistics + other descriptive values####
descriptives_list<-mean_sd_median_min_max(df)
vars_not_normal<-which_var_not_normal(df)
vars_not_normal_with_p_values<-which_var_not_normal_p(df) %>% mutate(across(where(is.numeric), ~ ifelse(. < 0.001, "< .001", as.character(round(.,3)))))
stat.desc(df$Locus, basic = F, norm = T)
stat.desc(df$Dynamics, basic = F, norm = T)
hist(df$Locus, breaks = 15)
#### counting excluded participants ####
raw_data_n <-nrow(df)
filtered_n <-nrow(df)

####demographics table####
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

#### Skewness, Kurtosis and min-max range table####

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
#### Multilevel Analysis ####

null_model_sessions<- lme(completed_count ~ 1, 
                      data=long_df, 
                      random= ~1|ID, 
                      method="ML", 
                      na.action = na.omit) 

summary(null_model_sessions)

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
#### Model with fixed and Random effects
#Center the time varying predictors to disentangle the repeated measurements from PA and NA traits
long_df$PositiveAffect_centered <- long_df$PositiveAffect - ave(long_df$PositiveAffect, long_df$ID, FUN = mean)
long_df$NegativeAffect_centered <- long_df$NegativeAffect - ave(long_df$NegativeAffect, long_df$ID, FUN = mean)

#Hypothesis 1.1
h1.1_model <- glmer(completed_count ~ 1 + Time + Locus + Dynamics + (1 | ID), 
                    data = long_df, 
                    family = "poisson")
summary(h1.1_model)

# Hypothesis 2.1
h2.1_model <- glmer(completed_count ~ 1 + Time + NA_base + NegativeAffect_centered + (1 | ID), 
                    data = long_df, 
                    family = "poisson")
summary(h2.1_model)

hlmtable1<-huxreg("Nullmodell" = null_model_sessions, "Attribution-Modell (H 1.1)" = h1.1_model, "Affekt-Modell (H 2.1)" = h2.1_model ,statistics = NULL, number_format = 3, bold_signif = 0.05) 
# Hypothesis 1.2
goal_model1 <- lme(Goal ~ 1+ Time+Locus+Dynamics, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                  data=long_df, 
                  random= ~ Time |ID, method="ML", # The "|" sort of means "group by"
                  na.action = na.omit, correlation = corAR1(form = ~ Time|ID))# I am not using ~ 1|ID)), because the gap between measurement points was not uniform and some data are missing. 
summary(goal_model1)

#Hypothesis 2.2
goal_model2 <- lme(Goal ~ 1+ Time+NA_base+NegativeAffect_centered, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                  data=long_df, 
                  random= ~ Time |ID, method="ML", # The "|" sort of means "group by"
                  na.action = na.omit, correlation = corAR1(form = ~ Time|ID))# I am not using ~ 1|ID)), because the gap between measurement points was not uniform and some data are missing. 
summary(goal_model2)

hlmtable2<-huxreg("Nullmodell" = null_model_goal, "Attribution-Modell (H 1.1)" = goal_model1, "Affekt-Modell (H 2.1)" = goal_model2 ,statistics = NULL, number_format = 3, bold_signif = 0.05)

#Hypothesis 2.2 + 1.2
goal_model3 <- lme(Goal ~ 1+ Time+Locus+Dynamics+NA_base+NegativeAffect_centered, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                  data=long_df, 
                  random= ~ Time |ID, method="ML", # The "|" sort of means "group by"
                  na.action = na.omit, correlation = corAR1(form = ~ Time|ID))# I am not using ~ 1|ID)), because the gap between measurement points was not uniform and some data are missing. 
summary(goal_model3)

##### Checking Assumptions http://www.regorz-statistik.de/inhalte/r_HLM_2.html#####
goal1_residuals <- hlm_resid(goal_model1, level=1) # Funktion aus HLMdiag-Package
#Now, I use the ".ls.resid" to make a graph. these are the "Least squares residuals", and they have the advantage that influences from level 2 and 1 are not mixed up. 
ggplot(data = goal1_residuals, aes(.ls.resid)) +
  geom_histogram(aes(y = after_stat(density)), bins=10) +
  stat_function(fun = dnorm,
                args = list(mean = mean(goal1_residuals$.ls.resid),
                            sd = sd(goal1_residuals$.ls.resid)), size=2) # Seems pretty normal to me. 
######Shapiro test of normality#######
shaptest_goal_attrib <-shapiro.test(goal1_residuals$.ls.resid)# not normally distributed after removing Participants with too many NaS
shaptest_goal_attrib_p <-if (shaptest_goal_attrib$p.value < 0.001) {
  "< .001"
} else {
  round( shaptest_goal_affect$p.value, 3)
}
goal2_residuals <- hlm_resid(goal_model2, level=1)


shaptest_goal_affect<-shapiro.test(goal1_residuals$.ls.resid)

shaptest_goal_affect_p <-if (shaptest_goal_affect$p.value < 0.001) {
  "< .001"
} else {
  round( shaptest_goal_affect$p.value, 3)
}
# Testing for homoscedasticity: The varianz of residuals must be constant for all values
ggplot(data=goal1_residuals, aes(x=.ls.fitted, y=.ls.resid)) +
  geom_point() # Weird linear patterns emerge: This means, homoscedasticity is violated. 
# Outliers
ggplot(data = goal1_residuals, aes(y= .ls.resid)) + theme_gray() + geom_boxplot() #overall
ggplot(data = goal1_residuals, aes( x= .ls.resid, y= as.factor(ID))) + theme_gray() + geom_boxplot() # per group, the outliers are pretty random and the width varies substantially between groups

# Homoskedastizität
ggplot(data=goal1_residuals, aes(x=.fitted, y=.resid)) +
  geom_point()
qqmath(~resid(goal_model3), # Interesting, especially low values of "goal" are messy
       type = c("p", "g"), 
       xlab = "Theoretical Quantiles", 
       ylab = "Standardized Residuals")


# Ausreißer
ggplot(data = goal1_residuals, aes(y= .resid)) + theme_gray() + geom_boxplot()
ggplot(data = goal1_residuals, aes(x= .resid, y= as.factor(ID))) + theme_gray() + geom_boxplot()
# Assumptions of normality and homoscedasticity are violated. Possible solutions could come from "robustlmm" package - to deal with "contamination", or using robust standard errors with "clubSandwich" and lme4


# Ausreißer
ggplot(data = goal1_residuals, aes(y= .resid)) + theme_gray() + geom_boxplot()
ggplot(data = goal1_residuals, aes(x= .resid, y= as.factor(ID))) + theme_gray() + geom_boxplot()
# Assumptions of normality and homoscedasticity are violated. Possible solutions could come from "robustlmm" package - to deal with "contamination", or using robust standard errors with "clubSandwich" and lme4

