#### Creating a list with all commonly used descriptive statistics + other descriptive values####
descriptives_list<-mean_sd_median_min_max(df)

stat.desc(long_df$Goal_ave, basic = F, norm = T)
df

#### counting excluded participants ####
raw_data_n <-nrow(df)
filtered_n <-nrow(df)

####demographics table####
demographicsdata<- select(df, Age, Gender, Sport) %>% tbl_summary( percent = "column", by = Gender) %>%  add_p() %>% add_overall()

data <- as.data.frame(demographicsdata) %>%
  rename_with(~ gsub("\\*\\*", "", .)) # To remove the asterisks from the headers. 

#Creating the table like this does not allow for control of the row height. But if original APA format is needed, this is just right. 
# data %>% 
#    flextable() %>% 
#    theme_apa() %>% 
#    autofit()

demographicstable<- data %>%
  flextable() %>%
  set_table_properties(layout = "autofit") %>%
  height(height = 0.3, part = "body") %>%  # Set row height for body
  height(height = 0.5, part = "header") %>% # Set row height for header
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 12, part = "all") %>%
  padding(padding.top = 5, padding.bottom = 5) %>%
  align(align = "center", part = "header") %>%
  align(align = "center", part = "body")

#### correlation table ####
average_variables<-df %>% select(ends_with("_ave")) %>% names()
base_variables<-df %>% select(ends_with("_base")) %>% names()
correlation_variables<-c("Age", "Locus", "Dynamics", "completed_count", average_variables, base_variables)
ave_corr_table<-df[,correlation_variables] %>% 
  generate_correlation_table(c("Alter", "Locus", "Variabilität", "Abgeschlossene\nTrainingseinheiten", "Durchschnitt\nZiellerreichung", 
                               "Durchschnitt\nCommitment", "Durchschnitt KM pro Einheit", "Durchschnitt\nH pro Einheit", "Durchschnitt\nSessionRPE", 
                               "Durchnschnitt\nPositiver Affekt", "Durchschnitt\nNegativer Affekt", "Baseline\nWöchentliche KM", "Baseline\nWöchentliche H", "Baseline\nWöchentliche RPE", 
                               "Baseline\nPositiver Affekt", "Baseline\nNegativer Affekt"))

#### Power analysis ####
pwr_result <- pwr.r.test(n = NULL,         
                     r = 0.5,           
                     sig.level = 0.05,  
                     power = 0.95,      
                     alternative = "greater") 

#### Variable to count training sessions ####
#function to return "No" if something is FALSE, and vice versa...
is.training.completed <- function(x) {
    if (is.na(x)) {
      return("No!")
    } else {
      return("Yes!")
    }
}
# Running that whole thing through a for loop
for (i in 1:6) {
  goal_col <- paste0("Goal_", i)
  newvar_col <- paste0("complete_", i)
  df[[newvar_col]] <- sapply(df[[goal_col]], FUN = is.training.completed)
}
# count the number of "Yes!"es in those
df$completed_count<- apply(select(df, starts_with(match = "complete_")), 1, function(x) length(which(x=="Yes!"))) # the "1" stands for rows here. see https://stackoverflow.com/questions/24015557/count-occurrences-of-value-in-a-set-of-variables-in-r-per-row

#### Multilevel Analysis ####
# Nullmodell
null_model<- lme(Goal ~ 1, 
                 data=long_df, 
                 random= ~1|ID, 
                 method="ML", 
                 na.action = na.omit) # See the documentation for lme: ?lme --> other options: na.exclude, na.pass...#
# The "1" stands for the "intercept"
#The formula means: Fixed effects: for "Goal", only the intercepts are estimated. Random effects: "The intercept varies between participants". 
summary(null_model)
icc(null_model) # The ICC is a lot lower with multiple imputation
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


# Hypothesis 1.2
goal_model <- lme(Goal ~ 1+ Time+Locus+Dynamics, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                  data=long_df, 
                  random= ~ 1 |ID, method="ML", # The "|" sort of means "group by"
                  na.action = na.omit, correlation = corAR1(form = ~ Time|ID))# I am not using ~ 1|ID)), because the gap between measurement points was not uniform and some data are missing. 
summary(goal_model)

goal_model <- lme(Goal ~ 1+ Time+NA_base+NegativeAffect_centered, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                  data=long_df, 
                  random= ~ 1 |ID, method="ML", # The "|" sort of means "group by"
                  na.action = na.omit, correlation = corAR1(form = ~ Time|ID))# I am not using ~ 1|ID)), because the gap between measurement points was not uniform and some data are missing. 
summary(goal_model)

#Hypothesis 2.2 + 1.2
goal_model <- lme(Goal ~ 1+ Time+Locus+Dynamics+NA_base+NegativeAffect_centered, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                  data=long_df, 
                  random= ~ 1 |ID, method="ML", # The "|" sort of means "group by"
                  na.action = na.omit, correlation = corAR1(form = ~ Time|ID))# I am not using ~ 1|ID)), because the gap between measurement points was not uniform and some data are missing. 
summary(goal_model)

##### Checking Assumptions http://www.regorz-statistik.de/inhalte/r_HLM_2.html#####
l1_residuals <- hlm_resid(commitment_model, level=1) # Funktion aus HLMdiag-Package
#Now, I use the ".ls.resid" to make a graph. these are the "Least squares residuals", and they have the advantage that influences from level 2 and 1 are not mixed up. 
ggplot(data = l1_residuals, aes(.ls.resid)) +
  geom_histogram(aes(y = after_stat(density)), bins=10) +
  stat_function(fun = dnorm,
                args = list(mean = mean(l1_residuals$.ls.resid),
                            sd = sd(l1_residuals$.ls.resid)), size=2) # Seems pretty normal to me. 
# Shapiro test of normality
shapiro.test(l1_residuals$.ls.resid)# not normally distributed after removing Participants with too many NaS
# Testing for homoscedasticity: The varianz of residuals must be constant for all values
ggplot(data=l1_residuals, aes(x=.ls.fitted, y=.ls.resid)) +
  geom_point() # Weird linear patterns emerge: This means, homoscedasticity is violated. 
# Outliers
ggplot(data = l1_residuals, aes(y= .ls.resid)) + theme_gray() + geom_boxplot()#overall
ggplot(data = l1_residuals, aes( x= .ls.resid, y= as.factor(ID))) + theme_gray() + geom_boxplot() # per group, the outliers are pretty random and the width varies substantially between groups
#### Überprüfung der gewöhnlichen, nicht least squares Residuen#####
ggplot(data = l1_residuals, aes(.resid)) +
  geom_histogram(aes(y = after_stat(density)), bins=10) +
  stat_function(fun = dnorm,
                args = list(mean = mean(l1_residuals$.resid),
                            sd = sd(l1_residuals$.resid)), size=2)
shapiro.test(l1_residuals$.resid) # not normally distributed
# Homoskedastizität
ggplot(data=l1_residuals, aes(x=.fitted, y=.resid)) +
  geom_point()
qqmath(~resid(commitment_model), # Interesting, especially low values of "goal" are messy
       type = c("p", "g"), 
       xlab = "Theoretical Quantiles", 
       ylab = "Standardized Residuals")
# Ausreißer
ggplot(data = l1_residuals, aes(y= .resid)) + theme_gray() + geom_boxplot()
ggplot(data = l1_residuals, aes(x= .resid, y= as.factor(ID))) + theme_gray() + geom_boxplot()
# Assumptions of normality and homoscedasticity are violated. Possible solutions could come from "robustlmm" package - to deal with "contamination", or using robust standard errors with "clubSandwich" and lme4
#### Model for "goal" ####
goal_model <- lme(Goal ~ 1+ Time+PA_base+NA_base+Locus+Dynamics+PositiveAffect_centered+NegativeAffect_centered, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                  data=long_df, 
                  random= ~ 1 |ID, method="ML", # The "|" sort of means "group by"
                  na.action = na.omit, correlation = corAR1(form = ~ Time|ID))# I am not using ~ 1|ID)), because the gap between measurement points was not uniform and some data are missing. 
summary(goal_model)
##### Checking Assumptions http://www.regorz-statistik.de/inhalte/r_HLM_2.html#####
l1_residuals <- hlm_resid(goal_model, level=1) # Funktion aus HLMdiag-Package
#Now, I use the ".ls.resid" to make a graph. these are the "Least squares residuals", and they have the advantage that influences from level 2 and 1 are not mixed up. 
ggplot(data = l1_residuals, aes(.ls.resid)) +
  geom_histogram(aes(y = after_stat(density)), bins=10) +
  stat_function(fun = dnorm,
                args = list(mean = mean(l1_residuals$.ls.resid),
                            sd = sd(l1_residuals$.ls.resid)), size=2) # Seems pretty normal to me. 
# Shapiro test of normality
shapiro.test(l1_residuals$.ls.resid)# not normally distributed after removing Participants with too many NaS
# Testing for homoscedasticity: The varianz of residuals must be constant for all values
ggplot(data=l1_residuals, aes(x=.ls.fitted, y=.ls.resid)) +
  geom_point() # Weird linear patterns emerge: This means, homoscedasticity is violated. 
# Outliers
ggplot(data = l1_residuals, aes(y= .ls.resid)) + theme_gray() + geom_boxplot()#overall
ggplot(data = l1_residuals, aes( x= .ls.resid, y= as.factor(ID))) + theme_gray() + geom_boxplot() # per group, the outliers are pretty random and the width varies substantially between groups
#### Überprüfung der gewöhnlichen, nicht least squares Residuen#####
ggplot(data = l1_residuals, aes(.resid)) +
  geom_histogram(aes(y = after_stat(density)), bins=10) +
  stat_function(fun = dnorm,
                args = list(mean = mean(l1_residuals$.resid),
                            sd = sd(l1_residuals$.resid)), size=2)
shapiro.test(l1_residuals$.resid) # not normally distributed
# Homoskedastizität
ggplot(data=l1_residuals, aes(x=.fitted, y=.resid)) +
  geom_point()
qqmath(~resid(commitment_model), # Interesting, especially low values of "goal" are messy
       type = c("p", "g"), 
       xlab = "Theoretical Quantiles", 
       ylab = "Standardized Residuals")
# Ausreißer
ggplot(data = l1_residuals, aes(y= .resid)) + theme_gray() + geom_boxplot()
ggplot(data = l1_residuals, aes(x= .resid, y= as.factor(ID))) + theme_gray() + geom_boxplot()
# Assumptions of normality and homoscedasticity are violated. Possible solutions could come from "robustlmm" package - to deal with "contamination", or using robust standard errors with "clubSandwich" and lme4