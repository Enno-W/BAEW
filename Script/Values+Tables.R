#### Creating a list with all commonly used descriptive statistics####
descriptives_list<-mean_sd_median_min_max(df)

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
ave_corr_table<-df %>% 
  select(ends_with("_ave")) %>% 
  generate_correlation_table(c("Average Goal Attainment", "Average Commitment", "Average KM per Session", "Average hours per Session", "Average SessionRPE",  "Average Positive Affect", "Average Negative Affect"))
#### Power analysis ####
pwr_result <- pwr.r.test(n = NULL,         
                     r = 0.5,           
                     sig.level = 0.05,  
                     power = 0.95,      
                     alternative = "greater") 

#### Distribution tests #####
## extract not normally distributed variables
vars_not_normal<- which_var_not_normal (df) 
desc_vars_not_normal<- stat.desc(df[,vars_not_normal])
#### Multilevel Analysis ####
#####Convert Data to long format For "Goal" only#####
weekly_measures<-select(df, matches("_[1-6]$")) %>% names()# This is a regex, a regular expression to find a certain pattern. The Dollar sign is for "Ends with". Learn more here: https://github.com/ziishaned/learn-regex/blob/master/translations/README-de.md

long_df <- df %>%
  pivot_longer(
    cols = all_of(weekly_measures), # 
    names_to = c(".value", "Time"),   # Split into a base name and the timepoint
    names_pattern = "(.*)_(\\d+)"     # Regex to split column names like "Goal_1"
  ) %>%
  mutate(
    Time = as.numeric(Time)           # Convert extracted timepoint to numeric
  )
long_df<-long_df %>% rename( PositiveAffect=PA,NegativeAffect="NA")
# Test that the transformation worked as intended: We have 33 obs. in "df", and here, each individual gets 6 rows for each measurement point. 
nrow(long_df)== nrow(df)*6
df$Goal_5[1]==long_df$Goal[5]# The 5th measurement point of "Goal" is equal to the variable "Goal_5" of participant number 1 in the original df. 

#multiple imputation
imp <- mice(long_df, m=5, maxit=5, method="pmm") # number of multiple imputations, maximum iterations, method: predictive mean matching
# https://bookdown.org/mwheymans/bookmi/multiple-imputation.html#multiple-imputation-in-r
long_df<-complete(imp)

#### Models###
# Nullmodell
null_model<- lme(Goal ~ 1, 
                 data=long_df, 
                 random= ~1|ID, 
                 method="ML", 
                 na.action = na.omit) # See the documentation for lme: ?lme --> other options: na.exclude, na.pass...#
# The "1" stands for the "intercept"
#The formula means: Fixed effects: for "Goal", only the intercepts are estimated. Random effects: "The intercept varies between participants". 
summary(null_model)
icc(null_model)
time_model <- lme(Goal ~ 1+ Time, 
                  data=long_df, 
                  random= ~1|ID, method="ML", 
                  na.action = na.omit)
summary(time_model)# This model barely fits better than the null model
# Time varying covariates: Affect
time_varying_model <- lme(Goal ~ 1+ Time+PositiveAffect+NegativeAffect, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                  data=long_df, 
                  random= ~1|ID, method="ML", 
                  na.action = na.omit)
time_varying_model$coefficients$fixed["PositiveAffect"]
summary(time_varying_model)
##Stable Covariates: Affect and Attribution
time_varying_and_stable_model <- lme(Goal ~ 1+ Time+PositiveAffect+NegativeAffect+PA_base+NA_base+Locus+Dynamics, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                          data=long_df, 
                          random= ~1|ID, method="ML", 
                          na.action = na.omit)
summary(time_varying_and_stable_model)
#Level 1 Residuals (aka, at the time level), have thus far been viewed as uncorrelated. Here I introduce a covariation matrix for them.
time_varying_and_stable_model_with_covar <- lme(Goal ~ 1+ Time+PositiveAffect+NegativeAffect+PA_base+NA_base+Locus+Dynamics, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                                     data=long_df, 
                                     random= ~1|ID, method="ML", 
                                     na.action = na.omit, correlation = corAR1(form = ~ Time|ID))# I am not using ~ 1|ID)), because the gap between measurement points was not uniform and some data are missing. 
summary(time_varying_and_stable_model_with_covar)
##### Checking Assumptions http://www.regorz-statistik.de/inhalte/r_HLM_2.html#####
l1_residuals <- hlm_resid(time_varying_and_stable_model_with_covar, level=1) # Funktion aus HLMdiag-Package
#Now, I use the ".ls.resid" to make a graph. these are the "Least squares residuals", and they have the advantage that influences from level 2 and 1 are not mixed up. 
ggplot(data = l1_residuals, aes(.ls.resid)) +
  geom_histogram(aes(y = ..density..), bins=10) +
  stat_function(fun = dnorm,
                args = list(mean = mean(l1_residuals$.ls.resid),
                            sd = sd(l1_residuals$.ls.resid)), size=2) # Seems pretty normal to me. 
# Shapiro test of normality
shapiro.test(l1_residuals$.ls.resid)# yup, normally distributed

# Testing for homoscedasticity: The varianz of residuals must be constant for all values
ggplot(data=l1_residuals, aes(x=.ls.fitted, y=.ls.resid)) +
  geom_point() # It looks like a random cloud, visually I do not see homoscedasticity. But apparently, the test is significant: This means, homoscedasticity is violated. 


