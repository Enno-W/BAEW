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
vars_not_normal<- select(df, !matches("_[1-6]$")) %>% which_var_not_normal () 
desc_vars_not_normal<- stat.desc(df[,vars_not_normal])

#### Multilevel Analysis ####

#####Convert Data to long format#####
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
icc(null_model) # The ICC is a lot lower with multiple imputation
#### Model with fixed and Random effects
#Center the time varying predictors to disentangle the repeated measurements from PA and NA traits
long_df$PositiveAffect_centered <- long_df$PositiveAffect - ave(long_df$PositiveAffect, long_df$ID, FUN = mean)
long_df$NegativeAffect_centered <- long_df$NegativeAffect - ave(long_df$NegativeAffect, long_df$ID, FUN = mean)

time_varying_and_stable_model_with_covar <- lme(Goal ~ 1+ Time+PA_base+NA_base+Locus+Dynamics+PositiveAffect_centered+NegativeAffect_centered, # This would mean I assume that the fluctuations of affect across time predict goal Achivement
                                     data=long_df, 
                                     random= ~ 1 |ID, method="ML", # The "|" sort of means "group by"
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
shapiro.test(l1_residuals$.ls.resid)# not normally distributed after removing Participants with too many NaS

# Testing for homoscedasticity: The varianz of residuals must be constant for all values
ggplot(data=l1_residuals, aes(x=.ls.fitted, y=.ls.resid)) +
  geom_point() # Weird linear patterns emerge: This means, homoscedasticity is violated. 

# Outliers
ggplot(data = l1_residuals, aes(y= .ls.resid)) + theme_gray() + geom_boxplot()#overall
ggplot(data = l1_residuals, aes( x= .ls.resid, y= as.factor(ID))) + theme_gray() + geom_boxplot() # per group, the outliers are pretty random and the width varies substantially between groups

#### Überprüfung der gewöhnlichen, nicht least squares Residuen#####

ggplot(data = l1_residuals, aes(.resid)) +
  geom_histogram(aes(y = ..density..), bins=10) +
  stat_function(fun = dnorm,
                args = list(mean = mean(l1_residuals$.resid),
                            sd = sd(l1_residuals$.resid)), size=2)

shapiro.test(l1_residuals$.resid) # not normally distributed

# Homoskedastizität

ggplot(data=l1_residuals, aes(x=.fitted, y=.resid)) +
  geom_point()


qqmath(~resid(time_varying_and_stable_model_with_covar), # Interesting, especially low values of "goal" are messy
       type = c("p", "g"), 
       xlab = "Theoretical Quantiles", 
       ylab = "Standardized Residuals")

# Ausreißer

ggplot(data = l1_residuals, aes(y= .resid)) + theme_gray() + geom_boxplot()

ggplot(data = l1_residuals, aes(x= .resid, y= as.factor(ID))) + theme_gray() + geom_boxplot()
# Assumptions of normality and homoscedasticity are violated. Possible solutions could come from "robustlmm" package - to deal with "contamination", or using robust standard errors with "clubSandwich" and lme4
