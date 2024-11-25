
####Importing the dataset####
load(magic_path("241021PRIMOCA_data.Rdata"))
sum(is.na(df))

#### Removing Participants based on the filter varialbe "Programme"#####
df<-df %>% filter(Programme == 1|is.na(Programme))

#### NAs and Outliers ####
df$NA_amount <- rowSums(is.na(df))
df<-df %>% filter(NA_amount<51)#removing participants with more than 50 outliers

#### Removing Variables that are not considered in this thesis ####
df<- df %>% select(!matches("_fear|_hope|Achievement|Affiliation|Power|Programme|Pride|Hubris"))
#### Grouping the different kinds of sports and goals ####
df$Sport2 <- NA
df$Sport2[19] <- "Laufen" # Here, I add a second sport for participant 19
patterns <- c(  "Kraftsport" = "kraft",   "Laufen" = "lauf")
df <- replace_patterns(df, "Sport", patterns)# See the script "Functions.R" to examine the function

#### Deal with hyphenss in weekly_KM_base ####
df <- handle_hyphen(df, "WeeklyKM_base")# See the script "Functions.R" to examine the function
df <- df %>%
  mutate(
    WeeklyH_base = gsub(",", ".", WeeklyH_base)
  )
df <- handle_hyphen(df, "WeeklyH_base") # also a function from the function script, it averages two numbers out if the person wrote something like 40-45

#### Means based on a string that appears in the variable name####
df$Goal_ave <- mean_by_pattern(df, "goal")# see functions script
df$Commit_ave <- mean_by_pattern(df, "commit")
df$SessionKM_ave <- mean_by_pattern(df, "sessionkm")
df$SesseionH_ave <- mean_by_pattern(df, "sessionh")
df$SessionRPE_ave <- mean_by_pattern(df, "sessionrpe")
df$PA_ave <- mean_by_pattern(df, "pa_")
df$NA_ave <- mean_by_pattern(df,"na_")

####rename the gender ####
df <- df %>%
  mutate(Gender = recode(Gender, "1" = "Männlich", "2" = "Weiblich", "3" = "Divers"))
####change character varialbes to numeric####
df$WeeklyKM_base<- as.numeric(df$WeeklyKM_base)
df$WeeklyH_base<- as.numeric(df$WeeklyH_base)

#### Handling NAs####
df[df=="NaN"]<-NA

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
  newvar_col <- paste0("complete.", i)
  df[[newvar_col]] <- sapply(df[[goal_col]], FUN = is.training.completed)
}
# count the number of "Yes!"es in those
df$completed_count<- apply(select(df, starts_with(match = "complete.")), 1, function(x) length(which(x=="Yes!"))) # the "1" stands for rows here. see https://stackoverflow.com/questions/24015557/count-occurrences-of-value-in-a-set-of-variables-in-r-per-row

# ID Variable
df$ID<-1:nrow(df)


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

