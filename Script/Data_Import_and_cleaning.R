#### Import and data cleansing ####
# The commands in this script lead to the data frame that is loaded from the script "Import+Bereinigung.R". This script is not used in the markdown file
####Importing the dataset####
load(magic_path("241021PRIMOCA_data.Rdata"))
sum(is.na(df))
#### counting excluded participants ####
raw_data_n <-nrow(df)

#### Removing Participants based on the filter varialbe "Programme"#####
df_filtered<-df %>% filter(Programme == 1|is.na(Programme))
filtered_n <-nrow(df_filtered)
#### NAs and Outliers ####
df_filtered$Missings_amount <- rowSums(is.na(df_filtered))
df_less_na<-df_filtered %>% filter(Missings_amount<50)#removing participants with more than 50 missings
na_removed_n <-nrow(df_less_na)
df<-df_less_na
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

### Variable to count training sessions ####
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

####Multiple imputation####
imp <- mice(df, m=5, maxit=5, method="pmm") # number of multiple imputations, maximum iterations, method: predictive mean matching
# https://bookdown.org/mwheymans/bookmi/multiple-imputation.html#multiple-imputation-in-r
df<-complete(imp)


save(df, file = "df.Rdata")
