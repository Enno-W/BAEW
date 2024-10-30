
####Importing the dataset####
load(magic_path("241021PRIMOCA_data.Rdata"))
sum(is.na(df))
#### Removing Participants based on the filter varialbe "Programme"#####
df<-df %>% filter(Programme == 1|is.na(Programme))

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
df$Pride_ave <- mean_by_pattern(select(df,-Pride_base), "pride")
df$Pride_hubris <- mean_by_pattern(select(df,-Hubris_base), "hubris")
df$PA_ave <- mean_by_pattern(select(df,-PA_base), "pa_")
df$NA_ave <- mean_by_pattern(select(df,-NA_base), "na_")

####rename the gender ####
df <- df %>%
  mutate(Gender = recode(Gender, "1" = "Männlich", "2" = "Weiblich", "3" = "Divers"))
#### Handling NAs####
df$Goal_ave[8]
df[df=="NaN"]<-NA
df$Goal_ave[8]
imp <- mice(df, m=5, maxit=5, method="pmm") # number of multiple imputations, maximum iterations, method: predictive mean matching
# I have to read more here: https://bookdown.org/mwheymans/bookmi/multiple-imputation.html#multiple-imputation-in-r
df2<-complete(imp)
