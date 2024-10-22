#### I inspected and converted the dataset to .Rdata with these commands####

# df_from_xlsx1<-readxl::read_excel(magic_path("240913PRIMOCA_data.xlsx"), sheet = 1)
# df_from_xlsx2<-read.xlsx2(magic_path("240913PRIMOCA_data.xlsx"),sheetIndex = 1)
# 
# all(df_from_xlsx1==df_from_xlsx2) # The result is FALSE: There is a difference
# df_from_xlsx1==df_from_xlsx2 # to see which values are not identical and inspect
# df_from_xlsx1$Achievement[3]
# df_from_xlsx2$Achievement[3]# The differences is due to the scientific notation of long numbers in read_excel. I keep all digits, in "df".
# rm(df_from_xlsx2)
# # The calculation of averages is cumbersome to validate in excel, and some rounding differences occur with the Excel-calculated averages. So, I removed them:
# df<- df_from_xlsx1 %>% select(-contains("_ave"))
# save(df,file= "241021PRIMOCA_data.Rdata") # export, to avoid potential complications with Excel
#### Test if any complications occured during the conversion ####
#df2<-readxl::read_excel(magic_path("240913PRIMOCA_data.xlsx"), sheet = 1)
#df2<- df_from_xlsx1 %>% select(-contains("_ave"))
load(magic_path("241021PRIMOCA_data.Rdata"))
#all.equal(df_from_xlsx1,df)# TRUE, no complications
#### Removing NA's#####
df<-df %>% filter(Programme == 1)
#### Grouping the different kinds of sports and goals ####
df$Sport2 <- NA
df$Sport2[19] <- "Laufen" # Here, I add a second sport for participant 19
patterns <- c(  "Kraftsport" = "kraft",   "Laufen" = "lauf")
df <- replace_patterns(df, "Sport", patterns)# See the script "Functions.R" to examine the function
df <- handle_hyphen(df, "WeeklyKM_base")# See the script "Functions.R" to examine the function
df <- df %>%
  mutate(
    WeeklyH_base = gsub(",", ".", WeeklyH_base)
  )
df <- handle_hyphen(df, "WeeklyH_base")
df <- mean_by_pattern(df, "goal", "Goal_ave")
df <- mean_by_pattern(df, "commit", "Commit_ave")
df <- mean_by_pattern(df, "sessionkm", "SessionKM_ave")
df <- mean_by_pattern(df, "sessionh", "SesseionH_ave")
df <- mean_by_pattern(df, "sessionrpe", "SessionRPE_ave")
df <- mean_by_pattern(select(df,-Pride_base), "pride", "Pride_ave")

patterns <- list(
  goal = "Goal_ave",
  commit = "Commit_ave",
  sessionkm = "SessionKM_ave"
)# This creates a list, where the search string is the value and the new variable name is the name for that value, respectively
# Loop through the list and apply the function
for (pattern in names(patterns)) {
  df <- mean_by_pattern(df, pattern, patterns[[pattern]])
}
