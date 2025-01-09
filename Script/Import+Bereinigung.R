#### Import and data cleansing ####
# The commands in this script lead to the data frame that is loaded from the script "Import+Bereinigung.R". This script is not used in the markdown file
####Importing the dataset####
load(magic_path("241021PRIMOCA_data.Rdata")) # load original data
sum(is.na(df))
#### counting excluded participants ####
raw_data_n <-nrow(df)

#### Removing Participants based on the filter varialbe "Programme"#####
df_filtered<-df %>% filter(Programme == 1|is.na(Programme))
filtered_n <-nrow(df_filtered)
#### NAs and Outliers ####
df_filtered$NA_amount <- rowSums(is.na(df_filtered))
df_filtered$NA_amount <- rowSums(is.na(df_filtered))
df_less_na<-df_filtered %>% filter(NA_amount<50)#removing participants with more than 50 missings
na_removed_n <-nrow(df_less_na)
df<-df_less_na

load(magic_path("df.Rdata"))
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


