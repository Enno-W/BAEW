#### Import and data cleansing ####
# The commands in this script lead to the data frame that is loaded from the script "Data import and cleaning. This script is not used in the markdown file
####Importing the original dataset to get data on missings etc. ####
load(magic_path("241021PRIMOCA_data.Rdata")) # load original data
sum(is.na(df))
#### counting excluded participants ####
raw_data_n <-nrow(df)

#### Entfernen der TN anhand der Filtervariable "Programme"#####
df_filtered<-df %>% filter(Programme == 1|is.na(Programme))
filtered_n <-nrow(df_filtered)
#### NAs and Outliers ####
df_filtered$missings_amount <- rowSums(is.na(df_filtered))
df_filtered$missings_amount <- rowSums(is.na(df_filtered))
df_less_na<-df_filtered %>% filter(missings_amount<50)#removing participants with more than 50 missings
na_removed_n <-nrow(df_less_na)
df<-df_less_na

load(magic_path("df.Rdata")) # Laden des Datensets mit multipler imputation, siehe "Data import and cleaning.R" für den Code zur Erstellung dieses Datensatzes


#####Konvertierung in ein Langformat#####
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

# Testen, ob die Transformation wie gewünscht funktioniert hat: Wir haben 33 Beobachtungen in „df“, und hier erhält jedes Individuum 6 Zeilen für jeden Messpunkt.
nrow(long_df)== nrow(df)*6
df$Goal_5[1]==long_df$Goal[5]# The 5th measurement point of "Goal" is equal to the variable "Goal_5" of participant number 1 in the original df. 

###Zentrieren der Stabilen Prädiktoren anhand des Gesamtmittelwerts, und der variablen Prädiktoren Pride und Affekt anhand des Gruppenmittelwerts

long_df[, c("Dynamics_centered",
            "Locus_centered",
            "Globality_centered",
            "NA_base_centered",
            "PA_base_centered")] <- scale(
              long_df[, c("Dynamics",
                          "Locus",
                          "Globality", 
                          "NA_base",
                          "PA_base")],
              center = TRUE, scale = FALSE)

# Zentrieren von negativem Affekt anhand des Gruppenmittelwerts
long_df <- long_df %>%
  group_by(ID) %>%
  mutate(across(c(NegativeAffect, PositiveAffect, SessionRPE), 
                ~ . - mean(., na.rm = TRUE), 
                .names = "{.col}_cm_centered")) %>%
  ungroup()

#z-Standardisierung zur besseren Interpretierbarkeit aller Variablen
zstandard_varnames<-df %>% ungroup() %>% dplyr::select( -"Notes", -"Sport2", - "ID",-"Age"    ,- "Aim", -"Sport", -   "Gender" , - "WeeklyKM_base", "WeeklyH_base", -ends_with("_ave"), -starts_with("complet")) %>% names() #https://stackoverflow.com/questions/38511743/adding-missing-grouping-variables-message-in-dplyr-in-r
df_z <- df %>%  mutate_at(zstandard_varnames, ~ as.numeric(scale(.)))
zstandard_varnames_long<-long_df %>% ungroup() %>% dplyr::select(-"Time", -"Notes", -"Sport2", - "ID",-"Age"    ,- "Aim", -"Sport", -   "Gender" , - "WeeklyKM_base", "WeeklyH_base", -ends_with("_ave"), -starts_with("complet")) %>% names() #https://stackoverflow.com/questions/38511743/adding-missing-grouping-variables-message-in-dplyr-in-r
long_df_z <- long_df %>%  mutate_at(zstandard_varnames_long, ~ as.numeric(scale(.)))
class(df_z$Locus)
