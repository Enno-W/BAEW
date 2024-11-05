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
ave_corr_table<-df %>% select(ends_with("_ave")) %>% generate_correlation_table(c("Average Goal Attainment", "Average Commitment", "Average KM per Session", "Average hours per Session", "Average SessionRPE",  "Average Positive Affect", "Average Negative Affect"))
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