# Creating a list with all commonly used descriptive statistics
descriptives_list<-mean_sd_median_min_max(df)

#demographics table
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
