# Creating a list with all commonly used descriptive statistics
descriptives_list<-mean_sd_median_min_max(df)

#demographics table
demographicstable<- select(df, Age, Gender, Sport) %>% tbl_summary( percent = "column", by = Gender) %>%  add_p() %>% add_overall()
