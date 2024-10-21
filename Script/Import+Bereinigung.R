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
load(magic_path("241021PRIMOCA_data.Rdata"))
df_from_xlsx1<-readxl::read_excel(magic_path("240913PRIMOCA_data.xlsx"), sheet = 1)
df_from_xlsx1<- df_from_xlsx1 %>% select(-contains("_ave"))
all.equal(df_from_xlsx1,df)# TRUE, no complications
rm(df_from_xlsx1)
#### Removing NA's#####
df %>% filter(Programme == 1)

