#library(readxl)
#library(dplyr)

##### SSDSE_B #####
# Loading English data
data_path <- "../dataset_files/SSDSE-BD_extracted.xlsx"
SSDSE_B <- as.data.frame(read_excel(data_path, sheet = 1, col_names = TRUE))
region_names <- unique(SSDSE_B[,1]) # extract region names for row names
SSDSE_B <- SSDSE_B[,-1] # delete overlapped region names

# Filter the data for the year 2019
Edu.Cul_2020 <- SSDSE_B %>% 
  filter(SSDSE_B$Year == 2020) %>% 
  select(Edu.budget, Cul.budget)
colnames(Edu.Cul_2020) <- c('Edu.2020','Cul.2020')

# Calculate the average data for each region (2008 ~ 2019)
average_Edu <- aggregate(Edu.budget ~ R_code, data = SSDSE_B, FUN = mean)
average_Cul <- aggregate(Cul.budget ~ R_code, data = SSDSE_B, FUN = mean)
Edu.Cul_ave <- merge(average_Edu, average_Cul, by = "R_code")
Edu.Cul_ave <- Edu.Cul_ave[,-1]
colnames(Edu.Cul_ave) <- c('Edu.ave','Cul.ave')

# Merge the two datasets
B_df <- cbind(Edu.Cul_2020, Edu.Cul_ave)
rownames(B_df) <- region_names

##### SSDSE_D #####
# Loading English data
data_path <- "~/Data Science/Stat Data Analysis Compe/SSDSE/SSDSE-BD_extracted.xlsx"
SSDSE_D <- as.data.frame(read_excel(data_path, sheet = 2, col_names = TRUE))
row.names(SSDSE_D) <- SSDSE_D[,1]
SSDSE_D <- SSDSE_D[,-c(1:2)]

# Split by gender
Dt_df <- SSDSE_D %>% 
  select(ends_with("_T")) # use only this
colnames(Dt_df) <- gsub("_T$", "", colnames(Dt_df))

#Dm_df <- SSDSE_D %>% 
#  select(ends_with("_M"))
#Df_df <- SSDSE_D %>% 
#  select(ends_with("_F"))

# Now you have 2 data frames to use
# B_df, Dt_df
