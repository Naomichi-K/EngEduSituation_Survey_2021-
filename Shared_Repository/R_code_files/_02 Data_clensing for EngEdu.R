library(readxl)
library(dplyr)

# File path setting
data_path <- "../dataset_files/EngEdu_Survey.xlsx"

# Loading English data
EngData <- as.data.frame(read_excel(data_path, sheet = 1, col_names = TRUE))
row.names(EngData) <- EngData[,1]
EngData <- EngData[,-1] # data framed

# Row number combinations to be merged or deleted
merge_pref_list <- list(c(1,48),c(2,49),c(11,50),c(12,51),c(14,52,53,54),    # Hokkaido ~ Kanagawa
                        c(15,55),c(22,56,57),c(23,58),c(26,59),c(27,60,61),  # Niigata ~ Osaka
                        c(28,62),c(33,63),c(34,64),c(40,65,66),c(43,67))     # Hyogo ~ Kumamoto

merge_pref_list2 <- list(c(1,48),c(2,49),c(11,50),c(12,51),c(14,52,53),      # Hokkaido ~ Kanagawa
                         c(15,55),c(22,56,57),c(23,58),c(26,59),c(27,60,61),  # Niigata ~ Osaka
                         c(28,62),c(33,63),c(34,64),c(40,65,66),c(43,67))     # Hyogo ~ Kumamoto
Cities <- c(48:67)

# Filtering K/C_ by _student/school
K_data <- EngData %>% 
  select(starts_with("K_"))
K_data_student <- K_data[-Cities,c(1,3,4,5)] # Remove NA rows (No need to merge)
K_data_school <- K_data[,-c(1,3,4,5)]

C_data <- EngData %>% 
  select(starts_with("C_"))
C_data_student <- C_data[,c(1,3,4,5)]
C_data_school <- C_data[,-c(1,3,4,5)]

# Merging and Mutating
K_data_school <- merge_rate_count(K_data_school,merge_pref_list2)
K_data_school <- K_data_school[-Cities,] # Keep only merged rows

C_data_student <- merge_rate_count(C_data_student,merge_pref_list)
C_data_student <- C_data_student[-Cities,]
C_data_school <- merge_rate_count(C_data_school,merge_pref_list)
C_data_school <- C_data_school[-Cities,]

# Mutation (tE等追加)
K_data_student$K_exam.rate <- K_data_student$K_exam / K_data_student$K_s3rd
K_data_student$K_A2seem.rate <- K_data_student$K_A2seem / K_data_student$K_s3rd
K_data_student$K_A2cefr.rate <- K_data_student$K_A2cefr / K_data_student$K_s3rd
K_data_student$K_A2Lv <- K_data_student$K_A2seem + K_data_student$K_A2cefr
K_data_student$K_A2Lv.rate <- (K_data_student$K_A2seem + K_data_student$K_A2cefr)/K_data_student$K_s3rd
K_data_school$K_LA50more <- K_data_school$K_LA50 + K_data_school$K_LA75
K_data_school$K_tE50more <- K_data_school$K_tE50 + K_data_school$K_tE75
K_data_school$K_CDpublic <- K_data_school$K_CDpublic / K_data_school$K_sch
K_data_school$K_CDgrasp <- K_data_school$K_CDgrasp / K_data_school$K_sch
K_data_school$K_ALT <- K_data_school$K_ALT / K_data_school$K_sch # not rate, but ratio
K_data_school[22,10] <- 0.81 # NA treatment for Shizuoka-pref by overwriting

C_data_student$C_exam.rate <- C_data_student$C_exam / C_data_student$C_s3rd
C_data_student$C_A1seem.rate <- C_data_student$C_A1seem / C_data_student$C_s3rd
C_data_student$C_A1cefr.rate <- C_data_student$C_A1cefr / C_data_student$C_s3rd
C_data_student$C_A1Lv <- C_data_student$C_A1seem + C_data_student$C_A1cefr
C_data_student$C_A1Lv.rate <- (C_data_student$C_A1seem + C_data_student$C_A1cefr)/C_data_student$C_s3rd
C_data_school$C_LA50more <- C_data_school$C_LA50 + C_data_school$C_LA75
C_data_school$C_tE50more <- C_data_school$C_tE50 + C_data_school$C_tE75
C_data_school$C_CDpublic <- C_data_school$C_CDpublic / C_data_school$C_sch
C_data_school$C_CDgrasp <- C_data_school$C_CDgrasp / C_data_school$C_sch
C_data_school$C_ALT <- C_data_school$C_ALT / C_data_school$C_sch # ratio

# Bind student-based data & school-based data
K_df <- cbind(K_data_student,K_data_school)
C_df <- cbind(C_data_student,C_data_school)

# Now you have 2 data frames to use
# K_df, C_df
