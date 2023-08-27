#detach(name = "package:MASS", unload = TRUE) # after loading MASS
#library(dplyr)

# take abs of ML1 & Factorize cluster
Dt_2Fscore$ML1abs <- abs(Dt_2Fscore$ML1)
# Integrate: K_df, C_df, B_df, Dt_2Fscore
All_df <- cbind(K_df, C_df, B_df, Dt_2Fscore)
# standardizing
All_dfs <- as.data.frame(scale(All_df))
colnames(All_df)
OriginalNum <- c(1:4,8,10,21:24,28,30) # Recover count data
for (i in OriginalNum) {
  All_dfs[, i] <- All_df[, i]
}
# add cluster number
All_dfs$cluster <- kmFA$cluster
All_dfs$cluster <- as.factor(All_dfs$cluster)
All_dfs$cluster <- relevel(All_dfs$cluster, ref = "1") # relevel

colnames(All_dfs) # should be 46 cols

# split K_ and C_
Kall_df <- All_dfs %>% 
  select(-starts_with("C_")) %>% 
  rename_with(~ sub("^K_", "", .), starts_with("K_"))
Call_df <- All_dfs %>% 
  select(-starts_with("K_")) %>% 
  rename_with(~ sub("^C_", "", .), starts_with("C_"))

# Preparing binomial count matrices
K_A2cefr.mat <- as.matrix(data.frame(Success = Kall_df$A2cefr, Failure = Kall_df$s3rd - Kall_df$A2cefr))
K_A2seem.mat <- as.matrix(data.frame(Success = Kall_df$A2seem, Failure = Kall_df$s3rd - Kall_df$A2seem))
K_A2Lv.mat <- as.matrix(data.frame(Success = Kall_df$A2Lv, Failure = Kall_df$s3rd - Kall_df$A2Lv))
K_A2exam.mat <- as.matrix(data.frame(Success = Kall_df$exam, Failure = Kall_df$s3rd - Kall_df$exam))
C_A1cefr.mat <- as.matrix(data.frame(Success = Call_df$A1cefr, Failure = Call_df$s3rd - Call_df$A1cefr))
C_A1seem.mat <- as.matrix(data.frame(Success = Call_df$A1seem, Failure = Call_df$s3rd - Call_df$A1seem))
C_A1Lv.mat <- as.matrix(data.frame(Success = Call_df$A1Lv, Failure = Call_df$s3rd - Call_df$A1Lv))
C_A1exam.mat <- as.matrix(data.frame(Success = Call_df$exam, Failure = Call_df$s3rd - Call_df$exam))
