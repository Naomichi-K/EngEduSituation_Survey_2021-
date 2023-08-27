# Run the data adjustment code (line.56~67) just once,
 # or rerun the code-file_02~03!!

### Overview ###########################
# 1.1) EngEdu Normality check ##########
head(K_df); length(K_df)
Norm_check_df(K_df,0,c(1:23)) # logswitch=1...log-transform
Norm_check_df(C_df,0,c(1:23)) # logswitch=1...log-transform
dev.off()
Norm_check_vec(K_df$K_ALT,0)
Norm_check_vec(K_df$K_ALT,1) # K_ALT ~ log Normality

# 1.2) EngEdu Correlation check
pairs(K_df[,c(5:7,9)]) # exam/cefr/seem/Lv.rate
pairs(K_df[,c(5:7,9, 12:18)])
pairs(K_df[,c(5:7,9, 19:23)])
pairs(C_df[,c(5:7,9)]) # exam/cefr/seem/Lv.rate
pairs(C_df[,c(5:7,9, 12:18)])
pairs(C_df[,c(5:7,9, 19:23)])
#cor(K_df[,c(5:7,9)])

# 2) SSDSE_B ##########
head(B_df); length(B_df)
pairs(B_df); cor(B_df) # Relatively high-cor between all valuables
Norm_check_df(B_df,0,c(1:4))
Norm_check_vec(B_df$Edu.2020,1) # Edu.2020 ~ log Normality
dev.off()

# 3) SSDSE_D ##########
head(Dt_df); length(Dt_df) # 17 cols (originally)
pairs(Dt_df); cor(Dt_df) # Relatively high-cor between all valuables
Norm_check_df(Dt_df,0,c(11,17))
dev.off()

### Visualize with a combined title #############
dev.off()
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
Vec1 <- K_df$K_ALT #1
hist(Vec,main="K_ALT", xlab = "", ylab = "hist (freq)")
qqnorm(Vec, xlab = "", ylab = "QQ-plot",
       main=paste("p.value: ",round(shapiro.test(Vec)$p.value,4)))
qqline(Vec)
Vec2 <- log(K_df$K_ALT) #2
hist(Vec,main="log(K_ALT)", xlab = "", ylab = "hist (freq)")
qqnorm(Vec2, xlab = "", ylab = "QQ-plot",
       main=paste("p.value: ",round(shapiro.test(Vec)$p.value,4)))
qqline(Vec2)
mtext("図１ 正規性検討過程の一部（K_ALT~Log-Normal）", side=1, line=-2, outer=TRUE)
dev.off()

colnames(K_df)
pairs(K_df[,c(5:7,9,11:20)])
mtext("図２　EngEduの相関（高等学校データ　※変数選抜後）", side = 1, line = 3.5)


### data adjustment #############################
# transform
K_df$K_ALT <- log(K_df$K_ALT)
# Ignore C_CDpublic (not remove for K_CDpublic)
K_df <- K_df[,-c(11,22,23)] # remove _CDpublic, _tE50more, _LA50more
C_df <- C_df[,-c(11,22,23)] # remove _tE50more, _LA50more

# remove Edu.ave, Cul.ave
B_df <- B_df[,c(1,2)]
# transform
B_df$Edu.2020 <- log(B_df$Edu.2020)

# remove L.foreign, L.nonEng, t.Study
Dt_df <- Dt_df[,-c(2,4)] # 15 cols