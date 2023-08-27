library(psych)
library(GPArotation)

# SSDSE_D ... EFA for Dimensionality reduction
colnames(Dt_df); length(Dt_df)

# Check for NA / zero-Variance values
any(is.na(Dt_df)) ;any(sapply(Dt_df, var)==0)

# Compute cor.matrix
# Solve for Eigen-values & Eigen-vectors
Dt_matrix <- cor(Dt_df)
eigen(Dt_matrix) # before rotation

# Determine the number of factors 
# Scree Plot; Parallel Analysis
#library(psych)
VSS.scree(Dt_df)
set.seed(1)
fa.parallel(Dt_df, fm="ml", fa="fa", main = "")
mtext("図３　SSDSE-D因子の平行分析（FN＝２を示唆）", side = 1, line = 4)

 # Scree plot implies 1~2 factors
 # Parallel analysis implies 2 factors (with the setting n.obs=470 (at least 10obs from each region) supports stronger)
 # Assume 2 factors for exploration

# Estimate Factor loadings
#library(GPArotation)
Dt_fa <- fa(Dt_df, nfactors=2, fm="ml", rotate="promax")
print(Dt_fa, sort=TRUE, digits=3)
Dt_fa$Vaccounted # Proportion Explained .852:.148 (=5.76:1)

# Plot Factor Loadings
plot(Dt_fa$loadings[,1],Dt_fa$loadings[,2], type = "n",
     xlab="F1_axis",ylab="F2_axis")
text(Dt_fa$loadings[,1],Dt_fa$loadings[,2], cex = 0.7,
     rownames(Dt_fa$loadings),col="blue")
abline(h=0, v=0, lty="dotted")
mtext("図４　SSDSE-D 因子負荷量プロット", side = 1, line = 4)

 # F1: - long activity/ + various activity (cf. t.study_student is negatively correlated to other variables)
 # F2: Stamina or Physical activity

# Reliability Coefficient
omega_result <- omega(Dt_df, nfactors = 2, fm="ml", rotate = "promax")
omega_result$alpha; omega_result$omega.tot
omega_result
# alpha/omega are a little too high (.920/.950)

# Factor scores
Dt_2Fscore <- as.data.frame(Dt_fa$scores)
head(Dt_2Fscore); summary(Dt_2Fscore); apply(Dt_2Fscore,2,sd)
plot(Dt_2Fscore$ML1,Dt_2Fscore$ML2)
abline(h=0, v=0)

### ~Optional~ ### Scaling MLs by Proportion Explained (multiply 4.68=.824/.176)
#Dt_2Fscore$ML1 <- Dt_2Fscore$ML1*5.76
#plot(Dt_2Fscore$ML1,Dt_2Fscore$ML2,xlim=c(-10,17),ylim=c(-10,17))
#abline(h=0, v=0) # ML2 seems minuscule to ML1

# clustering
set.seed(1)
kmFA <- kmeans(Dt_2Fscore,4,iter.max = 50)
color_kmFA <- as.factor(kmFA$cluster)
levels(color_kmFA) <- c("red","blue","black","green3")
color_kmFA <- as.character(color_kmFA)

plot(Dt_2Fscore$ML1,Dt_2Fscore$ML2,type="n",
     xlab="F1_axis",ylab="F2_axis")
text(Dt_2Fscore$ML1,Dt_2Fscore$ML2, cex=0.7,
     labels=rownames(Dt_2Fscore), col=color_kmFA)
abline(h=0, v=0, lty="dotted")
text(c(-1.5,2,2.5,-1.7),c(2,-1,2,-0.5),
     labels = c("Cluster.4", "Cluster.3", "Cluster.1","Cluster.2"), cex = 1, col = "darkred")
text(c(-1.5,2,2.5,-1.7),c(1.7,-1.3,1.7,-0.8),
     labels = c("(n=23)", "(n=11)", "(n=5)","(n=8)"), cex = 0.8, col = "darkred")
mtext("図５　都道府県 因子スコアプロット/クラスタリング", side = 1, line = 4)

kmFA$cluster
table(kmFA$cluster)
 # Update interpretation
 # F1: long / frequent (abundance)
 # F2: Suitability for outside activity
