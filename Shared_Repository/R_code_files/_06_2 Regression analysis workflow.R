### Load package (*MASS masks dplyr) ################
library(MASS)
library(ggplot2)

### workflow: Just manipulate SETTINGs ##############
#■■■ SETTINGS_2 ■■■ Regression ■■■■■■■■■■■■■■■■■■■■■■■■
data_used <- Kall_df # choose Kall_df / Call_df
colnames(data_used)  # predictors list
NULL_formula <- as.formula(K_A2cefr.mat ~ 1) # choose a matrix (check code-file 06_1) and predictors
FULL_formula <- as.formula(K_A2cefr.mat ~. -s3rd -sch -exam -A2cefr -A2seem -A2Lv -A2cefr.rate -A2Lv.rate -ML1 -ML2 -ML1abs)
 # separate: -ML2 -ML1abs / -cluster

# Just Run!
### Wf_1: Preparing formula objects #################
null_model <- glm(formula = NULL_formula, data = data_used, family=binomial(logit))
full_model <- glm(formula = FULL_formula, data = data_used, family=binomial(logit))

### Wf_2: Model selection ###########################
model_1 <- stepAIC(null_model, direction="forward",
                   scope=list(lower=null_model, upper=full_model))
model_2 <- stepAIC(full_model, direction="backward",
                   scope=list(lower=null_model, upper=full_model))
model_3 <- stepAIC(null_model, direction="both",
                   scope=list(lower=null_model, upper=full_model))
model_4 <- stepAIC(full_model, direction="both",
                   scope=list(lower=null_model, upper=full_model))

### Wf_3: Displaying results ########################
extractAIC(full_model)
extractAIC(model_1)
extractAIC(model_2)
extractAIC(model_3)
extractAIC(model_4)

summary(full_model)
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

model_1$coefficients

#####################################################
#■■■ SETTINGS_2 ■■■ Pred-Obs plot ■■■■■■■■■■■■■■■■■■■
model_used <- model_1
Obs <- K_df$K_A2cefr.rate # specify obs-data to use pre-standardized (non negative) value

### Wf_4: Compare fitted / observed values ##########
fitted_rate <- predict(model_used, type = "response")
fit.obs_rate <- data.frame(Predicted = fitted_rate,
                           Observed = Obs)
fit.obs_rate$cluster <- All_dfs$cluster
head(fit.obs_rate); plot(fit.obs_rate)

### Wf_4.2 ### ~Optional~ ### Logit Transformation (avoid negative value)
fitted_logit <- predict(model_used, type = "link")
fit.obs_logit <- data.frame(Predicted = fitted_logit,
                            Observed = log(Obs/(1-Obs))) # valid?
head(fit.obs_logit); plot(fit.obs_logit)

### Wf_5.1 ### Visualize precision of fitted line ###########
ggplot(fit.obs_rate, aes(x = Predicted, y = Observed)) +
  geom_point(size=2) +
  geom_smooth(method = "glm", method.args = list(family="binomial"),
              se = T, level = 0.80) +
  labs(title = "Binomial Logistic Regression (80%CI)",
       x = "Predicted", y = "Observed") +
  ylim(c(0,1))+
  theme_minimal()

### Wf_5.2 ### grouping by cluster ###########
Fig2 <- ggplot(fit.obs_rate, aes(x = Predicted, y = Observed,
                         group=cluster, color=cluster, shape=cluster)) +
  geom_point(size=1.5) +
  geom_smooth(method = "glm", method.args = list(family="binomial"),
              se = F, level = 0.80, size=0.5) +
  labs(title = "Binomial Logistic Regression (by cluster)",
       x = "Predicted", y = "Observed") +
  ylim(c(0,1))+
  theme_minimal()
Fig2
Fig2 + facet_wrap(~cluster,nrow = 3)

### Wf_6.1 ### Bar-plot comparing pred-obs #############
fit.obs_rate <- fit.obs_rate[order(fit.obs_rate$Observed, decreasing = T),] # rearrange
bp <- barplot(fit.obs_rate$Observed, 
              names.arg = rownames(fit.obs_rate), 
              las = 2, cex.names = 0.5,
              col = "white",
              main = c("+ Predicted value"),
              ylab = c("observed value"),
              ylim = c(0,0.6))
points(bp, fit.obs_rate$Predicted, pch = 3, col = "red")

### Wf_6.2 ### Ranking filled by cluster  #############
fit.obs_rate <- fit.obs_rate[order(fit.obs_rate$Observed, decreasing = T),]
fit.obs_rate$region <- rownames(fit.obs_rate)

 # Set the levels of the region factor to match the current order
detach("package:MASS",unload = T)
library(dplyr)
fit.obs_rate$region <- factor(fit.obs_rate$region, levels = fit.obs_rate$region)
fit.obs_rate <- fit.obs_rate %>%
  mutate(vjust_value = case_when(
    cluster == 1 ~ 0,
    cluster == 2 ~ -0.5,
    cluster == 3 ~ -1,
    cluster == 4 ~ -1.5
  ))
 # Visualize
ggplot(fit.obs_rate, aes(x=region, y=Observed, fill=factor(cluster))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=factor(cluster), y=0), vjust=fit.obs_rate$vjust_value) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(caption  = "図７　K_A2cefr.rate順位（クラスタ比較）",
       x = "",
       y = "K_A2cefr.rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.caption = element_text(hjust = 0.5,vjust = 5, size = 13),
        legend.position = "right") # Optional: rotate x labels if they are long

### Wf_7 ### Violin plot by clusters #############
ggplot(fit.obs_rate, aes(x = factor(cluster), y = Observed, fill = factor(cluster))) +
  geom_violin(width = 0.5, color = "lightgrey") + # Adjust width and color here
  geom_boxplot(width=0.1) +
  geom_point(size = 1) + 
  labs(caption = "図６　K_A2cefr.rateバイオリンプロット（クラスタ比較）",
       x = "",
       y = "K_A2cefr.rate") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel1") +
  theme(plot.caption = element_text(hjust = 0.5,vjust = 5, size = 13))

Kall_df[Kall_df$cluster == "1",]
summary(K_df$K_A2seem.rate)
