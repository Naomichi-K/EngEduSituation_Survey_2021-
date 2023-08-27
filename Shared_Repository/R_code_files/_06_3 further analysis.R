# Confirm you ran the SETTING_2 in code-file_06_2

# interpretation for coefficient ##########
Coef <- as.data.frame(model_used$coefficients)
Coef$exp_impact <- exp(Coef$`model_used$coefficients`) # explicative impact
print(Coef)

# interpretation for intercept ############
1/(1+exp(-Coef$`model_used$coefficients`[1])) # intercept inverse-logit-transformed
summary(K_df$K_A2cefr.rate) # compare with the observed mean

# group mean plotting ######################
# Load dplyr if you haven't already
detach("package:MASS",unload = T)
library(dplyr)

# Calculate the mean values for each cluster
cluster_means <- fit.obs_rate %>%
  group_by(cluster) %>%
  summarise(mean_observed = mean(Observed), mean_predicted = mean(Predicted))

# Fitted lines grouped by cluster
ggplot(fit.obs_rate, aes(x = Predicted, y = Observed,
                                 group = cluster, color = cluster, shape = cluster)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = T, level = 0.80, size = 0.5) +
  geom_point(data = cluster_means, aes(x = mean_predicted, y = mean_observed),
             size = 4, shape = 5, color = "red") + # Add this line
  labs(title = "Binomial Logistic Regression (by cluster)",
       x = "Predicted", y = "Observed") +
  ylim(c(0,1)) +
  theme_minimal() +
  facet_wrap(~cluster, nrow = 4)

