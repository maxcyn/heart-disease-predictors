data <- read.csv("heart_disease_indicators_dataset.csv")

set.seed(123) # Ensures you get the same random sample every time
sampled_data <- data[sample(nrow(data), 4000), ]

# EDA
print("--- CORRELATION MATRIX ---")
correlation_matrix <- cor(sampled_data[, 1:10])
print(round(correlation_matrix, 2))

# family = "binomial" uses the Logit Link function
full_model <- glm(HeartDiseaseorAttack ~ ., data = sampled_data, family = "binomial")

# shows results
print("--- FULL MODEL SUMMARY ---")
summary(full_model)

# model selection via AIC
reduced_model <- step(full_model, direction = "backward", trace = 0)

print("--- REDUCED MODEL SUMMARY (AFTER AIC SELECTION) ---")
summary(reduced_model)

# Conduct the LRT using the Chi-Square distribution
print("--- LRT ---")
anova(reduced_model, full_model, test = "Chisq")

print("--- ODDS RATIOS FOR FINAL MODEL ---")
odds_ratios <- exp(coef(reduced_model))
print(round(odds_ratios, 3))

# 95% Confidence Intervals:
print("--- 95% CONFIDENCE INTERVALS FOR ODDS RATIOS ---")
ci_odds_ratios <- exp(confint(reduced_model))
print(round(ci_odds_ratios, 3))