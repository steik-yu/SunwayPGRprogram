### ----- PERFORM Simple linear regression manually -----

# Question 1: Does tumor size predict Mortality_Rate_per_100K?
# • H0: β1 = 0 (tumor size has no effect on Mortality_Rate_per_100K)
# • H1: β1 ≠ 0 (tumor size has a significant effect on Mortality_Rate_per_100K)
# i. Perform a simple linear regression in R.

model_simple <- lm(Mortality_Rate_per_100K ~ Tumor_Size_mm, data = data)
summary(model_simple)

ggplot(data, aes(x = Tumor_Size_mm, y = Mortality_Rate_per_100K)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Tumor Size vs Mortality_Rate_per_100K (Simple Linear Regression)")


### ----- PERFORM Multiple linear regression manually -----

# Question 2: Do tumor size, age, and cancer stage together predict healthcare costs?
# • H0: β1 = β2 = β3 = 0 (none of the predictors affect healthcare costs)
# • H1: At least one β ≠ 0 (at least one predictor significantly affects healthcare costs)
# i. Perform a multiple linear regression in R.

model_multiple <- lm(Healthcare_Costs ~ Tumor_Size_mm + Age + Cancer_Stage, data = data)
summary(model_multiple)

data$predicted_costs <- predict(model_multiple)
ggplot(data, aes(x = predicted_costs, y = Healthcare_Costs)) +
  geom_point(color = "purple") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual Healthcare Costs (Multiple Regression)",
       x = "Predicted Costs", y = "Actual Costs")


# Question 3: Do tumor size and age interact to predict healthcare costs, controlling for cancer stage?
# • H0: β1 = β2 = β3 = β4 = 0 (no effect of tumor size, age, cancer stage, or their interaction)
# • H1: At least one β ≠ 0 (at least one predictor, including the interaction, significantly affects healthcare costs)
# i. Perform a multiple linear regression in R.

# Model with interaction between Tumor_Size_mm and Age
model_interaction <- lm(Healthcare_Costs ~ Tumor_Size_mm * Age + Cancer_Stage, data = data)

summary(model_interaction)


### ----- PERFORM Logistic regression manually -----

# Question 4: Do age and tumor size predict the likelihood of 5-year survival?
# • H0: β1 = β2 = 0 (predictors have no effect on the probability of survival)
# • H1: At least one β ≠ 0 (at least one predictor significantly affects survival probability)
# i. Perform a logistic regression in R.

# Ensure Survival_5_years is coded as a factor
data$Survival_5_years <- factor(data$Survival_5_years, levels = c("No", "Yes"))

model_logistic <- glm(Survival_5_years ~ Age + Tumor_Size_mm, 
                      data = data, family = binomial)
summary(model_logistic)

data$predicted_prob <- predict(model_logistic, type = "response")
ggplot(data, aes(x = Age, y = predicted_prob, color = Survival_5_years)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "Predicted Probability of Survival by Age (Logistic Regression)",
       y = "Predicted Probability")
