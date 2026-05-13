# The Cost of Complexity in Medical Diagnostics: Cardiovascular Risk Prediction

**Author:** Max Chong, Imperial College London.

## Project Overview
This project navigates the bias variance trade-off in high-dimensional data to quantify the cost of complexity in medical diagnostics. The objective is to identify the minimum number of covariates required to accurately predict heart disease, reducing a 21-parameter diagnostic questionnaire to a streamlined predictive model for rapid patient triaging.

## Dataset
The dataset consists of a random sample of 4000 observations from the CDC Heart Disease Health Indicators dataset. The target variable is binary, indicating the presence or absence of cardiovascular disease.

## Methodology
* **Exploratory Data Analysis:** A correlation matrix evaluates collinearity among physiological predictors.
* **Model Selection:** A Generalised Linear Model (GLM) employs the logit link function $g(\mu) = \log(\frac{\mu}{1-\mu})$ to model the Bernoulli distributed outcomes.
* **Variable Selection:** A backward stepwise algorithm minimizes the Akaike Information Criterion (AIC) to discard redundant variables and penalize unnecessary parameterization.
* **Statistical Validation:** The Likelihood Ratio Test (LRT) validates the reduction of the parameter space, comparing the full 21-variable model to the reduced model.
* **Parameter Estimation:** Parameters are estimated via Maximum Likelihood Estimation using the Fisher Scoring iterative algorithm.

## Key Results
* The backward selection algorithm converged after 6 Fisher Scoring iterations, eliminating 10 non-informative covariates such as BMI, fruit consumption, and general physical activity.
* The resulting 11-variable model achieved an AIC of 1901.3, an improvement over the full model's AIC of 1915.5.
* The LRT yielded a p-value of 0.8298, demonstrating no statistically significant loss of predictive capability after discarding the 10 variables.
* A prior stroke is the most critical risk factor, with an odds ratio of 2.515.
* Other significant predictors include High Cholesterol (OR = 2.195), Male Sex (OR = 2.054), and High Blood Pressure (OR = 1.850).

## Repository Structure
* `main.R`: Contains the R script for data loading, exploratory data analysis, GLM fitting, AIC stepwise selection, and asymptotic hypothesis testing.
* `health_indicators.pdf`: The complete theoretical report detailing the statistical derivations and clinical interpretations.
* `heart_disease_indicators_dataset.csv`: The primary dataset utilized for the statistical analysis.
