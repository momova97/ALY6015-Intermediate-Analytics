# Module 4 Assignment: Regularization with Ridge and LASSO Regression

## Introduction

The Module 4 assignment delves into the application of regularization techniques, specifically Ridge and LASSO regression, using the `glmnet` package in R. The College dataset from the ISLR library is utilized to predict the graduation rate (`Grad.Rate`). The assignment involves data preparation, model fitting, and evaluation, with a focus on understanding the impact of regularization on model complexity and performance.

## Summary

The assignment comprised several key steps:

- The dataset was split into training and testing sets to facilitate model validation.
- The `cv.glmnet` function was employed to estimate the optimal `lambda.min` and `lambda.1se` values for both Ridge and LASSO regression.
- Plots from the `cv.glmnet` function were generated and interpreted to understand the relationship between lambda values and model error.
- Ridge and LASSO regression models were fitted to the training data, and their coefficients were analyzed.
- The performance of each model was assessed using the root mean square error (RMSE) on both the training and testing sets.
- A stepwise selection process was conducted, and the resulting model was compared to the Ridge and LASSO models.

## Outcomes

The Ridge regression model demonstrated a significant reduction in RMSE, utilizing all the variables from the initial model. In contrast, the LASSO model, while slightly increasing RMSE, simplified the model by reducing the number of variables. The stepwise selection model yielded an RMSE comparable to the Ridge model, indicating its effectiveness.

## Reflection

This assignment provided practical insights into the trade-offs between model complexity and prediction accuracy. The regularization methods not only helped in improving model performance but also in feature selection, leading to simpler and more interpretable models. The experience gained from this assignment is invaluable for future analytical tasks involving model selection and evaluation.

Thank you for reviewing my work on this assignment.
