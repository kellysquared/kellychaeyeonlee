---
title: "final project"
author: "jian & kelly"
output: pdf_document
date: "2024-05-05"
---

Jian Jo: part 1 & part 2

Kelly Lee: part 3 & part 4

\newpage

# Part 1: Introduction

This project focuses on the analysis of factors contributing to the cost of treatment of patients and creating predictive models for the charges by the health insurance provider.

This insurance charge data is obtained from the website Kaggle. This data set contains 1338 observations of 7 variables. The variables include:

1.  Age: the age of the primary beneficiary under consideration

2.  Sex: gender of the insurance contractor — categorized as female or male

3.  BMI: Body Mass Index is a measure of body weight relative to height, indicating whether weight is comparatively high or low. It's the ratio of weight (in kilograms) to height (in meters) squared. (ideally, BMI values range between 18.5 and 24.9)

4.  Children: the number of children covered by health insurance or the count of dependents

5.  Smoker: whether the individual smokes or not

6.  Region: the residential area of the beneficiary within the United States, with options such as northeast, southeast, southwest, and northwest

7.  Charges: the individual medical costs billed by the health insurance provider

For modeling purposes, the dataset will be split into training and testing sets. All analytical procedures will be conducted using R. This report will comprise total of 4 sections, beginning with this introduction section. The part 2 will focus on Exploratory Data Analysis, while the part 3 will outline the methodology with predictive models. The final section, the part 4, will talk about the final summary and conclusions.

------------------------------------------------------------------------

# Part 2: Exploratory Data Analysis

```{r}
# import all necessary packages 
library(lmtest)
library(readr)
library(ggplot2)
library(faraway)
library(lattice)
library(caret)
library(knitr)
library(car)
```

```{r}
df <- read.csv("insurance.csv")
head(df)
```

```{r}
names(df)
```

```{r}
str(df)
```

From this table above, we got the basic idea of each variable. There are 1338 rows (observations) along with 7 columns (variables). There are 3 categorical variables—sex, smoker, and region—and 4 numerical variables such as age, bmi, children, and charges.Our response variable will be **charges**. Also, each data type looks valid—fitting to what they are supposed to be.

```{r}
df$sex <- factor(df$sex)
df$smoker <- factor(df$smoker)
df$region <- factor(df$region)
```

Turn categorical variables as factor to fit a model later on.

```{r}
# check for null values 
colSums(is.na(df))
```

There is no null values in this dataset.

### Plotting - individual variable 

```{r}
# visaulize distributions 
# categorical 
print(table(df['sex']))
ggplot(data = df) + theme(plot.title = element_text(hjust = 0.57)) + geom_bar(mapping = aes(x = sex))
```

```{r}
# children
print(table(df['children']))
ggplot(data = df) +
  geom_bar(mapping = aes(x = children))
```

```{r}
# smoker 
print(table(df['smoker']))
ggplot(data = df) +
  geom_bar(mapping = aes(x = smoker))
```

```{r}
# region
print(table(df['region']))
ggplot(data = df) +
  geom_bar(mapping = aes(x = region))
```

```{r}
# visualize distributions for continuous variables 
print(table(df['age']))
ggplot(data = df) +
  geom_histogram(mapping = aes(x = age), binwidth = 0.5)
```

```{r}
# bmi
ggplot(data = df) +
  geom_histogram(mapping = aes(x = bmi), binwidth = 0.5)
```

```{r}
# charges
ggplot(data = df) +
  geom_histogram(mapping = aes(x = charges), binwidth = 0.5)
```

The histogram of the "bmi" is the only one looking like it has a bell-curve but left-skewed.

```{r}
lapply(df, unique)
```

We confirmed that there's no null values by looking at the unique values from each variable. It's pretty safe to say there is no null value.

```{r}
# relationship between input variables and output variable, "charges"
# Box plots for numerical variables against charges
ggplot(df, aes(x=age, y=charges)) + geom_point() + geom_smooth(method="lm") + ggtitle("Figure 1") + theme(plot.title = element_text(hjust = 0.5))
ggplot(df, aes(x=bmi, y=charges)) + geom_point() + geom_smooth(method="lm") + ggtitle("Figure 2") + theme(plot.title = element_text(hjust = 0.5))
ggplot(df, aes(x=children, y=charges)) + geom_point() + geom_smooth(method="lm")

# Box plots for categorical variables against charges
ggplot(df, aes(x=sex, y=charges)) + geom_boxplot() + ggtitle("Figure 4") + theme(plot.title = element_text(hjust = 0.5))
ggplot(df, aes(x=smoker, y=charges)) + geom_boxplot() + ggtitle("Figure 5") + theme(plot.title = element_text(hjust = 0.5))
ggplot(df, aes(x=region, y=charges)) + geom_boxplot() + ggtitle("Figure 6") + theme(plot.title = element_text(hjust = 0.5))
```

From the scatter plot between age and charges, we can see that the charges increase as the ages increase, linear relationship. However, It seems like there are 3 different groups for charges which may have affected by the wealthiness.

The graph between BMI and charges shows a positive correlation as the trend line slope upwards. There's variability in charges by BMI, especially as BMI increases, which implies that while BMI is a factor in medical bill costs, other variables may influence the cost significantly. Also, there are notable outliers in higher BMI ranges where there's a huge gap difference among some individuals at similar BMI levels. We can consider BMI could be one of the risk factors when predicting insurance cost from this graph.

The scatterplot suggests that even though the variable "children" is numerical, it should be considered as categorical and converted using the factor function.

For the categorical variables, we applied boxplots to understand the relationship with the response variable.

From the graph of sex and charges, it's easy to notice that there are several outliers at high end of charges both in female and male. There's no significant differences in median of charges between males and females, but individual variability is high. Although the medians are similar, the presence of outliers in both groups indicates the factors other than sex might be more predictive of high charges.

The boxplot between smoker and charges shows a significant difference in median charges between smokers and non-smokers. Smokers incur far higher median insurance charges which suggests that smoking is one of the influential factors. The outliers in "no" group are more pronounced and numerous, indicating occasional high medical bills among non-smokers.

The boxplot of figure 6 shows the distribution of charges across different regions: northeast, northwest, southeast, and southwest. There is no huge difference across regions in terms of median charges but the spread and extremes (outliers) vary with the southeast showing higher outliers.

```{r}
df$children <- factor(df$children)
ggplot(df, aes(x=children, y=charges)) + geom_boxplot() + ggtitle("Figure 3") + theme(plot.title = element_text(hjust = 0.5))
```

After revising the children variable, the boxplot provides better insights. The median charges among all the number of children locates at similar price. However, the range (spread between the lowest and highest charges excluding outliers) of charges increase with more children up to 2 and start decreasing from 3 to 5 number of children. There are many outliers in the 0 children group and following number of children.

### Diagnostic Tests

Now, let's check if there are any outliers.

In order to check if there are any outliers or other influential points that may affect our model negatively, several tests need to be done.

```{r}
g = lm(charges~., data=df)
# cook distance 
c = cooks.distance(g)
halfnorm(c, 5, labs=row.names(df), ylab = 'cook', main="Figure 7")
```

```{r}
print(length(which(c > 0.5)))
print(length(which(c > 1)))
```

The Figure 7 shows the half-normal quantiles along with x-axis and the Cook's distances for the obervations in the dataset with y-axis. Most of the data points cluster around the lower end of the y-axis, indicating small influence on the model. However, those labeled (e.g., "1301", "322", "578") are especially distant from the rest, suggesting these are influential observations having a substantial impact on the model's predictions. The numbers—1301, 322, and 578—are the index of the rows which make it easier to identify and possibly exclude them from further analysis.

```{r}
influential <- which(c > 4/(nrow(df)-length(coef(g))))
print(length(influential))
```

```{r}
# number of data points and number of predictors
n = nobs(g)
p = length(coef(g)) - 1 
lev=influence(g)$hat
lev[lev>2*p/n]
```

```{r}
# high leverage points 
print(length(lev[lev>2*p/n]))
halfnorm(lev,5, labs=row.names(df),ylab='lev',main="Figure 8")
```

There are 44 high leverage points which incidate outliers in regards to the independent variables. They can be good or bad influential points.

```{r}
j <- rstudent(g)
Bonferroni = qt(0.05/(2*n), df= n-p-1)
print(Bonferroni)
sort(abs(j), decreasing = TRUE)[1:5]
```

The studentized residuals were checked to see if the abnormal points (1301, 322, 578) from the Cook's distance are outliers. The output shows that those two points appear among the 5 most extreme studentized residuals. Both the points 1301 and 578 have higher residuals (respectively 5.0222 and 4.255) than -4.137, the critical value we observed using R. This leads us to conclude that we need to remove the two outliers: index 1301 and 578.

```{r}
# remove outliers 
df <- df[-c(1301, 578), ]
dim(df)
```

After removing the two outliers, we have 1336 observations now.

### Correlated Features

Correlated Features were done to reduce the features for efficiency. The high correlated value usually have similar impact on predicted so getting rid of the features may help reducing the chance of over fitting and increasing efficiency.

```{r}
# convert categorical variables into factor to obtain correlation matrix
df$sex <- as.numeric(df$sex)
df$region <- as.numeric(df$region)
df$smoker <- as.numeric(df$smoker)
df$children <- as.numeric(df$children)

correlationMatrix <- cor(df)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=.75)

# print indexes of highly correlated attributes
print(highlyCorrelated)
```

From this correlation matrix, we can expect the smoker to be a key insight for predicting charges with a strong positive correlation (0.787). On the other hand, variable sex and children shows very low correlation with most variables, including charges (0.058 and 0.072), which suggests that sex and children barely have influence on charges. Age and BMI shows a low to somewhat moderate positive correlation with charges (0.303 and 0.198). This may indicate that age and BMI might not be a significant factor in the predictive models. Also, region has a small value of negative correlation with charges.

Variables with low correlation with the response variable (charges) may have less predictive power. However, we still keep them in the dataset since they might still be useful in the presence of non-linear relationships not captured by correlation.

```{r}
# Shapiro-Wilk normality test on residuals
shapiro.test(residuals(g))

# Durbin-Watson test for autocorrelation
dwtest(g)
```

We utilize the shapiro-wilk normality test and durbin-watson test to check whether the residuals are normally distributed which is an assumption of most linear regression models, and to check for the presence of autocorrelation in the residuals, which may be issues such as omitted variables.

The shapiro-wilk test has p-value of 2.2e-16 which is extremely small—reject the null hypothesis that the residuals are normally distributed. This suggests that the residuals don't follow a normal distribution. The Durbin-Watson test has a statistic of 2.084 suggesting little to no autocorrelation. The p-value of 0.9391 is higher than 0.05 (commonly used significance level), indicating that there is no statistically significant autocorrelation in the residuals. Having no significant autocorrelation among the residuals is good for modeling.

------------------------------------------------------------------------

# Part 3: Methodology

## 1) Model 1: Simple Linear Regression Model

```{r}
library(car)  # for diagnostic tools
# Fitting the model
lm_model <- lm(charges ~ age + bmi + smoker + children, data = df)
summary(lm_model)


```

### Diagnostics

```{r}
# Load necessary libraries
library(ggplot2)
library(car)

# Assuming the model is already fitted and is named lm_model
# lm_model <- lm(charges ~ age + bmi + smoker + children, data = df)

# 1. Plotting Residuals vs Fitted values to check for homoscedasticity and linearity
plot(lm_model, which = 1, main = "Residuals vs Fitted")
abline(h = 0, col = "red")  # Adding a horizontal line at 0


# Comment for R script: 
# Generate Figure for Section 2: Residuals vs Fitted plot to check for homoscedasticity and linearity

# 2. Normal Q-Q Plot to check for normality of residuals
plot(lm_model, which = 2, main = "Normal Q-Q")
# Comment for R script: 
# Generate Q-Q plot to assess normality of residuals

# 3. Scale-Location Plot (Spread vs Level) to check for equal spread of residuals
plot(lm_model, which = 3, main = "Scale-Location Plot")
# Comment for R script: 
# Generate Scale-Location plot to check for constant variance of residuals

# 4. Residuals vs Leverage plot to identify influential cases
plot(lm_model, which = 5, main = "Residuals vs Leverage")
abline(h = 0, col = "red")  # Adding a horizontal line at 0
# Adding Cook's distance contours
plot(lm_model, which = 5, cook.levels = c(0.5, 1), main = "Residuals vs Leverage Plot with Cook's Distance")
# Comment for R script: 
# Generate Residuals vs Leverage plot to identify influential observations

# Additional diagnostic: Cook's distance to identify influential points
cooks_dist <- cooks.distance(lm_model)
plot(cooks_dist, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index")
abline(h = 4 / length(cooks_dist), col = "blue", lty = 2)  # Threshold line
# Comment for R script: 
# Plot Cook's distance to identify potential influential points

# Checking if any Cook's distance values are significantly high
influential_points <- which(cooks_dist > (4 / length(cooks_dist)))
if(length(influential_points) > 0){
  print(paste("Influential points at indices:", paste(influential_points, collapse = ", ")))
} else {
  print("No influential points detected.")
}

# Comment for R script: 
# Check for influential points using Cook's distance

# Shapiro-Wilk test for normality of residuals
shapiro.test(resid(lm_model))
# Comment for R script:
# Conduct Shapiro-Wilk test to check normality of residuals

```

Section 3.2: Prediction Using Linear Regression

```{r}
# Assuming test data is loaded and named 'df_test'
predictions_lm <- predict(lm_model, newdata = df)
mse_lm <- mean((df$charges - predictions_lm)^2)
print(paste("MSE:", mse_lm))

```

Section 3.3: Ridge Regression Model

```{r}
# Load the necessary library
library(glmnet)

# Preparing the model matrix for the training data
data_matrix <- model.matrix(charges ~ ., data = df)[,-1]

# Fit the Ridge regression model
lambda_values <- 10^seq(3, -2, length = 100)
ridge_model <- glmnet(data_matrix, df$charges, alpha = 0, lambda = lambda_values)
# Choosing the best lambda using cross-validation
cv_ridge <- cv.glmnet(data_matrix, df$charges, alpha = 0, type.measure = "mse", nfolds = 10)
best_lambda <- cv_ridge$lambda.min


# Plotting the ridge model 
plot(ridge_model, xvar = "lambda", label = TRUE)
abline(v = log(best_lambda), col = "red", lwd = 2)
plot(cv_ridge)


```

```{r}

# Assuming 'df' is your dataframe and 'charges' is the response variable
x <- model.matrix(charges ~ age + bmi + smoker + children, data = df)[,-1]
y <- df$charges
# Fit Ridge regression with cross-validation
set.seed(123)  # for reproducibility
cv_model <- cv.glmnet(x, y, alpha = 0, nfolds = 10, type.measure = "mse")

# Best lambda
best_lambda <- cv_model$lambda.min
# Fit Ridge regression with cross-validation
set.seed(123)  # for reproducibility
cv_model <- cv.glmnet(x, y, alpha = 0, nfolds = 10, type.measure = "mse")

# Best lambda
best_lambda <- cv_model$lambda.min
# Coefficients at the best lambda
ridge_coefficients <- coef(cv_model, s = "lambda.min")

# Predict using the best lambda
predictions <- predict(cv_model, s = "lambda.min", newx = x)

# Manual calculation of R-squared
rss <- sum((predictions - y)^2)
tss <- sum((y - mean(y))^2)
r_squared <- 1 - rss/tss

# Printing results
print(paste("R-squared: ", r_squared))
print(paste("Optimal Lambda: ", best_lambda))


```

Modeling the ridge regression\^

```{r}

test_matrix <- model.matrix(~ ., data = df)[,-1]

missing_cols <- setdiff(colnames(data_matrix), colnames(test_matrix))


for (col in missing_cols) {
  test_matrix[[col]] <- 0  
}


test_matrix <- test_matrix[, colnames(data_matrix)]

ridge_predictions <- predict(ridge_model, s = best_lambda, newx = test_matrix)


```

```{r}
# Predicting with Ridge model
ridge_predictions <- predict(ridge_model, s = best_lambda, newx = test_matrix)

# Calculate MSE for Ridge Regression
mse_ridge <- mean((df$charges - ridge_predictions)^2)
print(paste("Ridge Regression MSE:", mse_ridge))


```

```{r}
library(glmnet)
library(ggplot2)

# Assuming x and y are your predictors and response matrix and vector respectively
# Fit the Ridge regression model
cv_model <- cv.glmnet(x, y, alpha = 0, nfolds = 10)

# Get predictions and calculate residuals
optimal_lambda <- cv_model$lambda.min
fitted_values <- predict(cv_model, s = optimal_lambda, newx = x, type = "response")
residuals <- y - fitted_values

# Create a data frame for plotting
df_diag <- data.frame(Fitted = as.vector(fitted_values), Residuals = as.vector(residuals))

# 1. Residuals vs. Fitted Values Plot
ggplot(df_diag, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")

# 2. Q-Q Plot of Residuals
qqnorm(residuals)
qqline(residuals, col = "red")

# 3. Scale-Location Plot
df_diag$SqrtAbsResiduals <- sqrt(abs(residuals))
ggplot(df_diag, aes(x = Fitted, y = SqrtAbsResiduals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Square Root of Absolute Residuals")

```

------------------------------------------------------------------------

## Part 4: Discussion and Conclusions

Throughout this project, we employed two distinct modeling approaches to predict health insurance charges based on several predictor variables such as age, BMI, smoking status, and number of children. The models used were:

1.  Simple Linear Regression Model
2.  Ridge Regression Model

Here’s a comparison and discussion of these approaches:

-   Performance Comparison: Both models were evaluated using the Mean Squared Error (MSE) metric. The Simple Linear Regression model achieved an MSE of 36,680,455.99, while the Ridge Regression model, which included regularization to manage multicollinearity and reduce overfitting, had a slightly higher MSE of 37,103,807.91. This indicates that while Ridge regression generally helps in reducing overfitting, in this specific scenario, it did not outperform the simpler model in terms of MSE on the provided data.

-   Coefficient Analysis: In Simple Linear Regression, all predictors had significant p-values, suggesting that age, BMI, smoker status, and number of children significantly affect insurance charges. The Ridge Regression, on the other hand, adjusted the coefficients, shrinking some towards zero which theoretically helps in improving the model's generalization capabilities.

-   Model Sensitivity: Ridge Regression showed less sensitivity to outliers as indicated by its regularization nature, which is beneficial in datasets with significant outliers or multicollinearity among predictor variables.

### Impact of Analysis

This analysis has multiple impacts: - Predictive Accuracy: Provides a robust foundation for predicting individual insurance charges based on demographic and health-related features, aiding in more accurate risk assessment. - Policy Formulation: Insights from the model can help insurance companies tailor their policies more effectively, adjusting premiums according to significant predictors like smoking status. - Healthcare Economics: Understanding the drivers of insurance costs can lead to more informed decisions on healthcare policies and individual health interventions.

### Main Conclusions

-   Influence of Smoking: Smoking status is the most influential predictor of health insurance charges, significantly increasing costs. This highlights the potential benefits of smoking cessation programs.
-   Age as a Predictor: There is a positive correlation between age and insurance charges, with older beneficiaries tending to incur higher charges. This aligns with general health risk increases with age.
-   Effect of BMI: Although BMI is a significant predictor, its impact on insurance charges is less pronounced than that of smoking or age. High BMI values do correlate with higher charges, but the relationship varies widely, suggesting other factors also play critical roles.
-   Model Selection: While the Simple Linear Regression model performed slightly better in terms of MSE, Ridge Regression offers advantages in handling multicollinearity and model robustness, which might be more beneficial in a broader dataset or with different variable selections.

This project demonstrates the value of using statistical models to predict health insurance costs and highlights the importance of choosing the right model based on the dataset characteristics and the specific needs of the analysis.
