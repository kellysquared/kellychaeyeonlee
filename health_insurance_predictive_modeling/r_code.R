

# Part 2: Exploratory Data Analysis

# import all necessary packages 
library(lmtest)
library(readr)
library(ggplot2)
library(faraway)
library(lattice)
library(caret)
library(knitr)
library(car)

df <- read.csv("insurance.csv")
head(df)
names(df)
str(df)


#From this table above, we got the basic idea of each variable. There are 1338 rows (observations) along with 7 columns (variables). There are 3 categorical variables—sex, smoker, and region—and 4 numerical variables such as age, bmi, children, and charges.Our response variable will be **charges**. Also, each data type looks valid—fitting to what they are supposed to be.


df$sex <- factor(df$sex)
df$smoker <- factor(df$smoker)
df$region <- factor(df$region)

#Turn categorical variables as factor to fit a model later on.
# check for null values 
colSums(is.na(df))
#There is no null values in this dataset.

### Plotting - individual variable 
# visaulize distributions 
# categorical 
print(table(df['sex']))
ggplot(data = df) + theme(plot.title = element_text(hjust = 0.57)) + geom_bar(mapping = aes(x = sex))



# children
print(table(df['children']))
ggplot(data = df) +
  geom_bar(mapping = aes(x = children))



# smoker 
print(table(df['smoker']))
ggplot(data = df) +
  geom_bar(mapping = aes(x = smoker))



# region
print(table(df['region']))
ggplot(data = df) +
  geom_bar(mapping = aes(x = region))



# visualize distributions for continuous variables 
print(table(df['age']))
ggplot(data = df) +
  geom_histogram(mapping = aes(x = age), binwidth = 0.5)



# bmi
ggplot(data = df) +
  geom_histogram(mapping = aes(x = bmi), binwidth = 0.5)



# charges
ggplot(data = df) +
  geom_histogram(mapping = aes(x = charges), binwidth = 0.5)


The histogram of the "bmi" is the only one looking like it has a bell-curve but left-skewed.


lapply(df, unique)


We confirmed that there's no null values by looking at the unique values from each variable. It's pretty safe to say there is no null value.


# relationship between input variables and output variable, "charges"
# Box plots for numerical variables against charges
ggplot(df, aes(x=age, y=charges)) + geom_point() + geom_smooth(method="lm") + ggtitle("Figure 1") + theme(plot.title = element_text(hjust = 0.5))
ggplot(df, aes(x=bmi, y=charges)) + geom_point() + geom_smooth(method="lm") + ggtitle("Figure 2") + theme(plot.title = element_text(hjust = 0.5))
ggplot(df, aes(x=children, y=charges)) + geom_point() + geom_smooth(method="lm")

# Box plots for categorical variables against charges
ggplot(df, aes(x=sex, y=charges)) + geom_boxplot() + ggtitle("Figure 4") + theme(plot.title = element_text(hjust = 0.5))
ggplot(df, aes(x=smoker, y=charges)) + geom_boxplot() + ggtitle("Figure 5") + theme(plot.title = element_text(hjust = 0.5))
ggplot(df, aes(x=region, y=charges)) + geom_boxplot() + ggtitle("Figure 6") + theme(plot.title = element_text(hjust = 0.5))


#From the scatter plot between age and charges, we can see that the charges increase as the ages increase, linear relationship. However, It seems like there are 3 different groups for charges which may have affected by the wealthiness.
#The graph between BMI and charges shows a positive correlation as the trend line slope upwards. There's variability in charges by BMI, especially as BMI increases, which implies that while BMI is a factor in medical bill costs, other variables may influence the cost significantly. Also, there are notable outliers in higher BMI ranges where there's a huge gap difference among some individuals at similar BMI levels. We can consider BMI could be one of the risk factors when predicting insurance cost from this graph.
#The scatterplot suggests that even though the variable "children" is numerical, it should be considered as categorical and converted using the factor function.
#For the categorical variables, we applied boxplots to understand the relationship with the response variable.
#From the graph of sex and charges, it's easy to notice that there are several outliers at high end of charges both in female and male. There's no significant differences in median of charges between males and females, but individual variability is high. Although the medians are similar, the presence of outliers in both groups indicates the factors other than sex might be more predictive of high charges.

df$children <- factor(df$children)
ggplot(df, aes(x=children, y=charges)) + geom_boxplot() + ggtitle("Figure 3") + theme(plot.title = element_text(hjust = 0.5))


#After revising the children variable, the boxplot provides better insights. The median charges among all the number of children locates at similar price. However, the range (spread between the lowest and highest charges excluding outliers) of charges increase with more children up to 2 and start decreasing from 3 to 5 number of children. There are many outliers in the 0 children group and following number of children.

### Diagnostic Tests

#Now, let's check if there are any outliers.

#In order to check if there are any outliers or other influential points that may affect our model negatively, several tests need to be done.
g = lm(charges~., data=df)
# cook distance 
c = cooks.distance(g)
halfnorm(c, 5, labs=row.names(df), ylab = 'cook', main="Figure 7")
print(length(which(c > 0.5)))
print(length(which(c > 1)))


#The Figure 7 shows the half-normal quantiles along with x-axis and the Cook's distances for the obervations in the dataset with y-axis. Most of the data points cluster around the lower end of the y-axis, indicating small influence on the model. However, those labeled (e.g., "1301", "322", "578") are especially distant from the rest, suggesting these are influential observations having a substantial impact on the model's predictions. The numbers—1301, 322, and 578—are the index of the rows which make it easier to identify and possibly exclude them from further analysis.


influential <- which(c > 4/(nrow(df)-length(coef(g))))
print(length(influential))
# number of data points and number of predictors
n = nobs(g)
p = length(coef(g)) - 1 
lev=influence(g)$hat
lev[lev>2*p/n]



# high leverage points 
print(length(lev[lev>2*p/n]))
halfnorm(lev,5, labs=row.names(df),ylab='lev',main="Figure 8")


#There are 44 high leverage points which incidate outliers in regards to the independent variables. They can be good or bad influential points.

j <- rstudent(g)
Bonferroni = qt(0.05/(2*n), df= n-p-1)
print(Bonferroni)
sort(abs(j), decreasing = TRUE)[1:5]


#The studentized residuals were checked to see if the abnormal points (1301, 322, 578) from the Cook's distance are outliers. The output shows that those two points appear among the 5 most extreme studentized residuals. Both the points 1301 and 578 have higher residuals (respectively 5.0222 and 4.255) than -4.137, the critical value we observed using R. This leads us to conclude that we need to remove the two outliers: index 1301 and 578.


# remove outliers 
df <- df[-c(1301, 578), ]
dim(df)


After removing the two outliers, we have 1336 observations now.

### Correlated Features

Correlated Features were done to reduce the features for efficiency. The high correlated value usually have similar impact on predicted so getting rid of the features may help reducing the chance of over fitting and increasing efficiency.


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


#From this correlation matrix, we can expect the smoker to be a key insight for predicting charges with a strong positive correlation (0.787). On the other hand, variable sex and children shows very low correlation with most variables, including charges (0.058 and 0.072), which suggests that sex and children barely have influence on charges. Age and BMI shows a low to somewhat moderate positive correlation with charges (0.303 and 0.198). This may indicate that age and BMI might not be a significant factor in the predictive models. Also, region has a small value of negative correlation with charges.

#Variables with low correlation with the response variable (charges) may have less predictive power. However, we still keep them in the dataset since they might still be useful in the presence of non-linear relationships not captured by correlation.
# Shapiro-Wilk normality test on residuals
shapiro.test(residuals(g))

# Durbin-Watson test for autocorrelation
dwtest(g)


#We utilize the shapiro-wilk normality test and durbin-watson test to check whether the residuals are normally distributed which is an assumption of most linear regression models, and to check for the presence of autocorrelation in the residuals, which may be issues such as omitted variables.

#The shapiro-wilk test has p-value of 2.2e-16 which is extremely small—reject the null hypothesis that the residuals are normally distributed. This suggests that the residuals don't follow a normal distribution. The Durbin-Watson test has a statistic of 2.084 suggesting little to no autocorrelation. The p-value of 0.9391 is higher than 0.05 (commonly used significance level), indicating that there is no statistically significant autocorrelation in the residuals. Having no significant autocorrelation among the residuals is good for modeling.

# Part 3: Methodology

## 1) Model 1: Simple Linear Regression Model


library(car)  # for diagnostic tools
# Fitting the model
lm_model <- lm(charges ~ age + bmi + smoker + children, data = df)
summary(lm_model)




### Diagnostics


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


# Assuming test data is loaded and named 'df_test'
predictions_lm <- predict(lm_model, newdata = df)
mse_lm <- mean((df$charges - predictions_lm)^2)
print(paste("MSE:", mse_lm))

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


test_matrix <- model.matrix(~ ., data = df)[,-1]
missing_cols <- setdiff(colnames(data_matrix), colnames(test_matrix))
for (col in missing_cols) {
  test_matrix[[col]] <- 0  
}
test_matrix <- test_matrix[, colnames(data_matrix)]
ridge_predictions <- predict(ridge_model, s = best_lambda, newx = test_matrix)
# Predicting with Ridge model
ridge_predictions <- predict(ridge_model, s = best_lambda, newx = test_matrix)

# Calculate MSE for Ridge Regression
mse_ridge <- mean((df$charges - ridge_predictions)^2)
print(paste("Ridge Regression MSE:", mse_ridge))





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
