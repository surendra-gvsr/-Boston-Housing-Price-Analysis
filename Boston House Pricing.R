

# Install packages and load libraries
library(stargazer)
library(readr)
library(dplyr)

#Question- 1

#Loading data set 
A2 <- read.table("housing.csv", sep = "", header = FALSE)
#Assigning column names
colnames(A2) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

#checking data
View(A2)  
head(A2)  
dim(A2)

#Question -2
# Loadong necessary libraries
library(ggplot2)

# 1. Reporting summary statistics for the dependent variable 'MEDV'
summary_stats <- summary(A2$MEDV)
print(summary_stats)

# 2. Plotting a histogram and a QQ plot of the dependent variable 'MEDV'
# Histogram with density line
hist(A2$MEDV, breaks = 15, prob = TRUE, main = "Histogram of MEDV with Density Curve",
     xlab = "MEDV (Median Value of Homes in $1000's)", col = "skyblue", border = "black")
lines(density(A2$MEDV), col = "darkblue", lwd = 2)

# QQ plot
qqnorm(A2$MEDV, main = "QQ Plot of MEDV")
qqline(A2$MEDV, col = "red")


# 3.Plotting histogram of log-transformed MEDV with density line
hist(log(A2$MEDV), breaks = 15, prob = TRUE, main = "Histogram of Log-Transformed MEDV with Density Curve",
     xlab = "Log(MEDV)", col = "lightgreen", border = "black")
lines(density(log(A2$MEDV)), col = "darkgreen", lwd = 2)


#Question-3

# 1.Tabulating CHAS (dummy variable, 1 if tract bounds river; 0 otherwise)
chas_tabulation <- table(A2$CHAS)
print(chas_tabulation)


# 2.Calculating proportions to see if there is sufficient variation
chas_proportions <- prop.table(chas_tabulation)
print(chas_proportions)

# 3. Tabulating RAD (index of accessibility to radial highways)
rad_tabulation <- table(A2$RAD)
print(rad_tabulation)

# 4. Calculating proportions to see if there is sufficient variation
rad_proportions <- prop.table(rad_tabulation)
print(rad_proportions)


#Question -4

# Estimating a simple linear regression model for MEDV using each variable in the dataset

# Defining dependent variable
dependent_var <- A2$MEDV

# Creating a list of independent variables excluding MEDV
independent_vars <- names(A2)[names(A2) != "MEDV"]

# Fitting linear regression models for each explanatory variable
models <- list()

for (var in independent_vars) {
  formula <- as.formula(paste("MEDV ~", var))
  model <- lm(formula, data = A2)
  models[[var]] <- model
}

# Use stargazer to report the results
stargazer(models, type = "text", title = "Simple Linear Regression Results for MEDV")


#Question - 5

# Fitting a multiple linear regression model using significant explanatory variables
significant_vars <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")

# Construct the formula for multiple regression using significant variables
formula <- as.formula(paste("MEDV ~", paste(significant_vars, collapse = " + ")))

# Fit the multiple linear regression model
multiple_model <- lm(formula, data = A2)

# Use stargazer to report the regression results
stargazer(multiple_model, type = "text", title = "Multiple Linear Regression Results for MEDV", digits = 2)
stargazer(multiple_model, type = "html", out = "multiple_model_results.doc",
          title = "Multiple Linear Regression Model Results",
          digits = 3)

hist(residuals(multiple_model), main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue")
qqnorm(residuals(multiple_model), main = "QQ Plot of Residuals")
qqline(residuals(multiple_model), col = "red")

plot(multiple_model$fitted.values, residuals(multiple_model), 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", 
     ylab = "Residuals", 
     col = "blue", pch = 20)
abline(h = 0, col = "red")

#Question - 6

# Selecting the explanatory variables used in the multiple regression model
explanatory_vars <- A2[, c("CRIM", "ZN", "CHAS", "NOX", "RM", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")]

# Calculating the correlation matrix for these variables
correlation_matrix <- cor(explanatory_vars, use = "complete.obs")

# Displaying the correlation matrix
print(correlation_matrix)

correlation_matrix <- as.matrix(correlation_matrix)


# loading corrplot package

#install.packages("corrplot")

library(corrplot)

# Create a heatmap using corrplot
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black", col = colorRampPalette(c("red", "white", "blue"))(200))




#Question - 7
library(car)
# From the model summary, the significant variables are CRIM, ZN, CHAS, NOX, RM, DIS, RAD, TAX, PTRATIO, B, LSTAT
significant_vars <- c("CRIM", "ZN", "CHAS", "NOX", "RM", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")

# Construct the formula for multiple regression using significant variables only
significant_formula <- as.formula(paste("MEDV ~", paste(significant_vars, collapse = " + ")))

# Fit the multiple linear regression model with only significant variables
significant_model <- lm(significant_formula, data = A2)

# Calculate VIF values for the model with significant variables only
vif_values_significant <- vif(significant_model)

# Print the VIF values for significant variables
print(vif_values_significant)



#Barplot for variables
barplot(vif_values_significant, 
        main = "Variance Inflation Factor (VIF) for Explanatory Variables",
        xlab = "Explanatory Variables",
        ylab = "VIF Value",
        col = "lightblue",
        las = 2) 
# Adding a horizontal reference line at VIF = 5
abline(h = 5, col = "red", lwd = 2, lty = 2) 



#Question - 8

#modal-1

# Fitting the revised multiple linear regression model excluding RAD and NOX
revised_model1 <- lm(MEDV ~ CRIM + ZN + CHAS + DIS + RM + PTRATIO + B + LSTAT + TAX , data = A2)


# Report the revised model results using stargazer
stargazer(revised_model1, type = "text", title = "Revised Multiple Linear Regression Model Results", digits = 3)



# Calculate VIF values for the new revised model
vif_values <- vif(revised_model1)

# Print the VIF values
print(vif_values)



#modal-2

# Create the necessary variables first
A2$log_LSTAT <- log(A2$LSTAT + 1)  # Log transform LSTAT to reduce skewness
A2$c_RM <- scale(A2$RM, center = TRUE, scale = FALSE)  # Center RM
A2$c_RM_LSTAT <- A2$c_RM * A2$log_LSTAT  # Interaction between centered RM and log-transformed LSTAT
A2$DIS_squared <- A2$DIS^2  # Squared term for DIS

# Fit the revised model with the specified variables
revised_model <- lm(MEDV ~ ZN + CHAS + PTRATIO + B + log_LSTAT + c_RM_LSTAT + DIS_squared, data = A2)

# Report the model using stargazer
library(stargazer)
stargazer(revised_model, type = "text", title = "Revised Multiple Linear Regression Model", digits = 3)

# Calculate VIF values for the new model
vif_values <- vif(revised_model)

# Print the VIF values for the new model
print(vif_values)


#Final Modal after multiple iterations

# Create the necessary variables
A2$log_CRIM <- log(A2$CRIM + 1)  # Log transform CRIM to reduce skewness
A2$c_RM <- scale(A2$RM, center = TRUE, scale = FALSE)  # Center RM
A2$c_RM_LSTAT <- A2$c_RM * A2$log_LSTAT  # Interaction between centered RM and log-transformed LSTAT
A2$c_RM_squared <- A2$c_RM^2  # Squared term for centered RM
A2$log_CRIM_sq <- A2$log_CRIM^2  # Squared term for log-transformed CRIM
A2$log_CRIM_LSTAT <- A2$log_CRIM * A2$log_LSTAT  # Interaction between log-transformed CRIM and log-transformed LSTAT




# Fit the revised model with the specified variables
revised_model_v9 <- lm(MEDV ~ ZN + CHAS + DIS + PTRATIO + B + log_LSTAT + TAX + c_RM_LSTAT + c_RM_squared + log_CRIM_sq, data = A2)

# Report the model using stargazer
library(stargazer)
stargazer(revised_model_v9, type = "text", title = "Revised Multiple Linear Regression Model with Specified Variables", digits = 3)

# Calculate VIF values for the new model
vif_values_v9 <- vif(revised_model_v9)

# Print the VIF values for the new model
print(vif_values_v9)




# Define the scenario characteristics
scenario1 <- data.frame(
  ZN = 25,
  CHAS = 0,
  DIS = 5,
  PTRATIO = 16,
  B = 390,
  log_LSTAT = 2.5,
  TAX = 300,
  RM = 6,
  CRIM = 0.1
)

# Apply the same transformations as in your model
scenario1$log_CRIM <- log(scenario1$CRIM + 1)
scenario1$c_RM <- scale(scenario1$RM, center = TRUE, scale = FALSE)
scenario1$c_RM_LSTAT <- scenario1$c_RM * scenario1$log_LSTAT
scenario1$c_RM_squared <- scenario1$c_RM^2
scenario1$log_CRIM_sq <- scenario1$log_CRIM^2
scenario1$log_CRIM_LSTAT <- scenario1$log_CRIM * scenario1$log_LSTAT

# Predict the home value
predicted_MEDV1 <- predict(revised_model_v9, newdata = scenario1)

cat("Scenario 1: A spacious home with 6 rooms in a low-crime neighborhood.\n")
cat("Predicted MEDV:", round(predicted_MEDV1, 2), "thousands of dollars.\n\n")


# Define the scenario characteristics
scenario2 <- data.frame(
  ZN = 0,
  CHAS = 1,
  DIS = 2,
  PTRATIO = 20,
  B = 500,
  log_LSTAT = 3.0,
  TAX = 400,
  RM = 4,
  CRIM = 5.0
)

# Apply the same transformations
scenario2$log_CRIM <- log(scenario2$CRIM + 1)
scenario2$c_RM <- scale(scenario2$RM, center = TRUE, scale = FALSE)
scenario2$c_RM_LSTAT <- scenario2$c_RM * scenario2$log_LSTAT
scenario2$c_RM_squared <- scenario2$c_RM^2
scenario2$log_CRIM_sq <- scenario2$log_CRIM^2
scenario2$log_CRIM_LSTAT <- scenario2$log_CRIM * scenario2$log_LSTAT

# Predict the home value
predicted_MEDV2 <- predict(revised_model_v9, newdata = scenario2)

cat("Scenario 2: A compact home with 4 rooms in a high-crime area near the river.\n")
cat("Predicted MEDV:", round(predicted_MEDV2, 2), "thousands of dollars.\n\n")




# Define the scenario characteristics
scenario3 <- data.frame(
  ZN = 12.5,
  CHAS = 0,
  DIS = 4,
  PTRATIO = 18,
  B = 450,
  log_LSTAT = 2.8,
  TAX = 350,
  RM = 5,
  CRIM = 2.0
)

# Apply the same transformations
scenario3$log_CRIM <- log(scenario3$CRIM + 1)
scenario3$c_RM <- scale(scenario3$RM, center = TRUE, scale = FALSE)
scenario3$c_RM_LSTAT <- scenario3$c_RM * scenario3$log_LSTAT
scenario3$c_RM_squared <- scenario3$c_RM^2
scenario3$log_CRIM_sq <- scenario3$log_CRIM^2
scenario3$log_CRIM_LSTAT <- scenario3$log_CRIM * scenario3$log_LSTAT

# Predict the home value
predicted_MEDV3 <- predict(revised_model_v9, newdata = scenario3)

cat("Scenario 3: A mid-sized home with 5 rooms in a medium-crime suburban area.\n")
cat("Predicted MEDV:", round(predicted_MEDV3, 2), "thousands of dollars.\n")





