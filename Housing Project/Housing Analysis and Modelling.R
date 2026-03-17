---
title: "Housing Project"
author: "Abdelrahman Mohamed"
output: html_document
date: "2024-05-21"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

### Importing libraries

```{r, warning=FALSE}
# importing Libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(broom)
library(lmtest)
library(DataExplorer)
library(misty)
library(dplyr)
library(finalfit)
library(mice)
library(readr)
library(VIM)
library(randomForest)
library(FactoMineR)
library(factoextra)
```

### reading the data

```{r}
housing_data <- read.csv("C:\\Users\\abdelrahman\\Downloads\\House_data.csv",
                         na.strings=c("","NA"))
```
### Lets check some info about the data
the data contain huge number of feature...
to make some info about the feature we will cutting it into 3 parts

```{r, echo=TRUE}
plot_str(housing_data[,0:30])

```

```{r, warning=FALSE,fig.width = 8,fig.height = 12}
plot_str(housing_data[,30:60])

```

```{r, warning=FALSE,fig.width = 8,fig.height = 12}
plot_str(housing_data[,60:82])
```
#### check some info about our data
```{r, warning=FALSE}
# lets introduce our data
transpose(view(introduce(housing_data)))
plot_intro(housing_data)
# we have about 50 of columns discrete and 50 continuous
# 6.6% missing Values Observation
```
### Missing Data
#### here we see that we have missing obs. lets check for each feature
```{r, warning=FALSE}

missing_percentage <- function(data) {
  colSums(is.na(data)) / nrow(data) * 100
}

missing_perc <- missing_percentage(housing_data)
missing_perc
```
###### plot the % of Missing in Each Column and Location Of Missing
##### % of missing in each column
```{r, warning=FALSE}
plot_missing(housing_data[,0:25])
plot_missing(housing_data[,25:50])
plot_missing(housing_data[,50:82])

```


##### Location of the Missing
```{r, warning=FALSE}
housing_data[,3:38]%>%
  missing_plot()
```


```{r, warning=FALSE}
housing_data[,58:77]%>%
  missing_plot()
```



```{r, warning=FALSE}
#pattern of the missing
aggr_plot <- aggr(housing_data, col=c('navyblue','red'),
                  numbers=TRUE,
                  sortVars=TRUE, labels=names(housing_data), cex.axis=.7, gap=3,
                  ylab=c("Histogram of missing data","Pattern"))

```
we will remove the features that have missing more than 40%

```{r, warning=FALSE}
data_after_removing_high_missing<- subset(housing_data, select = -c(X,
                                                                    Id,
                                                                    Alley,
                                                                    MasVnrType,
                                                                    FireplaceQu,
                                                                    PoolQC,
                                                                    Fence,
                                                                   MiscFeature))
print(dim(data_after_removing_high_missing))
print(dim(housing_data))
```

```{r, warning=FALSE}
# sorting the varibles accourding the number of missing
NAs <- data_after_removing_high_missing %>%
  is.na() %>%
  colSums() %>%
  sort(decreasing = FALSE) %>%
  names()

```


```{r, warning=FALSE}

# Filter columns with missing values
columns_with_na <- NAs[colSums(is.na(data_after_removing_high_missing[NAs])) > 0]

is_numeric_column <- function(df, col) {
  is.numeric(df[[col]])
}

```

```{r, warning=FALSE}
# check if the variable is numeric 
is_numeric_column <- function(df, col) {
  is.numeric(df[[col]])
}
```


```{r, warning=FALSE}

# impute the data with KNN imputation
Knnimputaion <- data_after_removing_high_missing %>%
  select(columns_with_na) %>%
  kNN(k = 5)
```


```{r, warning=FALSE}

# Combine the imputed data with the rest of the dataset
imputed_data <- data_after_removing_high_missing
imputed_data[columns_with_na] <- Knnimputaion[columns_with_na]


combined_data <- data_after_removing_high_missing %>%
  mutate(source = "before") %>%
  bind_rows(imputed_data %>%
              mutate(source = "after"))


plot_combined_distribution <- function(data, cols) {
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      p <- ggplot(data, aes(x = !!sym(col), fill = source)) +
        geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +
        theme_minimal() +
        labs(title = paste("Distribution of", col, "before and after imputation"),
             fill = "Source")
    } else {
      p <- ggplot(data, aes(x = !!sym(col), fill = source)) +
        geom_bar(position = "dodge", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Distribution of", col, "before and after imputation"),
             fill = "Source")
    }
    print(p)
  }
}

# Plot combined distributions of columns with missing values
plot_combined_distribution(combined_data, columns_with_na)

```

The data has huge number of columns so We need to make dimensionality reduction
## dimensionality reduction
### PCA
##### select the Features with out the target
## select the Features with out the target because This is done by only considering the features (independent variables) and not the target variable (dependent variable). The reason for not including the target variable in PCA is that PCA is an unsupervised dimensionality reduction technique, meaning it focuses solely on the variation within the features of the dataset without taking the target variable into account.

```{r, warning=FALSE}
X <- select(imputed_data, -c('SalePrice'))
y<- select(imputed_data, SalePrice)
dim(X)
dim(y)
```


##### Select the Numeric data for PCA

```{r, warning=FALSE}
numeric_X_for_pca <- select_if(X, is.numeric)
dim(numeric_X_for_pca)



categorical_X_pca<- X %>%
  select(-names(numeric_X_for_pca))

dim(categorical_X_pca)

```


```{r, warning=FALSE}
plot_bar(categorical_X_pca)
```

```{r, warning=FALSE}
categorical_X_pca<-select(categorical_X_pca,c('BsmtFinType1', 'GarageFinish', 'KitchenQual' , 'Neighborhood'))

```

##### Scaling the data for PCA
##scales the numeric features in the numeric_X_for_pca data frame.
#It ensures that each feature has a mean of 0 and a standard deviation of 1.
#Without scaling, features with larger scales (e.g., income in thousands vs. age in years) can dominate the principal components.
```{r, warning=FALSE}
scaled_data_for_pca <- scale(numeric_X_for_pca)
view(scaled_data_for_pca)
```
#### PCA
```{r, warning=FALSE}
data_pca <- princomp(scaled_data_for_pca)
#princomp(scaled_data_for_pca): This line computes the principal components of the scaled_data_for_pca matrix.
#s assumed to be a numeric matrix where each row represents an observation and each column represents afeature
summary(data_pca)
```



##### Visualization Of PCA

```{r, warning=FALSE, fig.height= 8, fig.width=13}
fviz_eig(data_pca, addlabels = TRUE)

```

```{r, warning=FALSE}
# Visualize PCA
pca_plot <- fviz_pca_ind(data_pca,
                         geom.ind = "point",
                         col.ind = "cos2", # Color by the quality of representation
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE)    # Avoid text overlapping
print(pca_plot)

```
```{r, warning=FALSE}
# Visualize PCA
fviz_pca_var(data_pca, col.var = "cos2",
            gradient.cols = c("black", "orange", "green"),
            repel = TRUE)

```



we will select the first 26 componant which covers 95% of the variation of the data
##### selecting the componants


```{r, warning=FALSE}
final_pca_data <- view(data.frame(data_pca$scores[,1:26]))
colnames(final_pca_data) <- paste0("PC", 1:26)
view(final_pca_data)

```

##### compaining the pca data and the categorical data
```{r, warning=FALSE}
full_pca_X <- cbind(final_pca_data, categorical_X_pca)
dim(full_pca_X)

full_pca_X_incoded <- full_pca_X %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  model.matrix(~ . - 1, data = .) %>%
  as.data.frame()
view(full_pca_X_incoded)
```
##### Modeling for PCA


```{r, warning=FALSE}

New_data_with_pca <- cbind(full_pca_X, y) 

```


##### ploting the dist of our target
```{r, warning=FALSE}
ggplot(imputed_data, aes(x=SalePrice)) + geom_density(fill="#008080",
                                         color="#008080",
                                         alpha=0.8)
```
our target is skewed right 

```{r, warning=FALSE}
model <- lm(SalePrice ~ ., data = New_data_with_pca)
summary(model)

coef(model)

summary(model)
```
#### Check Auto Correlection
```{r, warning=FALSE}
dw_result <- dwtest(model)
print(dw_result)
```


```{r, warning=FALSE}
plot(lm(SalePrice ~ ., data = New_data_with_pca))
```
##### may be we need to take log y

```{r, warning=FALSE}
par(mfrow = c(2, 2))
plot(model)
```

check multicollinearity

```{r, warning=FALSE}
library(car)
VIF <- vif(model)
print(VIF)
```
##### Remove Neighborhood and take log y

```{r, warning=FALSE}
ggplot(imputed_data, aes(x=log(SalePrice))) + geom_density(fill="#008080",
                                         color="#008080",
                                         alpha=0.8)
```
its looks better after taking log y
```{r, warning=FALSE}
New_data_with_pca<- subset(New_data_with_pca, select = -c(Neighborhood))

```

```{r, warning=FALSE}
model2 <- lm(log(SalePrice) ~ ., data = New_data_with_pca)
summary(model)

coef(model2)

summary(model2)
```
#### Check Auto Correlection
```{r, warning=FALSE}
dw_result <- dwtest(model2)
print(dw_result)
```


```{r, warning=FALSE}
plot(lm(log(SalePrice) ~ ., data = New_data_with_pca))
```
##### may be we need to take log y

```{r, warning=FALSE}
par(mfrow = c(2, 2))
plot(model2)
```

check multicollinearity

```{r, warning=FALSE}
library(car)
VIF <- vif(model2)
print(VIF)
```
the model not Satsified lets make feature selection using Random Forest

### Feature Selection
#### Random Forest

```{r, warning=FALSE, ,fig.width = 12,fig.height = 18}
# Create Random Forest model
set.seed(123)  # For reproducibility
rf_model <- randomForest(x = X, ntree = 100, importance = TRUE)

# Get variable importance
importance <- importance(rf_model, type = 1)
var_importance <- data.frame(Variables = rownames(importance), Importance = importance[, 1])

# Visualize variable importance
var_importance_plot <- ggplot(var_importance, aes(x = reorder(Variables, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance from Random Forest",
       x = "Variables",
       y = "Importance")
print(var_importance_plot)


```








### Modelling

```{r, warning=FALSE}
# Select 7 features
selected_features <- c('LotArea', 'YearBuilt', 'BedroomAbvGr', 'GarageArea', 'OverallQual', 'YearRemodAdd')
X <- X[, selected_features]
y <- log(imputed_data$SalePrice)

# Split the data into training and test sets
set.seed(42)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]

# Combine X_train and y_train into a single data frame
train_data <- data.frame(X_train, SalePrice = y_train)

# Fit the linear regression model
model <- lm(SalePrice ~ ., data = train_data)

# Combine X_test and y_test into a single data frame for prediction
test_data <- data.frame(X_test)

# Evaluate the model
y_pred <- predict(model, newdata = test_data)
r2 <- R2(y_test, y_pred)
mse <- mean((y_test - y_pred)^2)
cat(sprintf('R-squared: %.2f\n', r2))
cat(sprintf('Mean Squared Error: %.2f\n', mse))

```




```{r}
# Check for normality of residuals
shapiro.test(resid(model))


# Check for independence of residuals
durbinWatsonTest(model)

# Check for homoscedasticity

bptest(model)

# Check for multicollinearity
vif(model)

```


```{r}
libraries(MASS)
# Fit the full linear regression model
full_model <- lm(SalePrice ~ ., data = imputed_data)

# Perform stepwise regression
step_model <- stepAIC(full_model, direction = "both")

# Summary of the final model
summary(step_model)

```
```{r}
# Check for normality of residuals
shapiro.test(resid(step_model))


# Check for independence of residuals
durbinWatsonTest(step_model)

# Check for homoscedasticity

bptest(step_model)

# Check for multicollinearity
vif(step_model)

```
#### Step-wise Regression with log y
```{r}
# Fit the full linear regression model
full_model_log_model <- lm(log(SalePrice) ~ ., data = imputed_data)

# Perform stepwise regression
step_model_log_model <- stepAIC(full_model_log_model, direction = "both")

# Summary of the final model
summary(step_model_log_model)

```

#### check assumptions for Step-wise Regression with log y

```{r}
# Check for normality of residuals
shapiro.test(resid(step_model_log_model))


# Check for independence of residuals
durbinWatsonTest(step_model_log_model)

# Check for homoscedasticity

bptest(step_model_log_model)

# Check for multicollinearity
vif(step_model_log_model)

```


#### Lasso Regression
```{r}
library(glmnet)
# Set a seed for reproducibility
set.seed(42)
X <- as.matrix(X)
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 10)
# Plot cross-validation results
plot(lasso_cv)
```

#### Ridge Regression
```{r}

# Set a seed for reproducibility
set.seed(42)
X <- as.matrix(X)
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)

ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambdas_to_try,
                      standardize = TRUE, nfolds = 5)
# Plot cross-validation results
plot(ridge_cv)
```
#### Random forest Regressor
```{r}

set.seed(123)  # For reproducibility
n <- 100

# Train the Random Forest model
rf_model <- randomForest(x = X, y = y, ntree = n)

# Print the summary of the model
print(rf_model)

# Make predictions
predictions <- predict(rf_model, X)

# Evaluate the model (for example, using mean squared error)
mse <- mean((predictions - y)^2)
print(paste("Mean Squared Error:", mse))


# Calculate mean of actual response values
mean_actual <- mean(y)
# Calculate total sum of squares
total_ss <- sum((y - mean_actual)^2)
# Calculate residual sum of squares
residual_ss <- sum((y - predictions)^2)
# Calculate R-squared
r_squared_manual <- 1 - (residual_ss / total_ss)
print(paste("R-squared (Manual):", r_squared_manual))
```


#### HyperTuning the N to achive the best model
```{r}
# Range of n values to try
n_values <- seq(10, 300, by = 10)

# Initialize vectors to store R-squared and MSE values
rsquared_values <- numeric(length(n_values))
mse_values <- numeric(length(n_values))

# Loop through each value of n
for (i in seq_along(n_values)) {
  # Train the Random Forest model
  rf_model <- randomForest(x = X, y = y, ntree = n_values[i])
  
  # Make predictions
  predictions <- predict(rf_model, X)
  
  
   # Calculate R-squared
  mean_y <- mean(y)
  total_ss <- sum((y - mean_y)^2)
  residual_ss <- sum((y - predictions)^2)
  rsquared_values[i] <- 1 - (residual_ss / total_ss)
  
  # Calculate MSE
  mse_values[i] <- mean((predictions - y)^2)
}

# Plot both R-squared and MSE values against n
par(mfrow=c(1,2))  # Set up a 1x2 plotting grid
plot(n_values, rsquared_values, type = "l", 
     xlab = "Number of Trees (n)", ylab = "R-squared",
     main = "R-squared vs. Number of Trees")
plot(n_values, mse_values, type = "l", 
     xlab = "Number of Trees (n)", ylab = "Mean Squared Error",
     main = "MSE vs. Number of Trees")

```


### Best Model
```{r}
set.seed(123)  # For reproducibility
n <- 260

# Train the Random Forest model
rf_model <- randomForest(x = X, y = y, ntree = n)

# Print the summary of the model
print(rf_model)

# Make predictions
predictions <- predict(rf_model, X)

# Evaluate the model (for example, using mean squared error)
mse <- mean((predictions - y)^2)
print(paste("Mean Squared Error:", mse))


# Calculate mean of actual response values
mean_actual <- mean(y)
# Calculate total sum of squares
total_ss <- sum((y - mean_actual)^2)
# Calculate residual sum of squares
residual_ss <- sum((y - predictions)^2)
# Calculate R-squared
r_squared_manual <- 1 - (residual_ss / total_ss)
print(paste("R-squared (Manual):", r_squared_manual))

```