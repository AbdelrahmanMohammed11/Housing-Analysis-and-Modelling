# Housing Price Prediction: An Analysis and Modeling Project

This repository contains the code and analysis for a housing price prediction project. The goal of this project is to analyze a dataset of housing features and build a predictive model to estimate the sale price of a house. This README provides a comprehensive overview of the project, including the data, analysis, modeling techniques, and results.

## Table of Contents
- [Project Overview](#project-overview)
- [Data](#data)
- [Methodology](#methodology)
- [Modeling](#modeling)
- [Results](#results)
- [Getting Started](#getting-started)
- [Dependencies](#dependencies)

## Project Overview

This project explores a housing dataset to understand the relationships between various house features and their sale prices. The analysis involves data cleaning, exploratory data analysis (EDA), feature engineering, and the development of several regression models to predict house prices. The project was implemented using the R programming language.

## Data

The dataset used in this project contains a large number of features describing various aspects of residential homes. These features include information about the property's size, location, age, and various amenities.

**Data Source:** The data was read from a CSV file named `House_data.csv`.

## Methodology

The project follows a structured approach to data analysis and modeling:

1.  **Data Import and Initial Exploration:**
    *   Libraries such as `tidyverse`, `caret`, `ggplot2`, and `DataExplorer` were used for data manipulation, visualization, and modeling.
    *   An initial assessment of the data was performed to understand its structure, including the number of rows, columns, and data types.

2.  **Missing Data Analysis and Imputation:**
    *   A significant amount of missing data was identified in several features.
    *   Features with a high percentage of missing values (over 40%) were removed from the dataset.
    *   For the remaining features with missing values, K-Nearest Neighbors (KNN) imputation was used to fill in the missing data points.

3.  **Dimensionality Reduction with PCA:**
    *   Principal Component Analysis (PCA) was employed to reduce the dimensionality of the dataset.
    *   The numeric features were scaled before applying PCA to ensure that variables with larger scales did not dominate the analysis.
    *   The first 26 principal components, which accounted for 95% of the data's variation, were selected for modeling.

4.  **Feature Selection with Random Forest:**
    *   A Random Forest model was used to evaluate the importance of each feature in predicting the sale price.
    *   This provided insights into which features were the most influential.

## Modeling

Several regression models were developed and evaluated to predict the log-transformed sale price:

1.  **Linear Regression with PCA Components:**
    *   A linear regression model was built using the selected principal components and some of the original categorical features.
    *   The model's performance was assessed, and checks for multicollinearity were conducted.

2.  **Linear Regression with Selected Features:**
    *   A simpler linear regression model was created using only a subset of the most important features identified by the Random Forest analysis. These features included 'LotArea', 'YearBuilt', 'BedroomAbvGr', 'GarageArea', and 'OverallQual'.

3.  **Stepwise Regression:**
    *   Stepwise regression was performed on the full dataset (with imputed values) to automatically select the most significant features for the model.

4.  **Lasso and Ridge Regression:**
    *   Lasso (L1) and Ridge (L2) regression models were implemented to handle potential multicollinearity and for feature selection.

5.  **Random Forest Regressor:**
    *   A Random Forest regression model was trained on the data.
    *   Hyperparameter tuning was performed on the number of trees (`ntree`) to find the optimal value that maximized the R-squared and minimized the Mean Squared Error. The best model was achieved with 260 trees.

## Results

The performance of the various models was evaluated based on their R-squared and Mean Squared Error. The Random Forest Regressor, after hyperparameter tuning, was identified as the best-performing model for this dataset.

## Getting Started

To get a local copy up and running, follow these simple steps.

### Prerequisites

You will need to have R and RStudio installed on your machine.

### Installation

1.  Clone the repo
    ```sh
    git clone https://github.com/AbdelrahmanMohammed11/Housing-Analysis-and-Modelling.git
    ```
2.  Open the `housing_Report_With_modelling.Rmd` file in RStudio.
3.  Install the required packages listed in the "Dependencies" section.
4.  Run the code chunks in the R Markdown file to reproduce the analysis and modeling.

## Dependencies

The following R packages are required to run this project:

*   tidyverse
*   caret
*   ggplot2
*   corrplot
*   broom
*   lmtest
*   zoo
*   DataExplorer
*   misty
*   dplyr
*   finalfit
*   mice
*   readr
*   VIM
*   randomForest
*   FactoMineR
*   factoextra
*   car
*   MASS
*   glmnet
