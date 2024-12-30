# Boston-Housing-Price-Analysis


1. EXECUTIVE SUMMARY
This analysis investigates housing prices in Boston using data from 506 properties to understand the relationship between median home values and various neighborhood characteristics. Our analysis reveals that room count (RM) and lower status population percentage (LSTAT) are the strongest individual predictors, with RM explaining 48.4% and LSTAT explaining 54.4% of price variation respectively. The final optimized model, incorporating transformed variables and interaction terms, explains 79.8% of price variation. Notably, properties near Charles River (6.92% of sample) show significantly higher values, with an average increase of $2,690. Based on these findings, we recommend considering both property characteristics and neighborhood demographics for accurate price predictions.

2. OBJECTIVE AND QUESTIONS

2.1. Project Purpose
2.1.1. Objective of the Project
To identify the key determinants of housing prices in Boston and develop an optimal model for predicting median home values.

2.1.2. Questions
- What is the relationship between neighborhood characteristics and housing prices?
- How do structural characteristics like room count affect property values?
- Which variables show significant multicollinearity?
- How can model performance be improved through variable transformations and interactions?

2.1.3. Data
The analysis utilized data from 506 Boston neighborhoods with 14 key variables:
- CRIM (per capita crime rate)
- ZN (residential land zoning)
- INDUS (non-retail business acres)
- CHAS (Charles River dummy variable)
- NOX (nitric oxide concentration)
- RM (average rooms per dwelling)
- AGE (proportion of old units)
- DIS (weighted distances to employment centers)
- RAD (highway accessibility index)
- TAX (property tax rate)
- PTRATIO (pupil-teacher ratio)
- B (Black population proportion)
- LSTAT (lower status population percentage)
- MEDV (Median value of homes in $1000s)

3. DATA ANALYSIS AND RESULTS

3.1. Initial Variable Analysis

Distribution Analysis
- MEDV shows right-skewed distribution (Mean: $22,530, Median: $21,200)
- Log transformation of MEDV produces more normal distribution
- CHAS variable: 93.08% (471) non-river properties, 6.92% (35) river properties
- RAD variable shows concentration in categories 4, 5, and 24

3.2. Regression Analysis Results

Simple Linear Regression Findings:
- LSTAT: Strongest negative relationship (-0.950, R² = 0.544)
- RM: Strong positive relationship (9.102, R² = 0.484)
- NOX: Strong negative relationship (-33.916, R² = 0.427)
- CHAS: Positive effect (6.346, R² = 0.059)

Multiple Regression Model Results:
Initial model R² = 0.74
Final optimized model:
- Adjusted R² = 0.798
- Includes transformed variables (log_LSTAT, log_CRIM_sq)
- Incorporates interaction terms (c_RM_LSTAT)
- All VIF values below threshold of 5

4. RECOMMENDATIONS AND CONCLUSION

Based on our comprehensive analysis, we recommend:

1. Model Application Guidelines
   - Use transformed variables for improved predictions
   - Consider interaction effects between room count and neighborhood status
   - Account for proximity to Charles River in valuations

2. Variable Selection Strategy
   - Focus on key predictors (RM, LSTAT, NOX)
   - Consider removing highly correlated variables (RAD/TAX)
   - Include transformed variables for non-linear relationships

3. Price Prediction Framework
   - Implement log transformations for skewed variables
   - Account for interaction effects
   - Consider neighborhood characteristics comprehensively

4. Future Improvements
   - Collect additional property characteristics
   - Monitor temporal changes in relationships
   - Develop separate models for different property types

The analysis demonstrates that Boston housing prices are influenced by multiple factors, with room count and neighborhood status playing crucial roles. The final model's high explanatory power (79.8%) provides a reliable foundation for price prediction, while the identified interaction effects suggest complex relationships between variables that should be considered in valuation processes.
