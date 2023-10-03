# Body-Fat
This is a machine learning model that predicts the body fat of a person.
This project uses a couple methods to determine which predictors are important. I have used correlation analysis, recursive feature elimination, feature importance with random forest, lasso regression, and principal component analysis to select the top 10 most informative predictors for predicting the body fat percent. Then, the data was cleaned to remove outliers from the data and highlights where the outliers were. It also plots the standardized data and saves it as cleaned data. This and the original data was ran through multiple linear regression, random forest, and support vector regression. There are plots for easy comparison between the models.
This project uses a couple metrics: accuracy with a tolerance, RMSE, R^2, and WMAPE. The accuracy has a tolerance level of plus or minus the tolerance. The higher it is, the better. The RMSE measures deviaton from the bodyfat. The lower it is, the better. The R^2 value tells us how good the fit of the model is. The higher it is, the better. The WMAPE measures the absolute error. The lower it is, the better.
