# Body-Fat
This is a machine learning model that predicts the body fat of a person.
This project uses a couple methods to determine which predictors are important. I have used correlation analysis, recursive feature elimination, feature importance with random forest, lasso regression, and principal component analysis to select the top 3 most informative predictors for predicting the density and select the top 3 most informative predictors to predict body fat. The same methods are used to predict both sets of predictors and predicted density will be added to the list of potential predictors. The data was cleaned to remove outliers from the data and highlights where the outliers were. It also plots the standardized data and saves it as cleaned data. Body fat with less than 2% body fat was also removed because it is impossible to survive at less than 2% body fat. The original data was ran through multi-variable linear regression, random forest, and support vector regression. There are plots for easy comparison between the models. The cleaned data was also ran through the same models.

# Metrics
This project uses a couple metrics: accuracy with a tolerance, RMSE, R^2, and WMAPE. The accuracy has a tolerance level of plus or minus the tolerance. The higher it is, the better. The RMSE measures deviaton from the bodyfat. The lower it is, the better. The R^2 value tells us how good the fit of the model is. The higher it is, the better. The WMAPE measures the absolute error. The lower it is, the better.

# RUNNING CODE
To use the Body Fat.ipynb python notebook, download the notebook and open in Jupyter Notebook and download the data found at Data/BodyFat.csv. Change the data = pd.read_csv(r'C:\Users\phili\Downloads\BodyFat.csv') lines to reflect the location where the data is stored. This line is found twice in the second code chunk. The entire notebook can be ran by clicking Kernel/Restart & Run All located at the top console. The python notebook was completed by Philip.

To run the shiny app, download Images/Abs.png, Images/Chest.png, Images/Wrist.png, Data/cleaned_data.csv, and Code/combined_shiny_app.R. Open the shiny code in R studio and make sure the proper libraries are installed. Click Run App located in the top right corner. You can now enter inputs for the predictors and when the calculate button is pressed, there will be scatter plots for the predicted density and predicted body fat. There is also another tab for the outliers. This is the link to the app:  https://jinwen626.shinyapps.io/shiny/
The front page with images, scatterplot, and table were coded by Philip. The z-score outlier was plotted by Jinwen. 

To see the combined scatter plot of the predicted density and predicted body fat, download Images/Abs.png, Images/Chest.png, Images/Wrist.png, Data/cleaned_data.csv, and Code/Combined Scatter.R. Open the shiny code in R studio and make sure the proper libraries are installed. The same functions as the combined_shiny_app.R app but without the outliers plot and the original scatter is now replaced with the combined scatter plot.

# IMAGES
The images for the shiny app have all been generated using bdgramR. The code used is found at Code/Body Diagram.R. All that is needed is installation of bdgramR and to click run in the top right corner. 
The other images have been saved from the python notebook. The title of the images serve as a description of what the image is. For example, Body Fat Accuracy.png is an image of the accuracy for all three regression models used. 
Some images in the powerpoint are screenshots of the scatter plots generated in the shiny app.

# Data cleaning
The outliers were detected using Z-score, DFBETA, DBSCAN, and Cook's Distance. Some were also removed because of too high or too low body fat. The outliers are IDNO 39,41, 182, 172, and 216. This can be found in Data/BodyFat.csv and Code/Outlier detection_density_clustering.ipynb. The latter is coded by Jinwen and the former by Philip.
