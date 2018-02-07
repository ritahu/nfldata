# Python 3.6
import pandas as pd
import numpy as np
import math

# Load Data
print("Loading Data...")
NFL_Data = pd.read_csv("/Users/user/Desktop/Classes/Fall 2016/STAT 425/Projects/Final Submit/NFL_Data.csv")
del NFL_Data['Unnamed: 0']

# Split Train Data and Test Data
# Use Data in Season 2015-2016 as Test Data
Train_Data = NFL_Data.loc[NFL_Data['Season']!=2015]
Test_Data = NFL_Data.loc[NFL_Data['Season']==2015]
print('The number of observations in Training Data: %d' % len(Train_Data))
print('The number of observations in Test Data: %d' % len(Test_Data))
# pct: winning percentage => [0,1]
# Transformation (0,1) to (-Inf, Inf) for regression analysis
# Logit transform： log(x/(1-x))
# Replace 1 with 0.999 and 0 with 0.001 to avoid the situation that (1-x) is zero or x/(1-x) is zero
Train_Data['pct'].replace(1,0.999,inplace=True)
Train_Data['pct'].replace(0,0.001,inplace=True)
Test_Data['pct'].replace(1,0.999,inplace=True)
Test_Data['pct'].replace(0,0.001,inplace=True)

Train_y = Train_Data['pct'].apply(lambda x: math.log(x/(1-x)))
Test_y = Test_Data['pct'].apply(lambda x: math.log(x/(1-x)))

col_sel = Train_Data.columns.values.tolist()[2:4] + Train_Data.columns.values.tolist()[5:27]
Train_X = Train_Data[col_sel]
Test_X = Test_Data[col_sel]

# Linear Regression Model
# OLS as Benchmark
print("Starting Linear Regression Model...")
from sklearn.linear_model import LinearRegression
regression_model = LinearRegression()
regression_model.fit(Train_X, Train_y)

Pred_y = regression_model.predict(Test_X)

# Output result
Team_pred = pd.DataFrame()
Team_pred['Team'] = Test_Data['Team']
Team_pred['Pred_y_OLS'] = pd.Series(Pred_y).apply(lambda x: math.exp(x)/(math.exp(x)+1)).tolist()
Team_pred = Team_pred.sort_values(by='Pred_y_OLS', ascending=False)
Team_pred['rank'] = range(1,33)
print(Team_pred)

# Random Forest
print("Starting Random Forest Model...")
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import GridSearchCV
# Cross validation set-up
rf_cv = RandomForestRegressor(random_state=0)
trees = [1,5,10,50,100,150,200,300,400,500,800,1000]
tuned_parameters = [{'n_estimators': trees}]
n_folds = 10
print("Staring Cross Validation...")
clf = GridSearchCV(rf_cv, tuned_parameters, cv=n_folds, refit=False)
clf.fit(Train_X, Train_y)
clf.cv_results_['mean_test_score']

rf = RandomForestRegressor(n_estimators=200, oob_score=True, random_state=0)
rf.fit(Train_X, Train_y)
# Variable Importance
rf.feature_importances_

# Prediction
Pred_y_rf = rf.predict(Test_X)
Team_pred_rf = pd.DataFrame()
Team_pred_rf['Team'] = Test_Data['Team']
Team_pred_rf['Pred_y_rf'] = pd.Series(Pred_y_rf).apply(lambda x: math.exp(x)/(math.exp(x)+1)).tolist()
Team_pred_rf = Team_pred_rf.sort_values(by='Pred_y_rf', ascending=False)
Team_pred_rf['rank'] = range(1,33)
print(Team_pred_rf)

# Lasso Regularization
print("Starting Lasso Regression Model...")
from sklearn.linear_model import Lasso

lasso_cv = Lasso(random_state=0, max_iter=10000)
alphas = np.logspace(-2, -0.5, 10)
tuned_parameters = [{'alpha': alphas}]
n_folds = 10
# Note: error about unable to converge try to increase the iteration number
print("Staring Cross Validation...")
clf = GridSearchCV(lasso_cv, tuned_parameters, cv=n_folds, refit=False)
clf.fit(Train_X, Train_y)
clf.cv_results_['mean_test_score']

lasso_model = Lasso(random_state=0, alpha=0.1)
lasso_model.fit(Train_X, Train_y)
Pred_y_lasso = lasso_model.predict(Test_X)
Team_pred_lasso = pd.DataFrame()
Team_pred_lasso['Team'] = Test_Data['Team']
Team_pred_lasso['Pred_y_lasso'] = pd.Series(Pred_y_lasso).apply(lambda x: math.exp(x)/(math.exp(x)+1)).tolist()
Team_pred_lasso = Team_pred_lasso.sort_values(by='Pred_y_lasso', ascending=False)
Team_pred_lasso['rank'] = range(1,33)
print(Team_pred_lasso)
