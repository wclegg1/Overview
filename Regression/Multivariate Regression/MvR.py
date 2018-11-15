# Multivariate multiple regression
import numpy as np
import pandas as pd
import statsmodels.api as sm
import plotly.plotly as py
import plotly.graph_objs as go
import matplotlib.pyplot as plt
from scipy import stats
from ggplot import mtcars
from sklearn import linear_model
from sklearn.metrics import log_loss
from matplotlib import pylab


# Define Functions
def aic(y, y_pred, k):
    resid = np.subtract(y, y_pred)
    sse = (resid ** 2)
    sse = sse.sum().sum()
    AIC = 2*k - 2*np.log(sse)
    return AIC

def scatterEDA(x_data, y_data, color,
               x_label, y_label, title):
    # Create the plot object
    _, ax = plt.subplots()
    # Plot the data, set the linewidth, color and transparency of the
    # line, provide a label for the legend
    ax.scatter(x_data, y_data, color = color, alpha = 1,
               marker = 'x')
    # Label the axes and provide a title
    ax.set_title(title)
    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)

    # Display legend
    #ax.legend(loc = 'best')

def residHist(residuals):
    _, ax = plt.subplots()
    stdRes = (residuals - np.mean(residuals))/np.std(residuals)

    n, bins, patches = ax.hist(x = stdRes, bins='auto', color='#0504aa',
                                alpha=0.7, rwidth=0.85, density = True)
    ax.set_xlabel('Value')
    ax.set_ylabel('Frequency')
    ax.set_title('Normalized Histogram of Residuals')
    maxfreq = n.max()
    xx = list(map(lambda x: x/10, range(-30, 30)))
    #ax.scatter(xx, stats.norm.pdf(xx))

# Get Data
type(mtcars)
y = mtcars[['mpg','disp']]
print(list(mtcars))
depVars =[i for i in list(mtcars) if i not in ['name', 'mpg', 'disp']]
print(depVars)
Xa = mtcars[depVars]
#
### EDA


### Fit Multivariate Multiple Model
# With sklearn module
ols = linear_model.LinearRegression()
modelA = ols.fit(Xa, y)
X = Xa[:][:]
print("R-Squared Estimate: ", modelA.score(Xa, y))
print("Intercept: ", modelA.intercept_)
print("Regression Coefficients: ", modelA.coef_, '\n')
try:
    print("AIC: ", aic(y, modelA.predict(Xa), k = 1))
except BaseException as e:
    print("AIC function not working.", e.args)
# With statsmodels package
#sm.add_constant(Xa)
#modelB = sm.OLS(y, Xa).fit()
#print(modelB.summary())
# This doesn't seem to work


#coef = modelA.coef_
#print(np.subtract(Xa.dot(np.transpose(coef)), y))
#data_loss = 0.5 * ((Xa.dot(np.transpose(coef)) - y) ** 2).sum()
#n_samples, n_features = Xa.shape
#penalty = n_samples * 1 * np.abs(coef).sum()
#likelihood = np.exp(-(data_loss + penalty))

### Variable Selection
# We'll do Backward variable selection
X = mtcars[depVars]
cNames = depVars[:]
try:
    for b in depVars:
        tempNames = [i for i in cNames if i != b]
        print("Iteration: ", b)
        #print(tempNames)
        ols1 = linear_model.LinearRegression()
        modelA = ols1.fit(X, y)
        predsA = modelA.predict(X)
        critA = aic(y, predsA, k = 1)
        X1 = mtcars[tempNames]
        ols2 = linear_model.LinearRegression()
        modelB = ols2.fit(X1, y)
        predsB = modelB.predict(X1)
        critB = aic(y, predsB, k = 1)
        #critA = modelA.score(X,y)
        #critB = modelB.score(X1,y)
        print("Criterion A: ", critA)
        print("Criterion B: ", critB)
        if((critA - critB) < 0.1):
            X = X1[:][:]
            cNames = tempNames[:]
            print("cNames: ", cNames)
except BaseException as e:
    print(e.args)
    print("Stupid thing doesn't work")
else:
    print("Working")
    print(cNames)

olsF = linear_model.LinearRegression()
modelFinal = olsF.fit(X, y)
print("Final R-squared: ", modelFinal.score(X, y))
print(depVars)
### Check Assumptions and Examine Results
#print("X:\n", mtcars["drat"])
try:
    for a in depVars:
        for d in ['mpg', 'disp']:
            #scatterEDA(x_data = mtcars[a], y_data = y[d], x_label = a, y_label = b,
            #  color = "black", title = "")
            #plt.show()
            print("\n")
except BaseException as e:
    print(e.args)
else:
    print("Plotting Successful")


# Fitted v. Residual Plots
residuals = np.subtract(y, modelFinal.predict(X))
preds = modelFinal.predict(X)
respVars = ["mpg", "disp"]
try:
    print(len(residuals[respVars[0]]))
    print(len(preds[:,0]))
except:
    print("Failed Slicing")
# Equal Variance Plots
try:
    for v in range(len(respVars)):
        scatterEDA(x_data = preds[:,v], y_data = residuals[respVars[v]],
                   x_label = "Fitted Values", y_label = "Residuals",
                   color = "black", title = "")
        plt.axhline(y=0, color='r', linestyle='-')
        plt.show()
except BaseException as e:
    print("Failed Last Plotting")
    print(e.args)

# Residual Normality Plots
try:
    for v in range(len(respVars)):
        residHist(residuals[respVars[v]])
        plt.show()
except BaseException as e:
    print("Failed Last Plotting")
    print(e.args)

# It appears we're fine on equal variance and normality claims
