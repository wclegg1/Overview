# Multiple Linear Regression in Python
import statsmodels.api as sm
import numpy as np
import pandas as pd
import plotly.plotly as py
import plotly.graph_objs as go
import matplotlib.pyplot as plt
from matplotlib import pylab
from scipy import stats
import os
from plotly.offline import iplot, init_notebook_mode
import plotly.io as pio
from statsmodels.stats.outliers_influence import summary_table

from sklearn import datasets, linear_model

data = datasets.load_boston()
# Define the data
df = pd.DataFrame(data.data, columns = data.feature_names)
# Put the target (housing value -- MEDV) into another dataFrame
target = pd.DataFrame(data.target, columns = ["MEDV"])
# data.DESCR is used to examine the data set
y = target["MEDV"]
X = df

# EDA
#plt.plot(df[:]["RM"], y, 'o')
#plt.show()
trace = go.Scatter(
    x = df[:]["RM"],
    y = y,
    mode = "markers"
)
D = [trace]
if not os.path.exists('images'):
    os.mkdir('images')

N = len(y)
colors = 1
sz = 1

#fig = go.Figure()
#fig.add_scatter(x=df[:]["RM"],
#                y=y,
#                mode='markers',
#                marker={'size': sz,
#                        'color': colors,
#                        'opacity': 0.6,
#                        'colorscale': 'Viridis'
#                       });
#pio.write_image(fig, 'images/fig1.png')

# Fit a model using OLS
X1 = sm.add_constant(X)
model1= sm.OLS(y, X1).fit()
preds1= model1.predict(X1)
print(model1.summary(), '\n')

# Fit a model using linear_model
lm = linear_model.LinearRegression()
model = lm.fit(X,y)
print("R-squared for SKlearn: ", lm.score(X, y))
print("Intercept: ", lm.intercept_)
print("Regression Coefficients: ", lm.coef_, '\n')

# Get the confidence intervals of the model
st, data, ss2 = summary_table(model1, alpha=0.05)
fitted_values = data[:, 2]
predict_mean_ci_low, predict_mean_ci_upp = data[:,4:6].T # What is .T?
# Data for regions where we want to shade to indicate the intervals has
# to be sorted by the x axis to display correctly
xi = X["RM"]
CI_df = pd.DataFrame(columns = ['x_data', 'low_CI', 'upper_CI'])
CI_df['x_data'] = xi
CI_df['low_CI'] = predict_mean_ci_low
CI_df['upper_CI'] = predict_mean_ci_upp
CI_df.sort_values('x_data', inplace = True)


# Define a function for the line plot with intervals
def lineplotCI(x_data, y_data, sorted_x, low_CI, upper_CI, x_label, y_label, title):
    # Create the plot object
    _, ax = plt.subplots()
    # Plot the data, set the linewidth, color and transparency of the
    # line, provide a label for the legend
    ax.plot(x_data, y_data, lw = 1, color = '#539caf', alpha = 1, label = 'Fit')
    # Shade the confidence interval
    ax.fill_between(sorted_x, low_CI, upper_CI, color = '#539caf', alpha = 0.4, label = '95% CI')
    # Label the axes and provide a title
    ax.set_title(title)
    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)

    # Display legend
    ax.legend(loc = 'best')


# Call the function to create plot
lineplotCI(x_data = xi,
    y_data = fitted_values,
    sorted_x = CI_df['x_data'],
    low_CI = CI_df['low_CI'],
    upper_CI = CI_df['upper_CI'],
    x_label = "# of Rooms",
    y_label = "Selling Price",
    title = "MLR Best Fit for Boston Data with CI")

plt.show()



