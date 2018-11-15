# Simple Linear Regression in Python
import statsmodels.api as sm
import numpy as np
import pandas as pd

from sklearn import datasets
data = datasets.load_boston()
print("statsModels Example")
# Define the data
df = pd.DataFrame(data.data, columns = data.feature_names)

# Put the target (housing value -- MEDV) into another dataFrame
target = pd.DataFrame(data.target, columns = ["MEDV"])
# data.DESCR is used to examine the data set
y = target["MEDV"]
X = df["RM"] # We take one variable for X
X = sm.add_constant(X) # Add a constant to make this sensible.
model = sm.OLS(y,X).fit()
preds1 = model.predict(X)

# Print Results
print(model.summary())
# The adjusted R-squared is reasonable at 0.483.

# Now we'll do the same thing with SKlearn
print("SKLearn Example")
from sklearn import linear_model
lm2 = linear_model.LinearRegression()

model2 = lm2.fit(X, y)
preds2 = lm2.predict(X)
print("Predictions\n", preds2[0:5])
# Let's find the R-squared value
print("R-squared for SKlearn: ", lm2.score(X, y))
# 0.483, the same as the OLS example
# Let's look at the coefficients
print("Intercept: ", lm2.intercept_)
print("Regression Coefficients: ", lm2.coef_)
# Note that one of the coefficients is 0. This is because we are performing
# simple linear regression, but SKLearn works really for multiple linear regression.
# The intercept and first column are therefore the same.

# Now let's plot the sucker.
import plotly.plotly as py
import plotly.graph_objs as go

# MathPlotlib
import matplotlib.pyplot as plt
from matplotlib import pylab
from scipy import stats
xi, b0, b1 = X["RM"], lm2.intercept_, lm2.coef_[1]
line = b0 + b1 * xi
plt.plot(xi, y, 'o', xi, line)
pylab.title('Linear Fit with Matplotlib')
#ax = plt.gca()
#ax.set_axis_bgcolor((0.898, 0.898, 0.898))
#fig = plt.gcf()
#py.plot_mpl(fig, filename = 'slrPlot')
plt.show()

# We do a more involved plot using the first model.
from statsmodels.stats.outliers_influence import summary_table
st, data, ss2 = summary_table(model, alpha=0.05)
fitted_values = data[:, 2]
def lineplot(x_data, y_data, x_label, y_label, title):
    # Create the plot object
    _, ax = plt.subplots()

    # Plot the best fit line
    ax.plot(x_data, y_data, lw = 2, color = "#539caf", alpha=1)

    # Label the axes and probide a title
    ax.set_title(title)
    ax.set_xlabel(x_label)
    ax.set_ylabel(y_label)

lineplot(x_data = xi,
         y_data = fitted_values,
         x_label = "Rooms",
         y_label = "Selling Price",
         title = "SLR Best Fit for Boston Data")
plt.show()
# Get the confidence intervals of the model
predict_mean_ci_low, predict_mean_ci_upp = data[:,4:6].T # What is .T?
# Data for regions where we want to shade to indicate the intervals has
# to be sorted by the x axis to display correctly
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
    title = "SLR Best Fit for Boston Data with CI")
plt.scatter(xi, y, marker=".")
plt.show()
# It seems obvious that there are more varaibles that are important,
# as this line doesn't appear to fit very well. Let's look at the residuals.
residuals = y - model.predict(X)
zeroes = list(map(lambda x: x*0, range(51)))
_, ax = plt.subplots()
ax.scatter(model.predict(X), residuals, marker = '.')
ax.plot(range(51), zeroes, color = "red")
ax.set_title("Fitted v. Residuals Plot")
ax.set_xlabel("Fitted Values")
ax.set_ylabel("Residuals")
plt.show()
# There appear to be some unaccounted for trends in the data. This analysis
# is inadequate, we need to perform at least multiple regression.

# Let's look at a histogram of the residuals, just for kicks.

n, bins, patches = plt.hist(x = residuals, bins='auto', color='#0504aa',
                            alpha=0.7, rwidth=0.85, density = True)
plt.grid(axis='y', alpha=0.75)
plt.xlabel('Value')
plt.ylabel('Frequency')
plt.title('Unnormalized Histogram of Residuals')
plt.text(23, 45, r'$\mu=15, b=3$')
maxfreq = n.max()
# Set a clean upper y-axis limit.
#plt.ylim(ymax=np.ceil(maxfreq / 10) * 10 if maxfreq % 10 else maxfreq + 10)
xx = list(map(lambda x: x, range(-30, 30)))
plt.plot(xx, stats.norm.pdf(xx, loc = np.mean(residuals),
                            scale = np.std(residuals)))
plt.show()

stdRes = (residuals - np.mean(residuals))/np.std(residuals)

n, bins, patches = plt.hist(x = stdRes, bins='auto', color='#0504aa',
                            alpha=0.7, rwidth=0.85, density = True)
plt.grid(axis='y', alpha=0.75)
plt.xlabel('Value')
plt.ylabel('Frequency')
plt.title('Normalized Histogram of Residuals')
plt.text(23, 45, r'$\mu=15, b=3$')
maxfreq = n.max()
xx = list(map(lambda x: x/10, range(-30, 30)))
plt.plot(xx, stats.norm.pdf(xx))
# Set a clean upper y-axis limit.
#plt.ylim(ymax=np.ceil(maxfreq / 10) * 10 if maxfreq % 10 else maxfreq + 10)
plt.show()
# These residuals look good, but the equal variance assumption is still violated.
