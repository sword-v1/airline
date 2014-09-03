#
#     O R A C L E  R  E N T E R P R I S E  S A M P L E   L I B R A R Y
#
#     Name: distributions.R
#     Description: Distribution, Density and Quantile Functions
#
#
#

## Set page width
options(width = 80)

# List all accessible tables and views in the Oracle database
ore.ls()

# Create a new table called IRIS_TABLE in the Oracle database
# using the built-in iris data.frame

# First remove previously created IRIS_TABLE objects from the
# global environment and the database
if (exists("IRIS_TABLE", globalenv(), inherits = FALSE))
    rm("IRIS_TABLE", envir = globalenv())
ore.drop(table = "IRIS_TABLE")

# Create the table
ore.create(iris, table = "IRIS_TABLE")

# Show the updated list of accessible table and views
ore.ls()

# Display the class of IRIS_TABLE and where it can be found in
# the search path
class(IRIS_TABLE)
search()
find("IRIS_TABLE")

# fitdistr() support
fitdistr(IRIS_TABLE$Petal.Length, "normal")
fitdistr(IRIS_TABLE$Petal.Length, "gamma")
fitdistr(IRIS_TABLE$Sepal.Width, "lognormal")
fitdistr(IRIS_TABLE$Sepal.Width, "weibull")
fitdistr(IRIS_TABLE$Sepal.Width, "exponential")
fitdistr(IRIS_TABLE$Sepal.Width, "cauchy")
fitdistr(IRIS_TABLE$Sepal.Width, "geometric")
fitdistr(IRIS_TABLE$Sepal.Width, "logistic")

# Create an alias for ease of reference
x = IRIS_TABLE$Petal.Length

# Normal Distribution - density, probability and quantile functions
dnorm(x)
pnorm(x)
qnorm(x)

# Chi Square Distribution
dchisq(x, 0.2)
pchisq(x, 0.2)
qchisq(x, 0.2)

# Exponential Distribution
dexp(x, rate=3)
pexp(x, rate=3)
qexp(x, rate=3)

# F Distribution
df(x, df1 = 2, df2 = 3)
pf(x, df1 = 2, df2 = 3)
qf(x, df1 = 2, df2 = 3)

# Gamma distribution
dgamma(x, shape = 1.6, scale = 1.1)
pgamma(x, shape = 1.6, scale = 1.1)
qgamma(x, shape = 1.6, scale = 1.1)

# Beta distribution
dbeta(x, shape1 = 3.5, shape2 = 1)
pbeta(x, shape1 = 3.5, shape2 = 1)
qbeta(x, shape1 = 3.5, shape2 = 1)

# Binomial Distribution
dbinom(x, size = 10, prob = 0.1)
pbinom(x, size = 10, prob = 0.1)
qbinom(x, size = 10, prob = 0.1)

# Cauchy Distribution
dcauchy(x, location = 2.5, scale = 1.5)
pcauchy(x, location = 2.5, scale = 1.5)
qcauchy(x, location = 2.5, scale = 1.5)

# Poisson Distribution
dpois(x, lambda = 2.5)
ppois(x, lambda = 2.5)
qpois(x, lambda = 2.5)

# Student's T Distribution
dt(x, df = 2)
pt(x, df = 2)
qt(x, df = 2)

# Weibull Distribution
dweibull(x, shape = 2.5, scale = 1.5)
pweibull(x, shape = 2.5, scale = 1.5)
qweibull(x, shape = 2.5, scale = 1.5)
