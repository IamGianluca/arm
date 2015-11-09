Plotting linear and nonlinear regressions: we downloaded data with weight (in pounds) and age (in years) from a random sample of American adults. We first created new variables: age10 = age/10 and age10.sq = (age/10)2, and indicators age18.29, age30.44, age45.64, and age65up for four age categories. We then fit some regressions, with the following results:

``` r
lm(formula = weight ~ age10) coef.est coef.se (Intercept) 161.0 7.3 age10 2.6 1.6
n = 2009, k = 2
residual sd = 119.7, R-Squared = 0.00
lm(formula = weight ~ age10 + age10.sq) coef.est coef.se
R output
(Intercept) age10 age10.sq
n = 2009, k = residual sd =
96.2 19.3 33.6 8.7 -3.2 0.9 3
119.3, R-Squared = 0.01
lm(formula =
(Intercept) age30.44TRUE age45.64TRUE age65upTRUE
weight ~ coef.est 157.2 19.1 27.2 8.5
age30.44 + age45.64 + age65up) coef.se
5.4 7.0 7.6 8.7
n = 2009, k = residual sd =
4
119.4, R-Squared = 0.01
```

### Part A

*On a graph of weights versus age (that is, weight on y-axis, age on x-axis), draw the fitted regression line from the first model.*

``` r
require(ggplot2)
```

    ## Loading required package: ggplot2

### Part B

*On the same graph, draw the fitted regression line from the second model.*

### Part C

*On another graph with the same axes and scale, draw the fitted regression line from the third model. (It will be discontinuous.)*
