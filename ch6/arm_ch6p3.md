# Chapter 6 Problem 3 (Gelman & Hill)
Gianluca Rossi  
08/12/2015  

*Comparing logit and probit: take one of the data examples from Chapter 5. Fit these data using both logit and probit model. Check that the results are essentially the same (after scaling by factor of 1.6; see Figure 6.2 on page 118).*

For this exercise we used the `arsenic` dataset. 


```r
require(foreign)
require(arm)
```


```r
well <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/arsenic/wells.dat")
well$log.arsenic <- log(well$arsenic)
summary(well)
```

```
##      switch          arsenic           dist             assoc       
##  Min.   :0.0000   Min.   :0.510   Min.   :  0.387   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.820   1st Qu.: 21.117   1st Qu.:0.0000  
##  Median :1.0000   Median :1.300   Median : 36.761   Median :0.0000  
##  Mean   :0.5752   Mean   :1.657   Mean   : 48.332   Mean   :0.4228  
##  3rd Qu.:1.0000   3rd Qu.:2.200   3rd Qu.: 64.041   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :9.650   Max.   :339.531   Max.   :1.0000  
##       educ         log.arsenic     
##  Min.   : 0.000   Min.   :-0.6733  
##  1st Qu.: 0.000   1st Qu.:-0.1985  
##  Median : 5.000   Median : 0.2624  
##  Mean   : 4.828   Mean   : 0.3139  
##  3rd Qu.: 8.000   3rd Qu.: 0.7885  
##  Max.   :17.000   Max.   : 2.2670
```

We will now fit a very simple model which tries to predict the likelihood of switching well using as predictors the level of arsenic, the distance for the closest safe well and level of education.


```r
logit <- glm(switch ~ log.arsenic + dist + educ, family=binomial(link="logit"), data=well)
display(logit)
```

```
## glm(formula = switch ~ log.arsenic + dist + educ, family = binomial(link = "logit"), 
##     data = well)
##             coef.est coef.se
## (Intercept)  0.32     0.08  
## log.arsenic  0.89     0.07  
## dist        -0.01     0.00  
## educ         0.04     0.01  
## ---
##   n = 3020, k = 4
##   residual deviance = 3878.2, null deviance = 4118.1 (difference = 239.9)
```

We expect the coefficients of the probit model to have the same sign of the ones in the logit model, but divided by a factor of 1.6. We can easily check this fitting a second model and changing the link function in the `glm` command.


```r
probit <- glm(switch ~ log.arsenic + dist + educ, family=binomial(link="probit"), data=well)
display(probit)
```

```
## glm(formula = switch ~ log.arsenic + dist + educ, family = binomial(link = "probit"), 
##     data = well)
##             coef.est coef.se
## (Intercept)  0.19     0.05  
## log.arsenic  0.54     0.04  
## dist        -0.01     0.00  
## educ         0.03     0.01  
## ---
##   n = 3020, k = 4
##   residual deviance = 3878.3, null deviance = 4118.1 (difference = 239.8)
```

The coefficient of `log.arsenic` becomes 0.54 (0.89 / 1.6 = 0.5563), the coefficient of the distance stays -0.01 (-0.01 / 1.6 = -0.0062) and the one of education becomes 0.03 (0.04 / 1.6 = 0.0250). These are essentially the coefficients we would have scaling by 1.6 the coefficients of the logit model.
