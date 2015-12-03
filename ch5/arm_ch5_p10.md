# Chapter 5, Problem 10 (Gelman & Hill)
Gianluca Rossi  
3 December 2015  

*Model building and comparison: continue with the well-switching data described in the previous exercise.*


```r
require(arm)
require(ggplot2)
require(foreign)
```


```r
df <- read.table("http://www.stat.columbia.edu/~gelman/arm/examples/arsenic/wells.dat")
summary(df)
```

```
##      switch          arsenic           dist             assoc       
##  Min.   :0.0000   Min.   :0.510   Min.   :  0.387   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.820   1st Qu.: 21.117   1st Qu.:0.0000  
##  Median :1.0000   Median :1.300   Median : 36.761   Median :0.0000  
##  Mean   :0.5752   Mean   :1.657   Mean   : 48.332   Mean   :0.4228  
##  3rd Qu.:1.0000   3rd Qu.:2.200   3rd Qu.: 64.041   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :9.650   Max.   :339.531   Max.   :1.0000  
##       educ       
##  Min.   : 0.000  
##  1st Qu.: 0.000  
##  Median : 5.000  
##  Mean   : 4.828  
##  3rd Qu.: 8.000  
##  Max.   :17.000
```

### Part A

*Fit a logistic regression for the probability of switching using, as predictors, distance, log(arsenic), and their interaction. Interpret the estimated coefficients and their standard errors.*


```r
df$log.arsenic <- log(df$arsenic)
m1 <- glm(switch ~ dist * log.arsenic, family=binomial(link="logit"), data=df)
display(m1)
```

```
## glm(formula = switch ~ dist * log.arsenic, family = binomial(link = "logit"), 
##     data = df)
##                  coef.est coef.se
## (Intercept)       0.49     0.07  
## dist             -0.01     0.00  
## log.arsenic       0.98     0.11  
## dist:log.arsenic  0.00     0.00  
## ---
##   n = 3020, k = 4
##   residual deviance = 3896.8, null deviance = 4118.1 (difference = 221.3)
```

* `Intercept`: a person with an average distance from a well with clean water and average `log.arsenic` has a $logit^{-1}(0.49) = 62.01\%$ probability to switch well
* `dist`: a one meter increase in distance from a well with safe water, holding all other predictors to their mean, has the effect of decreasing the probability of switch well by $\frac{-0.01}{4} = -0.25\%$. Using a more reasonable scale, this is equivalent to say that every 10 meters increase in distance the probability to switch decreases by 2.5%
* `log.arsenic`: all other predictors hold at their mean, we can say that a 10% increase in `arsenic` corresponds  in a difference in the expected probability of switching well of $\beta_{log.arsenic} * log(\frac{r2}{r1}) = 0.98 * log(\frac{0.3138608*1.1}{0.3138608}) = 9.34\%$. The choice of 0.3138608 as a value to plug into the formulat is due to the fact that is the average value of `log.arsenic` as you can see using the following command


```r
mean(df$log.arsenic)
```

```
## [1] 0.3138608
```

* `dist:log.arsenic`: 


### Part B

*Make graphs as in Figure 5.12 to show the relation between probability of switching, distance, and arsenic level.*


### Part C

*Following the procedure described in Section 5.7, compute the average pre- dictive differences corresponding to:*

i. *A comparison of dist = 0 to dist = 100, with arsenic held constant.*
ii. *A comparison of dist = 100 to dist = 200, with arsenic held constant.*
iii. *A comparison of arsenic = 0.5 to arsenic = 1.0, with dist held constant.* 
iv. *A comparison of arsenic = 1.0 to arsenic = 2.0, with dist held constant. Discuss these results.*
