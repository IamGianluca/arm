In this exercise you will simulate two variables that are statistically independent of each other to see what happens when we run a regression of one on the other.

1.  First generate 1000 data points from a normal distribution with mean 0 and standard deviation 1 by typing `var1 <- rnorm(1000,0,1)` in R. Generate another variable in the same way (call it `var2`). Run a regression of one variable on the other. Is the slope coefficient statistically significant?
2.  Now run a simulation repeating this process 100 times. This can be done using a loop. From each simulation, save the z-score (the estimated coefficient of var1 divided by its standard error). If the absolute value of the z-score exceeds 2, the estimate is statistically significant. Here is code to perform the simulation:

``` r
z.scores <- rep (NA, 100) 
for (k in 1:100) {
  var1 <- rnorm (1000,0,1)
  var2 <- rnorm (1000,0,1)
  fit <- lm (var2 ~ var1)
  z.scores[k] <- coef(fit)[2]/se.coef(fit)[2]
}
```

How many of these 100 z-scores are statistically significant?

### Part a

``` r
set.seed(1234)
var1 <- rnorm(1000,0,1)
var2 <- rnorm(1000,0,1)
m1 <- lm(var1 ~ var2)
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = var1 ~ var2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3063 -0.6373 -0.0228  0.6433  3.2453 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -0.02743    0.03151  -0.871   0.3841  
    ## var2         0.05756    0.03212   1.792   0.0735 .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9962 on 998 degrees of freedom
    ## Multiple R-squared:  0.003207,   Adjusted R-squared:  0.002208 
    ## F-statistic: 3.211 on 1 and 998 DF,  p-value: 0.07345

The slope is not significant at a 5% significance level. We set a seed in other to have reproduceable results.

### Part b

``` r
z.scores <- rep (NA, 100) 
for (k in 1:100) {
  var1 <- rnorm (1000,0,1)
  var2 <- rnorm (1000,0,1)
  fit <- lm (var2 ~ var1)
  z.scores[k] <- coef(fit)[2]/se.coef(fit)[2]
}
sum(z.scores > 1.96)
```

    ## [1] 2

Only on two (out of 100) iterations the z-score is greater than 1.96, thus significant at a 5% significance level.
