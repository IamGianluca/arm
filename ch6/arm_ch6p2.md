# Chapter 6 Problem 2 (Gelman & Hill)
Gianluca Rossi  
05/12/2015  

*Multinomial logit: using the individual-level survey data from the 2000 National Election Study (data in folder `nes`), predict party identification (which is on a five-point scale) using ideology and demographics with an ordered multinomial logit model.*


```r
require(arm)
require(foreign)
require(MASS)
```


```r
df <- read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/nes/nes5200_processed_voters_realideo.dta")

df$partyid3 <- factor(df$partyid3, labels=c("democrats", "independents",
                                            "republicans", "apolitical"))
df$gender <- factor(df$gender, labels=c("male", "female"))
df$race <- factor(df$race, labels=c("white", "black", "asian", 
                                    "native american", "hispanic",
                                    "other"))
df$south <- factor(df$south)
df$ideo <- factor(df$ideo, labels=c("liberal", "moderate", "conservative"))

# filter out cases where `partyid3` is NA
x = df$partyid3
df <- df[!is.na(levels(x)[x]),]

# exclude apolitical to have an ordered outcome (i.e. democrats, independents, republicans)
df <- subset(df, partyid3!="apolitical")
df$partyid3 <- factor(df$partyid3)
```


### Part A

*Summarize the parameter estimates numerically and also graphically.*


```r
multi.log <- polr(partyid3 ~ ideo + race + age_10, Hess=TRUE, data=df)
summary(multi.log)
```

```
## Call:
## polr(formula = partyid3 ~ ideo + race + age_10, data = df, Hess = TRUE)
## 
## Coefficients:
##                       Value Std. Error  t value
## ideomoderate         1.0923    0.05183  21.0738
## ideoconservative     2.0209    0.04449  45.4226
## raceblack           -2.0887    0.07266 -28.7455
## raceasian            0.2056    0.14655   1.4030
## racenative american -0.4204    0.10648  -3.9483
## racehispanic        -0.9211    0.07610 -12.1030
## raceother           -0.3989    0.48895  -0.8159
## age_10              -0.1147    0.01037 -11.0537
## 
## Intercepts:
##                          Value    Std. Error t value 
## democrats|independents     0.4669   0.0581     8.0385
## independents|republicans   0.8959   0.0585    15.3225
## 
## Residual Deviance: 23593.16 
## AIC: 23613.16 
## (25245 observations deleted due to missingness)
```


### Part B

*Explain the results from the fitted model.*


```r
confint(multi.log)
```

```
## Waiting for profiling to be done...
```

```
##                           2.5 %      97.5 %
## ideomoderate         0.99088703  1.19409012
## ideoconservative     1.93404008  2.10845161
## raceblack           -2.23286503 -1.94793720
## raceasian           -0.08077981  0.49403605
## racenative american -0.62975657 -0.21225583
## racehispanic        -1.07107585 -0.77272060
## raceother           -1.37398545  0.56561431
## age_10              -0.13502245 -0.09436586
```

Some coefficients don't seem to be significant (the levels asian and other of the `race` predictor), but in general we can say we don't need to discard any predictor.

We can re-write the model outputs in a much easier to read form:

$$
\begin{equation}
  y_{i} = 
  \begin{cases}
    \text{Democracts} \text{ if } z_{i} \lt c_{\text{democrats}|\text{independents}} \\
    \text{Independents} \text{ if } z_{i} \in (0, c_{\text{indepentents}|\text{republicans}}) \\
    \text{Republicans} \text{ if } z_{i} \gt c_{\text{indepentents}|\text{republicans}}
  \end{cases}
\end{equation}
$$

where $z_{i} = \beta*x + \epsilon_{i}$, with independent errors $\epsilon_{i}$ that have the logistic distribution. 

In our particular case the estimates for the cutpoints are $c_{\text{democrats}|\text{independents}} = 0.47$ and $c_{\text{indepentents}|\text{republicans}} = 0.90$.

We can also interpret the coefficients from Part A in the following way:

* `age_10`: The estimates in the output are given in units of ordered logits, or ordered log odds. So for `age_10`, we would say that for a one unit increase in age (i.e. going from 20s to 30s), we expect a -0.11 increase in the expect value of `partyid3` on the log odds scale, given all of the other variables in the model are held constant 
* `ideo`: moderates and especially conservatives are more likely to be republicans. In particular, a moderate has 1.09 increase in the expected value of `partyid3` on the log odds scale, given all of the other variables in the model are held constant. Conservatives, are even more likely, with a 2.02 increase in the log odds scale
* `race`: whites, and asian are more likely to identify themselves as republicans. Whereas, blacks are strongly skewed towards the democrat party.


### Part C

*Use a binned residual plot to assess the fit of the model.*

I'm not sure how you can compute the residuals for a categorical response (?).


```r
residuals(multi.log)
```

```
## NULL
```

```r
#binnedplot(predict(multi.log), resid(multi.log))
```
