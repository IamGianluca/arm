# Chapter 5, Problem 11 (Gelman & Hill)
Gianluca Rossi  
7 December 2015  

*Identifiability: the folder `nes` has data from the National Election Studies that were used in Section 5.1 to model vote preferences given income. When we try to fit a similar model using ethnicity as a predictor, we run into a problem. Here are fits from 1960, 1964, 1968, and 1972:*


```r
glm(formula = vote ~ female + black + income, 
  family=binomial(link="logit"), subset=(year==1960))

			coef.est coef.se    
(Intercept) -0.14 		0.23
female 		0.24 		0.14
black		-1.03 		0.36
income 		0.03 		0.06


glm(formula = vote ~ female + black + income,
	family=binomial(link="logit"), subset=(year==1964))
			coef.est	coef.se
(Intercept)	-1.15 		0.22
female 		-0.09 		0.14
black 		-16.83 		420.40
income 		0.19  		0.06


glm(formula = vote ~ female + black + income,
	family=binomial(link="logit"), subset=(year==1968))
			coef.est 	coef.se 
(Intercept)	0.47		0.24
female 		-0.01 		0.15
black 		-3.64 		0.59
income 		-0.03		0.07


glm(formula = vote ~ female + black + income, 
	family=binomial(link="logit"), subset=(year==1972))
			coef.est 	coef.se 
(Intercept) 0.67 		0.18 
female 		-0.25 		0.12 
black 		-2.63 		0.27 
income 		0.09 		0.05
```

*What happened with the coefficient of black in 1964? Take a look at the data and figure out where this extreme estimate came from. What can be done to fit the model in 1964?*


```r
require(foreign)
require(arm)
require(ggplot2)
```


```r
nes <- read.dta("http://www.stat.columbia.edu/~gelman/arm/examples/nes/nes5200_processed_voters_realideo.dta")
summary(nes$income)
```

```
## 0. dk/ na/ refused to answer/ inap, no p 
##                                        0 
##                    1. 0 to 16 percentile 
##                                     5881 
##                   2. 17 to 33 percentile 
##                                     5941 
##                   3. 34 to 67 percentile 
##                                    12034 
##                   4. 68 to 95 percentile 
##                                    11043 
##                  5. 96 to 100 percentile 
##                                     2121 
##                                     NA's 
##                                     4478
```

Unfortunately we can't replicate the issue because in the dataset we downloaded (from the repository made available by the authors) the variable `income` is categorical and not continuous as in the exercise text.

However, we hypothesise the problem is be caused by collinearity in the predictors. If predictors are collinear, then estimation of the linear predictor, $X\beta$, does not allow separate estimation of the individual parameters $\beta$. 
