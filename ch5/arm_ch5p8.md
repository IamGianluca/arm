# Chapter 5, Problem 8 (Gelman & Hill)
Gianluca Rossi  
28 November 2015  

*Building a logistic regression model: the folder `rodents` contains data on rodents in a sample of New York City apartments.*


```r
require(arm)
require(foreign)
require(ggplot2)
```


```r
df <- read.csv("http://www.stat.columbia.edu/~gelman/arm/examples/rodents/hvs02_sorted.csv")

df$race <- factor(df$race, labels=c("White (non-hispanic)", "Black (non-hispanic)", "Puerto Rican", "Other Hispanic", "Asian/Pacific Islander", "Amer-Indian/Native Alaskan", "Two or more races"))

df$unitflr2 <- as.factor(df$unitflr2)
df$numunits <- as.factor(df$numunits)
df$stories <- as.factor(df$stories)
df$extwin4_2 <- as.factor(df$extwin4_2)
df$extflr5_2 <- as.factor(df$extflr5_2)
df$borough <- factor(df$borough, labels=c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))
df$cd <- as.factor(df$cd)
df$intcrack2 <- as.factor(df$intcrack2)
df$inthole2 <- as.factor(df$inthole2)
df$intleak2 <- as.factor(df$intleak2)
df$intpeel_cat <- as.factor(df$intpeel_cat)
df$help <- as.factor(df$help)
df$old <- as.factor(df$old)
df$dilap <- as.factor(df$dilap)
df$regext <- as.factor(df$regext)
df$poverty <- as.factor(df$poverty)
df$povertyx2 <- as.factor(df$povertyx2)
df$housing <- factor(df$housing, labels=c("public", "rent controlled/stabilized", "owned", "other rentals"))
df$board2 <- as.factor(df$board2)
df$subsidy <- as.factor(df$subsidy)
df$under6 <- as.factor(df$under6)

summary(df)
```

```
##           borough        numunits    stories 
##  Bronx        :2472   12     :2941   1:4073  
##  Brooklyn     :4546   10     :2667   2:2365  
##  Manhattan    :4057   11     :2194   3:1440  
##  Queens       :3990   3      :2116   4:1628  
##  Staten Island: 829   1      :2049   5:3537  
##                       5      :1048   6:1831  
##                       (Other):2879   7:1020  
##                          race         personrm         housewgt     
##  White (non-hispanic)      :6867   Min.   :0.1100   Min.   : 12.19  
##  Black (non-hispanic)      :3876   1st Qu.:0.3300   1st Qu.:171.48  
##  Puerto Rican              :1465   Median :0.5000   Median :192.99  
##  Other Hispanic            :2173   Mean   :0.6522   Mean   :189.09  
##  Asian/Pacific Islander    :1393   3rd Qu.:0.8300   3rd Qu.:210.64  
##  Amer-Indian/Native Alaskan:  30   Max.   :6.0000   Max.   :317.95  
##  Two or more races         :  90                                    
##    sequenceno     under6          cd           unitflr2     regext    
##  Min.   :     7   1:13681   34     :  634   2      :5010   0   :6841  
##  1st Qu.:246756   2: 1612   33     :  572   3      :3361   1   :5583  
##  Median :496736   3:  515   45     :  472   4      :1960   NA's:3470  
##  Mean   :498457   4:   72   38     :  437   7      :1660              
##  3rd Qu.:750275   5:   13   32     :  427   5      :1383              
##  Max.   :999966   6:    1   39     :  380   (Other):2387              
##                             (Other):12972   NA's   : 133              
##    totincom2       subsidy                           housing     poverty  
##  Min.   : -35000   0   :7765   public                    : 911   0:12693  
##  1st Qu.:  16000   1   :1345   rent controlled/stabilized:5436   1: 3201  
##  Median :  37800   NA's:6784   owned                     :5039            
##  Mean   :  57240               other rentals             :4508            
##  3rd Qu.:  70000                                                          
##  Max.   :1595608                                                          
##                                                                           
##  povertyx2 extwin4_2    extflr5_2    intcrack2    inthole2    
##  0:9780    0   :15402   0   :14143   0   :12166   0   :12686  
##  1:6114    1   :  425   1   :  636   1   : 1778   1   :  869  
##            NA's:   67   NA's: 1115   NA's: 1950   NA's: 2339  
##                                                               
##                                                               
##                                                               
##                                                               
##  intpeel_cat  intleak2        duration      board2          struct      
##  0   :11672   0   :11400   Min.   : 0.00   0   :13545   Min.   :0.0000  
##  1   : 1146   1   : 2513   1st Qu.: 3.00   1   : 2283   1st Qu.:0.0000  
##  2   : 1007   NA's: 1981   Median : 7.00   NA's:   66   Median :1.0000  
##  NA's: 2069                Mean   :12.42                Mean   :0.7453  
##                            3rd Qu.:19.00                3rd Qu.:1.0000  
##                            Max.   :87.00                Max.   :1.0000  
##                                                         NA's   :2005    
##  old        help          rodent2          foreign        dilap      
##  0:5969   0   : 2610   Min.   :0.0000   Min.   :0.0000   0   :15058  
##  1:9925   1   :10764   1st Qu.:0.0000   1st Qu.:0.0000   1   :  722  
##           NA's: 2520   Median :0.0000   Median :0.0000   NA's:  114  
##                        Mean   :0.2348   Mean   :0.4826               
##                        3rd Qu.:0.0000   3rd Qu.:1.0000               
##                        Max.   :1.0000   Max.   :1.0000               
##                        NA's   :1963     NA's   :1951                 
##    black_Mean      board2_Mean      foreign_Mean      help_Mean     
##  Min.   :0.0000   Min.   :0.0200   Min.   :0.1900   Min.   :0.5200  
##  1st Qu.:0.0400   1st Qu.:0.0500   1st Qu.:0.3100   1st Qu.:0.7400  
##  Median :0.1100   Median :0.0900   Median :0.5200   Median :0.8400  
##  Mean   :0.2437   Mean   :0.1446   Mean   :0.4799   Mean   :0.8056  
##  3rd Qu.:0.3800   3rd Qu.:0.1800   3rd Qu.:0.6000   3rd Qu.:0.8900  
##  Max.   :0.9300   Max.   :0.6600   Max.   :0.7700   Max.   :0.9400  
##                                                                     
##  hispanic_Mean       old_Mean       poverty_Mean    povertyx2_Mean  
##  Min.   :0.0400   Min.   :0.1000   Min.   :0.0600   Min.   :0.1300  
##  1st Qu.:0.1000   1st Qu.:0.5000   1st Qu.:0.1300   1st Qu.:0.2900  
##  Median :0.1600   Median :0.6200   Median :0.1700   Median :0.3600  
##  Mean   :0.2282   Mean   :0.6246   Mean   :0.2012   Mean   :0.3848  
##  3rd Qu.:0.3100   3rd Qu.:0.7800   3rd Qu.:0.2700   3rd Qu.:0.4800  
##  Max.   :0.7400   Max.   :0.9600   Max.   :0.4800   Max.   :0.7800  
##                                                                     
##   pubhous_Mean      regext_Mean      struct_Mean       dilap_Mean     
##  Min.   :0.00000   Min.   :0.1200   Min.   :0.3300   Min.   :0.00000  
##  1st Qu.:0.00000   1st Qu.:0.3100   1st Qu.:0.5900   1st Qu.:0.02000  
##  Median :0.03000   Median :0.4300   Median :0.7900   Median :0.03000  
##  Mean   :0.05681   Mean   :0.4558   Mean   :0.7484   Mean   :0.04611  
##  3rd Qu.:0.09000   3rd Qu.:0.6200   3rd Qu.:0.8800   3rd Qu.:0.06000  
##  Max.   :0.39000   Max.   :0.7900   Max.   :0.9700   Max.   :0.17000  
##                                                                       
##   ownhous_Mean    duration_Mean   extwin4_2_Mean    extflr5_2_Mean   
##  Min.   :0.0200   Min.   : 8.15   Min.   :0.00000   Min.   :0.00000  
##  1st Qu.:0.1800   1st Qu.:11.31   1st Qu.:0.01000   1st Qu.:0.01000  
##  Median :0.2900   Median :12.67   Median :0.02000   Median :0.03000  
##  Mean   :0.3171   Mean   :12.42   Mean   :0.02702   Mean   :0.04298  
##  3rd Qu.:0.4200   3rd Qu.:13.35   3rd Qu.:0.04000   3rd Qu.:0.06000  
##  Max.   :0.7500   Max.   :16.54   Max.   :0.11000   Max.   :0.14000  
##                                                                      
##  intcrack2_Mean   inthole2_Mean     intleak2_Mean       vacrate       
##  Min.   :0.0200   Min.   :0.00000   Min.   :0.0200   Min.   :0.02000  
##  1st Qu.:0.0600   1st Qu.:0.02000   1st Qu.:0.1100   1st Qu.:0.04000  
##  Median :0.1000   Median :0.05000   Median :0.1700   Median :0.06000  
##  Mean   :0.1268   Mean   :0.06334   Mean   :0.1805   Mean   :0.06254  
##  3rd Qu.:0.1900   3rd Qu.:0.10000   3rd Qu.:0.2300   3rd Qu.:0.08000  
##  Max.   :0.3300   Max.   :0.18000   Max.   :0.4500   Max.   :0.16000  
## 
```

### Part A

*Build a logistic regression model to predict the presence of rodents (the variable `rodent2` in the dataset) given indicators for the ethnic groups (race). Combine categories as appropriate. Discuss the estimated coefficients in the model.*

In our first model we will use as predictor: `race` and the average of black and hispanic population in the district. 

In this case there is no need to scale the variables because they all have a IQR close to 1 or are factors.


```r
df$hispanic_Mean10 <- df$hispanic_Mean * 10
df$black_Mean10 <- df$black_Mean * 10

m1 <- glm(rodent2 ~ race + hispanic_Mean10 + black_Mean10, data=df, family=binomial(link="logit"))
display(m1)
```

```
## glm(formula = rodent2 ~ race + hispanic_Mean10 + black_Mean10, 
##     family = binomial(link = "logit"), data = df)
##                                coef.est coef.se
## (Intercept)                    -2.62     0.05  
## raceBlack (non-hispanic)        0.87     0.07  
## racePuerto Rican                1.00     0.08  
## raceOther Hispanic              1.19     0.07  
## raceAsian/Pacific Islander      0.67     0.08  
## raceAmer-Indian/Native Alaskan  0.77     0.41  
## raceTwo or more races           0.75     0.26  
## hispanic_Mean10                 0.19     0.01  
## black_Mean10                    0.11     0.01  
## ---
##   n = 13931, k = 9
##   residual deviance = 13675.4, null deviance = 15185.1 (difference = 1509.7)
```

We can interpret the results as follows:

* `Intercept`: an apartment where white (non-hispanic) people live, situated in an area with average black and hispanic population, has probability $logit^{-1}(-2.62) = 0.0679 = 6.79%$ of having rodent infestation in the building
* `race`: this is the coefficient for race (on the logit scale) if any other predictor is at its average value.
Being a factor, we can see how different races differ in terms of predicting the outcome. The base level for this factor is `White (non-hispanic)`. We can notice the coefficients for all level are positive and statistically significant, with the only expection of `Amer-Indian/Native Alaskan`. In particular, if anything else is hold at the average point, apartments where Hispanic ($\frac{1.19}{4} = 0.2975 = 29.75\%$ more likely) and Puerto Rican ($\frac{1.00}{4} = 0.25 = 25\%$ more likely) live have a higher chance to be in building infestated by roditors
* `hispanic_Mean10`: a 10% increase in hispanic presence in the district is associated with a $\frac{0.19}{4} = 0.0475 = 4.75\%$ increase in probability that the building is infestated by roditors, when the race of the people living in the flat is white (non-hispanic)
* `black_Mean10`: as on the previous coefficient, a flat occupied by whites, with average hispanic presence in the district, is $\frac{0.11}{4} = 0.0275 = 2.75\%$ more likely to be infestated if the ratio of black people living in the district is 10% higher 

We tried to fit a model with the interaction terms between race and hispanic or black presence in the district. As expected, the coefficients were not statistically significant. The interaction term in this case represents the difference in slope for each race linked to the increase of black and hispanic population ratio in the neighbour. Intuitivily, this says that is not much the race of the people living in the apartment to determine the conditions of the building (infestated or not infestated). Instead what really seems to make a difference is the neighbour the building is situated. Flats occupied by people of different races, in the same building should not affect the likelihood that the building is infestated.   

The main conclusion we can drive from this initial model is that districts where hispanic and black people lives are generally associated with buildings more likely to be infestated by roditors.

### Part B

*Add to your model some other potentially relevant predictors describing the apartment, building, and community district. Build your model using the general principles explained in Section 4.6. Discuss the coefficients for the ethnicity indicators in your model.*


```r
# fit model
m2 <- glm(rodent2 ~ race + hispanic_Mean10 + black_Mean10 + borough + old + housing + personrm + struct + foreign, data=df, family=binomial(link="logit"))
display(m2)
```

```
## glm(formula = rodent2 ~ race + hispanic_Mean10 + black_Mean10 + 
##     borough + old + housing + personrm + struct + foreign, family = binomial(link = "logit"), 
##     data = df)
##                                   coef.est coef.se
## (Intercept)                       -2.72     0.14  
## raceBlack (non-hispanic)           0.79     0.08  
## racePuerto Rican                   0.85     0.09  
## raceOther Hispanic                 0.77     0.08  
## raceAsian/Pacific Islander         0.39     0.10  
## raceAmer-Indian/Native Alaskan     0.84     0.44  
## raceTwo or more races              0.56     0.27  
## hispanic_Mean10                    0.13     0.02  
## black_Mean10                       0.06     0.01  
## boroughBrooklyn                    0.43     0.08  
## boroughManhattan                   0.33     0.08  
## boroughQueens                     -0.11     0.09  
## boroughStaten Island              -0.09     0.17  
## old1                               0.36     0.05  
## housingrent controlled/stabilized  0.65     0.10  
## housingowned                      -0.26     0.11  
## housingother rentals               0.24     0.10  
## personrm                           0.48     0.05  
## struct                            -0.94     0.05  
## foreign                            0.20     0.05  
## ---
##   n = 13763, k = 20
##   residual deviance = 12336.1, null deviance = 15024.3 (difference = 2688.1)
```


* `Intercept`: a public flat built after 1947, occupied by white people and owned by a non-foreign born individual, located in the Bronx borough in a discrict of average black and hispanic presence, and an average number of persons per room, has a probability of $logit^{-1}(-2.72) = 0.0618 = 6.18\%$ to be in a building infested by rats
* `race`: at the mean level of all other predictors, any non white race has a higher probability to be associated with a building infestated by rodents. As on the previous model, Puerto Ricans, Blacks and Hispanics are more likely than other races to live in such conditions
* `hispanic_Mean10`: at the mean level of all other predictors, a 10% increase in hispanic population in the district is associated with $\frac{0.13}{4} = 3.25\%$ more likelihood to live in a building infestated by rodents
* `black_Mean10`: at the mean level of all other predictors, a 10% increase in black population in the district is associated with a $\frac{0.06}{4} = 1.5\%$ higher probability to live in a building infestated by rodents
* `borough`: Brooklyn and Manhattan have the highest probability to rats infestations. Both coefficients are positive and highly significant. Queens and Staten Island instead don't significantly differ from Bronx in this particular analysis we are performing
* `old`: at the mean level of all other predictors, buildings built before 1947 have $\frac{0.36}{4} = 9\%$ more likely to have rats infestations
* `housing`: holding all other predictors at their mean level, privately owned apartments are $\frac{0.26}{4} = -6.50\%$ more likely to have rodents infestations 
* `personrm`: the higher the number of people per room, the higher the chances of rats investations in the building
* `struct`: every other predictors hold at its mean level, when a building structure was reported as good or excelent there is less chance of having a rodent investation. To quickly interpret the coefficient in the probability scale, we divide by 4: $\frac{-0.94}{4} = -23.5\%$
* `foreign`: at the mean level of all other predictors, foreign-born owners tends possess apartments located in building $\frac{0.20}{4} = 5\%$ more likely to be infestated by rats.
