# Chapter 4, Problem 5 (Gelman & Hill)
Gianluca Rossi  
4 November 2015  

*Special-purpose transformations: for a study of congressional elections, you would like a measure of the relative amount of money raised by each of the two major-party candidates in each district. Suppose that you know the amount of money raised by each candidate; label these dollar values $D_{i}$ and $R_{i}$. You would like to combine these into a single variable that can be included as an input variable into a model predicting vote share for the Democrats*


### Part A

*Discuss the advantages and disadvantages of the following measures:*

* *The simple difference, $D_{i} − R_{i}$*: this seems like a good transformation because is symmetric and centered at zero. One disadvantage of this transformation is that it's not proportional. For instance, if Democrats got $6M and Republicans just $4M the difference will be $2M. Now, in the hypothesis Democrats raised $3M and Republicans $1M the absolute difference will still be $2M. However, $2M in the first example corresponds to a much closed gap between the two parties than in the second example. This could limit the effectiveness of the predictor if districts differ widely in average money raised
* *The ratio, $\frac{D_{i}}{R_{i}}$*: this transformation has the disadvantage of being centered at 1 and that is asymmetric. In particualar it tends to zero for case where the Republics have more money raised than Democrats, and tend to infinity on the opposite case. This means that it will weight more cases where Democrats raised more money than the opposite party
* *The difference on the logarithmic scale, $log(D_{i}) − log(R_{i})$*: this is similar to the first transformation, with the difference that is less sensitive to outliers. It is centered to zero and is symmetric. Another advantage of this transformation is that is proportional to the magnitude of the difference. So a $2M difference between the parties on a county where each raise on average above $100M will have a lower value than the same diffence on a district where foundraising is poorer
* *The relative proportion, $\frac{D_{i}}{D_{i} + R_{i}}$*: this transformation is centered at 0.5 and symmetric


### Part B

*Propose an idiosyncratic transformation (as in the example on page 65) and discuss the advantages and disadvantages of using it as a regression input.*

The transformation we propose is to discretise the difference between $D_{i}$ and $R_{i}$ into three ranges $[-\infty, -5M), [-5M, +5M] \text{ and } (+5M, +\infty]$. These ranges will then be mapped to the values -1, 0 and +1. Another option, which could be interesting to explore if counties have widely different size, thus odds of collecting large sums of money, is to consider the percentage of difference between the two values. In this case, we will discretise the ratio $\frac{D_{i} - R_{i}}{D_{i}}$into three ranges $[-\infty, -10\%), [-10\%, +10\%] \text{ and } (+10\%, +\infty]$. These ranges will then be mapped to the values -1, 0 and +1. 

The decision of which transformation makes more sense depends on the data at hand. The suggested transformations could be sentible when the data shows particular cut points. For instance, we would use such transformation when the data shows that when founds raised by the two parties differ by more than 10%, the party who raised more money is highly likely to have a large share of votes; whereas when the difference is smaller, there is high uncertainty over the final outcome.
