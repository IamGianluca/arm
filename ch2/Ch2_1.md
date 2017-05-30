# Chapter 2

*A test is graded from 0 to 50, with an average score of 35 and a standard deviation of 10. For comparison to other tests, it would be convenient to rescale to a mean of 100 and standard deviation of 15.*

*(a) How can the scores be linearly transformed to have this new mean and standard deviation?*

We know, E[X] = 35 and StdDev[X] = 10 (or Var[X] = 10^{2} = 100)

Consider a linear transformation of X using, Y = a X + b 

Thus, E[Y]= a E[X] + b and Var[Y] = a^{2} * Var[X] 

We want E[Y] = 100 and Var[Y] = 15^{2} = 225. Using these, we get that:

100 = a * 35 + b

225 = a^{2} * 100

Solving for a and b we get, 

a = 1.5 and b = 47.5. 

So the linear transformation required is Y = 1.5X + 47.5 

*(b) There is another linear transformation that also rescales the scores to have
mean 100 and standard deviation 15. What is it, and why would you not want to use it for this purpose?*

Nod to http://stats.stackexchange.com/questions/45816/gelman-hill-textbook-question-about-linear-transformation

Note that 225 = a^{2} * 100, actually implies that a can be either +1.5 or -1.5. 

If a = -1.5 then b = 100 + 1.5 * 35 = 152.5. Hence an alternate linear transformation is

Y = -1.5X + 152.5 

Consider the transformed score for of students who scored 25 and 35 on the original test 
(one standard deviation below and above the mean):

Y(25) = -1.5 * 25 + 152.5 = 115

Y(35) = -1.5 * 35 + 152.5 = 100

As can be seen, an original score of 25 (1 standard deviation below the mean in original), 
post transformation results in a higher score than an original score of 35 
(1 standard deviation above the mean in original). This is undesirable behavior.