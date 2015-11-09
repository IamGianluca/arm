*Sequence of regressions: find a regression problem that is of interest to you and can be performed repeatedly (for example, data from several years, or for several countries). Perform a separate analysis for each year, or country, and display the estimates in a plot as in Figure 4.6 on page 74.*

We have been inspired by [this](http://www.london.gov.uk/sites/default/files/Transport%20and%20health%20in%20London_March%202014.pdf) report by the Greater London Authority published in February 2014, titled "Transport and Health in London: The main impacts of London road transport on health". The document gives an overview of the the main impacts of London road transport on health. I've been living in London for almost four years, thus the topic is absolutely of interest to me.

My analysis will try to explain the variance of perceived well-being for each borough in London using as predictors:

-   Air pollution ()
-   Numbers of live births ()
-   Population ()
-   Road traffic ()
-   Public expenditures for transportations ()
-   Quality of public services ()
-   Etnicity mix ()
-   Sport partecipation rate ()
-   Binge drinking (x)
-   Assault reports (x)

We don't expect the analysis to be exaustive, but nonetheless this could represent a good starting point for further investigations.

``` r
require(arm)
require(ggplot2)
require(dplyr)
```

``` r
# load dataset
```
