PCA\_using\_R\_functions.Rmd
================
Shravan Kuchkula
10/14/2017

-   [Introduction](#introduction)
-   [Getting the data](#getting-the-data)
-   [Data set description](#data-set-description)
-   [Exploratory Data Analysis](#exploratory-data-analysis)
-   [Doing PCA](#doing-pca)
-   [Varimax rotation](#varimax-rotation)

Introduction
------------

How to use the `prcomp` and `varimax` functions in R to accomplish a Principal Components Analysis. We cover the following steps:

-   Read in the Data
-   Plot a Correlation Matrix
-   Call prcomp
-   DotPlot the PCA loadings
-   Apply the Kaiser Criterion
-   Make a screeplot
-   Plot the Biplot and
-   Apply the varimax rotation.

Getting the data
----------------

``` r
# Install the required packages and load them.
# install.packages("calibrate")
source("libraries.R")
library(lattice)
library(dplyr)
```

Download the dataset

``` r
insurance <- read.csv("/Users/Shravan/Downloads/Unit8Exercise/Insurance.csv")
glimpse(insurance)
```

    ## Observations: 47
    ## Variables: 9
    ## $ ZIP      <int> 26, 40, 13, 57, 14, 10, 11, 25, 18, 47, 22, 31, 46, 5...
    ## $ Fire     <dbl> 6.2, 9.5, 10.5, 7.7, 8.6, 34.1, 11.0, 6.9, 7.3, 15.1,...
    ## $ Theft    <int> 29, 44, 36, 37, 53, 68, 75, 18, 31, 25, 34, 14, 11, 1...
    ## $ Age      <dbl> 60.4, 76.5, 73.5, 66.9, 81.4, 52.6, 42.6, 78.5, 90.1,...
    ## $ Income   <int> 11744, 9323, 9948, 10656, 9730, 8231, 21480, 11104, 1...
    ## $ Race     <dbl> 10.0, 22.2, 19.6, 17.3, 24.5, 54.0, 4.9, 7.1, 5.3, 21...
    ## $ Vol      <dbl> 5.3, 3.1, 4.8, 5.7, 5.9, 4.0, 17.9, 6.9, 7.6, 3.1, 1....
    ## $ Invol    <dbl> 0.0, 0.1, 1.2, 0.5, 0.7, 0.3, 0.0, 0.0, 0.4, 1.1, 1.9...
    ## $ Location <fctr> North, North, North, North, North, North, North, Nor...

Data set description
--------------------

In the 1970s, the U.S. Commission on Civil Rights investigated charges that insur- ance companies were attempting to redefine Chicago “neighborhoods” in order to cancel existing homeowner insurance policies or refuse to issue new ones. Data on homeowner and residential fire insurance policy issuances from 47 zip codes in the Chicago area form our data set. Six variables describe general zip code features:

-   **fire** - fires per 1,000 housing units
-   **theft** - thefts per 1,000 population
-   **age** - percentage of housing units built prior to 1940
-   **income** - median family income
-   **race** - percentage minority
-   **zip** - the last two digits of the zip code (the first three being 606)

Chicago’s FAIR plan provided a way for households that were rejected by the voluntary insurance market to obtain insurance coverage. These policies were issued involuntarily by the insurance industry.

-   **vol** - is the number of new policies per 100 housing units
-   **invol** - is the number of FAIR plan policies/renewals per 100 housing units.

Lastly, Location is a re-coded variable from the zip variable, dividing the localities into either `North` or `South`.

-   **location** - Derived from the zipcode of the locality as either North or South

Exploratory Data Analysis
-------------------------

Let's create a correlation matrix of all the zip code variables: `{fire, theft, age, income, race}`

``` r
corMatrix <- insurance %>%
                dplyr::select(Fire, Theft, Age, Income, Race)

# Create the correlation matrix
round(cor(corMatrix), 2)
```

    ##         Fire Theft   Age Income  Race
    ## Fire    1.00  0.41  0.41  -0.61  0.59
    ## Theft   0.41  1.00  0.34  -0.09  0.32
    ## Age     0.41  0.34  1.00  -0.53  0.25
    ## Income -0.61 -0.09 -0.53   1.00 -0.70
    ## Race    0.59  0.32  0.25  -0.70  1.00

Visualize this in a corrplot

``` r
M <- round(cor(corMatrix), 2)
corrplot(M, method="pie", type = "lower")
```

![](PCA_using_R_functions_files/figure-markdown_github/unnamed-chunk-4-1.png)

Scatter plot of the zip code variables

``` r
corMatrix %>%
  ggpairs()
```

![](PCA_using_R_functions_files/figure-markdown_github/unnamed-chunk-5-1.png)

Doing PCA
---------

Next let's call the procedure `prcomp` to do the PCA.

``` r
# running pca on the zip code variables.
my.prc <- prcomp(corMatrix, center = T, scale = T)
```

The name of the procedure is prcomp and underneath the hood it uses a singular value decomposition to accomplish the PCA. The procedure takes the dataset and `center = T` which indicates that we want to center the data and `scale = T` scale it to unit variance.

Now we have the result of the analysis in my.prc. Let's examine this object by calling `ls()` on this object.

``` r
ls(my.prc)
```

    ## [1] "center"   "rotation" "scale"    "sdev"     "x"

So with this object, we can get the centered data, the rotations, the standard deviations and the scores which the prcomp function calls them as x.

Now let's take a summary look at `my.prc` object.

``` r
summary(my.prc)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3     PC4     PC5
    ## Standard deviation     1.6609 0.9745 0.8682 0.62175 0.38912
    ## Proportion of Variance 0.5517 0.1899 0.1507 0.07731 0.03028
    ## Cumulative Proportion  0.5517 0.7417 0.8924 0.96972 1.00000

So, what we see here is that we have five components. The first component accounts for 55% of the variation, second component is 18%, third component is 15% etc.

Now, the first 3 components account for 89% of the variation. At this point, you may have a question of how many of these components to keep ? There is atleast two ways to answer this:

1.  First is using the Keiser Criterion. What this says is that, the `eigen values` associated with each component if that is greater than 1, then you retain that component. **NOTE**: The `eigen value` associated with each component not the `eigen vector`. Now you may be wondering how do we get the eigen value for each component ? We actually have that information on our finger tips in the form of `sdev` standard deviations of the my.prc object. Now, if we square the `sdev` values we get the `eigen values` back. Now the reason this works is because the sum of eigen values is the sum total of the variance in the dataset. The `sdev` is the square root of that, a singular value. So if we square that information we get the eigen values back.

``` r
my.prc$sdev ^ 2
```

    ## [1] 2.7586160 0.9496824 0.7537101 0.3865744 0.1514171

What we see here is that only the first is greater than 1 and the rest are not. So according to Keiser Criterion, we would retain only PC1.

1.  Second way to answer this question is using a plot called the `scree plot`. A bar plot of the principal components. You see that they account for less variation down the line. The idea here is that to retain the components that are contributing significantly.

``` r
screeplot(my.prc, main= "Scree Plot", xlab = "Components")
```

![](PCA_using_R_functions_files/figure-markdown_github/unnamed-chunk-10-1.png)

Another way to look at screeplots is by using a line plot instead

``` r
screeplot(my.prc, type = "line", main = "Scree Plot")
```

![](PCA_using_R_functions_files/figure-markdown_github/unnamed-chunk-11-1.png)

It is the same plot, instead of bars we have lines. We see that PC4 and PC5 are approaching zero. So using this criteria, I would keep principal components 1 definitely and 2 may be. Let's keep the first two principal components.

The next steps is to take a look at the principal components themselves.

``` r
my.prc$rotation
```

    ##               PC1         PC2        PC3        PC4        PC5
    ## Fire    0.5048229 -0.03090255  0.1890711 -0.8296296  0.1420061
    ## Theft   0.3090776 -0.83261158  0.2407813  0.2183405 -0.3249320
    ## Age     0.3985347 -0.19233538 -0.8163274  0.1235444  0.3500337
    ## Income -0.5050429 -0.45724546  0.1426136 -0.1371460  0.7047769
    ## Race    0.4855169  0.24441088  0.4685589  0.4795518  0.5049944

What we can see here is that each of the PC's are a linear combination of each of the variables. These coefficients are called as `loadings` or `rotations`. And they indicate to what extent that variable is correlated with that specific component. For example, in PC1, Fire and Income are the major contributors. Another concept here is that Income is being contrasted by the other variables, since it has an opposite sign. In PC2, we see that Theft is strongly correlated with principal component and is being contrasted by Race.

There is an easier way to look at these loadings than by inspecting the values. In our case we have 5 variables, but imagine having 100 variables. That becomes tedious. So we have an easy way to visualize this information. In other words, we need a better approach to examine these relationships. Now, ofcourse we can look at the "Bi-plot", but we will get to that momentarily. But one useful techniques is to look at the `dotplot` of the loadings.

``` r
load <- my.prc$rotation
sorted.loadings <- load[order(load[, 1]), 1]
myTitle <- "Loadings Plot for PC1" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")
```

![](PCA_using_R_functions_files/figure-markdown_github/unnamed-chunk-13-1.png)

Here we are seeing the loadings for PC1, it is much easier to see relationships amoung variables in this plot. We can see here that Fire, Race and Income are major contributors of PC1.

``` r
load <- my.prc$rotation
sorted.loadings <- load[order(load[, 1]), 2]
myTitle <- "Loadings Plot for PC2" 
myXlab  <- "Variable Loadings"
dotplot(sorted.loadings, main=myTitle, xlab=myXlab, cex=1.5, col="red")
```

![](PCA_using_R_functions_files/figure-markdown_github/unnamed-chunk-14-1.png)

In this case, we see that Theft is contributing to the PC2 component. In PC2, the dominant variable relate to Theft.

Of course the premier visualization tool for PCA is the "Bi-Plot". Recall, that the Bi in the Bi-Plot comes from the fact that we are plotting the variables as vectors onto the same graph as the observations as points. I will go ahead and call that function:

``` r
# Now draw the BiPlot
biplot(my.prc, cex=c(1, 0.7))
```

![](PCA_using_R_functions_files/figure-markdown_github/unnamed-chunk-15-1.png)

So, here is the "Bi-Plot". Now, we have already done the "dotplots" of the principal components, so we have some idea about the variables and observations. One thing you should recall about the variables is that the cosine of the angle between the vectors is infact the correlation between the variables. So for example, Income is strongly negatively correlated with Race and Fire. We already looked at the correlation and dotplots and knew that this might be the case. So in this component the PC1 we might consider that points towards income are contrasted with points close to Race and Fire. Let's take a look at observation 23 which is close to Race and Fire and observation 13 which is close to Income.

``` r
insurance[c(13, 23),]
```

    ##    ZIP Fire Theft  Age Income Race  Vol Invol Location
    ## 13  46  5.7    11 27.9  16250  1.0 12.1   0.0    North
    ## 23  12 36.2    41 63.1   6565 86.2  0.8   1.8    North

This is what the Bi-Plot shows.

Now when we look at PC2, the observations 45 is contrasted with observation 7. Take a look:

``` r
insurance[c(7,45),]
```

    ##    ZIP Fire Theft  Age Income Race  Vol Invol Location
    ## 7   11   11    75 42.6  21480  4.9 17.9   0.0    North
    ## 45  27    7     3 11.4  10080 47.4  7.7   0.2    South

In PC2, Theft is the major contributor, thus look at the contrast for observations 7 and 45.

Therefore, by looking at the Bi-plot, we would expect to find that points to be of a certain value based on where they are located.

Varimax rotation
----------------

One thing we can do to improve our understanding of Bi-plots and PCA is to apply something called `Varimax Rotation`. What that is, is that it is a change of co-ordinates such that it maximizes the sum of the variance of the squared loadings. It's whole goal is to clean up, if you will, all the rotations that we found when we called prcomp.

Varimax takes as an argument the rotation values returned from prcomp.

``` r
my.var <- varimax(my.prc$rotation)
```

Now, if we take a look at that, what we get is a rotation on top of a rotation.

``` r
my.var
```

    ## $loadings
    ## 
    ## Loadings:
    ##        PC1 PC2 PC3 PC4 PC5
    ## Fire               -1     
    ## Theft      -1             
    ## Age            -1         
    ## Income                  1 
    ## Race    1                 
    ## 
    ##                PC1 PC2 PC3 PC4 PC5
    ## SS loadings    1.0 1.0 1.0 1.0 1.0
    ## Proportion Var 0.2 0.2 0.2 0.2 0.2
    ## Cumulative Var 0.2 0.4 0.6 0.8 1.0
    ## 
    ## $rotmat
    ##           [,1]       [,2]       [,3]        [,4]       [,5]
    ## [1,] 0.4855377 -0.3090863 -0.3985350 -0.50481432 -0.5050259
    ## [2,] 0.2444596  0.8325943  0.1923584  0.03091725 -0.4572403
    ## [3,] 0.4685537 -0.2408082  0.8163166 -0.18906620  0.1426533
    ## [4,] 0.4795308 -0.2183660 -0.1235456  0.82963829 -0.1371254
    ## [5,] 0.5049755  0.3249310 -0.3500454 -0.14198911  0.7047885

Now, what we are seeing here in the loadings is that, for each Factor (that is PC column in loadings section) high correlation were resolved for few of these variables and the rest will be near-zero. So the blanks suggest that these variables are more or less towards zero after the varimax rotation, leaving us to consider that Race is the primary contributor of PC1 and Theft is the primary contributor in PC2.

So we refine our understanding of what is going on in PC1, that Race is the dominant contributor and not Fire or Income, which we originally thought are the primary contributors (by looking at the dot plots for the PC1). With regards to PC2, Theft is still the dominant contributor, which is consistent with what we found looking at the dotplot for PC2.

The Varimax rotation is a tweak and is part of the base R package, so you can use it whenever you do principal components analysis.
