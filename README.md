## Data-analysis-with-R

# Loading the crossnat dataset from directory
crossnat <- read.csv("crossnat_finalassign.csv", header = TRUE, sep = ";")
View(crossnat)
str(crossnat)

# Loading packages for importing data
I need to install the ggplot2 package in order to create graphics and map variables to aesthitics.
I need to install the psych package in order to describe the different variables.
I need to install the tidyverse package in order to solve the most common data manipulation challenges
I need to install the lsr package in order to provide tools for statistics
I need to install the foreign package in order to import the data file
I need to install the gmodels package in order to test a general linear hypothesis for a regression model

library(ggplot2)
library(psych)
library(tidyverse)
library(lsr)
library(foreign)
library(gmodels)
getwd()

# Question 1: Examine the basic statistics (including appropriate measures of central tendency and dispersion) of the interval level variable measuring corruption. What can you say about the shape of its distribution? What do the plots and tests tell you about the normality of the distribution? 
describeBy(crossnat$corrupt10)

The median (3.2) is the middle number in a sorted list of numbers. The mean (3.97) calculates the arithmetic mean of the elements of the numeric vector passed to its argument. The SD (2.09) computes the standard deviation of the values in x.

# Produce a boxplot for the interval level variable measuring corruption.
ggplot(subset(crossnat, !is.na(corrupt10)), aes(x = corrupt10)) + 
  geom_boxplot(fill="#FFDB6D", color="#56B4E9") +
  theme_bw() +
  coord_flip() +
  labs(title = "Boxplot of Corruption variable", y = "", x = "")
  
A boxplot is a graph that gives a good indication of how the values in the data are spread out (in this case, how the interval level variable measuring corruption level is spread out).
It is shown that the median is below the half of the interquartile range (between 2.5 - 3.5). The range of the interquartile range is from 2.5 to 5.0, with outliers above 7.5.

# Produce a density plot for the interval level variable measuring corruption.
ggplot(subset(crossnat, !is.na(corrupt10)), aes(x = corrupt10)) +
        geom_density(fill="#FFDB6D", color="#56B4E9") +
        theme_bw() +
        labs(title = "Density Plot of Corruption Variable", y = "", x = "")
        
Density plots are used to observe the distribution of a variable in a dataset. The peaks of a density plot display where values are concentrated over the interval.
In this graph, it is observed that the peak is just over 2.5, with the distribution being normal. The data is right-skewed (positive skewness).
  
# Create a one-sample t.test and confidence interval to test the hypothesis that the true mean is equal to 0.
t.test(crossnat$corrupt10)
t.test(crossnat$corrupt10, conf.level = .99)

Hypothesis is a premise or claim that we want to test. Null hypothesis is what is currently accepted as the correct value. In order to test the hypothesis that the true mean is equal to 0 (null hypothesis), an observation of the data is needed.
Based on the mean value above it can be seen that the mean is 3.969594=1. By creating a t.test, it can be observed that the p value is smaller than 2.2e-16 which is a very small value and smaller than 5%. Since the difference of the p value is statistically significant (smaller than the alpha level), then the null hypothesis is rejected.
The t.test() function uses a normal approximation to figure out the confidence interval. In the first code line, the confidence interval is 95%, which means there is a 95% confidence that the value is correct, and 5% chance that it is false. This approximation may not satisfy all the values to the fullest; thus, in the second code line, the confidence level is 99%.
Therefore, 95% of the time, when the confidence interval is calculated, the true mean will be between 3.653555-4.285626. 5% of the time, it will not fall between that interval. Thus, there is a 95% confidence level that the mean value is between that interval. Taking the confidence level to 99%, the true mean will be between 3.552527-4.386655. 1% of the time, it will not fall between that interval. Thus, there is a 99% confidence level that the mean value is between that interval.
Thus, both null hypothesis are rejected.

# Question 2: Based on the basic statistics above, create a new factor variable, grouping countries into low and high corruption groups. Explain how you created this variable. Bear in mind that higher values of the original variable indicate less corruption. 
crossnat$corruptionlevel[crossnat$corrupt10 <= 3.97] <- "high corruption"
crossnat$corruptionlevel[crossnat$corrupt10 > 3.97] <- "low corruption"

I created this variable by firstly naming it ‘corruptionlevel’ and differentiating it into two groups: ‘high corruption’ and ‘low corruption’. Since higher values of the original variable indicate less corruption, the value that indicates whether the level is high or low is ‘3.97’. I used the value 3.97 because it is the mean of the variable.

# Question 3: Using cross-tabulations and appropriate statistics examine the relationship between OECD membership and the variable you created above of low or high corruption. What are your conclusions? 
ggplot(subset(crossnat, !is.na(corruptionlevel) & !is.na(ocde)), aes(x = corruptionlevel, fill = ocde)) +
        geom_bar(position = "fill") +
        labs(title = "Bar chart showing levels of corruption by OECD membership", x = "Corruption Level", fill = "OECD Membership")
        
This barchart shows the relationship between OECD membership and the corruption level of countries. It is shown that a significant number of countries that are not OECD members have high corruption levels, whilst countries that are OECD members have lower corruption levels. Last but not least, this barchart shows the proportion of countries in the y-axis, instead of the number of people.

# In crosstable, the first argument is the name of the variable defining the rows (corruptionlevel) and the second argument is the name of the variable defining the columns (ocde).
table(crossnat$corruptionlevel, crossnat$ocde)
CrossTable(crossnat$corruptionlevel, crossnat$ocde, format = c("SPSS"), chisq = TRUE, prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE, asresid = TRUE)

The first table shows the proportion of the countries that have OECD membership in relation to their level of corruption. For countries that are not OECD members, 106 countries have high corruption levels, and 32 have low corruption levels. For countries that are OECD members, 3 countries have high corruption levels, and 30 countries have low corruption levels. The numbers are significantly higher in the ‘no OECD memer’ when examining high corruption levels in both columns. On the contrary, when examining low corruption levels, in both columns the numbers are very similar.
There are 106 with no OECD membership countries that have high corruption levels, this represents the 97.248% of all the countries who have high corruption levels. There are 32 with no OECD membership countries that have low corruption levels, this represents the 51.613% of all the countries who have low corruption levels.
There are 3 with a OECD memership countries that have high corruption levels, this represents the 2.752% of all the countries who have high corruption levels. There are 30 with a OECD membership countries that have low corruption levels, this represents the 48.387% of all the countries who have low corruption levels.
The row total shows that there are 109 countries who have high corruption levels, this represents the 63.743% of all the countries. The row total also shows that there are 62 countries who have low corruption levels, this represents the 36.257% of all the countries.
Thus, it is apparent that countries with high corruption levels are much more than countries with low corruption levels. Regarding the relationship between OECD membership and corruption levels, it is shown that generally, high corruption level countries are more than the low corruption level countries.
Last but not least, the Chi-squared test is a statistical method which determines if two variables have a significant correlation between them. Regarding the p value, it is 3.607187e-13 which is very low (0.05 being the standard alpha value). So there is a high chi-squared value (52.84618) and a p-value of less than 0.05 significance level. Thus, there is a statistically significant relationship between these two variables (corruption levels and OECD membership) and the null hypothesis is rejected (null hypothesis is that there is not a relationship between corruption levels and OECD membership).

# To measure the association between two values, Cramer’s V needs to be used.
myTab <- table(crossnat$corruptionlevel, crossnat$ocde)
cramersV(myTab)

Cramer’s V varies from 0 to 1, with 1 indicating a perfect association. In this case, Cramer’s V is 0.5405034, which is in the middle of 0 and 1. This indicates that there is a moderate relationship between corruption levels and OECD membership.
–> In conclusion, based on the above, it seems that there is a moderate relationship between corruption levels and OECD membership.

# Question 4: Can you detect significant differences in the infant mortality levels across the different levels of categorised HDI (variable cathdi)? Please use pairwise comparisons and effect size measures if appropriate and discuss. (20% mark)
# comparing two independent samples of infant mortality and cathdi
describeBy(crossnat$infantmortality, crossnat$cathdi)

Looking at the results, there are big differences in the variables across the two groups. 
The mean of an observation variable is a numerical measure of the central location of the data values. Low HDI has the highest mean (63.06) and the very high HDI has the lowest mean (5.1), this is consistent with the boxplot displayed below.
The median of an observation variable is a value separating the higher half from the lower half. In this dataset, the median of the low HDI is 66, and the median of the very high HDI is 4. Hence, there is a difference in the median as well.
The standard deviation (SD) of an observation variable is the square root of its variance. In the table, SD is lower in the very high HDI (3.27) and higher in the low HDI (24.36)

# In order to examine the data, ggplot2 needs to be used to create plots.
# Re-arranging the levels of cathdi
crossnat$cathdiR <- factor(crossnat$cathdi, levels = c('Low HDI', 'Medium HDI', 'High HDI', 'Very High HDI'))
ggplot(subset(crossnat, !is.na(cathdiR) &!is.na(infantmortality)), aes(colour = cathdiR, y = infantmortality)) +
        geom_boxplot() +
        theme_bw() +
        labs(title = "Boxplot showing infant mortality by HDI", x = "HDI", y = "Infant Mortality", colour = "HDI")

Based on the plot, the median increases accordingly with the HDI levels (starting from very high HDi to low HDI). The only ‘abnormality’ of the plot appears in the ‘low HDI’ where there is an outlier in a low infant mortality value.

ggplot(subset(crossnat, !is.na(infantmortality) &!is.na(cathdiR)), aes(x = infantmortality, colour = cathdiR)) +
        geom_density() +
        theme_bw() +
        labs(title = "Density plot showing infant mortality by HDI", x = "Infant Mortality", y = "HDI", colour = "HDI")
        
Density plot visualizes the distribution of data over a continuous interval. The peak of the values show where the values are concentrated over the interval. In this density plot it is shown that the very high HDI group has higher density than the rest of the groups.
The way the distributions are spread (the variance they capture) do not differ that much, except of the ‘very high HDI’ distribution which starts with very high density and then falls to 0. The Kurtosis in the very high HDI group is very pointy, in the high HDI group somewhat pointy, whilst in the rest of the groups is not. In terms of the variance, it appears to have a range of differences. 

# Some differences in means are apparent. In order to infer that they exist in the population from which the sample was drawn, ANOVA will be used.
# I need to install the rstatix and coin packages in order to perform basic statistical tests.
library(rstatix)
library(coin)
infantmortalityBycathdiR <- aov(infantmortality ~ cathdiR, data=crossnat)
summary(infantmortalityBycathdiR)

ANOVA contrasts the variability between the groups with the variability within the groups to produce a ratio. If there is more variability between the groups than within the groups the more confidence there is in a conclusion that the population means are not equal. At the same time, if there is more variation within the groups than between the groups, the weaker the ground will be to conclude that the population means are different. The F distribution is another way to find the probability distributions.
The result is significant suggesting that there are some significant differences across the marital status in fear of becoming victims of property crime.

# An alternative to the standard on-way ANOVA in the situation where the homogeneity of variance assumption is violated is the Welch’s test for unequal variances.
oneway.test(infantmortality ~ cathdiR, data = crossnat)

In this analysis, it can be observed that the conclusion is the same. The low p value is closely associated with the observed F value above. Since the p value is small and thus statistically significant, the null hypothesis is rejected.

Based on the above, it is observed that there are differences in the infant mortality levels across the different levels of categorised HDI (variable cathdi). However, it is not established which of those differences are significant. In order to find this, post-hoc comparisons must be run.

# The pairwise.t.test() function runs multiple t tests.
pairwise.t.test(crossnat$infantmortality, crossnat$cathdiR, p.adjust.method = "bonferroni")

This test shows the p values for each of the different HDI levels. Since the p value is small in all the categories and thus statistically significant, the null hypothesis is rejected (null hypothesis is that there is a relationship between infant mortality levels and HDI levels).

# effect size for ANOVA (eta squared = R-squared)
summary.lm(infantmortalityBycathdiR)

This summary function invokes the results obtained above through ANOVA. The eta squared (aka percent of variance explained) is 0.6609 and it shows the proportion of the total sum of squares that is accounted by the between sum of squares. The larger the proportion, the stronger the relationship between the metric and the categorical variable.
In this summary, the value of the eta squared (square root of 0.6609) is 0.813. This suggests that HDI levels have a big effect on the infant mortality levels.
–> In conclusion, based on the above, there is not a big difference between the infant mortality levels across the different levels of categorised HDI (variable cathdi).

# Question 5: Develop a multiple regression model to predict prison rate using at least three predictor variables (you can use the ones that you think are most reasonable, but do justify why you chose them). Discuss your results. (40% mark)
Using prisonrate as a dependent variable, the three independent variables I am choosing are: homicide (=Homicide rate), hdinxe (=Human Development Index (HDI)), and youthunemp (=Proportion of Unemployed Youth). I chose these variables because homicide leads to prison; as proven above, low HDI leads to low corruption levels and therefore higher chances of imprisonment; and unemployment also leads to crime and hence imprisonment.

# In order to choose these specific variables, I need to use the dplyr package.
library(dplyr, quietly = TRUE, warn.conflict = FALSE)
mydataR <- select(crossnat, prisonrate, homicide, hdinxe, youthunemp)
sapply(mydataR, summary)

This summary function invokes the results obtained above through ANOVA. The eta squared (aka percent of variance explained) is 0.6609 and it shows the proportion of the total sum of squares that is accounted by the between sum of squares. The larger the proportion, the stronger the relationship between the metric and the categorical variable.
In this summary, the value of the eta squared (square root of 0.6609) is 0.813. This suggests that HDI levels have a big effect on the infant mortality levels.
–> In conclusion, based on the above, there is not a big difference between the infant mortality levels across the different levels of categorised HDI (variable cathdi).

# correlation matrix: each intersection has a specific correlation between the variables that tells us how strong (regarding the other variables) the variables are.
cor(na.omit(mydataR))

Correlation is a standarised covariance measures that tells both the strength and direction of the relationship between two variables (i.e. as one variable increases in value, does the other variable also increase?). Correlation coefficients can only take on values between -1 and +1.
Based on this, homicide and prisonrate has a value of 0.2226751 which shows a weak moderate relationship between homicide rates and prison rates. Prisonrate and hdinxe has a value of -0.06956904 which indicates a weak negative relationship between prison rates and human development index rates. Finally, prisonrate and youthunemp has a value of 0.01343352 which indicates a weak positive relationship between prison rates and the proportion of youth unemployment.
Thus, the strongest positive relationship between prisonrate and the independent variables is between prisonrate and homicide (0.22267510). The strongest negative relationship between prisonrate and the independent variables is between prisonrate and hdinxe (-0.06956904).

# Scatterplot matrix gives an idea of the relaitonship between each pair of variables in the dataset.
pairs(mydataR,
      col = c('blue', 'red'),
      labels = c('Prisonrate', 'Homicide', 'HDI', 'Youth Unemployment'),
      main = 'Relationship between variables')

Scatterplots show possible associations or relationships between variables. This is particularly useful for multiple regression where one of the assumptions is that each independent variable (prisonrate) shows a linear relationship with the dependent variables (homicide, HDI, and youth unemployment).
If the data show an uphill pattern as you move from left to right, this indicates a positive relationship between X and Y. As the X values increase (move right), the Y values tend to increase (move up). If the data don’t seem to resemble any kind of pattern, then no relationship exists between X and Y.
Taking prison rate as the dependent variable, it seems that there is a weak relationship between any of the independent variables.

# Model3, seeing the relationship betewen homicide, corruption, HDI, and youth unemployment
model3 <- lm(prisonrate ~ homicide + hdinxe + youthunemp, data = crossnat)
summary(model3)
confint(model3)

This table shows the relationship between prisonrate, homicide rate, human index development and the proportion of youth unemployment. The estimate value for homicide is 2.7862, for human index development 1.8953 and for the proportion of youth unemployment -0.1809, which means that when all the features are at 0, the expected response is the intercept value 2.7862, 1.8953 and -0.1809 respectively.
The standard error is the standard error of the estimate, which allows the construction of marginal confidence intervals for the estimate of that particular feature. The value for homicide is 1.5496, for human index development 129.6774 and for the proportion of youth unemployment 1.0598. Looking at the confint model as well, the entire confidence interval for homicide is -0.3059401, for the human index development -256.8718999 and for the proportion of youth unemployment -2.2955901.
A t test can also be seen in the table. These numbers test whether each of these predictors are associated with the response variable when adjusting for the other variables in the model. In this case, it can be seen that the variable is associated with the response variable.
The pr(>|t|) value is the p value for the individual coefficient. If that probability is sufficiently low, the null hypothesis is rejected (null hypothesis is that the coefficient is 0). Based on this, the value is low and we can reject the null hypothesis.
Regarding the R square, its value is 0.008098. R square tells us what proportion of the variance is explained by the model and the residual standard deviation tells how the model fits the data. The adjusted R square deals with an increase in the square spuriously due to adding features, essentially fitting noise in the data. As the number of features p increases, the required R square needed will increase as well to maintain the same adjusted R squared. This means that homicide, human development index and the proportion of youth unemployment explains approximately 0.08% of the variability in prison rate.
p value relates to testing the hypothesis whether the coefficient is 0 or not. The p value is high (0.319) (as it is bigger than 0.05) and therefore the null hypothesis is not rejected (null hypothesis being that there is not a relationship between prisonrate, homicide rate, human index developent and proportion of youth unemployment). Thus this effect is not statistically significant because of the p value.
Homicide has the strongest effect on prisonrate based on the data.
Thus, there is not a strong relationship between prisonrates, homicide, hdinxe and youth unemployment.

# Loading the package car as a companion to applied regression.
library(car)
# In assessing the impact that extreme observations (outliers) may have in the results, the below need to be tested.
# Checking parametric assumptions and whether our model fits in regards to the normality of residuals
plot(density(rstudent(model3)),
     col = 'blue',
     main = 'Density Plot for model3')

Density plot visualises the distribution of data over a continous interval. The peaks of a density plot help display where values are concentrated over the interval. Residuals have a normal distribution. closer to the negative axis and start of the positive axis. The mean is between -1 and 0. It is also worth mentioning that between 3-5 the density value is 0, and after it goes higher again.

# homoskedaticity.
# Want to see how the residuals (difference between predicted and actual values) are different between the regression line.
# id.n demonstrates how to identify some of the outline observations.
qqPlot(model3, id.n = TRUE)
ncvTest(model3)

Homoscedasticity describes a situation in which the error term is the same across all values of the independent variables. The normal probability plot of residuals should approximately follow a straight line. In this plot, all the points fall approximately along this reference line and thus demonstrate normality. However there is one value (189) that is outside the reference line.
The p value is also high (0.13847) (0.05 being the alpha p value).

# Outliers and influential observations
outlierTest(model3)
influencePlot(model3, id.n = 1)

The outliers function locates the largest Studentised residuals in absolute value and computes a Bonferroni-corrected t test. If the probability value is associated with this test is smaller than the alpha value, then the observation is likely an outlier. The Bonferroni adjustment multiplies the usual two-sided p value by the number of observations. Based on this, the probability value is bigger than the p value so the observation is unlikely to be an outlier.
For identifying influential observations, visual method will be used as well. By using the influence plot, a bubble plot will be produced which combines measures of the residuals and other statistics (hat values and Cook distance) oriented to identify influential observations. The size of the bubble gives the Cook distance, which is a statistic that tries to summarise how much the regression coefficients may change without that particular case. The size of the bubble refers to its distance (how far that particular observation pulls the regression line towards itself).
In this bubble plot, it can be seen that there are some values in the plot (189, 144, 190, 84) that have high residual (144, 189 a positive residual, whilst 190, 84 a negative residual) and they pull the regression towards themselves more than the others. In addition, observation 144 and 186 have a higher value than expected (2.629318 and 5.936043 respectively).

# Multicollinearity - correlation between the predictors.
vif(model3)

Vif is a measure for how easily it is predicted from a linear regression using the other predictors.Typically a VIF larger than 5 or 10 indicates serious problems with collinearity. In this dataset, this does not seem to be the case, thus there is no problem with multicollinearity.
