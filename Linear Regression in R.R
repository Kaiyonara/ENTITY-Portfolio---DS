#Lesson 8 Practice Hands-On
#Kai Bartlette

mtcars

#Create a scatter plot with a trend line where the horizontal axis is engine 
#horsepower and the vertical axis is quarter mile time. What is the relationship
#between time and engine horsepower: positively correlated, negatively 
#correlated, or uncorrelated?

The relationship between time and engine horsepower is negatively correlated.

d <- ggplot(mtcars, aes(x = hp, y = qsec))
d + geom_point() + geom_smooth(method=lm, se=FALSE)
  
#Compute the linear regression for time and engine horsepower.  

lin_reg <- lm(qsec ~ hp, mtcars)
print(lin_reg)

#Gives us:

Call:
  lm(formula = qsec ~ hp, data = mtcars)

Coefficients:
  (Intercept)           hp  
20.55635     -0.01846  
  
#What is the equation of the line?

y = -0.01846x + 20.55635

summary(lin_reg)

#Gives us:

Call:
  lm(formula = qsec ~ hp, data = mtcars)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.1766 -0.6975  0.0348  0.6520  4.0972 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 20.556354   0.542424  37.897  < 2e-16 ***
  hp          -0.018458   0.003359  -5.495 5.77e-06 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.282 on 30 degrees of freedom
Multiple R-squared:  0.5016,	Adjusted R-squared:  0.485 
F-statistic: 30.19 on 1 and 30 DF,  p-value: 5.766e-06

#What is the R-squared value? 

The adjusted R squared value is 0.485, so the line explains approx. 48.5% of 
the variability of the data, or that horsepower explains about 48.5% of the
factors that go into quarter mile times.

#Is this what you would expect?

Yes, there is a negative correlation, which the best fit line illustrates,
that explains about half the reason why quarter miles times drop with an
increase in horsepower - they are more powerful and moving faster.

#Create a scatter plot with a trend line where the horizontal axis is vehicle 
#weight and the vertical axis is quarter mile time. What is this relationship: 
#positively correlated, negatively correlated, or uncorrelated?

k <- ggplot(mtcars, aes(x = wt, y = qsec))
k + geom_point() + geom_smooth(method=lm, se=FALSE)
  
#Compute the linear regression for these two variables. What is the equation of 
#the line? What is the R-squared value? Is this what you would expect?

lin_reg1 <- lm(qsec ~ wt, mtcars)
print(lin_reg1)

#Gives us:

Call:
  lm(formula = qsec ~ wt, data = mtcars)

Coefficients:
  (Intercept)           wt  
18.8753      -0.3191  

#The equation of the line is:

y = -0.3191x + 18.8753

summary(lin_reg1)

#Gives us:

Call:
  lm(formula = qsec ~ wt, data = mtcars)

Residuals:
  Min      1Q  Median      3Q     Max 
-3.3638 -1.0766  0.2051  0.8655  5.0298 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  18.8753     1.1025  17.120   <2e-16 ***
  wt           -0.3191     0.3283  -0.972    0.339    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.789 on 30 degrees of freedom
Multiple R-squared:  0.03053,	Adjusted R-squared:  -0.00179 
F-statistic: 0.9446 on 1 and 30 DF,  p-value: 0.3389

#What is the R squared value?

The R squared value is -0.00179. 

Judging by the graphical model and this very small r-squared value, the
variables are not related.
  
#Create a report (MS Powerpoint or equivalent) that shows your results and the 
#code you used to generate the results. Please include your interpretation of 
#the data included and answer all the questions posed above.


