#Lesson 10 Final Project
#Kai Bartlette

#RStudio was open so we don't need to load in libraries, but if we were in a
#new session:

library("ggplot")
library("dplyr")


install.packages("Ecdat")
#Installed successfully to 	
#C:\Users\kaiba\AppData\Local\Temp\Rtmp0iLlub\downloaded_packages

library(Ecdat)
#Makes the package accessible to this R session.

head(Cigarette)
#Shows the first six rows of the data frame Cigarette.

#state: the two letter abbreviation for the state.
#year: the year.
#cpi: consumer price index for the year.
#pop: state population
#packpc: average number of packs of cigarettes per capita per year
#income: total state personal income.
#tax: average state, federal, and average local excise taxes for fiscal year.
#avgprs: average price per pack during fiscal year, including sales taxes, in cents.
#taxs: average excise taxes per pack for fiscal year, including sales taxes, in cents.


#1. Create a boxplot of the average number of packs per capita by state.

ggplot(Cigarette, aes(x = state, y = packpc)) + geom_boxplot()

#1a. Which states have the highest number of packs? Which have the lowest?

#New Hampshire, Kentucky and North Carolina have the highest numbers.
#Utah, California and New Mexico have the lowest.


#2. Find the median over all the states of the number of packs per capita for each 
#year. Plot this median value for the years from 1985 to 1995. What can you say 
#about cigarette usage in these years?

median_packpc <- Cigarette %>% group_by(year) %>% summarise(Median = median(packpc))

#Before even doing the plot, you can tell that cigarette usage was in steady
#decline as the years advance.

unique(Cigarette$year)
#To verify we are only considering 1985 - 1995.

ggplot(median_packpc, aes(x = year, y = Median)) + geom_line()
ggplot(median_packpc, aes(x = year, y = Median)) + geom_point()


#3. Create a scatter plot of price per pack vs number of packs per capita for all 
#states and years.

ggplot(Cigarette, aes(x = avgprs, y = packpc)) + geom_point() + geom_smooth(method = lm)


#4. Are the price and the per capita packs positively correlated, negatively 
#correlated, or uncorrelated? Explain why your answer would be expected.

cor.test(Cigarette$avgprs, Cigarette$packpc, method = "pearson", use = "complete.obs")

#Yes, they are significantly negatively correlated.

#The price and the per capita packs are negatively correlated, meaning, as the
#average number of packs of cigarettes per capita per year decrease, the 
#average price per pack during the fiscal year, including sales taxes, in cents, 
#increased. This unfortunately makes sense because, if a cigarette
#manufacturer is looking to maintain profitability when the overall population
#of smokers is dropping, they will raise the price to maintain profitability.

#Pearson's product-moment correlation

#data:  Cigarette$avgprs and Cigarette$packpc
#t = -16.562, df = 526, p-value < 2.2e-16
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.6388606 -0.5264104
#sample estimates:
#  cor 
#-0.5854443


#5. Change your scatter plot to show the points for each year in a different 
#color.

ggplot(Cigarette, aes(x = avgprs, y = packpc, color = year)) + geom_point() + geom_smooth(method = lm)

#5a. Does the relationship between the two variables change over time?

#The relationship changes in the same way as I mentioned above; cigarettes grew
#to be more expensive in latter years.


#6. Do a linear regression for these two variables. How much variability does the 
#line explain?

reg_avgprs_packpc <- lm(packpc~avgprs, Cigarette)
summary(reg_avgprs_packpc)

#Call:
#lm(formula = packpc ~ avgprs, data = Cigarette)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-56.977  -9.710  -0.716   8.550  69.451 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 167.87737    3.79749   44.21   <2e-16 ***
#  avgprs       -0.40879    0.02468  -16.56   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 18.76 on 526 degrees of freedom
#Multiple R-squared:  0.3427,	Adjusted R-squared:  0.3415 
#F-statistic: 274.3 on 1 and 526 DF,  p-value: < 2.2e-16

#This is what we estimate the packpc to be (0.04) based on a one unit decrease 
#in avgprs. We expect that packpc is going to increase by .04 for every one unit 
#increase in avgprs.

#This is significant because it is less than p of .05.

#In fact the significance code of *** makes us very confident of it's significance.

#Adjusted R-squared accounts for 34% of everything that influences packpc.
#The line explains 34% of the variability.


#7. The plot above does not adjust for inflation. You can adjust the price of a
#pack of cigarettes for inflation by dividing the avgprs variable by the cpi
#variable. 

price_inflation <- Cigarette %>% mutate(Inflation = avgprs / cpi)
View(price_inflation)

#7a. Create an adjusted price for each row, then re-do your scatter plot and
#linear regression using this adjusted price.

ggplot(price_inflation, aes(x = Inflation, y = packpc, color = year)) + geom_point() + geom_smooth(method = lm)

#8. Create a data frame with just the rows from 1985. 

rows_1985 <- Cigarette %>% filter(year == 1985)
#48 obs. of 9 var.

#Create a second data frame with just the rows from 1995. 

rows_1995 <- Cigarette %>% filter(year == 1995)
#48 obs. of 9 var.

#When you're doing a t-test, and a paired t-test in particular, you need to 
#have the same number of observations.

#8a. Then, from each of these data frames, get a vector of the number of packs per 
#capita. 

View(rows_1995)

#8b. Use a paired t-test to see if the number of packs per capita in 1995 
#was significantly different than the number of packs per capita in 1985.

t.test(rows_1995$packpc, rows_1985$packpc, paired = TRUE)

#Paired t-test

#data:  rows_1995$packpc and rows_1985$packpc
#t = -14.789, df = 47, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -29.20576 -22.21151
#sample estimates:
#  mean of the differences 
#-25.70863 

#The number of packs per capita in 1995 was significantly different than the 
#number of packs per capita in 1985.

#9. In the process of doing this project, have any questions come to mind that 
#this data set could answer? If so, pick one and do the analysis to find the 
#answer to your question.

#9a. Was there a significant difference in taxation of cigarettes from 1995 to 1985?

t.test(rows_1995$tax, rows_1985$tax, paired = TRUE)

#Paired t-test

#data:  rows_1995$tax and rows_1985$tax
#t = 12.641, df = 47, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  18.51281 25.52052
#sample estimates:
#  mean of the differences 
#22.01667 

#Average state, federal, and average local excise taxes for fiscal year 1995 was
#significantly different than the same taxes in 1985.

ggplot(Cigarette, aes(x = tax, y = packpc, color = year)) + geom_point() + geom_smooth(method = lm)

#9b. Are the tax and the per capita packs positively correlated, negatively 
#correlated, or uncorrelated? 

cor.test(Cigarette$tax, Cigarette$packpc, method = "pearson", use = "complete.obs")

Pearson's product-moment correlation

data:  Cigarette$tax and Cigarette$packpc
t = -15.817, df = 526, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.6228942 -0.5069682
sample estimates:
       cor 
-0.5677393 

#They are significantly negatively correlated.








