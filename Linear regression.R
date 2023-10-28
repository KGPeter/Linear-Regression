install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")


#Loading the Packages
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

#importing the dataset
income.data <- read.csv("D:/R/Datasets/income.data.csv")
income.data

#To check that my data has been loaded okay
summary(income.data)

#We check that our data meet the main assumptions of linear regression

#1..Autocorrelation - since we have one independent variable and one dependent variable then we do not need to test

#2..Normality - we use hist to check whether the dependent variable follows a normal distribution
hist(income.data$happiness)

#obs are bell shaped so we proceed

#3..Linearity - the relationship between the ind variable and dep variable must be linear
plot(happiness ~  income, data =income.data)

#the rship looks roughlr linear

#4..Homoscedasticity - we'll test this later: the prediction error doesn't change significantly over the range of prediction of the model

#PERFORM THE LINEAR REGRESSION ANALYSIS
#Incomes range from $15k to $75k
#Happiness is measured on a scale of 1 to 10
income.happiness.lm <- lm(happiness~income, data = income.data)
summary(income.happiness.lm)

#The Coefficients section shows:

     #1.The estimates (Estimate) for the model parameters – the value of the y-intercept (in this case 0.204) and the estimated effect of income on happiness (0.713).
     #2.The standard error of the estimated values (Std. Error).
     #3.The test statistic (t value, in this case the t statistic).
     #4.The p value ( Pr(>| t | ) ), aka the probability of finding the given t statistic if the null hypothesis of no relationship were true.

#The final three lines are model diagnostics – the most important thing to note is the p value
#(here it is 2.2e-16, or almost zero), which will indicate whether the model fits the data well.
#From these results, we can say that there is a significant positive relationship between
#income and happiness (p value < 0.001), with a 0.713-unit (+/- 0.01) increase in happiness
#for every unit increase in income.

#Now to check for homoscedasticity
par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))
plot(income.happiness.lm)


#par(mfrow()) command will divide the Plots window into the number of rows and columns specified in the brackets.
#Residuals are the unexplained variance. They are not exactly the same as model error, but 
#they are calculated from it, so seeing a bias in the residuals would also indicate a bias in
#the error

#The most important thing to look for is that the red lines representing the mean of the 
#residuals are all basically horizontal and centered around zero. This means there are no 
#outliers or biases in the data that would make a linear regression invalid

#In the Normal Q-Qplot in the top right, we can see that the real residuals from our model 
#form an almost perfectly one-to-one line with the theoretical residuals from a perfect model.

#Based on these residuals, we can say that our model meets the assumption of homoscedasticity.



#VISUALIZE THE RESULTS WITH A GRAPH
#We shall follow four steps:

   #1...Plot the data points on a graph
income.graph <- ggplot(income.data, aes(x=income, y=happiness))+
  geom_point()
income.graph


   #2..Add the linear regression line to the plotted data
#Add the regression line using geom_smooth() and typing in lm as your method for creating 
#the line. This will add the line of the linear regression as well as the standard error 
#of the estimate (in this case +/- 0.01) as a light grey stripe surrounding the line:

income.graph <- income.graph + geom_smooth(method = "lm", col="black")
income.graph


   #3..Add the equation for the regression line
income.graph <- income.graph +
  stat_regline_equation(label.x = 3, label.y = 7)
income.graph


#Make the graph ready for publication
income.graph +
  theme_bw()+
  labs(title = "Reported happiness as a function of Income",
       x = "Income (*$10,000)",
       y = "Happiness Score (0 to 10)")
