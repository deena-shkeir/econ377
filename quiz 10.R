# questions on quiz 10 #1, #2, #3, #4, #6, #7, #9, #11, #14, #16, #17
#1
#A regression gives log(y) = 0.5 + 0.065x1 - 0.019x2. If x1 rises by 4 and x2
#rises by 3, by about what percent does predicted y change? 
#Hint: log-level MLR: %ychange = 100*(B^1*x1change + B^2*x2change) Here B^2 is negative
b1 = 0.065
b2 = -0.019
x1change = 4
x2change = 3
ychange = 100*(b1*x1change+b2*x2change)
ychange

#2
#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer).
#Run reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1). 
#By about what percent does predicted wage change when experience rises by 2 years and tenure rises by 1 year, holding education fixed? 
#Hint: %wage change = 100*(B^exper * 2 + B^tenure *1)
library(wooldridge)
data = wage1
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b1 = reg$coefficients[2]
b2 = reg$coefficients[3]
b3 = reg$coefficients[4]
exper_change = 2
tenure_change = 1
wage_change = 100*(b2*exper_change + b3*tenure_change)
wage_change

#3
#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). 
#Using the same regression of log(wage) on educ, exper and tenure, 
#what is the predicted hourly wage, in dollars, of a worker with 12 years of education, 10 years of experience and 3 years of tenure?
#Hint: predict log(wage) first, then undo the log with exp(): exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3)
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
b = reg$coefficients
educ = 12
exper = 10
tenure = 3
wage = 100*(b[2]*educ + b[3]*exper + b[4]*tenure)
exp(b[1] + b[2]*educ + b[3]*exper + b[4]*tenure)

#4
#After an OLS multiple regression of Y on X1,...,Xk (with an intercept) and residuals U^i = Yi - Y^i
#which statements are always true? Select all that apply.
#Hint: the notes list three properties, the same as in simple regression. Careful: the residuals are uncorrelated with the fitted values, not with Y itself.
# The sample covariance between each Xj and the residuals is 0.
# The sample average of the residuals is 0.
# The regression passes through the point of sample means: Ybar = B^0 + B^1Xbar1 + ... + B^kXbark

#6
#A multiple regression gives y^ = 5.8 + 0.9x1 - 2.4x2. The sample means of the independent variables are 
#xbar1 = 13.2 and xbar2 = 2.5. What is the sample mean ybar?
#Hint: the OLS regression passes through the point of means:ybar = B^0 + B^1xbar1 + B^2xbar2. here B^2 is negative
b0 = 5.8
b1 = 0.9
b2 = -2.4
xbar1 = 13.2
xbar2 = 2.5
ybar = b0 + b1*xbar1 + b2*xbar2
ybar

#7
#Which of these regressions violate MLR3 (no perfect collinearity), so that OLS cannot estimate them? Select all that apply.
# wage on income in dollars and income in thousands of dollars
# a regression with 5 independent variables, estimated on 4 observations
# voteA on expendA, expendB and totexpend, where totexpend = expendA + expendB

#9
#Which situation makes MLR4, E[U|X1, ..., Xk] = 0, fail (so that the X variables are endogenous)?
#Hint: MLR4 can fail because of omitted variables or the wrong functional form.
# A variable that affects Y and is correlated with X1 is left out of the regression.

#11
#Formula (3.23): B~1 = B^1 + B^2*delta~1. The simple regression of Y on X1 gives B~1 = -1.27
#The multiple regression of Y on X1 and X2 gives B^2 = 0.55, and the slope from regressing X2 on X1
#is delta~1 = -0.66. Find the multiple-regression slope B^1
#Hint: solve (3.23) for B^1: B^1 = B~1 - B^2*delta~1 watch the signs
b1_titla = -1.27
b2 = 0.55
delta1_tilta = -0.66
b1 = b1_titla - b2*delta1_tilta
b1

#14
#Data: bwght (bwght = a baby's birth weight in ounces; cigs = cigarettes the mother smoked per day during pregnancy; faminc = the family's income in thousands of dollars). 
#Regress faminc (the variable left out of the simple regression in problem 12) on cigs, lm(faminc ~ cigs, data = bwght), to get delta~1.
#Compute B^2 * delta~1, the faminc coefficient from the multiple regression times this slope. 
#Hint: by (3.23), B^2*delta~1 = B1~ = B^1: the simple-regression cigs slope minus the multiple-regression one. Use that to check your answer.
library(wooldridge)
data = bwght
reg <- lm(bwght ~ cigs + faminc, data = bwght)
reg2 = lm(faminc ~ cigs, data = bwght)
b2 = reg$coefficients["faminc"]
delta1_tilta = reg2$coefficients["cigs"] 
b2*delta1_tilta

#16
#An omitted variable X2 lowers Y (B2<0), and X2 is negatively correlated with the included X1.
#The simple-regression slope on X1 is:
#Hint: Table 3.2: Bias(B1~) = B2 *delta1~ and delta1~ has the sign of Cor(X1, X2)
# biased upward

#17
#A researcher regresses crop yield on fertilizer use across farms, leaving out land quality. Better land raises yield, and farmers tend to use more fertilizer on better land. The simple-regression fertilizer slope most likely:
#Hint: two legs: the sign of land quality's effect on yield (B2) and the sign of its correlation with fertilizer (delta1~)
# overstates the effect of fertilizer (upward bias)
