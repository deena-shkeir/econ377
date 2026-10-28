# Quiz 9 questions  1, 2, 3, 4, 6, 7, 9, 12, 13, 14, 15

#1 
#A regression gives test scores = 12 + 8log(income) what does the 8 mean
## A 1% rise in income is associated with a rise of about 0.08 points in the predicted test score.

#2
#A log-level regression gives log(y) = 2.3 + 0.01x by abt what percent does y change when x falls by 3
#Hint: log-level: %ychange = 100*B^1*xchange
b1 = 0.01
xchange = -3
ychange = 100*b1*xchange
ychange

#3 
#Data: bwght (bwght = a baby's birth weight in ounces; faminc = the family's income in thousands of dollars). 
#Run the log-log regression lm(log(bwght) ~ log(faminc), data = bwght). By about what percent does predicted birth weight change when family income rises by 20%?
#Hint: log-log: %ychange = B^1 * %xchange
library(wooldridge)
data("bwght")
reg = lm(log(bwght) ~ log(faminc), data = bwght)
b1 = reg$coefficients[2]
xchange = 20
ychange = b1*xchange
ychange

#4
#Suppose you could draw 1,000 different random samples from the same population and compute 
#B^1 in each one. If OLS is unbiased (SLR1–SLR4 hold), which statement is correct?
## The average of the 1,000 estimates is close to B^1

#6
#Under SLR1–SLR5, Var(B^1)= o^2/SSTx. Holding everything else fixed, which changes would make Var(B^1) larger? 
## X values bunched more tightly together (a smaller SSTx)
## A smaller sample (fewer observations with X spread the same way, so a smaller SSTx)
## A larger error variance o^2

#7 
#A simple regression on n = 25 observations has ohat = 8.7, the sample standard deviation of 
#X is Sx = 0.6. Find the standard error se(B^1)
#Hint: SSTx = (n-1)Sx^2 (square the standard deviation), then se(B^1) = ohat/sqrt(SSTx)
n = 25
ohat = 8.7
Sx = 0.6
SSTx = (n-1)*Sx^2
se = ohat/(sqrt(SSTx))
se

#9
#Under SLR1–SLR5, a regression has sd(B^1) = 1.09 If the sample were 16 times as large, with the 
#X values spread out in the same way (so SSTx is also 16 times as large), what would sd(B^1) be? 
#Hint: sd(B^1) = o/sqrt(SSTx) multiplying SSTx by k divides the sd by sqrt(k) not by k
sd = 1.09
k = 16    #sample size multiplier
newsd = sd / sqrt(k)
newsd

#12
#A multiple regression gives yhat = 10 + 3.7x1 + 1.5x2 Find the predicted value yhat at x1 = 13 and x2 = 9
#Hint: plug both values in, exactly as in simple regression: yhat = B^0 + B^1x1 + B^2x2
x1 = 13
x2 = 9
b0 = 10
b1 = 3.7
b2 = 1.5
yhat = b0 + b1*x1 + b2*x2
yhat

#13 
#A multiple regression gives yhat = 2 + 3.8x1 - 0.7x2 If x1 rises by 1 and x2 rises by 4 how much does yhat change 
#Hint: ychange = B^1*x1change + B^2*x2change -- here B^2 is negative and the intercept doesn't matter
b1 = 3.8
b2 = -0.7
x1change = 1
x2change = 4
ychange = b1*x1change +b2*x2change
ychange

#14
#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). 
#Run reg <- lm(wage ~ educ + exper + tenure, data = wage1). 
#What is the predicted hourly wage of a worker with 14 years of education, 10 years of experience and 5 years of tenure?
#Hint: e.g. save the coefficients with b <- reg$coefficients, then do something like b[1] + b[2]*14 + b[3]*10 + b[4]*5.
data("wage1")
reg3 <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg3$coefficients                              
b[[1]] + b[[2]] * 14 + b[[3]] * 10 + b[[4]] * 5

#15
#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). 
#Using the same regression of wage on educ, exper and tenure, 
#by how much does the predicted hourly wage change when tenure rises by 3 years, holding education and experience fixed? 
#Hint: change in wage = B^3 * change in tenure
reg3 <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg3$coefficients                              
tenure_change = 3
wage_change = b[[4]]*tenure_change
wage_change
