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

#5
#Why do we prefer an unbiased estimator with a smaller variance Var(B^1)
## Its estimates land closer to B1 from sample to sample, so any one estimate is more reliable.

#6
#Under SLR1–SLR5, Var(B^1)= o^2/SSTx. Holding everything else fixed, which changes would make Var(B^1) larger? 
## X values bunched more tightly together (a smaller SSTx)
## A smaller sample (fewer observations with X spread the same way, so a smaller SSTx)
## A larger error variance o^2

#7 
#A simple regression on n = 25 observations has ohat = 8.7, the sample standard deviation of 
#X is Sx = 0.6. Find the standard error se(B^1)
#Hint: SSTx = (n-1)Sx^2 (square the standard deviation), then se(B^1) = ohat/sqrt(SSTx)
n = 11
ohat = 8.6
Sx = 1.4
SSTx = (n-1)*Sx^2
se = ohat/(sqrt(SSTx))
se

#8
#A simple regression uses the five X values x = (3,7,9,10,11)
#and has SSR = 102. Find the standard error se(B^1)
#Hint: with n = 5, ohat = sqrt(SSR/(n-2)) Then SSTx = Sum(xi-xbar)^2 and se (B^1) = ohat/sqrt(SSTx)
x = c(1,4,8,10,12)
n = 5
SSR = 76
ohat = sqrt(SSR/(n-2))
SSTx = sum((x - mean(x))^2)
se = ohat/sqrt(SSTx)
se

#9
#Under SLR1–SLR5, a regression has sd(B^1) = 1.09 If the sample were 16 times as large, with the 
#X values spread out in the same way (so SSTx is also 16 times as large), what would sd(B^1) be? 
#Hint: sd(B^1) = o/sqrt(SSTx) multiplying SSTx by k divides the sd by sqrt(k) not by k
sd = 2.28
k = 16    #sample size multiplier
newsd = sd / sqrt(k)
newsd

#10
#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). 
#Run reg <- lm(salary ~ roe, data = ceosal1). Compute the standard error se(B^1)by hand from ohat and SSTx
#Hint: ohat = sqrt(SSR/(n-2)) and SSTx = sum((ceosal1$roe - mean(ceosal1$roe))^2) then se(B^1) = ohat/sqrt(SSTx) check using summary(reg) --> column "Std. Error"
reg2 <- lm(salary ~ roe, data = ceosal1)
n = nobs(reg2)
SSR = sum(reg2$residuals^2)
ohat = sqrt(SSR / (n-2))
SSTx = (n-1)*var(ceosal1$roe)
seb1 = ohat / sqrt(SSTx)
seb1
summary(reg2) #verify standard error


#11
#In wage = B^0 + B^1*educ + B^2*exper what does B^1 measure
## The predicted change in wage from one more year of education, holding experience fixed.

#12
#A multiple regression gives yhat = 10 + 3.7x1 + 1.5x2 Find the predicted value yhat at x1 = 13 and x2 = 9
#Hint: plug both values in, exactly as in simple regression: yhat = B^0 + B^1x1 + B^2x2
x1 = 10
x2 = 3
b0 = 3
b1 = 2.6
b2 = 1.3
yhat = b0 + b1*x1 + b2*x2
yhat

#13 
#A multiple regression gives yhat = 2 + 3.8x1 - 0.7x2 If x1 rises by 1 and x2 rises by 4 how much does yhat change 
#Hint: ychange = B^1*x1change + B^2*x2change -- here B^2 is negative and the intercept doesn't matter
b1 = 3.1
b2 = -2.3
x1change = 5
x2change = 3
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

#16
#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). 
#Run reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1). 
#By about what percent does predicted salary change when return on equity rises by 10 points, holding sales fixed? 
#Hint: the roe term is log-level, so salary change = 100*B^roe *change in roe, holding sales fixed.
reg4 <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b = reg4$coefficients  
roechange = 10
salarychange = 100 * b[[3]] *roechange
salarychange

#17
#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). 
#Using the same regression of log(salary) on log(sales) and roe, what is the predicted salary, in thousands of dollars, of a CEO whose firm has sales of 5,000 (that is, $5 billion) and a return on equity of 15?
#Hint: first predict log(salary) by plugging in log(5000) and roe = 15, then undo the log with exp() (do not round anything before taking exp()). With b <- reg$coefficients: exp(b[1] + b[2]*log(5000) + b[3]*15).
reg4 <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b = reg4$coefficients
salary = exp(b[1] + b[2]*log(5000) + b[3]*15)
salary


#18
b1 = 1.11
b2 = -0.87
delta1 = 0.69
slope = b1 + b2 *delta1
slope

#19
b2 = -2.17
delta1 = -0.45
bias = b2*delta1
bias

#20
#An omitted variable X2 lowers Y(B2<0) and X2 is positively correlated with X1.
#The simple-regression slope on X1 is:
## Biased downward

#21
reg5 = lm(wage ~ educ + exper, data = wage1)
reg6 = lm(exper ~ educ, data = wage1)
b2 = reg5$coefficients[3]
delta1 = reg6$coefficients[2]
bias = b2 *delta1
bias
