#quiz

# In test score = B0+B1class size +U, family income is part of U. Richer school districts tend to have smaller classes. Which statement is correct?
## The zero conditional mean assumption fails, because the average of  changes with class size.

## From table find OLS line
# go to column with U^i2 and pick smallest one

## given E[X] = 4 and Ex2 = 20 find standard deviation of 3X-4
EX <- 4
EX2 <- 20
a <- 3
b <- -4
a * EX + b                              # E[aX + b] = a E[X] + b          (the MEAN)
a^2 * (EX2 - EX^2)                      # Var(aX + b) = a^2 Var(X)         (b drops out)
abs(a) * sqrt(EX2 - EX^2)    # sd(aX + b) = |a| sd(X)           (not the mean!)

## find Var(3X-3Y) given Var(X)=10, Var(Y)=12, Cov(X,Y) = -5.3
VarX <- 10
VarY <- 12
CovXY <- -5.3
a <- 3
c2 <- -3                               # coefficient on Y: "2X - Y" means c2 = -1
a^2 * VarX + c2^2 * VarY + 2 * a * c2 * CovXY


## find Cor(X,Y) from joint distribution table
x = c(1,2,4,7)
y = c(8,5,11,4)
p = c(.2,.3,.4,.1)
EX = sum(x*p)
EY = sum(y*p)
EXY = sum(x*y*p)
cov = EXY - EX*EY
Ex2 = sum(x^2*p)
varx = Ex2-EX^2
Ey2 = sum(y^2*p)
vary = Ey2-EY^2
cov/(sqrt(varx)*sqrt(vary)) ## Cor(X,Y)




## given sample x = (1,2,6,9,10) and y = (11,8,7,2,9) , regress y on x by OLS. Find residual of third observation given (x3,y3) = (6,7)
## residual = actual minus fitted --> u^3 = y3 - (B^0+B^1x3)
x = c(1,2,6,9,10)
y = c(11,8,7,2,9)
b1 = cov(x,y)/var(x)
b0 = mean(y)-b1*mean(x)
x3 = 6
y3 = 7
yhati = b0+b1*x3
uhati = y3 - yhati
uhati


##An OLS regression on six observations has residuals -1.6, 1.2, -2.4, -2.5, 0.7, and one more. What is the sixth residual? (round to the hundredths)
##Hint: OLS residuals always sum to zero.
x = c(-1.6,1.2,-2.4,-2.5,0.7)
sum(x)*-1

## samp1e x = (1,2,4,7,12,13) y = (3,4,1,7,14,9) compute sumation xiU^i
## answer is ZERO

## SSE = 48 R2 = 0.82 Find the sum of squared residuals SSR
## R2 = SSE/SST gives SST, and SST = SSE + SSR
SSE = 48
R2 = 0.82
SST = SSE/R2
SSR = SST - SSE
SSR

#A sample of n = 30 observations has sample variance Sy2 = 23. Find the total sum of squares 
#SST. (round to the hundredths)
# Sy2 = SST/(n-1)
n = 30
sy2 = 23
SST = 23*(n-1)
SST


## What is the OLS residual of the worker in row 12?
library(wooldridge)
data(wage1)
reg = lm(wage~tenure, data = wage1)
res = reg$residuals
res[12]

#An OLS regression line has intercept B^0 = 2 and slope B^1 = 1.8. The sample mean of x
#is 12. What is the sample mean of y (round to the hundredths)
#Hint: the point (x mean, y mean) is always on the OLS line.
b0 = 2    # intercept
b1 = 1.8    # slope
xmean = 12
ymean = b0+b1*xmean
ymean








# question 13 on exam 1
#A vector of 15 numbers has first element 58, and each of the remaining elements equals 2. Find the sample mean. (round to the hundredths)
x = c(58, rep(2, times = 14))
mean(x)




##prob set 8

#13
# B^1 = 1.49 Every value of y is then multiplied by 100 What is the new OLS slope? 
# Hint: if y is multiplied by c, both B^0 and B^1 are multiplied by c


.65/10

#If salary is measured in dollars instead of thousands of dollars, what is the OLS slope?
library(wooldridge)
data("ceosal1")
reg2 = lm(salary ~ sales, data = ceosal1)
b1 = reg2$coefficients [2] #slope
b1*1000


#log - level regression percent change in y 
b1 = 0.06
xchange = 2 
100*b1*xchange


#log-log regression percent change in y
b1 = 1.33
xchange = 20 
b1*xchange

#level-log regression unit change in y 
b1 = 6.4
xchange = 2
(b1/100)*xchange


data("wage1")
reg3 = lm(log(wage)~exper, data = wage1)
b1 = reg3$coefficients [2] #slope
experchange = 10
100*b1*experchange





##  A1 given E[X] and Ex2 find standard deviation of 3X-7
## ============ RULES FOR E, Var, Cov (no data needed) ====
ex =
ex2 =
a = 
varx = EX2-EX^2 
varx
sqrt(a^2*varx)

##
VarX <- 10
VarY <- 12
CovXY <- -5.3
a <- 3
c2 <- -3                               # coefficient on Y: "2X - Y" means c2 = -1
a^2 * VarX + c2^2 * VarY + 2 * a * c2 * CovXY   # Var(aX + c2*Y)  (watch the signs)

## find standard deviation using Var(x) properties
EX <- 4
EX2 <- 20
a <- 3
b <- -4
a * EX + b                              # E[aX + b] = a E[X] + b          (the MEAN)
a^2 * (EX2 - EX^2)                      # Var(aX + b) = a^2 Var(X)         (b drops out)
abs(a) * sqrt(EX2 - EX^2)    # sd(aX + b) = |a| sd(X)           (not the mean!)


## given equation and changes of x1 and x2 how much does y change
b1 = 1.5
b2 = -0.8
changex1 = 4
changex2 = -3
changey = b1*changex1 + b2*changex2
changey
