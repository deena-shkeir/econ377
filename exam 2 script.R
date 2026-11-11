## sample world stats
x = c()
y = c()
sum(x)
mean(x)
var(x)
sd(x)
cov(x,y)
cor(x,y)

#Sample mean and sample standard deviation including the mean within a group:
mean(wage1$wage, na.rm = TRUE)  # Sample mean
var(wage1$wage, na.rm = TRUE)   # Sample variance
sd(wage1$wage, na.rm = TRUE)    # Sample standard deviation
# FEMALES ONLY (female == 1)
mean(wage1$wage[wage1$female == 1], na.rm = TRUE)
var(wage1$wage[wage1$female == 1], na.rm = TRUE)
sd(wage1$wage[wage1$female == 1], na.rm = TRUE)
# MALES ONLY (female == 0)
mean(wage1$wage[wage1$female == 0], na.rm = TRUE)
var(wage1$wage[wage1$female == 0], na.rm = TRUE)
sd(wage1$wage[wage1$female == 0], na.rm = TRUE)

# finding E[X]
x = c()
p = c()
sum(x*p)

# finding E[X^2]
x = c()
p = c()
sum(x^2*p)

# finding E[Y]
y = c()
p = c()
sum(y*p)

# X and Y have the joint probability distribution below (table). Find E[X]
x = c(3,1,5)
y = c(0,0,6)
p = c(0.2,0.3,0.5)
sum(x*p) #E[X]

# Finding E[X+Y] = E[X] + E[Y] from joint probability table
x = c()
y = c()
p = c()
EX = sum(x*p)
EY = sum(y*p)
EX+EY

# expectation is linear: E[aX+b] = aE[X]+b given E[X], a, b.
EX = 7
a = 5
b = 5
a*EX+b

# given E[X], E[Y], a, b find E[aX+bY] = aE[X]+bE[Y]
EX = 3
EY = 6
a = 3
b = 2
a*EX + b*EY


# finding E[aX+bY-c] given given E[X], E[Y], a, b, c. (same for if its + c)
EX = 3
EY = 8
a = 3
b = 5
c = 3
a*EX + b*EY - c

# A RANDOM VARIABLE FROM A PROBABILITY TABLE
xv <- c(0, 2, 5)                        # values X can take
p <- c(0.5, 0.3, 0.2)                   # their probabilities (must sum to 1)
EX <- sum(p * xv)                       # E[X]
EX2 <- sum(p * xv^2)                    # E[X^2]
VarX <- EX2 - EX^2                      # Var(X) = E[X^2] - (E[X])^2
sqrt(VarX)                              # sd(X)

# Var(X + Y) FROM PROBABILITY TABLES  X and Y are independent = Var(X) + Var(Y)
x <- c(1, 2, 3)
px <- c(0.2, 0.5, 0.3)
EX <- sum(x * px)
VarX <- sum(x^2 * px) - EX^2 # Finding Var(X)
y <- c(3, 5, 8)
py <- c(0.5, 0.3, 0.2)
EY <- sum(y * py)
VarY <- sum(y^2 * py) - EY^2 # Finding Var(Y)
VarX + VarY # Finding Var(X + Y)

##  RULES FOR E, Var, Cov 
EX <- 2
EX2 <- 13
a <- 3
b <- -4
a * EX + b                              # E[aX + b] = a E[X] + b          (the MEAN)
a^2 * (EX2 - EX^2)                      # Var(aX + b) = a^2 Var(X)         (b drops out)
abs(a) * sqrt(EX2 - EX^2)               # sd(aX + b) = |a| sd(X)           (not the mean!)

#Var(aX+bY) = a^2 * Var(X) + b^2 * Var(Y) + 2ab * Cov(X,Y)
VarX <- 4
VarY <- 9
CovXY <- -1.5
a <- 2
c2 <- -1                                # coefficient on Y: "2X - Y" means c2 = -1
a^2 * VarX + c2^2 * VarY + 2 * a * c2 * CovXY   # Var(aX + c2*Y)  (watch the signs)

## CONDITIONAL EXPECTATION
#Given X = x, Y takes 3,0,9 with probabilities 0.2,0.3,0.5 find E[Y|X=x]
y = c(3,0,9)
p = c(0.2,0.3,0.5)
sum(y*p)

#Conditional expectation is also linear: E[aY+b|X=x] = aE[Y|X=x]+b given E[Y|X=x] = 4 , a = 2, b =2 find  E[aY+b|X=x]
EYX = 4
a = 2
b = 2
a*EYX + b  


# ============================================
# RESCALING REGRESSION SLOPES
# ============================================
b1 <- 5  # Original slope 
# Y: Dollars -> Thousands
b1 / 1000
# Y: Thousands -> Dollars
b1 * 1000
# X: Dollars -> Thousands
b1 * 1000
# X: Thousands -> Dollars
b1 / 1000
# Y: Decimal -> Percent
b1 * 100
# Y: Percent -> Decimal
b1 / 100
# X: Decimal -> Percent
b1 / 100
# X: Percent -> Decimal
b1 * 100

## log-log thousands of dollars to dollars
#reg gives log(salary) = 5.73 + 0.54log(sales) w/ salary in thousands of dollars
#If salary is measured in dollars instead (every salary times 1000), what is the new intercept?
#Hint: log(1000*y) = log(1000)+log(y), and log(1000) = 6.91 in R
b1 = 5.73
b1 + log(1000)


##  LOGS (Table 2.3)
##   level-level:  change in y   = b1 * (change in x)
##   log-level:    % change in y = 100 * b1 * (change in x)
##   level-log:    change in y   = (b1 / 100) * (% change in x)
##   log-log:      % change in y = b1 * (% change in x)

#log-level
reg3 <- lm(log(wage) ~ educ, data = wage1)             # log-level
100 * reg3$coefficients[[2]]            # % change in wage from +1 year of educ

#log-log
reg4 <- lm(log(salary) ~ log(sales), data = ceosal1)   # log-log
reg4$coefficients[[2]]  # elasticity: % change in salary from +1% sales

#log-log
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


#level-log
reg5 <- lm(salary ~ log(sales), data = ceosal1)        # level-log
reg5$coefficients[[2]] / 100            # change in salary ($1000s) from +1% sales

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

#If Y is logged in the regression, but the question asks for Y in its original units, use exp().
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
exp(b[1] + b[2]*educ + b[3]*exper + b[4]*tenure) #turning log(y) back to y 

#Under SLR1–SLR5, the error variance is o^2 = 46 and SSTx = 33 find Var(B^1)
#Hint: thrm 2.2 Var(B^1) = o^2/SSTx
o2 = 93
SSTx = 14
Var = o2/SSTx
Var

#Under SLR1–SLR5, the error standard deviation is o = 4 A sample has the five X
#values x = (1,4,5,7,10) find sd(B^1)
#Hint: first SSTx = sum(xi-xbar)^2 ---> sum((x-mean(x))^2) then sd(B^1) = o/sqrt(SSTx)
x = c(1,7,8,9,10)
SSTx = sum((x-mean(x))^2)
o = 2
sd = o/sqrt(SSTx)
sd

#Under SLR1–SLR5, the error standard deviation is o = 17 A sample has n = 12
#observations, and the sample variance of X is Sx^2 = 6 find sd(B^1)
#Hint: SSTx = (n-1)Sx^2, then sd(B^1) = o/sqrt(SSTx)
n = 6
Sx2= 6
SSTx = (n-1)*Sx2
o = 17
sd = o/sqrt(SSTx)
sd

#simple reg on n = 27 obs has SSR = 513 find the estimate of the error variance o2hat
#Hint: o2hat = SSR/(n-2)
n = 51
SSR = 227
o2hat = SSR/(n-2)
o2hat

#simple reg on n = 10 has SSR = 462 and SSTx = 12 find standard error se(B^1)
#Hint: first ohat = sqrt(SSR/(n-2)) the se(B^1) = ohat/sqrt(SSTx)
n = 11
SSR = 487
SSTx = 8
ohat = sqrt(SSR/(n-2))
se = ohat/sqrt(SSTx)
se

#A simple regression on n = 25 observations has ohat = 8.7, the sample standard deviation of 
#X is Sx = 0.6. Find the standard error se(B^1)
#Hint: SSTx = (n-1)Sx^2 (square the standard deviation), then se(B^1) = ohat/sqrt(SSTx)
n = 11
ohat = 8.6
Sx = 1.4
SSTx = (n-1)*Sx^2
se = ohat/(sqrt(SSTx))
se

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

#Under SLR1–SLR5, a regression has sd(B^1) = 1.09 If the sample were 16 times as large, with the 
#X values spread out in the same way (so SSTx is also 16 times as large), what would sd(B^1) be? 
#Hint: sd(B^1) = o/sqrt(SSTx) multiplying SSTx by k divides the sd by sqrt(k) not by k
sd = 2.28
k = 16    #sample size multiplier
newsd = sd / sqrt(k)
newsd

#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). 
#Run reg <- lm(salary ~ roe, data = ceosal1). Compute the standard error se(B^1)by hand from ohat and SSTx
#Hint: ohat = sqrt(SSR/(n-2)) and SSTx = sum((ceosal1$roe - mean(ceosal1$roe))^2) then se(B^1) = ohat/sqrt(SSTx) check using summary(reg) --> column "Std. Error"
reg2 <- lm(salary ~ roe, data = ceosal1)
n = nrow(reg2)
SSR = sum(reg2$residuals^2)
ohat = sqrt(SSR / (n-2))
SSTx = (n-1)*var(ceosal1$roe)
seb1 = ohat / sqrt(SSTx)
seb1
summary(reg2) #verify standard error

#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education). 
#Run reg <- lm(wage ~ educ, data = wage1). What is ohat, the standard error of the regression? 
#Hint: ohat = sqrt(SSR/(n-2)) ---> In R: sqrt(sum(reg$residuals^2) / (nrow(wage1) - 2)), and check it against summary(reg)$sigma.
data(wage1)
reg3 <- lm(wage ~ educ, data = wage1)
ohat = sqrt(sum(reg3$residuals^2) / (nrow(wage1) - 2))
ohat
summary(reg3)$sigma

#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education). What is 
#SSTx for educ, the total sum of squares of the explanatory variable? 
# SSTx = sum((x-mean(x))^2)
SSTx = sum((wage1$educ - mean(wage1$educ))^2)
SSTx

## ------------- PREDICTING Y --------------
#A multiple regression gives yhat = 10 + 3.7x1 + 1.5x2 Find the predicted value yhat at x1 = 13 and x2 = 9
#Hint: plug both values in, exactly as in simple regression: yhat = B^0 + B^1x1 + B^2x2
x1 = 10
x2 = 3
b0 = 3
b1 = 2.6
b2 = 1.3
yhat = b0 + b1*x1 + b2*x2
yhat

#A multiple regression gives yhat = 2 + 3.8x1 - 0.7x2 If x1 rises by 1 and x2 rises by 4 how much does yhat change 
#Hint: ychange = B^1*x1change + B^2*x2change -- here B^2 is negative and the intercept doesn't matter
b1 = 3.1
b2 = -2.3
x1change = 5
x2change = 3
ychange = b1*x1change +b2*x2change
ychange

#A regression gives log(y) = 0.5 + 0.065x1 - 0.019x2. If x1 rises by 4 and x2
#rises by 3, by about what percent does predicted y change? 
#Hint: log-level MLR: %ychange = 100*(B^1*x1change + B^2*x2change) Here B^2 is negative
#log-level MLR predicted change
b1 = 0.065
b2 = -0.019
x1 = 4
x2 = 3
pct_change = (b1*x1+b2*x2)*100
pct_change

#Log-level reg of log(wage) on tenure, w/ tenure measured in months, gives slope of 0.0093.
#by abt what percent does wage rise w/ one more yr of tenure
#Hint: one yr = 12 months so xchange = 12
#log lvl %changey = B1*100%*changex
B1 = 0.0054
xchange = 12      #make sure to match units to months, 1 yr = 12 months
ychange = B1*100*xchange
ychange


#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer).
#Run reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1). 
#By about what percent does predicted wage change when experience rises by 2 years and tenure rises by 1 year, holding education fixed? 
#Hint: %wage change = 100*(B^exper * 2 + B^tenure *1) In R: b <- reg$coefficients: 100 * (b[3]*2 + b[4]*1).
#log-level predicted changes in y MLR
# hint: wage y is logged and none of the x variables are logged, so it is log-level
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1) 
b = reg$coefficients
exper_change = 2
tenure_change = 1
wage_change = 100*(b[[3]]*exper_change + b[[4]]*tenure_change)
wage_change

#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). 
#Run reg <- lm(wage ~ educ + exper + tenure, data = wage1). 
#What is the predicted hourly wage of a worker with 14 years of education, 10 years of experience and 5 years of tenure?
#Hint: e.g. save the coefficients with b <- reg$coefficients, then do something like b[1] + b[2]*14 + b[3]*10 + b[4]*5.
data("wage1")
reg3 <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg3$coefficients                              
b[[1]] + b[[2]] * 14 + b[[3]] * 10 + b[[4]] * 5

#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). 
#Using the same regression of wage on educ, exper and tenure, 
#by how much does the predicted hourly wage change when tenure rises by 3 years, holding education and experience fixed? 
#Hint: change in wage = B^3 * change in tenure
reg3 <- lm(wage ~ educ + exper + tenure, data = wage1)
b <- reg3$coefficients                              
tenure_change = 3
wage_change = b[[4]]*tenure_change
wage_change

#Data: gpa1 (colGPA = college GPA; ACT = ACT score; hsGPA = high-school GPA). 
#Run reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1). Compute the predicted colGPA at ACT = mean(gpa1$ACT) and hsGPA = mean(gpa1$hsGPA).
#Hint: plug the two means into the fitted equation: b <- reg$coefficients, then b[1] + b[2]*mean(gpa1$ACT) + b[3]*mean(gpa1$hsGPA). Compare your answer with mean(gpa1$colGPA). Why are they the same?
reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b = reg$coefficients
col_gpa = b[1] + b[2]*mean(gpa1$ACT) + b[3]*mean(gpa1$hsGPA)
col_gpa
mean(gpa1$colGPA)  ##OLS property: ybar = B^0 + B1*x1bar + ... + Bk*xkbar so finding mean of y (colGPA) give same answer

## --- predicting y with exp() function ----- when y is logged and asked for in original units, use exp() to undo log

#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). 
#Run reg <- lm(log(salary) ~ log(sales) + roe, data = ceosal1). 
#By about what percent does predicted salary change when return on equity rises by 10 points, holding sales fixed? 
#Hint: the roe term is log-level, so salary change = 100*B^roe *change in roe, holding sales fixed.
reg4 <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b = reg4$coefficients  
roechange = 10
salarychange = 100 * b[[3]] *roechange
salarychange

#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars). 
#Using the same regression of log(salary) on log(sales) and roe, what is the predicted salary, in thousands of dollars, of a CEO whose firm has sales of 5,000 (that is, $5 billion) and a return on equity of 15?
#Hint: first predict log(salary) by plugging in log(5000) and roe = 15, then undo the log with exp(). With b <- reg$coefficients: exp(b[1] + b[2]*log(5000) + b[3]*15).
reg4 <- lm(log(salary) ~ log(sales) + roe, data = ceosal1)
b = reg4$coefficients
salary = exp(b[1] + b[2]*log(5000) + b[3]*15)
salary

#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). 
#Using the same regression of log(wage) on educ, exper and tenure, 
#what is the predicted hourly wage, in dollars, of a worker with 12 years of education, 10 years of experience and 3 years of tenure?
#Hint: predict log(wage) first, then undo the log with exp(): exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3)
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1) 
b = reg$coefficients
educ_change = 12
exper_change = 10
tenure_change = 3
wage_change = 100 * (b[[2]]*educ_change + b[[3]]*exper_change + b[[4]]*tenure_change)
exp(b[1] + b[2]*educ_change + b[3]*exper_change + b[4]*tenure_change)


#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). 
#Using the same regression of log(wage) on educ, exper and tenure, 
#what is the predicted hourly wage, in dollars, of a worker with 12 years of education, 10 years of experience and 3 years of tenure?
#Hint: predict log(wage) first, then undo the log with exp(): exp(b[1] + b[2]*12 + b[3]*10 + b[4]*3)
reg <- lm(log(wage) ~ educ + exper + tenure, data = wage1) 
b = reg$coefficients
educ_change = 12
exper_change = 10
tenure_change = 3
wage_change = 100 * (b[[2]]*educ_change + b[[3]]*exper_change + b[[4]]*tenure_change)
exp(b[1] + b[2]*educ_change + b[3]*exper_change + b[4]*tenure_change)


## ---- Example 3.1: college GPA on high-school GPA and ACT ----
## What you're learning: each slope is a ceteris-paribus effect, and Y-hat is the prediction.
library(wooldridge)
data("gpa1")
reg1 <- lm(colGPA~hsGPA + ACT, data = gpa1) # KNOW THIS: regress colGPA on hsGPA and ACT (add variables with +)
summary(reg1)
b <- reg1$coefficients                              # 1.29, 0.45, 0.0094
pred1 <- b[[1]] + b[[2]]*3.5 + b[[3]]*24                                     # Y-hat at hsGPA = 3.5, ACT = 24   (hint: b[[1]] + b[[2]]*3.5 + b[[3]]*24)
chg_ACT <- b[[3]] * 10                                   # +10 ACT points, hsGPA fixed      (hint: b[[3]] * 10)
lm(colGPA ~ ACT, data = gpa1)$coefficients          # SLR: the ACT slope is much bigger without hsGPA

## ---- Example 3.2: log-wage on educ, exper, tenure ----
## What you're learning: predict log(wage), then exp() back to dollars.
data("wage1")
reg2 <- lm(log(wage) ~ educ + exper + tenure, data = wage1)
summary(reg2)
b <- reg2$coefficients                              # 0.28, 0.092, 0.0041, 0.022
exp(b[[1]] + b[[2]] * 16 + b[[3]] * 4 + b[[4]] * 2) # wage-hat ($) at educ=16, exper=4, tenure=2 (~6.16)
pct_educ <-  100 * b[[2]]                           # % change in wage from +1 year educ   (hint: 100 * b[[2]])
pct_educ

## ---- Example 3.1 (gpa1) ---- OMITTED VARIABLE BIAS (SLR vs MLR Slopes)
## SLR: colGPA on ACT (save the ACT slope)
reg_SLR = lm(colGPA~ACT, data = gpa1)
summary(reg_SLR)
## MLR: add hsGPA; change in colGPA-hat from +10 ACT, hsGPA fixed
reg_MLR = lm(colGPA~ACT + hsGPA, data = gpa1)
summary(reg_MLR)
#predict colGPA changing +10 ACT points
act = 10 
b1 = reg_MLR$coefficient[2]
b1*act

## ---- Omitted-variable bias (Example 3.3, gpa1) ---- (equation 3.23)
## delta1: regress hsGPA (omitted) on ACT (included)
reg_delta = lm(hsGPA ~ ACT, data = gpa1)
d1 = reg_delta$coefficients[2]
b1hat = reg_MLR$coefficients[2]
b2hat = reg_MLR$coefficients[3]
b1tilde = reg_SLR$coefficients[2]
## check (3.23): SLR slope = MLR slope + (hsGPA slope) * delta1
b1tilde
b1hat + b2hat * d1

#formula 3.23 links SLR and MLR slopes: B1~ = B^1 + B^2delta1. The multiple regression of Y on X1 and X2 gives
#B^1 = 1.11 and B^2 = -0.87. Slope regressing X2 on X1 is delta1 = 0.69. Find the simple-regression slope B1~ 
#from regressing Y on X1 alone
# Hint: plug into  B1~ = B^1 + B^2delta1 watch the signs
b1 = 1.11
b2 = -0.87
delta1 = 0.69
slope = b1 + b2 *delta1           #finding B1~
slope 

#Formula (3.23): B~1 = B^1 + B^2*delta~1. The simple regression of Y on X1 gives B~1 = -1.27
#The multiple regression of Y on X1 and X2 gives B^2 = 0.55, and the slope from regressing X2 on X1
#is delta~1 = -0.66. Find the multiple-regression slope B^1
#Hint: solve (3.23) for B^1: B^1 = B~1 - B^2*delta~1 watch the signs
b1_titla = -1.27
b2 = 0.55
delta1_tilta = -0.66
b1 = b1_titla - b2*delta1_tilta      #finding B1^
b1

#Data: bwght (bwght = a baby's birth weight in ounces; cigs = cigarettes the mother smoked per day during pregnancy; faminc = the family's income in thousands of dollars). 
#Run the simple regression lm(bwght ~ cigs, data = bwght). What is the slope on cigs (the predicted change in birth weight, in ounces, from one more cigarette per day)?
#Hint: read the cigs coefficient off lm(bwght ~ cigs, data = bwght)$coefficients. It is negative.
library(wooldridge)
data = bwght
reg = lm(bwght ~ cigs, data = bwght)
reg$coefficients

#13
#Data: bwght (bwght = a baby's birth weight in ounces; cigs = cigarettes the mother smoked per day during pregnancy; faminc = the family's income in thousands of dollars). 
#Now add family income: reg <- lm(bwght ~ cigs + faminc, data = bwght). 
#What is the slope on cigs, holding faminc fixed? 
#Hint: read the cigs coefficient off reg$coefficients. Compare it with your answer to the previous problem.
library(wooldridge)
data = bwght
reg <- lm(bwght ~ cigs + faminc, data = bwght)
reg$coefficients

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

## Part 1
#Data: bwght (bwght = a baby's birth weight in ounces; cigs = cigarettes the mother smoked per day during pregnancy; faminc = the family's income in thousands of dollars). 
#Run the simple regression lm(bwght ~ cigs, data = bwght). What is the slope on cigs (the predicted change in birth weight, in ounces, from one more cigarette per day)?
#Hint: read the cigs coefficient off lm(bwght ~ cigs, data = bwght)$coefficients. It is negative.
library(wooldridge)
data = bwght
reg = lm(bwght ~ cigs, data = bwght) #SLR birth weight changes with one additional cigarette per day, without controlling for family income
reg$coefficients
## Part 2
#Now add family income: reg <- lm(bwght ~ cigs + faminc, data = bwght). 
#What is the slope on cigs, holding faminc fixed? 
#Hint: read the cigs coefficient off reg$coefficients. Compare it with your answer to the previous problem.
reg <- lm(bwght ~ cigs + faminc, data = bwght) #MLR estimating the effect of cigarettes on birth weight holding family income fixed.
reg$coefficients
## Part 3
#Regress faminc (the variable left out of the simple regression in problem 12) on cigs, lm(faminc ~ cigs, data = bwght), to get delta~1.
#Compute B^2 * delta~1, the faminc coefficient from the multiple regression times this slope. 
#Hint: by (3.23), B^2*delta~1 = B1~ = B^1: the simple-regression cigs slope minus the multiple-regression one. Use that to check your answer.
reg <- lm(bwght ~ cigs + faminc, data = bwght)
reg2 = lm(faminc ~ cigs, data = bwght)  #predicting family income using cigarettes, rather than predicting birth weight. Get delta1~
b2 = reg$coefficients["faminc"]  #B^2
delta1_tilta = reg2$coefficients["cigs"]  #delta1~
b2*delta1_tilta
## Part 4 (interpretation)
#In the birth-weight regressions, the simple-regression cigs slope is about  -0.51 and 
#the multiple-regression slope (holding faminc fixed) is about -0.46 . Which reading is correct?
#Hint: B^2 > 0 (income raises birth weight) and delta1~ <0 (mothers who smoke more have lower family incomes).
# About -0.46 per cigarette is the estimated effect of smoking holding income fixed; the other -0.05 is income's effect riding along, because heavier smokers tend to have lower incomes.

#Data: gpa1 (colGPA = college GPA; ACT = ACT score; hsGPA = high-school GPA). 
#Run reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1). Compute the predicted colGPA at ACT = mean(gpa1$ACT) and hsGPA = mean(gpa1$hsGPA).
#Hint: plug the two means into the fitted equation: b <- reg$coefficients, then b[1] + b[2]*mean(gpa1$ACT) + b[3]*mean(gpa1$hsGPA). Compare your answer with mean(gpa1$colGPA). Why are they the same?
library(wooldridge)
data = gpa1
reg <- lm(colGPA ~ ACT + hsGPA, data = gpa1)
b = reg$coefficients
predicted_gpa = b[1] + b[2]*mean(gpa1$ACT) + b[3]*mean(gpa1$hsGPA)
predicted_gpa
mean(gpa1$colGPA) #verify answer

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

#Data: wage1 (wage = average hourly earnings in dollars; educ = years of education; exper = years of experience; tenure = years with the current employer). 
#Run the multiple regression lm(wage ~ educ + exper, data = wage1) and the 
#regression of the omitted variable on the included one, lm(exper ~ educ, data = wage1). 
#Compute B^2*delta1, the exper coefficient from the first regression times the educ slope from the second.
# Hint: by (3.23), this equals the simple-regression educ slope (from lm(wage ~ educ, data = wage1)) minus the multiple-regression educ slope. Use that to check your answer.
reg5 = lm(wage ~ educ + exper, data = wage1)
reg6 = lm(exper ~ educ, data = wage1)
b2 = reg5$coefficients[3]
delta1 = reg6$coefficients[2]
bias = b2 *delta1
bias

#In the population, Y = B0 + B1X1 + B2X2 + U with B2 = -2.17. You omit X2 and regress Y on X1 alone
#and the slope from regressing X2 on X1 is delta1 = -0.45. What is the bias of the simple regression slope,
#Bias (B1~) = E[B1~] - B1
# Hint: Bias(B1~) = B2 * delta1. A negative answer means the slope is biased downward
b2 = -2.17
delta1 = -0.45
bias = b2*delta1
bias

## ---- A standard error by hand: se(educ) in wage ~ educ + exper ----
## regress wage on educ and exper; find educ's "Std. Error" in summary()
library(wooldridge)
data("wage1")

reg = lm(wage ~ educ + exper, data = wage1)
#easiest way to get standard error
summary(reg) # under coefficients there are the std errors for each
## df = n - K - 1   (n from nrow(); K = 2)
n = nrow(wage1)
k = 2
df = n-k-1
df
## sigma-hat = sqrt(SSR / df)
SSR = sum(reg$residuals^2)
sigma_hat = sqrt(SSR/df)
## SST_j for educ --> (n-1) * sxj^2
SSTj = (n-1)*var(wage1$educ)
## R_j^2: regress educ on the other x, then take the R-squared
reg2 = lm(educ~exper, data = wage1)
Rj2 = summary(reg2)$r.squared # getting R-squared
## se(educ) = sigma-hat / sqrt(SST_j * (1 - R_j^2)); does it match summary()?
sigma_hat/sqrt(SSTj*(1-Rj2))
summary(reg) #check answer

## ---QUESTIONS FROM PROBLEM SET 12 --- 

#2 in wage = B0+B1educ+B2exper+B3tenure+U, you want se(B^2), the standard error of the exper coefficient. Which regression gives Rj^2
# Hint: Rj^2 measures how well the other independent variables explain Xj. Y is not involved.
# ANSWER: The regression of exper on educ and tenure: lm(exper ~ educ + tenure)

#4 A multiple regression with K = 4 independent variables, estimated on n = 37
#observations, has SSR = 2354. Find the standard error of the regression, ohat.
#Hint: df = n - K - 1 then ohat = sqrt(SSR/df)
K = 4
n = 37
SSR = 2354
df = n - K - 1
ohat = sqrt(SSR/df)
ohat

#6 the MLR has ohat = 6.9 for the indep var Xj, SSTj = 131 and regressing Xj on the other indep vars
#gives Rj^2 = 0.56. Find se(B^j).
# Hint: se(B^j) = ohat/sqrt(SSTj*(1-Rj^2)). Note: the 1-Rj^2
ohat = 6.9
SSTj = 131
Rj2 = 0.56
se = ohat/sqrt(SSTj * (1-Rj2))
se

#9 Data: hprice1 (price = house price in thousands of dollars; sqrft = size of the house in square feet; bdrms = number of bedrooms; lotsize = size of the lot in square feet). 
#Using the same regression reg, compute the standard error of the regression, sigma hat  = sqrt(SSR/df). Sigma hat is same as ohat in the code
# Hint: SSR <- sum(reg$residuals^2), then sigma_hat <- sqrt(SSR / 84)
library(wooldridge)
data("hprice1")
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
n = nrow(hprice1)
K = 3    # there are 3 indep vars: sqrft, bdrms, lotsize
df = n - K - 1
SSR = sum(reg$residuals^2)
sigma_hat = sqrt(SSR/df)
sigma_hat

#12 Data: hprice1 (price = house price in thousands of dollars; sqrft = size of the house in square feet; bdrms = number of bedrooms; lotsize = size of the lot in square feet). 
#Put the pieces together: compute se(B^j) = sigma_hat / sqrt(SSTj*(1-Rj^2)) for the bdrms coefficient
# Hint: use the unrounded objects from problems 9-11: sigma_hat / sqrt(SST_j * (1 - R2_j)).
reg <- lm(price ~ sqrft + bdrms + lotsize, data = hprice1)
n = nrow(hprice1)
K = 3    # there are 3 indep vars: sqrft, bdrms, lotsize
df = n - K - 1
SSR = sum(reg$residuals^2)
sigma_hat = sqrt(SSR/df)
SST_j <- sum((hprice1$bdrms - mean(hprice1$bdrms))^2)
R2_j <- summary(lm(bdrms ~ sqrft + lotsize, data = hprice1))$r.squared
se = sigma_hat / sqrt(SST_j*(1-R2_j))
se
summary(reg) #check answer 






## --------------------- Extras ------------------------

## find Cor(X,Y) from joint distribution table
x = c(1,2,4,7)
y = c(8,5,11,4)
p = c(.2,.3,.4,.1)
EX = sum(x*p)
EY = sum(y*p)
EXY = sum(x*y*p)
cov = EXY - EX*EY                       # Cov = E[XY] - E[X]E[Y]
Ex2 = sum(x^2*p)
varx = Ex2-EX^2
Ey2 = sum(y^2*p)
vary = Ey2-EY^2
cov/(sqrt(varx)*sqrt(vary))             # Cor(X,Y) = Cov(x,y)/sd(x)*sd(y)


## given sample x = (1,2,6,9,10) and y = (11,8,7,2,9) , regress y on x by OLS. Find residual of third observation given (x3,y3) = (6,7)
## residual = actual minus fitted --> u^3 = y3 - (B^0+B^1x3) In R: resid(lm(y ~ x))[3].
x = c(1,2,6,9,10)
y = c(11,8,7,2,9)
b1 = cov(x,y)/var(x)
b0 = mean(y)-b1*mean(x)
x3 = 6
y3 = 7
yhati = b0+b1*x3
uhati = y3 - yhati
uhati
resid(lm(y ~ x))[3]   #another way (short cut)

##An OLS regression on six observations has residuals -1.6, 1.2, -2.4, -2.5, 0.7, and one more. What is the sixth residual?
##Hint: OLS residuals always sum to zero.
x = c(-1.6,1.2,-2.4,-2.5,0.7)
sum(x)*-1  ##sum to 0, some of x is -4.6 so the next must be 4.6 to get to 0

#An OLS regression line has intercept B^0 = 2 and slope B^1 = 1.8. The sample mean of x
#is 12. What is the sample mean of y 
#Hint: the point (x mean, y mean) is always on the OLS line.
b0 = 2    # intercept
b1 = 1.8    # slope
xmean = 12
ymean = b0+b1*xmean
ymean

#Data: ceosal1 (salary = a CEO's salary in thousands of dollars; sales = firm sales in millions of dollars). 
#Run lm(salary ~ sales, data = ceosal1). If salary is measured in dollars instead of thousands of dollars, what is the OLS slope? 
library(wooldridge)
reg = lm(salary ~ sales, data = ceosal1)
b1 = reg$coefficients[2]
b1 * 1000

#reg gives log(salary) = 2.69 + 0.41log(sales) w/ salary in thousands of dollars
#If salary is measured in dollars instead (every salary times 1000), what is the new intercept?
#Hint: log(1000*y) = log(1000)+log(y), and log(1000) = 6.91 in R
5.73 + log(1000)






## ECN 377 - R RECIPE CARD  |  question type -> the R lines that answer it
## ------------------------------------------------------------------
library(wooldridge)

## ================= 1. SAMPLE STATISTICS (data you are given) =================
x <- c(2, 5, 9, 4)                      # type the data as a vector
y <- c(3, 7, 8, 6)
length(x)                               # sample size n
sum(x)                                  # sum of the x's
mean(x)                                 # sample mean  (x-bar)
var(x)                                  # sample variance  (divides by n - 1)
sd(x)                                   # sample standard deviation = sqrt(var(x))
cov(x, y)                               # sample covariance
cor(x, y)                               # sample correlation (between -1 and 1, no units)

## Correlation from pieces:  cor = cov / (sd_x * sd_y)
sxy <- 6
sx <- 3                                 # careful: is this a standard deviation ...
vy <- 16                                # ... or a variance? sd = sqrt(variance)
sxy / (sx * sqrt(vy))

## A vector of n numbers: the first is a, the other n - 1 all equal b
n <- 12
a <- 30
b <- 6
(a + (n - 1) * b) / n                   # sample mean


## ================= 2. LINES, CHANGES, PERCENTS =================
## y = b0 + b1*x1 + b2*x2:  change in y = b1*(change in x1) + b2*(change in x2)
## (the intercept never matters for a CHANGE, and a FALL is a negative change)
b1 <- 2.5
b2 <- -1.2
b1 * 3 + b2 * (-5)                      # x1 rises by 3, x2 falls by 5

## Percent change vs percentage-point change
old <- 4.2
new <- 5.1
100 * (new - old) / old                 # percent change  (21.43)
new - old                               # percentage-point change  (0.9)


## ================= 3. A RANDOM VARIABLE FROM A PROBABILITY TABLE =================
xv <- c(0, 2, 5)                        # values X can take
p <- c(0.5, 0.3, 0.2)                   # their probabilities (must sum to 1)
EX <- sum(p * xv)                       # E[X]
EX2 <- sum(p * xv^2)                    # E[X^2]
VarX <- EX2 - EX^2                      # Var(X) = E[X^2] - (E[X])^2
sqrt(VarX)                              # sd(X)

## Conditional versions: same recipe, using the probabilities GIVEN X = x
yv <- c(10, 20, 40)
pc <- c(0.5, 0.3, 0.2)
EYx <- sum(pc * yv)                     # E[Y | X = x]
sum(pc * yv^2) - EYx^2                  # Var(Y | X = x)


## ================= 4. RULES FOR E, Var, Cov (no data needed) =================
EX <- 2
EX2 <- 13
a <- 3
b <- -4
a * EX + b                              # E[aX + b] = a E[X] + b          (the MEAN)
a^2 * (EX2 - EX^2)                      # Var(aX + b) = a^2 Var(X)         (b drops out)
abs(a) * sqrt(EX2 - EX^2)               # sd(aX + b) = |a| sd(X)           (not the mean!)

VarX <- 4
VarY <- 9
CovXY <- -1.5
a <- 2
c2 <- -1                                # coefficient on Y: "2X - Y" means c2 = -1
a^2 * VarX + c2^2 * VarY + 2 * a * c2 * CovXY   # Var(aX + c2*Y)  (watch the signs)


## ================= 5. TWO RANDOM VARIABLES FROM A JOINT TABLE =================
## Population formulas WEIGHT BY THE PROBABILITIES. cor(xv, yv) would ignore them: wrong here.
## find Cor(X,Y) from joint distribution table
x = c(1,2,4,7)
y = c(8,5,11,4)
p = c(.2,.3,.4,.1)
EX = sum(x*p)
EY = sum(y*p)
EXY = sum(x*y*p)
cov = EXY - EX*EY    # Cov = E[XY] - E[X]E[Y]
Ex2 = sum(x^2*p)
varx = Ex2-EX^2
Ey2 = sum(y^2*p)
vary = Ey2-EY^2
cov/(sqrt(varx)*sqrt(vary)) ## Cor(X,Y) = Cov(X,Y)/sd(x)*sd(y)


## ================= 6. OLS BY HAND (a small sample) =================
x <- c(1, 3, 4, 6, 8)
y <- c(2, 5, 4, 9, 10)
b1 <- cov(x, y) / var(x)                # slope = cov(x, y) / var(x)
b0 <- mean(y) - b1 * mean(x)            # intercept = ybar - b1 * xbar
reg <- lm(y ~ x)                        # same numbers from lm()
reg$coefficients

## From summary numbers instead of data
r <- 0.6
sx <- 2
sy <- 5
r * sy / sx                             # slope = cor * sd_y / sd_x  (= cov / var_x)


## ================= 7. PREDICTION, CHANGE, RESIDUALS  (uses x, y, reg from section 6) =================
b0 <- reg$coefficients[[1]]             # save the estimates as plain numbers (do not round them yet)
b1 <- reg$coefficients[[2]]             # [[ ]] drops the label, so results do not print as "(Intercept)"
b0 + b1 * 6                             # predicted y at x = 6  (fitted value)
b1 * 4                                  # predicted CHANGE in y when x rises by 4
reg$fitted.values[3]                    # fitted value of observation 3
reg$residuals[3]                        # residual of observation 3 = actual - fitted
y[3] - (b0 + b1 * x[3])                 # the same residual, by hand


## ================= 8. OLS PROPERTIES (answers you can give without computing) =================
sum(reg$residuals)                      # always 0  (R may print something like -8.7e-15: that IS 0)
sum(x * reg$residuals)                  # always 0
b0 + b1 * mean(x)                       # (xbar, ybar) is on the OLS line: this equals mean(y)
-sum(c(1.2, -0.5, 2.1, -1.4))           # missing residual = minus the sum of the others
## OLS = the line with the SMALLEST sum of squared residuals (least squares)


## ================= 9. SST, SSE, SSR, R^2  (uses y, reg from section 6) =================
SST <- sum((y - mean(y))^2)             # total variation
SSR <- sum(reg$residuals^2)             # unexplained
SSE <- SST - SSR                        # explained
SSE / SST                               # R^2 = 1 - SSR / SST
summary(reg)$r.squared                  # check
100 * summary(reg)$r.squared            # "what percentage is explained?" -> a percent

## From summary numbers instead of data
n <- 25
s2y <- 16
(n - 1) * s2y                           # SST = (n - 1) * sample variance of y
SSE0 <- 90
R20 <- 0.3
SSE0 / R20 - SSE0                       # SSR, from SSE and R^2  (SST = SSE / R^2)


## ================= 10. REAL DATA (wooldridge) =================
data("wage1")
nrow(wage1)                             # sample size
mean(wage1$wage)                        # a column with $
reg1 <- lm(wage ~ tenure, data = wage1) # y ~ x, data = the dataset
reg1$coefficients
reg1$residuals[2]                       # residual of the worker in row 2
mean(wage1$wage[wage1$educ == 12])      # average wage among workers with educ = 12


## ================= 11. UNITS OF MEASUREMENT =================
data("ceosal1")
reg2 <- lm(salary ~ roe, data = ceosal1)               # salary in $1000s
reg2$coefficients                                      # before: 963.19 and 18.50
ceosal1$salarydol <- 1000 * ceosal1$salary             # y times c:  b0 and b1 times c
lm(salarydol ~ roe, data = ceosal1)$coefficients
ceosal1$roedec <- ceosal1$roe / 100                    # x times c:  b1 divided by c, b0 same
lm(salary ~ roedec, data = ceosal1)$coefficients
## R^2 does not change when units change


## ================= 12. LOGS (Table 2.3) =================
reg3 <- lm(log(wage) ~ educ, data = wage1)             # log-level
100 * reg3$coefficients[[2]]            # % change in wage from +1 year of educ
reg4 <- lm(log(salary) ~ log(sales), data = ceosal1)   # log-log
reg4$coefficients[[2]]                  # elasticity: % change in salary from +1% sales
reg5 <- lm(salary ~ log(sales), data = ceosal1)        # level-log
reg5$coefficients[[2]] / 100            # change in salary ($1000s) from +1% sales
##   level-level:  change in y   = b1 * (change in x)
##   log-level:    % change in y = 100 * b1 * (change in x)
##   level-log:    change in y   = (b1 / 100) * (% change in x)
##   log-log:      % change in y = b1 * (% change in x)
