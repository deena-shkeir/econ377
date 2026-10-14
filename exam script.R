##Exam Script

(5-4)/4
60/15
## sample world
x = c()
y = c()
sum(x)
mean(x)
var(x)
sd(x)
cov(x,y)
cor(x,y)

# finding E[X]
x = c()
p = c()
sum(x*p)

# finding E[X^2]
x = c()
p = c()
sum(x^2*p)

# finding E[aX+bY-c]
EX = 3
EY = 8
a = 3
b = 5
c = 3
a*EX + b*EY - c

# finding Var(X)
# Var(X) = E[X^2]- E[X]^2
EX = 5
EX2 = 44
varx = EX2 - (EX)^2
a = 2
c = 6

var1 = a^2*varx-c
sqrt(var1)
# finding Var(aX+b) 
# Var(aX+b) = a^2*Var(X)
VarX = 5
a = 2
a^2*VarX

# Finding Var(aX+bY)
# Var(aX+bY) = a^2*Var(X)+b^2*Var(Y)+2ab*Cov(X,Y)
varx = 7
vary = 5
cov = -1.6
a = 2
b = 2
a^2*varx+b^2*vary+2*a*b*cov

# find Var(X+Y) -- X and Y are independent
VarX = 6
VarY = 5
VarX+VarY

# find sd(X)
VarX = 23
sqrt(VarX)

# Cov(X,Y) with probabilities
# Cov(X,Y) = E[XY]-E[X]*E[Y]
x = c()
y = c()
p = c()
EXY = sum(x*y*p)
EX = sum(x*p)
EY = sum(y*p)
EXY - EX*EY

# Cov(X,Y) -- covariance property
# = a1*a2*Cov(X,Y)
Cov = -1
a1 = 5
a2 = 4
a1*a2*Cov

# finding Cov(X,Y) = E[XY]-E[X]*E[Y]
EXY = 29  
EX = 3
EY = 5
EXY - EX*EY

# E[Y|X=x]
y = c()
mean(y)

# E[Y|X=x] with probabilities -- conditional expect
y = c()
p = c()
sum(y*p)

# E[aY+b|X=x] == aE[X]+b --> conditional expectation is linear
E = 3
a = 5
b = 1
E*a+b


# finding E(aX+bY)
# E(aX+bY) = aE[X]+bE[Y]
EX = 6
EY = 4
a = 5
b = 5
a*EX + b*EY

# finding E[aX+b]
# E[aX+b] = a*E[X+b]
EX = 7
a = 5
b = 5
a*EX+b

# find E[aX+bY-c] = aE[X]+bE[Y]-c
y = c(11,18,24)
p = c(.5,.3,.2)

EY = sum(y*p)
a = 3
b = 7
c = 3
a*EY+b



# Finding Cor(X,Y) = Cov (X,Y)/sd(X)*sd(Y)
cov = 4
sdx = 3
sdy = 3
cov/(sdx*sdy)
8000/

# Finding E[Y|X = x]
y = c()
p = c()
sum(y*p)


# find the OLS intercept B0^ = ymean-B1^*xmean
x = c()
y = c()
xmean = mean(x)
ymean = mean(y)
B1 = cov(x,y)/var(x) # need B1^ to solve: use cov(x,y)/var(x)
B0= ymean-B1*xmean # Solve for B0^ by plugging into formula
B0

# OLS slope B1^ = cov(x,y)/var(x)
x = c(5,6,7,9,10)
y = c(6,12,2,6,3)
B1 = cov(x,y)/var(x)
B1


x = c(1,2,3,7,9)
y = c(8,4,5,12,4)

B1 = cov(x,y)/var(x)
xmean = mean(x)
ymean = mean(y)
B0 = ymean - B1*xmean
x = 5
y = B0+B1*x 

5-y


# predict y fitted regression
x = c()
y = c()
B1 = cov(x,y)/var(x)
xmean = mean(x)
ymean = mean(y)
B0 = ymean - B1*xmean
x =
B0+B1*x 

# find the residual u^ = y - y^
yhat = 1+(2/10)*6 #1st find y^ w/ given x val
y = 10
y-yhat

# for fitted line and 3 points find SSR = sum((yi-yi^)^2)
x = c()
y = c()
b0 = 4
b1 = 7/10
yhat = b0 + b1*x
SSR = sum((y-yhat)^2)
SSR

# given SST and SSR find SSE
#SST = SSE + SSR => SSE = SST-SSR
SST = 343
SSR = 172
SST - SSR

# given SST and SSR find R^2
#R^2 = SSE/SST or 1 - SSR/SST
SST = 373
SSR = 57
1 - (SSR/SST)


# given SSE and SST find R^2
#R^2 = SSE/SST or 1 - SSR/SST
SSE = 60
SST = 315
SSE/SST

8000/.1

# given SST and SSR what fraction of the variation in Y is unexplained
# SSE => explained, SSR => unexplained 
SST = 343
SSR = 172
SSR/SST

# Given SST and R^2 find SSR
#R^2 = SSE/SST or 1 - SSR/SST 
SST = 268
R2 = 13/100
(1-R2)*SST


SST = (25-1)*28

24*28
summary(2.2, -1.7, 1.1, 1).$residuals

summary.lm(2.2, -1.7, 1.1, 1).$residuals
sse = 200

r2 = .77
sst = r2/sse
ssr =sst-sse
1-(ssr/sst)


b0 = 10
b1 = 0.6
xmean = 2
b0/b1*xmean

## ---- colGPA as a FUNCTION of hsGPA ----
## Notes:  E[colGPA | hsGPA] = 1.5 + 0.5*hsGPA
b0 <- 1.5                      # intercept
b1 <- 0.5                      # slope
colgpa <- function(hsgpa) {       # define colGPA as a function of hsGPA
  b0+b1*hsgpa                          # the line:  b0 + b1*hsgpa
}
colgpa(3.6) # use it: predicted colGPA at hsGPA = 3.6   (-> 3.3)
b1*1      # how much does colGPA change from hsGPA 2.6 to 3.6?  (use colgpa)

# X and Y have the joint probability distribution below (table). 
# the population regression slope is B1 = Cov(X,Y)/Var(X). Find B1
# Find Cov(X,Y) = E[XY] - E[X]E[Y]
x = c(6,4,3)
y = c(3,6,1)
EX = sum(x*p)
EY = sum(y*p)
EXY = sum(x*y*p)
cov = EXY - EX*EY
# Find Var(X) = E[X^2]-E[X]^2
Ex2 = sum(x^2*p)
var = Ex2-EX^2
#solve for B1 using formula
cov/var #B1


#21
x = c(2,3,5,7)
y = c(12,8,7,12)
p= c(.1,.4,.3,.2)
EX = sum(x*p)
EY = sum(y*p)
EXY = sum(x*y*p)
cov = EXY - EX*EY
Ex2 = sum(x^2*p)
varx = Ex2-EX^2
Ey2 = sum(y^2*p)
vary = Ey2-EY^2
cov/(sqrt(varx)*sqrt(vary))
#How to set up dataset

install.package("wooldridge")
library(wooldridge)
data("ceosal1")
data("bwght")


# finding number of rows using nrow()
nrow(ceosal1)

# sample mean hrly wage 
mean(ceosal1$roe)

# sample standard deviation of yrs of educ
sd(wage1$educ) #sd(data$variable)
cor(ceosal1$salary,ceosal1$roe)

# Run OLS regression of wage on educ 
reg = lm(bwght~faminc, data = bwght)
b1 = reg$coefficients[2] #finding slope
b1
b0 = reg$coefficients[1] #finding intercept
b0
# predicted hrly wage for educ = 14 
x = 20
b0 + b1 * x
#how much predicted hrly wage change when educ rises by 2 yrs
xchange = 4
b1*xchange
# SSR for regression of wage on educ
SSR = sum(reg$residuals^2)
SSR
# R^2 for reg of wage on educ
# R^2 = 1-SSR/SST
SST = sum((bwght$bwght - mean(bwght$faminc))^2)
R2 = 1 - SSR/SST
R2

SST-SSR
# check answer of R2
summary(reg)$r.squared #another way to find R^2
reg$residuals

1.5*4-1.9*-3


x = (4.8-4.2)/4.2
x*100
x = c(1,5,5,11)
sd(x)

5

sxy = -6
sx = 6
sy2 = 4
sy = sqrt(sy2)
sxy/(sx*sy)


