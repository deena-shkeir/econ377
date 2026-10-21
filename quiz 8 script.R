#PS8 14, 17, 19 and PS9 1, 5, 6, 9, 13, 14, 15, 16.

#In a regression of y on x the OLS slope is B^1 = 0.65 Every value of x  is then
#divided by 10 (example, percent to decimal when the divisor is 100). What is the new OLS slope? 
#Hint: if x is multiplied by c, B^1 is multiplied by 1/c and B^0 doesn't change. dividing by k is multiplying by c = 1/k
b1 = 1.48
b1*100

#log-level reg gives log(y) = 1.8 + 0.06x By about what percent does y
#change when x rises by 2?
#Hint: log-level %ychange = 100*B^1*xchange
b1 = 0.06
xchange = 2 
100*b1*xchange

#level-log reg gives y^ = 99 + 6.4log(x) By about how many units does y
#change when x rises by 2%?
#Hint: level-log ychange = (B^1/100)*%xchange
b1 = 6.4
xchange = 2
(b1/100)*xchange

#Log-log regression of CEO salary on firm sales gives log(salary) = B^0+B^1log(sales)
#with salary in thousands. Salary is them measured in dollars instead, what happens?
# B^1 does not change; only B^0 changes 

#log-log reg gives log(y) = 5.4 + 1.24log(x) by about what percent does y change when x falls by 6% 
#Hint: log-log %ychange = B^1*%xchange
b1 = 1.24
xchange = -6
ychange = b1*xchange
ychange

#Data: ceosal1 (salary = a CEO's 1990 salary in thousands of dollars; 
#roe = the firm's return on equity, in percent; sales = firm sales in millions of dollars).
#Run the log-level regression lm(log(salary) ~ roe, data = ceosal1). 
#By about what percent does predicted salary change when return on equity rises by 5 points?
library(wooldridge)
data(ceosal1)
reg = lm(log(salary) ~ roe, data = ceosal1)
b1 = reg$coefficients[2]
roe = 5
ychange = b1*100*roe
ychange

#In wage = B0 + B1educ + U which of the following is the most likely to contribute
#omitted-variable bias to the OLS slope on educ?
# The worker's ability, which raises wages and tends to be higher for people with more education. 

#Holding everything else fixed, which change makes Var(B^1) smaller?
# More spread-out values of X (a larger SSTx)

#Under SLR1–SLR5, the error standard deviation is o = 4 A sample has the five X
#values x = (1,4,5,7,10) find sd(B^1)
#Hint: first SSTx = sum(xi-xbar)^2 ---> sum((x-mean(x))^2) then sd(B^1) = o/sqrt(SSTx)
x = c(1,7,8,9,10)
SSTx = sum((x-mean(x))^2)
o = 2
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
