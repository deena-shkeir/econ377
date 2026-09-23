##Problem Set 5
##extra note: 
## y is dep var, B0 is intercept, B1 is slope, x is indep var, u is error term

#1
#Q: in simple linear reg model Y=B0+B1X+U, the DEPENDENT variable is:
#A:Y

#2
#Q: in Y=B0+B1X+U the error term U represents:
#A: factors other than X that affect Y

#3
#Q: in Y=B0+B1X+U the slope B1 measures:
#A: the ceteris paribus effect of X on Y (change in Y per one unit rise in X, 
# holding other factors fixed)

#4
#Q: the zero conditional mean assumption E[U|X]=0 says:
#A: the avg of U does not depend on X

#5
#Q: under the SLR model w/ E[U|X]=0 the conditional mean E[Y|X=x] equals:
#A: B0+B1*X

#6
#Q: The OLS slope estimate B1^ can be written as:
#A: cov(x,y)/var(x,y) 
## also = Sxy/Sx^2

#7
#Q: We must estimate B0,B1 (rather than compute them exactly) because:
#A: they're population quantities; we observe only a sample

#8
#Q: in Y=(B0+B1X)+U the term B0+B1X is:
#A: the part of Y explained by X (its conditional mean)

#9
#Q: the OLS slope is B1^ = cov(x,y)/var(x). Given cov(x,y) = -7 and var(x) = 3 find B1^
#A: -2.33 
#plug in cov as numerator, var as denominator
(-7)/3

#10
#Q: the OLS intercept is B0^ = y mean - B1^*x mean. Given y mean = 7, x mean = 4, 
# slope B1^ = 4/10, find B0^
#A:5.4
7-(4/10)*4

4-(2/10)*9



#11
#Q: for sample x = (2,2,1), y = (3,8,2) find the OLS slope 
# B1^ = sumation(xi-x mean)(yi-y mean)/sumation(xi-x mean)^2
#A: 3.5
x = c(2,2,1)
y = c(3,8,2)
xmean = mean(x)
ymean = mean(y)
sum((x-xmean)*(y-ymean))/sum((x-xmean)^2)

x = c(2,7,8)
y = c(11,4,3)
xmean = mean(x)
ymean = mean(y)
sum((x-xmean)*(y-ymean))/sum((x-xmean)^2)






#12
#Q: sample x = (7,8,7), y = (2,7,10) find the OLS intercept B0^ = y mean-B1^*x mean
#A: -1
x = c(7,8,7)
y = c(2,7,10)
xmean = mean(x)
ymean = mean(y)
# need B1^ to solve: use cov(x,y)/var(x)
B1 = cov(x,y)/var(x)
#Solve for B0^ by plugging into formula
ymean-B1*xmean 

#13
#Q: a fitted regression line is y^ = B0^+B1^*x. WIth Bo^ = 5 and B1^ = 7/10, 
# find the predicted y^ at x = 6
#A: 9.2
B0 = 5
B1 = 7/10
x = 6
# find y^ by plugging into formula
B0+B1*x







#14
#Q:for sample x = (5,2,1), y = (9,12,4), fit the OLS line and predict y^ at x = 1
#A:
#B1^ = cov(x,y)/var(x)
#B0^ = y mean-B1^*x mean
# y^ = B0^+B1^*x 
x = c (5,2,1)
y = c(9,12,4)
# solve for B1^
B1 = cov(x,y)/var(x)
xmean = mean(x)
ymean = mean(y)
#solve for BO^
B0 = ymean - B1*xmean
#solve for y^ using formula and plug in given x value
B0+B1*1 #plug in the x = 1



#B1^ = cov(x,y)/var(x)
#B0^ = y mean-B1^*x mean
# y^ = B0^+B1^*x 
x = c (2,5,3)
y = c(9,11,1)
# solve for B1^
B1 = cov(x,y)/var(x)
xmean = mean(x)
ymean = mean(y)
#solve for BO^
B0 = ymean - B1*xmean
#solve for y^ using formula and plug in given x value
B0+B1*7 #plug in the x = 1



#15
#Q: E[colGPA|hsGPA] = 12/10 + 6/10 *hsGPA. Find the expected colGPA at hsGPA = 33/10
#A:3.18 
(20/10)+(3/10)*(31/10)
##another way to do this:
hsGPA = 33/10
(12/10)+(6/10)*hsGPA

#16
#Q: in wage = B0+B1educ+U the estimated slope is B1^ = 9/10. Ceteris paribus,
# if educ rises by 6 yrs by how much does predicted wage change
#A:5.4
#change in y = B1*change in x (ceteris paribus)
B1 = 9/10
# plug in B1 and multiple by change in x 
B1*9

#17
#Q: X any Y have the joint probability distribution below (table). Find E[X]
#A:4.9
x = c(3,1,5)
y = c(0,0,6)
p = c(0.2,0.3,0.5)
sum(x*p) #E[X]

#18
#Q: X any Y have the joint probability distribution below (table). 
# Find Cov(X,Y) = E[XY] - E[X]E[Y]
#A: -2.18
x = c(6,5,3)
y = c(4,7,1)
p = c(.2,.3,.5)
EX = sum(x*p)
EY = sum(y*p)
EXY = sum(x*y*p)
# solve for Cov using formula
EXY - EX*EY





#19
#Q: X any Y have the joint probability distribution below (table). 
# the population regression slope is B1 = Cov(X,Y)/Var(X). Find B1
#A: 0.77
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

#20
#Q: review (sample). For x = (4,4,0), y = (2,3,5), find the SAMPLE covariance 
# Sxy = (1/n-1)sumation(xi-x mean)(yi-y mean)
#A:
#sample cov is just cov(x,y)
x = c(4,4,0)
y = c(2,3,5)
cov(x,y)




## in class example:
## ---- Demo: colGPA as a FUNCTION of hsGPA  (illustrates R functions) ----
## Notes:  E[colGPA | hsGPA] = 1.5 + 0.5*hsGPA
b0 <- 1.5                      # intercept
b1 <- 0.5                      # slope
colgpa <- function(hsgpa) {       # define colGPA as a function of hsGPA
  b0+b1*hsgpa                          # the line:  b0 + b1*hsgpa
}
colgpa(3.6) # use it: predicted colGPA at hsGPA = 3.6   (-> 3.3)
b1*1      # how much does colGPA change from hsGPA 2.6 to 3.6?  (use colgpa)









