# find OLS slope (B1^)
x <- c(5,2,7) #x    
y <- c(6,11,10)  #y salary 
b1 <- cov(x,y)/var(x)  # slope      -- the derived formula:  cov(x, y) / var(x)
        

# find OLS intercept (B0^)
x <- c(8,8,2) #x    
y <- c(2,3,4)  #y 
b1 <- cov(x,y)/var(x)
b0 <- mean(y)-b1*mean(x)  # intercept -- mean(y) - b1 * mean(x)

# fit OLS line and predict y^ at x = __
x <- c(2,8,1) #x   
y <- c(6,5,3)   #y 
b1 <- cov(x,y)/var(x)  # slope      -- the derived formula:  cov(x, y) / var(x)
b0 <- mean(y)-b1*mean(x)        # intercept  -- mean(y) - b1 * mean(x)
b0 + b1 * (3)

# fitted line y^ = -2 + 3/10x a x = 8
(-2)+(3/10)*8

# find the residual u^ = y - y^
yhat = 4+(9/10)*12 #1st find y^ w/ given x val
13-yhat

#predicted change in y when x changes: change in y = B1^*change in x
b1 = 5/10
xchange = 1
b1*xchange

# for fitted line and 3 points find SSR = sum(yi-yi^)^2 
x = c(4,8,3)
y = c(9,2,11)
yhat = 3 + (8/10)*x
SSR = sum((y-yhat)^2)

# given SST and SSR find SSE
#SST = SSE + SSR => SSE = SST-SSR
SST = 248
SSR = 158
SST - SSR

# given SST and SSR find R^2
#R^2 = SSE/SST or 1 - SSR/SST
SST = 243
SSR = 101
1 - (SSR/SST)


# given SSE and SST find R^2
#R^2 = SSE/SST or 1 - SSR/SST
SSE = 68
SST = 234
SSE/SST

# given SST and SSR what fraction of the variation in Y is unexplained
# SSE => explained, SSR => unexplained 
SST = 239
SSR = 168
SSR/SST

# Given SST and R^2 find SSR
#R^2 = SSE/SST or 1 - SSR/SST 
SST = 352
R2 = 12/100
(1-R2)*SST



# Conceptual Questions 
# In OLS fitted value Yi^ = B0^ + B1^Xi is the model's prediction of Y at Xi
# The OLS residual Ui^ = Yi-Yi^ (actual minus fitted)
# Positive residual (Ui^ > 0) means OLS: under-predicted Yi (actual > fitted)
# R^2 is: the fraction of the sample variance in Y explained by X
# total sum of squares decomposes as SST = SSE + SSR
# sum of OLS residuals is always 0
# R^2 close to 0 means: OLS line explaines little of the Variation in Y-common & not necessarily a bad model
# the point (Xbar,Ybar): always lies on the OLS regression line




