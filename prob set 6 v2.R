# find OLS slope (B1^)
x <- c(1,5,5) #x    
y <- c(11,11,12)  #y  
b1 <- cov(x,y)/var(x)  # slope: cov(x, y) / var(x)
b1

# find OLS intercept (B0^)
x <- c(8,4,2) #x    
y <- c(5,12,1)  #y 
b1 <- cov(x,y)/var(x)
b0 <- mean(y)-b1*mean(x)  # intercept: mean(y) - b1 * mean(x)
b0

# fit OLS line & predict y^ at x = __  -- y^=B0^+B1^*X
x <- c(2,7,8) #x   
y <- c(11,10,1)   #y 
b1 <- cov(x,y)/var(x)   # slope:  cov(x, y) / var(x)
b0 <- mean(y)-b1*mean(x)# intercept: mean(y) - b1 * mean(x)
x = 4
b0 + b1 * (x)

# fitted line y^ = -2 + 3/10x at x = 8
2+(3/10)*9

# find the residual u^ = y - y^
yhat = 1+(2/10)*6 #1st find y^ w/ given x val
y = 10
y-yhat

#predicted change in y when x changes: change in y = B1^*change in x
b1 = 5/10
xchange = 3
b1*xchange

# for fitted line and 3 points find SSR = sum((yi-yi^)^2)
x = c(9,10,9)
y = c(9,12,11)
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



# Conceptual Questions 
# In OLS fitted value Yi^ = B0^ + B1^Xi is the model's prediction of Y at Xi

# The OLS residual Ui^ = Yi-Yi^ (actual minus fitted)

# Positive residual (Ui^ > 0) means OLS: under-predicted Yi (actual > fitted)

# R^2 is: the fraction of the sample variance in Y explained by X

# total sum of squares decomposes as SST = SSE + SSR

# sum of OLS residuals is always 0

# R^2 close to 0 means: OLS line explains little of the Variation in Y-common & not necessarily a bad model

# the point (Xbar,Ybar): always lies on the OLS regression line




