## >>> GET & SAVE (details in the R guide) -------------------------
## GET, git mode  -> run in the Console:
##   download.file("https://raw.githubusercontent.com/isaaccloh/ECON377/main/377_2026/D03/D03_starter.R", "D03.R")
## GET, easy mode -> copy this file from github.com/isaaccloh/ECON377 (377_2026/D03) into a new script
## SAVE your work -> commit + push D03.R to your own econ377 repo (or upload it on github.com)
## ----------------------------------------------------------------

## ECN 377 - Day 3  |  ceteris paribus + percentages.   (we fill this in together)

## --- Example A.2:  quantity = 120 - 9.8*price + 0.03*income ---
## the two slopes:
b1 <- -9.8        # slope on price    (hint: -9.8)
b2 <- 0.03        # slope on income   (hint:  0.03)
## price up $1, income held fixed  ->  change in quantity?
b1 * 1              # hint: b1*1 + b2*0
## income decrease by $100
b2*(-100)
## price DOWN $2 and income UP $300  ->  change in quantity?
b1*(-2) + b2*300             # hint: b1*(-2) + b2*300

## --- Percentages ---
## turn the proportion 0.250 into a percent:
0.250*100              # hint: 0.250 * 100

## percent change when x goes 52 -> 38:
xnew = 38          
xold = 52
(xnew-xold)/xold * 100

## percentage-POINT change 52 -> 38 (just the difference):
xnew-xold             

## ================= PROBLEMS (your turn) =========================
pct_chg <- (25-20)/20*100   # (a) percent change from $20 to $25   (hint: (25-20)/20*100)
pct_chg
##(b) interest rate 3% -> 5%: percentage-POINT change = ___ ;
xnew = 25
xold = 20
xnew-xold
##percent change = ___   (comment)
(xnew-xold)/xold * 100

