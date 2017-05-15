# HYPOTHESIS TESTING!!

"Use the ICC World Cup data available at cricinfo.
Set up hypotheses to disprove the following Null hypotheses
-Tendulkar's  batting average is not better than Rahul Dravid's.
(TWO SAMPLE TEST OF MEAN!)  
-The Strike rates of Tendulkar and Dravid are the same
(TWO SAMPLE TEST OF MEAN!)
-Percentage of of time Tendulkar gets out in single digits is the same as Dravid's
(TWO SAMPLE TEST OF PROPORTION)
-Dravid is more consistent than Tendulkar
(TWO SAMPLE TEST OF VARIANCE)"

# Set the working directory to the location of the data files
setwd("C:/DataScience/gitHub/hypothesis")

#Load the data for Tendulkar and Dravid 
tendulkar <- read.csv("tendulkar.csv")
dravid <- read.csv("dravid.csv")

# Test 1 -Tendulkar's  batting average is not better than Rahul Dravid's.
# H0: μt - μd <= 0
# Ha: μt - μd > 0
t.test(tendulkar$Runs, dravid$Runs, alternative = "g", conf.level = 0.98)
print("As the p-value is >0.02, the H0 can NOT be rejected and thus Tendulkar's  batting 
# average is not better than Rahul Dravid's.")

# Test 2 -The Strike rates of Tendulkar and Dravid are the same.
# H0: μt - μd = 0
# Ha: μt - μd =/ 0
t.test(tendulkar$SR, dravid$SR, alternative = "t", conf.level = 0.98)
print("As the p-value is >0.01, the H0 can NOT be rejected and thus The Strike rates of 
#Tendulkar and Dravid are the same.")

# Test 3 - Percentage of time Tendulkar gets out in single digits is the same as Dravid's.
# H0: pt - pd = 0
# Ha: pt - pd =/ 0
# The prop.test() function uses chi square distribution so can not be used. So, lets do 
# it manually.
# Calculate the test statistic (tabular)
TSTabular <- qnorm(0.99,0,1)
TSTabular
# Calculate the test statistic (computed)
# No. of samples in each of the data
nt <- length(tendulkar$Runs)
nt
nd <- length(dravid$Runs)
nd
# Proportion where Tendulkar and Dravid got out in single digits
pt <- sum(tendulkar$Runs<10)/nt
pt
pd <- sum(dravid$Runs<10)/nd
pd
# calculate overall proportion= phat
phat <- (nt*pt + nd*pd)/(nt+nd)
phat
#Test Statistic Computation 
TSComputed <- (pt-pd)/(sqrt(phat*(1-phat)*(1/nt+1/nd)))
TSComputed
TSComputed > TSTabular || TSComputed < -1 * TSTabular
print("As the TSComputed lies within the values of TSTabular and (-)TSTabular, H0 can not be 
rejected, thus... Percentage of time Tendulkar gets out in single digits is the same as Dravid's.")

# Test 4 - Dravid is more consistent than Tendulkar
# H0: σt^2 >= σd^2
# Ha: σt^2 < σd^2
var.test(tendulkar$Runs, dravid$Runs, alternative = "l", conf.level = 0.98)
print("As the p-value is >0.02, the H0 can NOT be rejected and thus Dravid is more consistent than Tendulkar")

