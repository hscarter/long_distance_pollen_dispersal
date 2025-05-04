#------------------------------------------
# Figure S2A - G-test goodness-of-fit
#------------------------------------------

library(DescTools)


## MALES (File: wind tunnel)

# For comparisons: DC+linalool vs DC control #
observed=c(11,12)    
expected=c(0.5, 0.5)
GTest(x=observed,p=expected,correct="none")


# Left vs right #
observed=c(7,16)    
expected=c(0.5, 0.5)
GTest(x=observed,p=expected,correct="none") 


## FEMALES ##

# For comparisons: DC+linalool vs DC control #
observed=c(9,8)    
expected=c(0.5, 0.5)
GTest(x=observed,p=expected,correct="none")


# Left vs right #
observed=c(11,6)    
expected=c(0.5, 0.5)
GTest(x=observed,p=expected,correct="none") 



#------------------------------------------
# Figure S2B - Latency 
#------------------------------------------

### UNPAIRED T-TEST


## MALES (File: latency_males.csv)

# Shapiro-Wilk normality test 

# Shapiro-Wilk normality test for DC+Lin latency
with(latency_males, shapiro.test(latency[treatment == "linalool"]))

# Shapiro-Wilk normality test for DC control latency
with(latency_males, shapiro.test(latency[treatment == "control"])) 


# F-test to test for homogeneity in variances. 
res.ftest <- var.test(latency ~ treatment, data = latency_males)
res.ftest


# Compute unpaired two-samples t-test
res <- t.test(latency ~ treatment, data = latency_males, var.equal = TRUE)
res




## FEMALES (File: latency_females.csv)

# Shapiro-Wilk normality test for DC+Lin latency
with(latency_females, shapiro.test(latency[treatment == "linalool"]))

# Shapiro-Wilk normality test for DC control latency
with(latency_females, shapiro.test(latency[treatment == "control"])) 

#F-test to test for homogeneity in variances. 
#this can be performed with the function var.test() as follow:
res.ftest <- var.test(latency ~ treatment, data = latency_females)
res.ftest

# Compute unpaired two-samples t-test
res <- t.test(latency ~ treatment, data = latency_females, var.equal = TRUE)
res


