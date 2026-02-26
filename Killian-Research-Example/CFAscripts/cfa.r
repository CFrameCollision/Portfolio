library(lavaan)
library(tidyverse)

dataMain <- read.csv("data/impData_D11.csv")[-c(1, 2)]

## This file is a dead end and not sound theory-wise. See: [https://phdcentre.com/efa-vs-cfa/]

# Test structure of 7 factor model
model1 <- '
fac1 =~ Q20 + Q21 + Q22 + Q28 + Q29 + Q31 + Q32 + Q33 + Q34 + Q35
fac2 =~ Q14 + Q15 + Q16 + Q17 + Q18 + Q19
fac3 =~ Q1 + Q2 + Q3
fac4 =~ Q23 + Q24 + Q25
fac5 =~ Q9 + Q10 + Q11 + Q12
fac6 =~ Q7 + Q8
fac7 =~ Q4 + Q5
'

print("====m1====")
m1 <- cfa(model1, dataMain, estimator = "MLR")
m1 %>% summary(fit.measure = TRUE, standardized = TRUE)

# Test structure of 7 factor model
model2 <- '
fac1 =~ Q20 + Q21 + Q22
fac2 =~ Q14 + Q15 + Q16 + Q17 + Q18 + Q19
fac3 =~ Q1 + Q2 + Q3 + Q4 + Q5
fac4 =~ Q23 + Q24 + Q25
fac5 =~ Q9 + Q10 + Q11 + Q12
fac6 =~ Q7 + Q8
fac7 =~ Q28 + Q29 + Q31 + Q32 + Q33 + Q34 + Q35
'

print("====m2====")
m2 <- cfa(model2, dataMain, estimator = "MLR")
m2 %>% summary(fit.measure = TRUE, standardized = TRUE)

model2.1 <- '
fac1 =~ Q20 + Q21 + Q22
fac2 =~ Q14 + Q15 + Q16 + Q17 + Q18 + Q19
fac3 =~ Q1 + Q2 + Q3 + Q4 + Q5
fac4 =~ Q23 + Q24 + Q25
fac5 =~ Q9 + Q10 + Q11 + Q12
fac6 =~ Q7 + Q8
fac7 =~ Q28 + Q29 + Q31 + Q32 + Q33 + Q34 + Q35
Q4 ~~ Q5
Q2 ~~ Q3
fac6 =~ Q29
'

m2.1 <- cfa(model2.1, dataMain, estimator = "MLR")
m2.1 %>% summary(fit.measure = TRUE, standardized = TRUE)

# Test CFA model through path analysis

print("====m3====")
m3 <- '
# CFA Factors
fac1 =~ Q20 + Q21 + Q22
fac2 =~ Q14 + Q15 + Q16 + Q17 + Q18 + Q19
fac3 =~ Q1 + Q2 + Q3 + Q4 + Q5
fac4 =~ Q23 + Q24 + Q25
fac5 =~ Q9 + Q10 + Q11 + Q12
fac6 =~ Q7 + Q8
fac7 =~ Q28 + Q29 + Q31 + Q32 + Q33 + Q34 + Q35
#
# Path Analysis

'
