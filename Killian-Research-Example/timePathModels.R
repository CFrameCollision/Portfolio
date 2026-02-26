library(lavaan)
library(tidyverse)
library(semPlot)

if ("OpenMX" %in% loadedNamespaces() == F) {
    Sys.setenv(OMP_NUM_THREADS = parallel::detectCores())
    library(OpenMx)
    mxOption(key = 'Number of Threads', value = parallel::detectCores())
}

## None of the below models have all significant predictors. Problem lies in act.5.eqmore
a <- 1
if (a == 1) {
    primaryData <- read.csv("data/timeOrdered.csv")[-1]
} else {
    # V2 has added diffs between factor 3 and 7
    primaryData <- read.csv("data/timeOrderedV2.csv")
}


print("================")
print("Model A")
print("================")

modelA <- '
cFactor4 ~ a*cFactor3 + b*cFactor7
cFactor3 ~~ cFactor7
Total := a + b
'

fitA <- sem(modelA, primaryData)
summary(fitA)

print("================")
print("Model B")
print("================")

modelB <- '
cFactor4 ~ a*cFactor7 + b*activities.5.eqmore
cFactor7 ~~ activities.5.eqmore
Total := a + b
'

fitB <- sem(modelB, primaryData)
summary(fitB)

print("================")
print("Model C")
print("================")

modelC <- '
cFactor4 ~ b*cFactor3 + a*activities.5.eqmore
activities.5.eqmore ~ c*cFactor3
facThroughAct := c*a
Total_fac3 := c*a + b
'

fitC <- sem(modelC, primaryData)
summary(fitC)

print("================")
print("Model D")
print("================")

modelD <- '
cFactor4 ~ a*cFactor7
activities.5.eqmore ~ b*cFactor7
Total := a*b
'

fitD <- sem(modelD, primaryData)
summary(fitD)

print("================")
print("Model E")
print("================")

modelE <- '
cFactor4 ~ a*cFactor3
cFactor3 ~ b*activities.5.eqmore
Total := a*b
'

fitE <- sem(modelE, primaryData)
summary(fitE)

## Graphical ##
# to change layout, use 'temp <- semPlotModel' and locate name order

semPaths(
    fitA,
    'mod',
    "est",
    edge.color = "black",
    edge.label.cex = 0.8
)

semPaths(
    fitB,
    'mod',
    "est",
    edge.color = "black",
    edge.label.cex = 0.8
)

semPaths(
    fitC,
    'mod',
    "est",
    edge.color = "black",
    edge.label.cex = 0.8
)

# Exploring the relationship (if any) between acts and cFac4.
ggplot(primaryData, mapping = aes(x = activities.5.eqmore, y = cFactor4)) +
    geom_point() +
    geom_smooth(
        formula = cFactor4 ~ activities.5.eqmore + cFac3.7Diffs,
        method = "lm"
    )

jitter <- position_jitter(width = 0.05, height = 0, seed = 1234)
ggplot(primaryData, mapping = aes(x = activities.5.eqmore, y = cFactor4)) +
    geom_point(position = jitter)
