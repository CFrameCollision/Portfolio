library(lavaan)
library(tidyverse)
library(semPlot)

if ("OpenMX" %in% loadedNamespaces() == F) {
    Sys.setenv(OMP_NUM_THREADS = parallel::detectCores())
    library(OpenMx)
    mxOption(key = 'Number of Threads', value = parallel::detectCores())
}

factorStats <- read.csv("factorstats.csv")
factorStats <- factorStats[-1]

modelA <- '
cFactor4 ~ e*cFactor7 + d*cFactor3 + f*activities.5.eqmore
activities.5.eqmore ~ b*cFactor7 + c*cFactor3
cFactor7 ~~ cFactor3
facsThroughAct := b*f + c*f
Total_fac7 := b*f + e
Total_fac3 := c*f + d
'

modelB <- '
cFactor4 ~ d*cFactor7 + e*cFactor3 + f*activities.5.eqmore
cFactor7 ~~ cFactor3
cFactor3 ~~ activities.5.eqmore
cFactor7 ~~ activities.5.eqmore
Total := d + e + f
'

modelC <- '
cFactor4 ~ e*cFactor7 + f*activities.5.eqmore
activities.5.eqmore ~ b*cFactor7
facThroughAct := b*f + e
'

modelD <- '
cFactor4 ~ e*cFactor7 + f*activities.5.eqmore
activities.5.eqmore ~~ cFactor7
Total := f + e
'

modelE <- '
cFactor4 ~ b*cFactor7 + c*activities.5.eqmore
cFactor7 ~ a*activities.5.eqmore
Total := a*b + c
'

semPrint <- function(semmodel, modeldata) {
    fit_name <- paste0("sem", deparse(substitute(semmodel)))
    fit_obj <- sem(model = semmodel, data = modeldata)
    assign(fit_name, fit_obj, envir = .GlobalEnv)
    summary(fit_obj)
}

print("================ModelA")
semPrint(modelA, factorStats)
print("================ModelB")
semPrint(modelB, factorStats)
print("================ModelC")
semPrint(modelC, factorStats)
print("================ModelD")
semPrint(modelD, factorStats)
print("================ModelE")
semPrint(modelE, factorStats)

## Graphical ##
# to change layout, use 'temp <- semPlotModel' and locate name order
manLayoutA <- matrix(
    c(
        3,
        2.5, #4
        1.7,
        2.5, #act
        1,
        4, #7
        1,
        1 #3
    ),
    ncol = 2,
    byrow = TRUE
)

semPaths(
    semmodelA,
    'mod',
    "est",
    layout = manLayoutA,
    edge.color = "black",
    edge.label.cex = 0.8
)

manLayoutB <- matrix(
    c(
        3,
        2.5, #4
        1,
        4, #7
        1,
        1, #3
        1.5,
        2.5 #act
    ),
    ncol = 2,
    byrow = TRUE
)

semPaths(
    semmodelB,
    'mod',
    'est',
    layout = manLayoutB,
    edge.color = "black",
    edge.label.cex = 0.8
)

semPaths(
    semmodelC,
    'mod',
    "est",
    edge.color = "black",
    edge.label.cex = 0.8
)

semPaths(
    semmodelD,
    'mod',
    "est",
    edge.color = "black",
    edge.label.cex = 0.8
)

semPaths(
    semmodelE,
    'mod',
    'est',
    edge.color = "black",
    edge.label.cex = 0.8
)
