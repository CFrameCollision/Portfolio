# # Mucking about with sensitive data: DO NOT commit data to GitHub

library(tidyverse)
library(dplyr)
library(readxl)
library(stats)
library(mice)
library(psych)
library(naniar)
library(nFactors)
library(GPArotation)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(RcmdrMisc)

# Var 1 is dates, every other var is a question

if (Sys.info()['sysname'] == c("Linux")) {
  setwd("~/Desktop/repositories/Killian-Research/")
} else {
  print("Windows boo")
}

main.data <- read_excel(
  "data/WSWBS Combined.xlsx",
  col_types = c(
    "numeric",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip",
    "skip"
  )
)
#### Cleans up data for analysis ####

missingness <- colSums(is.na(main.data))
missingness
missingness %>% summary()
missing_prop <- colMeans(is.na(main.data))

# Keeps only columns with <= 60% missing values
data_filtered <- main.data[, missing_prop <= 0.60]
data_filtered <- data_filtered[-c(1, 7, 27, 28, 31, 37:50)] # Remove date column, unwanted Qs, and demographics - see journal

corData <- cor(data_filtered, use = "pairwise.complete.obs")
overCorelated <- corData >= 0.9

# Cov for dispersion, cor for relation

# Factor scores for precision vs averaging
# Get rid of all variables with MSA < 0.50
data_filteredMSA <- data_filtered[, KMO(data_filtered)$MSAi > 0.50]
KMO(data_filteredMSA)$MSA
cortest.bartlett(data_filteredMSA)

### Imputation time ###

mcar_data <- data_filteredMSA %>%
  mutate(across(everything(), as.numeric))

# Due to a bug in mcar_test, had to do up to 29
mcar_test(mcar_data[1:29])

# I commented out this code and saved the imputation to a csv to cut down on compilation time.

# imp <- mice(data_filteredMSA, m = 20, method = 'pmm', seed = 1234, maxit = 10)

# Complete for EFA
# imp1 <- complete(imp, action = 1L)
# write.csv(imp1, file = "impData.csv")
# test <- with(imp, ) # Fix this....

# stop("End")

imp1 <- read.csv("impData.csv")

corDataImp <- cor(imp1, use = "pairwise.complete.obs")
overCorelatedImp <- corData >= 0.9


########################################################
#### Explanatory Factor Analysis ####
# ...and lots of crap loops.

# OG value is T, F
rotop <- c("promax")
for (a in c(T)) {
  useImp <- a
  # Runs factanal w/ and w/o imp
  if (useImp == T) {
    imp1 <- read.csv("impData.csv")
    imp1 <- imp1[-1]
  } else {
    imp1 <- data_filteredMSA %>% na.omit()
  }

  egval <- imp1 %>% cor(, use = "pairwise.complete.obs") %>% eigen()
  print("----Eigenvalues----")
  print(egval$values)

  scree(imp1, pc = FALSE)
  fa.parallel(imp1, fa = "fa")

  # OG value is 5:8
  for (x in 7) {
    Nfacs <- x # X in for loop is changeable to mess with factor amount

    # OG value of promax, oblimin. See rotop on line 163
    for (rot in rotop) {
      fit <- factanal(
        imp1[1:31],
        Nfacs,
        rotation = rot,
        na.action = "pairwise.complete.obs",
        scores = "regression"
      )
      print(paste(rot, useImp, sep = " "))
      print(fit, digits = 2, cutoff = 0.3, sort = FALSE)

      # Creates CSV of loaded factors to look at in Excel
      loadings <- fit$loadings
      loadings[abs(loadings) < 0.30] <- NA
      write.csv(
        loadings,
        paste(
          "csv/loadings_",
          Nfacs,
          "_impUse",
          useImp,
          "_",
          rot,
          ".csv",
          sep = ""
        )
      )

      # Next bit of code visualizes factors and cuts off unloaded variables to improve readability
      loads <- fit$loadings[, 1:2]
      significant_rows <- apply(abs(loads), 1, function(row) any(row >= 0.3))
      loads_filtered <- loads[significant_rows, ]
      png(
        filename = paste(
          "plots/",
          rot,
          "/",
          useImp,
          "/facs1-3_maxNFac",
          Nfacs,
          "_Imp",
          useImp,
          "_",
          rot,
          ".png",
          sep = ""
        ),
        res = 120,
        width = 900,
        height = 600
      )
      faPlot <- fa.diagram(
        loads_filtered,
        sort = TRUE,
        main = paste(
          "FA:",
          Nfacs,
          "-",
          "Imp:",
          useImp,
          "-",
          "Rotation:",
          rot,
          sep = " "
        )
      )
      dev.off()

      loads <- fit$loadings[, 3:Nfacs]
      significant_rows <- apply(abs(loads), 1, function(row) any(row >= 0.3))
      loads_filtered <- loads[significant_rows, ]
      png(
        filename = paste(
          "plots/",
          rot,
          "/",
          useImp,
          "/facs3-",
          Nfacs,
          "_",
          "Imp",
          useImp,
          "_",
          rot,
          ".png",
          sep = ""
        ),
        res = 120,
        width = 900,
        height = 600
      )
      fa.diagram(
        loads_filtered,
        sort = TRUE,
        main = paste(
          "FA:",
          Nfacs,
          "-",
          "Imp:",
          useImp,
          "-",
          "Rotation:",
          rot,
          sep = " "
        )
      )
      dev.off()
    }
  }
}

if (length(rotop) > 1 | length(useImp) > 1) {
  stop("Cannot have multiple rotations!")
}
### This section WILL NOT work properly with multiple input options in the for loops. It will display the...
# ...last rot/imp/fact
# Appends factor scores to imp1
imp1 <- cbind(imp1, fit$scores)
write.csv(fit$loadings, file = "data/fitLoadings.csv")

# Adds composite scores
cFactor1a <- rowMeans(imp1[, 19:21])
cFactor1b <- rowMeans(imp1[, 25:31])
factorStats <- cbind(imp1, cFactor1a, cFactor1b)

cFactor_list <- list()
for (a in 1:7) {
  compScore <- imp1[fit$loadings[, a] >= 0.3] %>% rowMeans()
  name <- paste0("cFactor", a)
  cFactor_list[[name]] <- compScore
}
factorStats <- cbind(imp1, cFactor_list)

########################################################
#### PCA Time ####
print("=====PCA=====")
imp1Norm <- scale(imp1[, 1:31])

data_pca <- princomp(imp1Norm)
summary(data_pca)

data_pca$loadings[, 1:2]

fviz_eig(data_pca, addlabels = TRUE, main = "PCA Eigenvalue Proportions")

fviz_pca_var(data_pca, col.var = "black")

fviz_cos2(data_pca, choice = "var", axes = 1:2)

fviz_pca_var(
  data_pca,
  col.var = "cos2",
  gradient.cols = c("black", "orange", "green"),
  repel = TRUE
)
