library(tidyverse)
library(plyr)
library(readxl)
library(stargazer)

# Decide on rotation, factor, and imputations before running.

# Var 1 is dates, every other var is a question
if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/repositories/Killian-Research/")
} else {
  print("Windows boo")
}

source("factorAnalysisStuff.R", echo = FALSE)

# Import imputation data. Always reimport, as factorAnalysisStuff.R overwrites imp1
imp1 <- read.csv("impData.csv")
imp1 <- imp1[-1] %>% as_tibble()

imp1 <- cbind(imp1, fit$scores)

# Adds composite scores for OLS lm
cFactor1a <- rowMeans(imp1[, 19:21])
cFactor1b <- rowMeans(imp1[, 25:31])
factorStats <- cbind(fit$scores, cFactor1a, cFactor1b)

cFactor_list <- list()
for (a in 1:7) {
  compScore <- imp1[fit$loadings[, a] >= 0.3] %>% rowMeans()
  name <- paste0("cFactor", a)
  cFactor_list[[name]] <- compScore
}
cFactor_list <- do.call(cbind.data.frame, cFactor_list)
factorStats <- cbind(factorStats, cFactor_list)
imp1 <- cbind(imp1, factorStats)


# Dummy code demographics. Already dummy: ethnicity, vet, athletics
dummy.var <- main.data %>%
  dplyr::mutate(
    # Grade
    grade.undergrad = if_else(D1 %in% 1:4, 1, 0),

    # Employment
    employed = if_else(D2 %in% 3:5, 1, 0),

    # Credits
    credits.31.eqmore = if_else(D3 %in% 4:5, 1, 0),

    # Race (international coded as POC)
    race.poc = if_else(D4 %in% c(1:5, 7), 1, 0),

    # Gender ID
    gender.female = if_else(D6 %in% 2, 1, 0),

    # Gen of student
    gen.first = if_else(D8 %in% 2, 1, 0),

    # Campus status
    campus.on = if_else(D10 %in% 1, 1, 0),

    # Activites
    activities.5.eqmore = if_else(D11 %in% 5:6, 1, 0),

    # Distance from home
    distance.250.eqmore = if_else(D12 %in% 1:2, 1, 0),

    # Age
    age.eqless.19 = if_else(D13 %in% 1:2, 1, 0),
    age.20.23 = if_else(D13 %in% 3:4, 1, 0),
    age.24.eqmore = if_else(D13 %in% 4:9, 1, 0),

    # Chronic illness
    illness.yes = if_else(D14 %in% 2, 1, 0),
    .keep = "none"
  )

# Bind
imp1 <- cbind(imp1, dummy.var)
factorStats <- cbind(factorStats, dummy.var)
factorStats %>% write.csv(, file = "factorstats.csv")

# cor(na.omit(imp1[55:67])) %>% write.csv("corDescrip.csv")

lmList <- list()
for (a in c(1:3, 5:7)) {
  name <- paste0("cFactor", a)
  lmList[[name]] <- lm(
    as.formula(paste0(
      "cFactor4 ~ cFactor",
      a,
      " + grade.undergrad + employed + credits.31.eqmore + race.poc + gender.female + gen.first + campus.on + activities.5.eqmore + distance.250.eqmore + illness.yes"
    )),
    factorStats,
    na.action = "na.omit"
  )
  if (a >= 5) {
    a <- a - 1
  }
  lmList[[a]] %>%
    summary() %>%
    print()
}

stargazer(
  lmList,
  type = "html",
  out = "ols_models_all.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)

##### Parsimonious Models - cFactor 4 - See notebook ####
#cFactor 1
lm(cFactor4 ~ cFactor1 + activities.5.eqmore, factorStats) %>% summary()

#cFactor 2 - see notebook
lm(cFactor4 ~ cFactor2 + gen.first + activities.5.eqmore, factorStats) %>%
  summary()
lm(cFactor4 ~ cFactor2 + activities.5.eqmore, factorStats) %>% summary()

#cFactor 3
lm(cFactor4 ~ cFactor3 + gen.first + activities.5.eqmore, factorStats) %>%
  summary()
lm(cFactor4 ~ cFactor3 + activities.5.eqmore, factorStats) %>% summary()

#cFactor 5
lm(cFactor4 ~ cFactor5 + gen.first + activities.5.eqmore, factorStats) %>%
  summary()
lm(cFactor4 ~ cFactor5 + activities.5.eqmore, factorStats) %>% summary()

#cFactor 6
lm(cFactor4 ~ cFactor6 + gen.first + activities.5.eqmore, factorStats) %>%
  summary()
lm(cFactor4 ~ cFactor6 + activities.5.eqmore, factorStats) %>% summary()

# cFactor 7
lm(cFactor4 ~ cFactor7 + activities.5.eqmore + gender.female, factorStats) %>%
  summary()
lm(cFactor4 ~ cFactor7 + gender.female, factorStats) %>% summary()
lm(cFactor4 ~ cFactor7 + activities.5.eqmore, factorStats) %>% summary() # Best adjusted R^2

lm(cFactor4 ~ cFactor7 + cFactor3 + activities.5.eqmore, factorStats) %>%
  summary()
