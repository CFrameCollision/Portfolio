library(tidyverse)
library(plyr)
library(readxl)
library(stargazer)

# Decide on rotation, factor, and imputations before running.

# Var 1 is dates, every other var is a question
if (Sys.info()['sysname'] == c("Linux")) {
  setwd("~/Desktop/repositories/Killian-Research/")
} else {
  print("Windows boo")
}

source("factorAnalysisStuff.R")

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

cor(na.omit(imp1[55:67])) %>% write.csv("corDescrip.csv")


# Annnnddd let's start the regressions

# First function ever!
lmPlot <- function(dat, yform, xform, meth = NULL) {
  model <- lm(as.formula(paste0(yform, " ~ ", xform)), dat) %>% summary()
  model %>% print()

  gplot <- ggplot(dat, aes_string(x = xform, y = yform)) +
    geom_point() +
    geom_smooth(
      method = meth,
      se = TRUE
    ) +
    labs(
      title = paste(meth, "for", yform, "regressed on", xform, sep = " ")
    )
  gplot
}

for (a in c(1:3, 5:7)) {
  print(paste("================== cFactor", a, " ==================", sep = ""))
  lmPlot(factorStats, "cFactor4", paste("cFactor", a, sep = ""), "lm") %>%
    print()

  print(paste("--- cFactor", a, "& age ---", sep = ""))
  lmPlot(
    factorStats,
    "cFactor4",
    paste("cFactor", a, "+ age.eqless.19 + age.24.eqmore", sep = ""),
    "lm"
  ) %>%
    print()
}

lmList <- list()

for (d in c(1:3, 5:7)) {
  for (a in c(17:25, 29)) {
    name <- paste0("cFactor", d, ".", colnames(factorStats)[a])
    lmList[[name]] <- lm(
      as.formula(paste(
        "cFactor4 ~ cFactor",
        d,
        " + ",
        colnames(factorStats)[a],
        sep = ""
      )),
      factorStats,
      na.action = "na.omit"
    )
  }
}

for (a in c(17:25, 29)) {
  name <- paste0("cFactor1a.", colnames(factorStats)[a])
  lmList[[name]] <- lm(
    as.formula(paste(
      "cFactor4 ~ cFactor1a + ",
      colnames(factorStats)[a],
      sep = ""
    )),
    factorStats,
    na.action = "na.omit"
  )
}
for (a in c(17:25, 29)) {
  name <- paste0("cFactor1b.", colnames(factorStats)[a])
  lmList[[name]] <- lm(
    as.formula(paste(
      "cFactor4 ~ cFactor1b + ",
      colnames(factorStats)[a],
      sep = ""
    )),
    factorStats,
    na.action = "na.omit"
  )
}

for (a in c(1:3, 5:7)) {
  name <- paste0("cFactor", a)
  lmList[[name]] <- lm(
    as.formula(paste("cFactor4 ~ cFactor", a, sep = "")),
    factorStats,
    na.action = "na.omit"
  )
}

# ===== Stargazer Chunk =====

stargazer(
  lmList,
  type = "html",
  out = "ols_models_summary.html",
  dep.var.labels = "Spiritual Life - cFactor4",
  ci.level = 0.95
)
stargazer(
  lmList[1:10],
  type = "html",
  out = "ols_models_summary1.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
stargazer(
  lmList[11:20],
  type = "html",
  out = "ols_models_summary2.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
stargazer(
  lmList[21:30],
  type = "html",
  out = "ols_models_summary3.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
stargazer(
  lmList[31:40],
  type = "html",
  out = "ols_models_summary4.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
stargazer(
  lmList[41:50],
  type = "html",
  out = "ols_models_summary5.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
stargazer(
  lmList[51:60],
  type = "html",
  out = "ols_models_summary6.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
stargazer(
  lmList[61:70],
  type = "html",
  out = "ols_models_summary7.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
stargazer(
  lmList[71:80],
  type = "html",
  out = "ols_models_summary8.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
stargazer(
  lmList[81:86],
  type = "html",
  out = "ols_models_summary9.html",
  dep.var.labels = "Spiritual Life - cFactor4"
)
