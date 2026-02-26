library(tidyverse)
library(readr)

dataWhole <- readxl::read_xlsx("data/WSWBS Combined ID.xlsx")
dataImp <- read.csv("impData.csv")

# Add in activities to differences
factorStats <- read.csv("factorstats.csv")
activities <- factorStats %>% select(activities.5.eqmore)

dataID <- cbind(dataImp, dataWhole[c("ResponseId", "P2", "P3")], activities)

if ("package:plyr" %in% search()) {
  detach("package:plyr", unload = TRUE)
}

n_occur <- data.frame(table(dataID$P2))
n_occurEmail <- data.frame(table(dataID$P3))

n_occur[n_occur$Freq > 1, ]
indices <- n_occur[which(n_occur$Freq > 1), ]

n_occurEmail[n_occurEmail$Freq > 1, ]
indicesEmail <- n_occurEmail[which(n_occurEmail$Freq > 1), ]

subData <- dataID %>% filter(P2 %in% indices$Var1 | P3 %in% indicesEmail$Var1)

# Attach D11 OG prior to renumbering mismatch P2/P3

subData <- subData %>%
  mutate(
    X = as.integer(X),
    D11 = dataWhole$D11[X]
  )

# Break D11 into continuous midpoints. See key for more info.
subDataD11Mid <- subData %>%
  mutate(
    D11Mid = case_match(
      D11,
      1 ~ 0,
      2 ~ 1,
      3 ~ 2,
      4 ~ 4,
      5 ~ 7.5,
      6 ~ 10
    )
  )

# P2 and P3 mismatch

mismatched <- subDataD11Mid %>%
  group_by(P3) %>%
  filter(n_distinct(P2, na.rm = TRUE) > 1) %>%
  ungroup() %>%
  arrange(P3)

mismatchedIDs <- mismatched %>%
  dplyr::distinct(P3) %>%
  mutate(P2ID = as.character(row_number()))

subDataD11Mid <- subDataD11Mid %>%
  left_join(mismatchedIDs, by = "P3") %>%
  mutate(P2 = coalesce(P2ID, P2)) %>%
  select(-P2ID)

# table(subData$P2)

subDataD11Mid <- subDataD11Mid %>%
  group_by(P2) %>%
  dplyr::mutate(X = row_number()) %>%
  ungroup()

wideSubData <- subDataD11Mid %>%
  group_by(P2) %>%
  pivot_wider(
    id_cols = P2,
    names_from = X,
    values_from = c(Q1:Q35, activities.5.eqmore, D11, D11Mid),
    names_sep = "."
  )

wideSubData %>% write.csv("data/wideSubDataV2.csv")


##### Differences between T2 and T1

data_long <- subDataD11Mid %>%
  pivot_longer(
    cols = -c(P2, X, ResponseId, P3),
    names_to = "Question",
    values_to = "Value"
  )

analysis <- data_long %>%
  group_by(P2, Question) %>%
  summarize(
    First_Response = first(Value, order_by = X),
    Last_Response = last(Value, order_by = X),
    Change = Last_Response - First_Response,
    Count = n(),
    .groups = "drop"
  )

analysis <- analysis %>%
  select(P2, Question, Change) %>%
  pivot_wider(names_from = Question, values_from = Change)

analysis <- analysis %>%
  select(P2, colnames(dataID)[2:32], activities.5.eqmore, D11, D11Mid)

# Composite Scores

fit <- read.csv("data/fitLoadings.csv")

fit <- fit %>% column_to_rownames(var = "X") %>% as.matrix()

cFactor1a <- rowMeans(analysis[, 20:22])
cFactor1b <- rowMeans(analysis[, 26:32])
analysis <- cbind(analysis, cFactor1a, cFactor1b)

# Composite scores of differences via factor-based scores

cFactor_list <- list()
for (a in 1:7) {
  selected_questions <- rownames(fit)[fit[, a] >= 0.3]

  compScore <- analysis %>%
    select(all_of(selected_questions)) %>%
    rowMeans() %>%
    round(., 2)

  name <- paste0("cFactor", a)
  cFactor_list[[name]] <- compScore
}
analysis <- cbind(analysis, cFactor_list)

write.csv(analysis, "data/timeOrderedV2.csv")
