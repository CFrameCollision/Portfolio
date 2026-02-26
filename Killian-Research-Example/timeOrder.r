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

# check if p2 and p3 are, in essence, identical
a <- is.na(dataWhole$"P2") & is.na(dataWhole$"P3")
table(a)

b <- is.na(dataWhole$"P2") != is.na(dataWhole$"P3")
table(b)

which(is.na(dataWhole$"P2") & is.na(dataWhole$"P3"))
print("---------")
which(is.na(dataWhole$"P2") != is.na(dataWhole$"P3"))

# Sorting data to multiple occurences for SID & Email. Email increases n by 6 (0.67%)

n_occur <- data.frame(table(dataID$P2))
n_occurEmail <- data.frame(table(dataID$P3))

n_occur[n_occur$Freq > 1, ]
indices <- n_occur[which(n_occur$Freq > 1), ]

n_occurEmail[n_occurEmail$Freq > 1, ]
indicesEmail <- n_occurEmail[which(n_occurEmail$Freq > 1), ]

subData <- dataID %>% filter(P2 %in% indices$Var1 | P3 %in% indicesEmail$Var1)

# Check P3 and P2 mismatch
mismatched <- subData %>%
    group_by(P3) %>%
    filter(n_distinct(P2, na.rm = TRUE) > 1) %>%
    ungroup() %>%
    arrange(P3)

mismatchedIDs <- mismatched %>%
    dplyr::distinct(P3) %>%
    mutate(P2ID = as.character(row_number()))

subData <- subData %>%
    left_join(mismatchedIDs, by = "P3") %>%
    mutate(P2 = coalesce(P2ID, P2)) %>%
    select(-P2ID)

# table(subData$P2)

subData <- subData %>%
    group_by(P2) %>%
    dplyr::mutate(X = row_number()) %>%
    ungroup()

wideSubData <- subData %>%
    group_by(P2) %>%
    pivot_wider(
        id_cols = P2,
        names_from = X,
        values_from = c(Q1:Q35, activities.5.eqmore),
        names_sep = "."
    )

write.csv(
    wideSubData,
    file = "/home/cframecollisions/Desktop/repositories/Killian-Research/data/wideSubData.csv"
)

##### Differences between T2 and T1

data_long <- subData %>%
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

analysis <- analysis %>% select(P2, colnames(dataID)[2:32], activities.5.eqmore)

# Composite Scores

# Import and ready factor loadings
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

write.csv(analysis, "data/timeOrdered.csv")

# Adding differences between cFac3 and cFac7 & recodes acts to og scale. FIXME

analysisV2 <- analysis %>%
    mutate(
        cFac3.7Diffs = cFactor3 + cFactor7
    )
D11OG <- dataWhole %>% select(D11, P2) %>% filter(P2 %in% analysisV2$P2)

analysisV2 <- left_join(analysisV2, D11OG)
write.csv(analysisV2, "data/timeOrderedV2.csv")
